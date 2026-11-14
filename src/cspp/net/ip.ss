(library (cspp net ip)
  (export ip-address?
          ipv4-address?
          ipv6-address?
          string->ip-address
          ip-address->string
          ip-address-version
          ip-address-loopback?
          ip-address-private?
          ip-address-multicast?
          ip-address-unspecified?
          ip-address-broadcast?
          ip-address-carrier-grade-nat?
          ip-address-benchmarking?
          ip-address-discard-only?
          ip-address-unique-local?
          ip-address-site-local?
          ip-address-reserved?
          ip-address-multicast-scope
          ip-address-mapped-ipv4?
          ip-address-unmap-ipv4
          ip-address-link-local?
          ip-address-documentation?
          cidr-parse
          cidr?
          cidr-contains?
          cidr-split
          cidr-merge
          cidr-address-count
          cidr-overlaps?
          cidr-network-address
          cidr-prefix-length)
  (import (cspp chez)
          (cspp utils)
          (cspp string))

  #|record:ip-address
The `ip-address` record is an immutable canonical IPv4 or IPv6 address.
Version is `4` or `6`, and the internal bytes are four or sixteen network-order octets.
Use `ip-address->string` for a stable textual representation.
|#
  (define-record-type (ip-address %make-ip-address ip-address?)
    (sealed #t)
    (opaque #f)
    (fields (immutable version ip-address-version)
            (immutable bytes ip-address-bytes)))

  #|record:cidr
The `cidr` record is an immutable normalized IP network.
Its network-address has all host bits cleared, and prefix-length is valid for that address family.
|#
  (define-record-type (cidr %make-cidr cidr?)
    (sealed #t)
    (opaque #f)
    (fields (immutable network-address cidr-network-address)
            (immutable prefix-length cidr-prefix-length)))

  (define make-zero-list
    (lambda (n)
      (let loop ([n n] [out '()])
        (if (fx<= n 0)
            out
            (loop (fx1- n) (cons 0 out))))))

  (define join-strings
    (lambda (parts sep)
      (if (null? parts)
          ""
          (let loop ([parts (cdr parts)] [out (car parts)])
            (if (null? parts)
                out
                (loop (cdr parts) (string-append out sep (car parts))))))))

  (define parse-decimal-byte
    (lambda (str)
      (let ([n (string->number str 10)])
        (and n
             (integer? n)
             (exact? n)
             (fx<= 0 n)
             (fx<= n 255)
             n))))

  (define parse-hex-word
    (lambda (str)
      (and (not (string=? str ""))
           (fx<= (string-length str) 4)
           (let ([n (string->number str 16)])
             (and n
                  (integer? n)
                  (exact? n)
                  (fx<= 0 n)
                  (fx<= n #xffff)
                  n)))))

  (define parse-ipv4-bytevector
    (lambda (str)
      (let ([parts (string-split str #\.)])
        (and (fx= (length parts) 4)
             (let ([bv (make-bytevector 4 0)])
               (let loop ([i 0] [parts parts])
                 (if (null? parts)
                     bv
                     (let ([octet (parse-decimal-byte (car parts))])
                       (and octet
                            (begin
                              (bytevector-u8-set! bv i octet)
                              (loop (fx1+ i) (cdr parts))))))))))))

  (define parse-ipv6-piece
    (lambda (piece)
      (if (string=? piece "")
          '()
          (let ([segments (string-split piece #\:)])
            (let loop ([segments segments] [out '()])
              (cond
                ((null? segments) (reverse out))
                (else
                 (let ([segment (car segments)])
                   (cond
                     ((string=? segment "") #f)
                     ((string-contains? segment ".")
                      (and (null? (cdr segments))
                           (let ([ipv4 (parse-ipv4-bytevector segment)])
                             (and ipv4
                                  (reverse
                                   (cons (fxlogor (fxsll (bytevector-u8-ref ipv4 2) 8)
                                                  (bytevector-u8-ref ipv4 3))
                                         (cons (fxlogor (fxsll (bytevector-u8-ref ipv4 0) 8)
                                                        (bytevector-u8-ref ipv4 1))
                                               out)))))))
                     (else
                      (let ([word (parse-hex-word segment)])
                        (and word
                             (loop (cdr segments) (cons word out))))))))))))))

  (define hextets->ipv6-bytevector
    (lambda (hextets)
      (and (fx= (length hextets) 8)
           (let ([bv (make-bytevector 16 0)])
             (let loop ([i 0] [hextets hextets])
               (if (null? hextets)
                   bv
                   (let ([word (car hextets)])
                     (bytevector-u8-set! bv i (fxlogand (fxsrl word 8) #xff))
                     (bytevector-u8-set! bv (fx1+ i) (fxlogand word #xff))
                     (loop (fx+ i 2) (cdr hextets)))))))))

  (define parse-ipv6-bytevector
    (lambda (str)
      (let ([double-colon (string-search-all str "::")])
        (cond
         [(and double-colon (not (null? (cdr double-colon)))) #f]
         [double-colon
          (let* ([pos (car double-colon)]
                 [left (substring str 0 pos)]
                 [right (substring str (fx+ pos 2) (string-length str))]
                 [lhs (parse-ipv6-piece left)]
                 [rhs (parse-ipv6-piece right)])
            (and lhs
                 rhs
                 (let ([missing (fx- 8 (fx+ (length lhs) (length rhs)))])
                   (and (fx>= missing 1)
                        (hextets->ipv6-bytevector
                         (append lhs (make-zero-list missing) rhs))))))]
         [else
          (let ([hextets (parse-ipv6-piece str)])
            (and hextets
                 (fx= (length hextets) 8)
                 (hextets->ipv6-bytevector hextets)))]))))

  (define ipv6-hextets
    (lambda (bv)
      (let loop ([i 0] [out '()])
        (if (fx= i 16)
            (reverse out)
            (loop (fx+ i 2)
                  (cons (fxlogor (fxsll (bytevector-u8-ref bv i) 8)
                                 (bytevector-u8-ref bv (fx1+ i)))
                        out))))))

  (define longest-zero-run
    (lambda (hextets)
      (let loop ([rest hextets] [i 0]
                 [best-start #f] [best-len 0]
                 [cur-start #f] [cur-len 0])
        (cond
         [(null? rest)
          (if (and cur-start (fx> cur-len best-len))
              (values cur-start cur-len)
              (values best-start best-len))]
         [(fx= (car rest) 0)
          (if cur-start
              (loop (cdr rest) (fx1+ i) best-start best-len cur-start (fx1+ cur-len))
              (loop (cdr rest) (fx1+ i) best-start best-len i 1))]
         [else
          (if (and cur-start (fx> cur-len best-len))
              (loop (cdr rest) (fx1+ i) cur-start cur-len #f 0)
              (loop (cdr rest) (fx1+ i) best-start best-len #f 0))]))))

  (define take-list
    (lambda (ls n)
      (if (or (fx<= n 0) (null? ls))
          '()
          (cons (car ls) (take-list (cdr ls) (fx1- n))))))

  (define drop-list
    (lambda (ls n)
      (if (or (fx<= n 0) (null? ls))
          ls
          (drop-list (cdr ls) (fx1- n)))))

  (define format-ipv6
    (lambda (bv)
      (let* ([hextets (ipv6-hextets bv)]
             [parts (map (lambda (word) (string-downcase (number->string word 16))) hextets)])
        (let-values ([(best-start best-len) (longest-zero-run hextets)])
          (if (and best-start (fx>= best-len 2))
              (let ([left (take-list parts best-start)]
                    [right (drop-list parts (fx+ best-start best-len))])
                (cond
                 [(and (null? left) (null? right)) "::"]
                 [(null? left) (string-append "::" (join-strings right ":"))]
                 [(null? right) (string-append (join-strings left ":") "::")]
                 [else
                  (string-append (join-strings left ":")
                                 "::"
                                 (join-strings right ":"))]))
              (join-strings parts ":"))))))

  (define mask-byte
    (lambda (x keep-bits)
      (cond
       [(fx<= keep-bits 0) 0]
       [(fx>= keep-bits 8) x]
       [else
        (fxlogand x
                  (fxsll (fx1- (fxsll 1 keep-bits))
                         (fx- 8 keep-bits)))])))

  (define ip-mask-bytevector
    (lambda (bv prefix)
      (let* ([len (bytevector-length bv)]
             [out (make-bytevector len 0)])
        (let loop ([i 0] [remaining prefix])
          (if (fx= i len)
              out
              (let ([value (bytevector-u8-ref bv i)])
                (cond
                 [(fx>= remaining 8)
                  (bytevector-u8-set! out i value)
                  (loop (fx1+ i) (fx- remaining 8))]
                 [else
                  (bytevector-u8-set! out i (mask-byte value remaining))
                  (loop (fx1+ i) 0)])))))))

  (define bytevector-prefix=?
    (lambda (a b prefix)
      (let ([ma (ip-mask-bytevector a prefix)]
            [mb (ip-mask-bytevector b prefix)])
        (let loop ([i 0])
          (or (fx= i (bytevector-length ma))
              (and (fx= (bytevector-u8-ref ma i) (bytevector-u8-ref mb i))
                   (loop (fx1+ i))))))))

  (define cidr-for
    (lambda (text)
      (cidr-parse text)))

  (define flip-bit
    (lambda (bv bit)
      (let* ([index (fxquotient bit 8)]
             [offset (fx- 7 (fxmod bit 8))]
             [out (bytevector-copy bv)])
        (bytevector-u8-set! out index
                            (fxlogxor (bytevector-u8-ref out index)
                                      (fxsll 1 offset)))
        out)))

  #|proc:ipv4-address?
The `ipv4-address?` procedure returns whether the given value is an IPv4 address object.
|#
  (define-who ipv4-address?
    (lambda (x)
      (and (ip-address? x)
           (fx= (ip-address-version x) 4))))

  #|proc:ipv6-address?
The `ipv6-address?` procedure returns whether the given value is an IPv6 address object.
|#
  (define-who ipv6-address?
    (lambda (x)
      (and (ip-address? x)
           (fx= (ip-address-version x) 6))))

  #|proc:string->ip-address
The `string->ip-address` procedure parses an IPv4 or IPv6 address string and returns an address
object, or `#f` on failure.
|#
  (define-who string->ip-address
    (lambda (str)
      (pcheck ([string? str])
              (cond
               [(parse-ipv4-bytevector str) => (lambda (bv) (%make-ip-address 4 bv))]
               [(parse-ipv6-bytevector str) => (lambda (bv) (%make-ip-address 6 bv))]
               [else #f]))))

  #|proc:ip-address->string
The `ip-address->string` procedure renders an IP address object to its canonical textual form.
|#
  (define-who ip-address->string
    (lambda (ip)
      (pcheck ([ip-address? ip])
              (let ([bv (ip-address-bytes ip)])
                (if (fx= (ip-address-version ip) 4)
                    (join-strings
                     (list (number->string (bytevector-u8-ref bv 0))
                           (number->string (bytevector-u8-ref bv 1))
                           (number->string (bytevector-u8-ref bv 2))
                           (number->string (bytevector-u8-ref bv 3)))
                     ".")
                    (format-ipv6 bv))))))

  #|proc:ip-address-loopback?
The `ip-address-loopback?` procedure returns whether an IP address is a loopback address.
|#
  (define-who ip-address-loopback?
    (lambda (ip)
      (pcheck ([ip-address? ip])
              (let ([bv (ip-address-bytes ip)])
                (if (fx= (ip-address-version ip) 4)
                    (fx= (bytevector-u8-ref bv 0) 127)
                    (and (let loop ([i 0])
                           (if (fx= i 15)
                               #t
                               (and (fx= (bytevector-u8-ref bv i) 0)
                                    (loop (fx1+ i)))))
                         (fx= (bytevector-u8-ref bv 15) 1)))))))

  #|proc:ip-address-private?
The `ip-address-private?` procedure returns whether an IP address is in a private-use range.
|#
  (define-who ip-address-private?
    (lambda (ip)
      (pcheck ([ip-address? ip])
              (let ([bv (ip-address-bytes ip)])
                (if (fx= (ip-address-version ip) 4)
                    (let ([a (bytevector-u8-ref bv 0)]
                          [b (bytevector-u8-ref bv 1)])
                      (or (fx= a 10)
                          (and (fx= a 172)
                               (fx<= 16 b)
                               (fx<= b 31))
                          (and (fx= a 192)
                               (fx= b 168))))
                    (let ([a (bytevector-u8-ref bv 0)])
                      (or (fx= (fxlogand a #xfe) #xfc)
                          (fx= a #xfe))))))))

  #|proc:ip-address-multicast?
The `ip-address-multicast?` procedure returns whether an IP address is a multicast address.
|#
  (define-who ip-address-multicast?
    (lambda (ip)
      (pcheck ([ip-address? ip])
              (let ([bv (ip-address-bytes ip)])
                (if (fx= (ip-address-version ip) 4)
                    (let ([a (bytevector-u8-ref bv 0)])
                      (and (fx<= 224 a) (fx<= a 239)))
                    (fx= (bytevector-u8-ref bv 0) #xff))))))

  (define ip-in-cidr?
    (lambda (text ip)
      (cidr-contains? (cidr-for text) ip)))

  #|proc:ip-address-unspecified?
The `ip-address-unspecified?` procedure reports whether `ip` is the all-zero unspecified address.
|#
  (define-who ip-address-unspecified?
    (lambda (ip)
      (pcheck ([ip-address? ip])
        (ip-in-cidr? (if (ipv4-address? ip) "0.0.0.0/32" "::/128") ip))))

  #|proc:ip-address-broadcast?
The `ip-address-broadcast?` procedure reports whether `ip` is the IPv4 limited broadcast address.
|#
  (define-who ip-address-broadcast?
    (lambda (ip)
      (pcheck ([ip-address? ip])
        (and (ipv4-address? ip) (ip-in-cidr? "255.255.255.255/32" ip)))))

  #|proc:ip-address-carrier-grade-nat?
The `ip-address-carrier-grade-nat?` procedure reports whether `ip` is in 100.64.0.0/10.
|#
  (define-who ip-address-carrier-grade-nat?
    (lambda (ip)
      (pcheck ([ip-address? ip])
        (and (ipv4-address? ip) (ip-in-cidr? "100.64.0.0/10" ip)))))

  #|proc:ip-address-benchmarking?
The `ip-address-benchmarking?` procedure reports whether `ip` is in a benchmarking range.
|#
  (define-who ip-address-benchmarking?
    (lambda (ip)
      (pcheck ([ip-address? ip])
        (ip-in-cidr? (if (ipv4-address? ip) "198.18.0.0/15" "2001:2::/48") ip))))

  #|proc:ip-address-discard-only?
The `ip-address-discard-only?` procedure reports whether `ip` is in the IPv6 discard prefix.
|#
  (define-who ip-address-discard-only?
    (lambda (ip)
      (pcheck ([ip-address? ip])
        (and (ipv6-address? ip) (ip-in-cidr? "100::/64" ip)))))

  #|proc:ip-address-unique-local?
The `ip-address-unique-local?` procedure reports whether `ip` is in fc00::/7.
|#
  (define-who ip-address-unique-local?
    (lambda (ip)
      (pcheck ([ip-address? ip])
        (and (ipv6-address? ip) (ip-in-cidr? "fc00::/7" ip)))))

  #|proc:ip-address-site-local?
The `ip-address-site-local?` procedure reports whether `ip` is in the deprecated fec0::/10 range.
|#
  (define-who ip-address-site-local?
    (lambda (ip)
      (pcheck ([ip-address? ip])
        (and (ipv6-address? ip) (ip-in-cidr? "fec0::/10" ip)))))

  #|proc:ip-address-reserved?
The `ip-address-reserved?` procedure reports whether `ip` is in a reserved address range.
|#
  (define-who ip-address-reserved?
    (lambda (ip)
      (pcheck ([ip-address? ip])
        (and (ipv4-address? ip) (ip-in-cidr? "240.0.0.0/4" ip)))))

  #|proc:ip-address-multicast-scope
The `ip-address-multicast-scope` procedure returns the multicast scope symbol for `ip`, or `#f`.
IPv6 scopes follow RFC 7346; IPv4 link-local and administratively scoped ranges are recognized.
|#
  (define-who ip-address-multicast-scope
    (lambda (ip)
      (pcheck ([ip-address? ip])
        (and (ip-address-multicast? ip)
             (if (ipv4-address? ip)
                 (let ([bv (ip-address-bytes ip)])
                   (cond
                    [(and (= (bytevector-u8-ref bv 0) 224)
                          (< (bytevector-u8-ref bv 1) 1)) 'link-local]
                    [(= (bytevector-u8-ref bv 0) 239) 'administrative]
                    [else 'global]))
                 (case (fxlogand (bytevector-u8-ref (ip-address-bytes ip) 1) #x0f)
                   [(1) 'interface-local]
                   [(2) 'link-local]
                   [(3) 'realm-local]
                   [(4) 'administrative]
                   [(5) 'site-local]
                   [(8) 'organization-local]
                   [(14) 'global]
                   [else 'reserved]))))))

  #|proc:ip-address-mapped-ipv4?
The `ip-address-mapped-ipv4?` procedure reports whether `ip` is an IPv4-mapped IPv6 address.
The return value is boolean.
|#
  (define-who ip-address-mapped-ipv4?
    (lambda (ip)
      (pcheck ([ip-address? ip])
        (and (ipv6-address? ip)
             (let ([bv (ip-address-bytes ip)])
               (let loop ([i 0])
                 (or (fx= i 10)
                     (and (zero? (bytevector-u8-ref bv i)) (loop (fx1+ i)))))
               (= #xff (bytevector-u8-ref bv 10))
               (= #xff (bytevector-u8-ref bv 11)))))))

  #|proc:ip-address-unmap-ipv4
The `ip-address-unmap-ipv4` procedure converts a mapped IPv6 `ip` to an IPv4 address.
It returns an IPv4 address, or `#f` when `ip` is not mapped.
|#
  (define-who ip-address-unmap-ipv4
    (lambda (ip)
      (pcheck ([ip-address? ip])
        (and (ip-address-mapped-ipv4? ip)
             (let ([bv (ip-address-bytes ip)] [out (make-bytevector 4 0)])
               (bytevector-copy! bv 12 out 0 4)
               (%make-ip-address 4 out))))))

  #|proc:ip-address-link-local?
The `ip-address-link-local?` procedure reports whether `ip` is in a link-local range.
|#
  (define-who ip-address-link-local?
    (lambda (ip)
      (pcheck ([ip-address? ip])
        (or (cidr-contains? (cidr-for "169.254.0.0/16") ip)
            (cidr-contains? (cidr-for "fe80::/10") ip)))))

  #|proc:ip-address-documentation?
The `ip-address-documentation?` procedure reports whether `ip` is in an RFC documentation range.
|#
  (define-who ip-address-documentation?
    (lambda (ip)
      (pcheck ([ip-address? ip])
        (or (cidr-contains? (cidr-for "192.0.2.0/24") ip)
            (cidr-contains? (cidr-for "198.51.100.0/24") ip)
            (cidr-contains? (cidr-for "203.0.113.0/24") ip)
            (cidr-contains? (cidr-for "2001:db8::/32") ip)))))

  #|proc:cidr-parse
The `cidr-parse` procedure parses a CIDR string and returns a CIDR object, or `#f` on failure.
|#
  (define-who cidr-parse
    (lambda (str)
      (pcheck ([string? str])
              (let ([parts (string-split str #\/)])
                (and (fx= (length parts) 2)
                     (let ([ip (string->ip-address (car parts))]
                           [prefix (string->number (cadr parts) 10)])
                       (and ip
                             prefix
                             (integer? prefix)
                             (exact? prefix)
                             (fx<= 0 prefix)
                             (fx<= prefix (if (fx= (ip-address-version ip) 4) 32 128))
                             (let ([network (ip-mask-bytevector (ip-address-bytes ip) prefix)])
                               (%make-cidr (%make-ip-address (ip-address-version ip) network)
                                          prefix)))))))))

  #|proc:cidr-contains?
The `cidr-contains?` procedure returns whether an IP address belongs to a CIDR range.
|#
  (define-who cidr-contains?
    (lambda (range ip)
      (pcheck ([cidr? range] [ip-address? ip])
              (and (fx= (ip-address-version (cidr-network-address range))
                        (ip-address-version ip))
                   (bytevector-prefix=?
                    (ip-address-bytes (cidr-network-address range))
                    (ip-address-bytes ip)
                    (cidr-prefix-length range))))))

  #|proc:cidr-address-count
The `cidr-address-count` procedure returns the exact number of addresses represented by `range`.
|#
  (define-who cidr-address-count
    (lambda (range)
      (pcheck ([cidr? range])
        (expt 2 (- (if (= (ip-address-version (cidr-network-address range)) 4) 32 128)
                   (cidr-prefix-length range))))))

  #|proc:cidr-overlaps?
The `cidr-overlaps?` procedure reports whether same-version CIDRs `left` and `right` intersect.
|#
  (define-who cidr-overlaps?
    (lambda (left right)
      (pcheck ([cidr? left right])
        (and (= (ip-address-version (cidr-network-address left))
                (ip-address-version (cidr-network-address right)))
             (let ([prefix (min (cidr-prefix-length left) (cidr-prefix-length right))])
               (bytevector-prefix=?
                (ip-address-bytes (cidr-network-address left))
                (ip-address-bytes (cidr-network-address right)) prefix))))))

  #|proc:cidr-split
The `cidr-split` procedure divides non-host `range` into its two equal child CIDRs.
The return value is a two-element list, or `#f` for a host-length range.
|#
  (define-who cidr-split
    (lambda (range)
      (pcheck ([cidr? range])
        (let ([prefix (cidr-prefix-length range)]
              [limit (if (= (ip-address-version (cidr-network-address range)) 4) 32 128)])
          (and (< prefix limit)
               (let* ([next (fx1+ prefix)]
                      [network (ip-address-bytes (cidr-network-address range))]
                      [child (%make-cidr (%make-ip-address
                                          (ip-address-version (cidr-network-address range))
                                          network) next)]
                      [sibling (%make-cidr (%make-ip-address
                                            (ip-address-version (cidr-network-address range))
                                            (flip-bit network prefix)) next)])
                 (list child sibling)))))))

  #|proc:cidr-merge
The `cidr-merge` procedure merges adjacent sibling CIDRs `left` and `right`.
The return value is the parent CIDR, or `#f` when they cannot be merged.
|#
  (define-who cidr-merge
    (lambda (left right)
      (pcheck ([cidr? left right])
        (and (= (ip-address-version (cidr-network-address left))
                (ip-address-version (cidr-network-address right)))
             (= (cidr-prefix-length left) (cidr-prefix-length right))
             (> (cidr-prefix-length left) 0)
             (let ([parent-prefix (fx1- (cidr-prefix-length left))])
               (and (bytevector-prefix=?
                     (ip-address-bytes (cidr-network-address left))
                     (ip-address-bytes (cidr-network-address right)) parent-prefix)
                    (not (bytevector-prefix=?
                          (ip-address-bytes (cidr-network-address left))
                          (ip-address-bytes (cidr-network-address right))
                          (cidr-prefix-length left)))
                    (%make-cidr
                     (%make-ip-address
                      (ip-address-version (cidr-network-address left))
                      (ip-mask-bytevector
                       (ip-address-bytes (cidr-network-address left)) parent-prefix))
                     parent-prefix)))))))
  )
