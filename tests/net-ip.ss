(import (cspp)
        (cspp net))

(mat net-ip-special
     (let ([mapped (string->ip-address "::ffff:192.0.2.1")])
       (and (ip-address-mapped-ipv4? mapped)
            (string=? "192.0.2.1"
                      (ip-address->string (ip-address-unmap-ipv4 mapped)))
            (ip-address-link-local? (string->ip-address "fe80::1"))
            (ip-address-documentation? (string->ip-address "192.0.2.1")))))

(mat net-ip-cidr-algebra
     (let* ([parent (cidr-parse "192.0.2.0/24")]
            [children (cidr-split parent)]
            [left (car children)]
            [right (cadr children)])
       (and (= 256 (cidr-address-count parent))
            (cidr-contains? parent (string->ip-address "192.0.2.200"))
            (cidr-overlaps? left parent)
            (not (cidr-overlaps? left (cidr-parse "192.0.3.0/24")))
            (cidr-merge left right)
            (= 24 (cidr-prefix-length (cidr-merge left right)))
            (not (cidr-merge left left)))))

(mat net-ip-special-range-table
     (ip-address-unspecified? (string->ip-address "0.0.0.0"))
     (ip-address-unspecified? (string->ip-address "::"))
     (ip-address-broadcast? (string->ip-address "255.255.255.255"))
     (ip-address-carrier-grade-nat? (string->ip-address "100.127.255.254"))
     (ip-address-benchmarking? (string->ip-address "198.19.1.1"))
     (ip-address-benchmarking? (string->ip-address "2001:2::1"))
     (ip-address-discard-only? (string->ip-address "100::1"))
     (ip-address-unique-local? (string->ip-address "fd12:3456::1"))
     (ip-address-site-local? (string->ip-address "fec0::1"))
     (ip-address-reserved? (string->ip-address "250.1.2.3"))
     (eq? 'link-local
          (ip-address-multicast-scope (string->ip-address "224.0.0.251")))
     (eq? 'administrative
          (ip-address-multicast-scope (string->ip-address "239.1.2.3")))
     (eq? 'interface-local
          (ip-address-multicast-scope (string->ip-address "ff01::1")))
     (eq? 'global
          (ip-address-multicast-scope (string->ip-address "ff0e::1")))
     (not (ip-address-carrier-grade-nat? (string->ip-address "100.128.0.1")))
     (not (ip-address-site-local? (string->ip-address "fe80::1"))))

(define deterministic-address-texts
  '("0.0.0.0" "1.2.3.4" "10.255.0.1" "127.0.0.1" "192.0.2.255"
    "255.255.255.255" "::" "::1" "2001:db8::dead:beef" "fe80::1234"
    "ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff"))

(mat net-ip-deterministic-properties
     (andmap
      (lambda (text)
        (let* ([address (string->ip-address text)]
               [rendered (ip-address->string address)]
               [round-trip (string->ip-address rendered)]
               [host-range (cidr-parse
                            (string-append rendered
                                           (if (ipv4-address? address) "/32" "/128")))])
          (and round-trip
               (string=? rendered (ip-address->string round-trip))
               (cidr-contains? host-range address))))
      deterministic-address-texts)
     (andmap
      (lambda (text)
        (let* ([parent (cidr-parse text)]
               [children (cidr-split parent)]
               [left (car children)]
               [right (cadr children)]
               [merged (cidr-merge left right)])
          (and (= (cidr-address-count parent)
                  (+ (cidr-address-count left) (cidr-address-count right)))
               merged
               (= (cidr-prefix-length parent) (cidr-prefix-length merged))
               (cidr-contains? parent (cidr-network-address left))
               (cidr-contains? parent (cidr-network-address right)))))
      '("0.0.0.0/0" "192.0.2.0/24" "198.51.100.128/25"
        "::/0" "2001:db8::/32" "fd00:1234::/64")))
