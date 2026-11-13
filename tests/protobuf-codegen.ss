(import (cspp) (cspp system process))
(load "generated/file-transfer.pb.ss")
(load "generated/codegen-features.pb.ss")

(define generated-environment
  (environment '(cspp) '(cspp examples transfer file-transfer protobuf)))

(define generated-eval
  (lambda (expression)
    (eval expression generated-environment)))

(define feature-environment
  (environment '(cspp)
               '(cspp tests codegen codegen-features protobuf)))

(define feature-eval
  (lambda (expression)
    (eval expression feature-environment)))

(mat protobuf-codegen
     (generated-eval
      '(let* ([message (make-file-chunk "payload.bin" 4096 #vu8(1 2 3 4)
                                        #vu8(9 8 7 6) #t)]
              [encoded (file-chunk-encode message)]
              [decoded (bytevector->file-chunk encoded)])
         (and (file-chunk? decoded)
              (string=? "payload.bin" (file-chunk-name decoded))
              (= 4096 (file-chunk-offset decoded))
              (equal? #vu8(1 2 3 4) (file-chunk-data decoded))
              (equal? #vu8(9 8 7 6) (file-chunk-sha256 decoded))
              (file-chunk-done? decoded)
              (= (bytevector-length encoded) (file-chunk-encoded-size message)))))

     (generated-eval
      '(let* ([known (file-chunk-encode (make-file-chunk "x" 0 #vu8() #vu8() #f))]
              [with-unknown (let-values ([(port get) (open-bytevector-output-port)])
                              (put-bytevector port known)
                              (put-bytevector port #vu8(152 6 7))
                              (get))]
              [decoded (bytevector->file-chunk with-unknown)])
         (and (equal? '#(#vu8(152 6 7)) (file-chunk-unknown-fields decoded))
              (equal? with-unknown (file-chunk-encode decoded)))))

     (generated-eval
      '(let* ([message (make-transfer-result 8192 #vu8(4 3 2 1))]
              [decoded (bytevector->transfer-result (transfer-result-encode message))])
         (and (= 8192 (transfer-result-size decoded))
              (equal? #vu8(4 3 2 1) (transfer-result-sha256 decoded)))))

     (string=? "/cspp.examples.transfer.FileTransfer/Upload"
               (generated-eval 'file-transfer-upload-method))

     (string=? "/cspp.examples.transfer.FileTransfer/Download"
               (generated-eval 'file-transfer-download-method))

     (feature-eval
      '(let ([counters (make-hashtable string-hash string=?)])
         (hashtable-set! counters "ok" 7)
         (let* ([payload (make-envelope-payload -9)]
                [message (make-envelope 0 #t '#("a" "b") 1 #t payload counters
                                        '(text . "hello"))]
                [encoded (envelope-encode message)]
                [decoded (bytevector->envelope encoded)])
           (and (envelope-id-present? decoded)
                (= 0 (envelope-id decoded))
                (equal? '#("a" "b") (envelope-tags decoded))
                (envelope-state-present? decoded)
                (= envelope-state-state-ready (envelope-state decoded))
                (= -9 (envelope-payload-delta (envelope-payload decoded)))
                (= 7 (hashtable-ref (envelope-counters decoded) "ok" #f))
                (equal? '(text . "hello") (envelope-content decoded))
                (= (bytevector-length encoded) (envelope-encoded-size message))))))

     (feature-eval
      '(and (string=? "/cspp.tests.codegen.Shapes/Unary" shapes-unary-method)
            (string=? "/cspp.tests.codegen.Shapes/Server" shapes-server-method)
            (string=? "/cspp.tests.codegen.Shapes/Client" shapes-client-method)
            (string=? "/cspp.tests.codegen.Shapes/Bidi" shapes-bidi-method)
            (positive? (bytevector-length protobuf-file-descriptor-bytes)))))

(mat protobuf-generator-process

     ;; A request with an empty parameter and no target files has an empty response.
     (let ([result
             (process-capture "../tools/protoc-gen-cspp"
               :stdin #vu8(18 0) :encoding binary :stderr capture)])
       (and (process-exit-success? (process-result-status result))
            (zero? (bytevector-length (process-result-stdout result)))
            (zero? (bytevector-length (process-result-stderr result))))))
