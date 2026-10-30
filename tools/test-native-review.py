#!/usr/bin/env python3
"""Check direct HTTP calls and crypto object contracts without entering unsafe native code."""
from pathlib import Path
import re
import subprocess
import sys

ROOT = Path(__file__).resolve().parent.parent


def contracts():
    source = (ROOT / 'src/cspp/crypto/ffi.ss').read_text()
    # Use the real exported wrappers, replacing only native procedures with a safe stub.
    # Every invalid-object case must raise pcheck, before either availability or native entry.
    script = r'''
(import (cspp chez) (cspp utils))
(define require-optional-library (lambda (who library) (values)))
(define replace-native
  (lambda (form)
    (cond
     [(and (pair? form) (eq? (car form) 'foreign-procedure))
      '(lambda arguments (error 'test-native-review "native-called"))]
     [(pair? form) (cons (replace-native (car form)) (replace-native (cdr form)))]
     [else form])))
(define evaluation-environment (copy-environment (environment '(cspp chez) '(cspp utils)) #t))
(eval '(define require-optional-library (lambda (who library) (values))) evaluation-environment)
(call-with-input-file "src/cspp/crypto/ffi.ss"
  (lambda (port)
    (let ([library (read port)])
      (for-each (lambda (form) (eval (replace-native form) evaluation-environment)) (cddddr library)))))
(define rejected-by-pcheck?
  (lambda (expression)
    (guard (condition
            [else (and (who-condition? condition) (eq? (condition-who condition) 'pcheck))])
      (eval expression evaluation-environment)
      #f)))
'''
    cases = [
        '(ffi-random-fill! "not-bytes" 0 0)',
        '(ffi-constant-time-eq "not-bytes" 0 0 #vu8() 0 0)',
        '(ffi-constant-time-eq #vu8() 0 0 "not-bytes" 0 0)',
        '(ffi-digest-bytevector \'sha256 \"not-bytes\" 0 0)',
        '(ffi-digest-string \'sha256 #vu8() 0 0)',
        '(ffi-blake3-bytevector \"not-bytes\" 0 0)',
        '(ffi-blake3-string #vu8() 0 0)',
        '(ffi-digest-output-size "sha256")', '(ffi-digest-block-size "sha256")',
        '(ffi-digest-state-create "sha256")',
        '(ffi-digest-state-update-bytevector! 0 "not-bytes" 0 0)',
        '(ffi-digest-state-update-string! 0 #vu8() 0 0)',
        '(ffi-hmac-state-create "sha256" #vu8() 0 0)',
        '(ffi-hmac-state-create \'sha256 "not-bytes" 0 0)',
        '(ffi-hmac-state-update-bytevector! 0 "not-bytes" 0 0)',
        '(ffi-hmac-state-update-string! 0 #vu8() 0 0)',
        '(ffi-cipher-key-size "aes-128-ctr")', '(ffi-cipher-iv-size "aes-128-ctr")',
        '(ffi-cipher-block-size "aes-128-ctr")',
        '(ffi-cipher-state-create "aes-128-ctr" 1 #vu8() 0 0 #vu8() 0 0)',
        '(ffi-cipher-state-create \'aes-128-ctr 1 "not-bytes" 0 0 #vu8() 0 0)',
        '(ffi-cipher-state-create \'aes-128-ctr 1 #vu8() 0 0 "not-bytes" 0 0)',
        '(ffi-pkey-generate "rsa" 2048 #f)', '(ffi-pkey-generate \'ecdsa 0 "p-256")',
        '(ffi-pkey-load-private-pem "not-bytes" 0 0)',
        '(ffi-pkey-load-public-pem "not-bytes" 0 0)',
        '(ffi-pkey-load-private-der "not-bytes" 0 0)',
        '(ffi-pkey-load-public-der "not-bytes" 0 0)',
        '(ffi-verify-message "rsa" \'sha256 0 #vu8() 0 0 #vu8() 0 0)',
        '(ffi-verify-message \'rsa "sha256" 0 #vu8() 0 0 #vu8() 0 0)',
        '(ffi-verify-message \'rsa \'sha256 0 "not-bytes" 0 0 #vu8() 0 0)',
        '(ffi-verify-message \'rsa \'sha256 0 #vu8() 0 0 "not-bytes" 0 0)',
        '(ffi-cert-load-pem "not-bytes" 0 0)', '(ffi-cert-load-der "not-bytes" 0 0)',
        '(ffi-cert-hostname-matches 0 "not-encoded")',
        '(ffi-cert-verify-chain 0 0 \'#() "not-encoded")',
        '(ffi-cert-verify-chain 0 0 #vu8() #f)',
        '(ffi-cert-verify-chain 0 0 \'#(1.0) #f)',
        '(ffi-cert-verify-chain 0 0 \'#(-1) #f)',
        '(ffi-cert-verify-chain 0 0 \'#(18446744073709551616) #f)',
    ]
    if '--enabled' in sys.argv:
        script = r'''
(import (cspp chez) (cspp crypto ffi) (cspp optional-library))
(unless (optional-library-available? (optional-library-info 'openssl))
  (error 'test-native-review "OpenSSL must be enabled for real contract verification"))
(define evaluation-environment (environment '(cspp chez) '(cspp crypto ffi)))
(define rejected-by-pcheck?
  (lambda (expression)
    (guard (condition
            [else (and (who-condition? condition) (eq? (condition-who condition) 'pcheck))])
      (eval expression evaluation-environment)
      #f)))
'''
    else:
        # Positive cases verify that optional false parameters still pass the actual checks.
        for case in ['(ffi-pkey-generate \'rsa 2048 #f)',
                     '(ffi-verify-message \'ed25519 #f 0 #vu8() 0 0 #vu8() 0 0)',
                     '(ffi-cert-hostname-matches 0 #f)',
                     '(ffi-cert-verify-chain 0 0 \'#() #f)']:
            script += f"\n(unless (guard (condition [else (and (who-condition? condition) (eq? (condition-who condition) 'test-native-review))]) (eval '{case} evaluation-environment) #f) (error 'test-native-review \"valid optional object rejected\"))\n"
    for case in cases:
        script += f'\n;; Reject the invalid Scheme object before safe-stub native entry: {case}.\n'
        script += f'(unless (rejected-by-pcheck? \'{case}) (error \'test-native-review "missing object contract" \'{case}))\n'
    result = subprocess.run(['./cspp', '--script=/dev/stdin'], input=script, text=True,
                            cwd=ROOT, capture_output=True)
    assert result.returncode == 0 and not result.stdout and not result.stderr, result.stdout + result.stderr


def direct_calls():
    source = (ROOT / 'src/cspp/c/net/lws_http.c').read_text()
    assert not re.search(r'\blws_\w+_fn\b|\bdynamic_(?:get_context|context_user|get_opaque_user_data)\b', source), \
        'HTTP backend retains a separate function-pointer ABI layer'
    assert 'ensure_lws_http_lifetime_context(void)' in source
    assert 'copied = lws_hdr_custom_copy(state->wsi,' in source
    assert 'defined(LWS_ROLE_H2)' in source
    for filename in ['ftp.c', 'grpc.c', 'websocket.c']:
        assert not re.search(r'typedef[^;]+\(\*\w+_fn\)', (ROOT / 'src/cspp/c/net' / filename).read_text())
    assert not re.search(r'typedef[^;]+\(\*\w+_fn\)', (ROOT / 'src/cspp/c/zlib_loader.c').read_text())


if __name__ == '__main__':
    contracts()
    direct_calls()
