(library (cspp private os)
  (export unix? windows? darwin?)
  (import (cspp chez)
          (cspp utils))


  ;; From ChezScheme cmacros.ss; subject to change.
  (define $machine-types
    '((windows . (i3nt    ti3nt
                  a6nt    ta6nt
                  arm64nt tarm64nt))
      (darwin . (a6osx     ta6osx
                 i3osx     ti3osx
                 ppc32osx  tppc32osx
                 arm64osx  tarm64osx))
      (unix . (i3le      ti3le
               i3fb      ti3fb
               i3ob      ti3ob
               i3nb      ti3nb
               a6le      ta6le
               a6fb      ta6fb
               a6ob      ta6ob
               a6nb      ta6nb
               ppc32le   tppc32le
               ppc32fb   tppc32fb
               ppc32ob   tppc32ob
               ppc32nb   tppc32nb
               arm32le   tarm32le
               arm32fb   tarm32fb
               arm32ob   tarm32ob
               arm32nb   tarm32nb
               arm64le   tarm64le
               arm64fb   tarm64fb
               arm64ob   tarm64ob
               arm64nb   tarm64nb
               rv64le    trv64le
               rv64fb    trv64fb
               rv64ob    trv64ob
               rv64nb    trv64nb
               la64le    tla64le))
      (others . (any
                 pb        tpb
                 pb32l     tpb32l
                 pb32b     tpb32b
                 pb64l     tpb64l
                 pb64b     tpb64b
                 i3s2      ti3s2
                 i3qnx     ti3qnx
                 i3gnu     ti3gnu
                 a6s2      ta6s2))))

  (define unix?
    (lambda ()
      (bool (memq (machine-type) (cdr (assq 'unix $machine-types))))))
  (define windows?
    (lambda ()
      (bool (memq (machine-type) (cdr (assq 'windows $machine-types))))))
  (define darwin?
    (lambda ()
      (bool (memq (machine-type) (cdr (assq 'darwin $machine-types))))))


  )
