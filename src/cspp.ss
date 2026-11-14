(library (cspp (0 0 0 10 4 1))
  (export)
  ;; Compile and retain explicit libraries without adding their bindings to (cspp).
  (import (cspp chez)

          ;; Collections.
          (cspp array)
          (cspp bitvec)
          (cspp bittree)
          (cspp dlist)
          (cspp dset)
          (cspp hashset)
          (cspp heap)
          (cspp queue)
          (cspp stack)
          (cspp treemap)
          (cspp treeset)

          ;; Traversal and text processing.
          (cspp navigator)
          (cspp regex)
          (cspp transducer)

          ;; Files, system services, and native libraries.
          (cspp file)
          (cspp glob)
          (cspp hash)
          (cspp io)
          (cspp optional-library)
          (cspp path)
          (cspp system)
          (cspp uuid)

          ;; Parsing and data formats.
          (cspp format)
          (cspp parser)

          ;; Cryptography, networking, and concurrency.
          (cspp concurrency)
          (cspp crypto)
          (cspp net)

          ;; Application support and development tools.
          (cspp benchmark)
          (cspp cli)
          (cspp logging)
          (cspp rich)
          (cspp test)

          ;; Experimental fiber APIs.
          (cspp concurrency fiber)
          (cspp concurrency fiber-net))

  ;; The default language environment. Other libraries require explicit imports.
  (export (import (cspp chez)
                  (cspp utils)
                  (cspp control)
                  (cspp match)
                  (cspp adt)

                  (cspp list)
                  (cspp string)
                  (cspp vector)

                  (cspp for)
                  (cspp iter))))
