#!chezscheme
(library (cspp rich renderable)
  (export rich-register-renderer!
          rich-renderer-for
          rich-renderable?
          ;; Internal sizing support, intentionally omitted from the rich facade.
          rich-register-container-renderer!
          rich-layout-render-cache)
  (import (cspp chez)
          (cspp utils)
          (cspp rich private common))

  (define $renderers '())

  #|proc:rich-layout-render-cache
  The internal `rich-layout-render-cache` parameter holds an identity hashtable during
  container sizing, or `#f` otherwise. Custom renderers reuse their first result while
  built-in containers reflow at assigned widths. The cache lasts for one sizing operation.
  |#
  (define rich-layout-render-cache
    (make-parameter #f
      (lambda (cache)
        (pcheck ([(lambda (value) (or (not value) (hashtable? value))) cache]) cache))))

  (define $cache-ref!
    (lambda (table key make-value)
      (or (hashtable-ref table key #f)
          (let ([value (make-value)])
            (hashtable-set! table key value)
            value))))

  ;; A registration owns its cache key. Containers cache each width and decoration policy.
  (define $cached-renderer
    (lambda (renderer width-sensitive?)
      (let ([key (list renderer)])
        (lambda (value)
          (let ([cache (rich-layout-render-cache)])
            (if (not cache)
                (renderer value)
                (let* ([values ($cache-ref! cache key make-eq-hashtable)]
                       [results (if width-sensitive?
                                    ($cache-ref! values value
                                                 (lambda () (make-hashtable equal-hash equal?)))
                                    values)]
                       [result-key (if width-sensitive?
                                       (cons (rich-render-width) (rich-render-ascii-only?)) value)])
                  (if (hashtable-contains? results result-key)
                      (hashtable-ref results result-key #f)
                      (let ([result (renderer value)])
                        (hashtable-set! results result-key result)
                        result)))))))))

  #|proc:rich-register-container-renderer!
  The internal `rich-register-container-renderer!` registers unary predicate `pred` and
  unary `renderer`, which returns a string or segment lines for matching values. Container
  renderers must be safe to call again when assigned widths change. During sizing, results
  are reused for matching values, available widths, and ASCII policy. Returns unspecified.
  |#
  (define rich-register-container-renderer!
    (lambda (pred renderer)
      (pcheck ([procedure? pred renderer])
        (set! $renderers (cons (cons pred ($cached-renderer renderer #t)) $renderers)))))

  #|proc:rich-register-renderer!
  The `rich-register-renderer!` procedure registers unary predicate `pred` and unary
  procedure `renderer` for future rich output. The predicate recognizes supported values;
  the renderer returns a string or segment lines for its argument. During container sizing,
  each matching value is rendered once and its result is reused for reflow. Returns unspecified.
  |#
  (define rich-register-renderer!
    (lambda (pred renderer)
      (pcheck ([procedure? pred renderer])
        (set! $renderers (cons (cons pred ($cached-renderer renderer #f)) $renderers)))))

  #|proc:rich-renderer-for
  The `rich-renderer-for` procedure returns the renderer registered for
  `value`, or `#f` when none is registered.
  |#
  (define rich-renderer-for
    (lambda (value)
      (let loop ([renderers $renderers])
        (cond [(null? renderers) #f]
              [((caar renderers) value) (cdar renderers)]
              [else (loop (cdr renderers))]))))

  #|proc:rich-renderable?
  The `rich-renderable?` procedure returns `#t` when a renderer is registered
  for `value`, and `#f` otherwise.
  |#
  (define rich-renderable?
    (lambda (value)
      (and (rich-renderer-for value) #t))))
