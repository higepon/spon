(library (spon base)
  (export system-name verbose? quiet?)
  (import (rnrs)
          (srfi :39))

  (define system-name "spon")

  (define verbose? (make-parameter #f))

  ;; (quiet? #t) implies (verbose? #f).
  (define quiet? (make-parameter #f
                   (lambda (v)
                     (when v
                       (verbose? #f))
                     v)))
  )
