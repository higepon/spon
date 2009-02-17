(library (spon aux)
  (export verbose? system-name)
  (import (rnrs)
      (srfi :39))

  (define system-name "spon")
  (define verbose? (make-parameter #f))
  )
