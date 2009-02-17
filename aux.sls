(library (spon aux)
  (export verbose?)
  (import (rnrs)
      (srfi :39))

  (define verbose? (make-parameter #f))
  )
