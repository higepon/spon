(import (rnrs)
        (srfi :48)
        (srfi :39)
        (spon tools))

(define (main args)
  (cond
   [(null? (cdr args))
    (display (format "ERROR ~a: package name not specified\n" system-name) (current-error-port))
    (exit -1)]
   [else
    (parameterize ((quiet? #t))
      (install (string->symbol (cadr args)))
      (exit 0))]))

(main (command-line))
