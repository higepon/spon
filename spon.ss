(import (rnrs)
        (srfi :48)
        (spon tools))

(define (main args)
  (cond
   [(null? (cdr args))
    (format (current-error-port) "ERROR ~a: package name not specified\n" system-name)
    (exit -1)]
   [else
    (install (string->symbol (cadr args)))
    (exit 0)]))

(main (command-line))
