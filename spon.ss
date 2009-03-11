(import (rnrs)
        (srfi :39)
        (srfi :48)
        (spon tools))

(define (main args)
  (cond
   [(null? (cdr args))
    (display (format "ERROR ~a: package name not specified\n" system-name)
             (current-error-port))
    (exit #f)]
   [else
    (parameterize ((verbose? #t))
      (cond
       ((install (cadr args))
        (exit))
       (else
        (display (format "ERROR ~A: install failed\n" system-name)
                 (current-error-port))
        (exit #f))))]))

(main (command-line))
