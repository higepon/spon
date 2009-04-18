(import (rnrs)
        (srfi :39)
        (srfi :48)
        (spon tools)
        (spon config))

(define (main args)
  (case (string->symbol (cadr args))
    ((install)
     (cond
       [(null? (cddr args))
        (display (format "ERROR ~a: package name not specified\n" system-name)
                 (current-error-port))
        (exit #f)]
       [else
         (parameterize ((verbose? #f))
           (guard (exception
             [(download-error? exception)
              (format (current-error-port) "\n failed to download package ~a.\n" (download-error-uri exception))]
             [else (raise exception)])
             (cond
               ((install (caddr args))
                (exit))
               (else
                 (display (format "ERROR ~A: install failed\n" system-name)
                          (current-error-port))
                 (exit #f)))))]))
    ((use)
     (let ((impl (caddr args)))
       (call-with-current-working-directory library-path
         (command impl (string-append base-path "/setup." impl ".ss")))
       (make-symbolic-link (string-append base-path "/spon." impl ".sh") command-path)))
    (else (exit #f))))

(main (command-line))
