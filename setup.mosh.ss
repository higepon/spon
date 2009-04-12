(import (rnrs)
        (only (mosh config) get-config)
        (only (mosh process) spawn waitpid))

(define (cmd-install . args)
  (let*-values (((pid . _) (spawn "install" args '(#f #f #f)))
                ((pid status) (waitpid pid)))
    (zero? status)))

(define (file-copy src dst mode)
  (cmd-install "-v" "-m" (number->string mode 8) src dst))

(define (make-directory dir mode)
  (cmd-install "-v" "-m" (number->string mode 8) "-d" dir))

(define (main args)
  (let ((spon-lib (cadr args))
        (sitelib-path (string-append (get-config "library-path") "/lib")))
    (make-directory (string-append sitelib-path "/spon") #o755)
    (file-copy
        (string-append spon-lib "/spon/compat.mosh.sls")
        (string-append sitelib-path "/spon/compat.sls") #o644)
    (file-copy
        (string-append spon-lib "/spon/config.sls")
        (string-append sitelib-path "/spon/config.sls") #o644)
    (file-copy
        (string-append spon-lib "/spon/tools.sls")
        (string-append sitelib-path "/spon/tools.sls") #o644)))

(main (command-line))
