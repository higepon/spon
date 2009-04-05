(import (rnrs)
        (mosh config)
        (only (mosh process) spawn waitpid))

(define (cmd-install . args)
  (let*-values (((pid . _) (spawn "install" args '(#f #f #f)))
                ((pid status) (waitpid pid)))
    (zero? status)))

(define (mkdir dir)
  (cmd-install "-v" "-m" "755" "-d" dir))

(define (file-copy src dst)
  (cmd-install "-v" "-m" "644" src dst))

(define (main args)
  (let ((spon-lib (cadr args))
        (sitelib-path (string-append (get-config "library-path") "/lib")))
    (mkdir (string-append sitelib-path "/spon"))
    (file-copy
        (string-append spon-lib "/spon/compat.mosh.sls")
        (string-append sitelib-path "/spon/compat.sls"))
    (file-copy
        (string-append spon-lib "/spon/config.sls")
        (string-append sitelib-path "/spon/config.sls"))
    (file-copy
        (string-append spon-lib "/spon/tools.sls")
        (string-append sitelib-path "/spon/tools.sls"))))

(main (command-line))
