(import (rnrs)
        (mosh config)
        (only (mosh process) spawn waitpid))

(define (cmd-install . args)
    (let*-values (((pid . _) (spawn "install" args '(#f #f #f)))
                  ((pid status) (waitpid pid)))
        (zero? status)))

(define (mkdir dir)
    (unless (file-exists? dir)
        (cmd-install "-v" "-m" "755" "-d" dir)))

(define (file-copy src dst)
    (unless (file-exists? dst)
        (cmd-install "-v" "-m" "644" src dst)))

(define (main args)
    (display args) (newline)
    #;(let ((spon-dir (cadr args))
          (sitelib-path (string-append (get-config "library-path") "/lib")))
        (mkdir (string-append sitelib-path "/spon"))
        (file-copy
            (string-append spon-dir "/spon/base.sls")
            (string-append sitelib-path "/spon/base.sls"))
        (file-copy
            (string-append spon-dir "/spon/compat.mosh.sls")
            (string-append sitelib-path "/spon/compat.sls"))
        (file-copy
            (string-append spon-dir "/spon/tools.sls")
            (string-append sitelib-path "/spon/tools.sls"))))

(main (command-line))
