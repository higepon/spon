(import (rnrs)
        (only (core)
          destructuring-bind
          process process-wait
          scheme-library-paths))

(define (cmd-install . args)
  (destructuring-bind
    (pid p-stdin p-stdout p-stderr)
    (apply process "install" args)
    (zero? (process-wait pid #f))))

(define (mkdir dir)
  (cmd-install "-v" "-m" "755" "-d" dir))

(define (file-copy src dst)
  (cmd-install "-v" "-m" "644" src dst))

(define (main args)
  (let ((spon-lib (cadr args))
        (sitelib-path (car (scheme-library-paths))))
    (mkdir (string-append sitelib-path "/spon"))
    (file-copy
        (string-append spon-lib "/spon/compat.ypsilon.sls")
        (string-append sitelib-path "/spon/compat.sls"))
    (file-copy
        (string-append spon-lib "/spon/config.sls")
        (string-append sitelib-path "/spon/config.sls"))
    (file-copy
        (string-append spon-lib "/spon/tools.sls")
        (string-append sitelib-path "/spon/tools.sls"))))

(main (command-line))
