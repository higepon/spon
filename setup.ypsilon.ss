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

(define (file-copy src dst mode)
  (cmd-install "-v" "-m" (number->string mode 8) src dst))

(define (make-directory dir mode)
  (cmd-install "-v" "-m" (number->string mode 8) "-d" dir))

(define (main args)
  (let ((library-path (car (scheme-library-paths))))
    (make-directory (string-append library-path "/spon") #o755)
    (file-copy "./spon/compat.ypsilon.sls"
        (string-append library-path "/spon/compat.sls") #o644)
    (file-copy "./spon/config.sls"
        (string-append library-path "/spon/config.sls") #o644)
    (file-copy "./spon/tools.sls"
        (string-append library-path "/spon/tools.sls") #o644)))

(main (command-line))
