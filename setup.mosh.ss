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
  (let ((library-path (string-append (get-config "library-path") "/lib")))
    (make-directory (string-append library-path "/spon") #o755)
    (file-copy "./spon/compat.mosh.sls"
        (string-append library-path "/spon/compat.sls") #o644)
    (file-copy "./spon/config.sls"
        (string-append library-path "/spon/config.sls") #o644)
    (file-copy "./spon/tools.sls"
        (string-append library-path "/spon/tools.sls") #o644)))

(main (command-line))
