(library (spon compat)
  (export current-system-name
          command
          file-copy
          make-directory
          make-symbolic-link
          current-directory
          set-current-directory!)
  (import (rnrs)
          (only (mosh) current-directory set-current-directory!)
          (only (mosh process) spawn waitpid pipe)
          (spon config))

  (define (current-system-name) "mosh")

  ;; todo replace with custom port
  (define (spawn2->null command args)
    (let-values ([(in out) (pipe)])
      (let-values ([(pid cin cout cerr) (spawn command args (list #f out out))])
        (close-port out)
        (close-port in)
        (waitpid pid))))

  (define (command cmd . args)
    (cond
     [(verbose?)
      (let*-values ([(pid . _) (spawn cmd args '(#f #f #f))]
                    [(pid status) (waitpid pid)])
        (zero? status))]
     [else
      (let-values ([(pid status) (spawn2->null cmd args)])
        (zero? status))]))

  (define (file-copy src dst mode)
    (command "install" "-m" (number->string mode 8) src dst))

  (define (make-directory dir mode)
    (command "install" "-m" (number->string mode 8) "-d" dir))

  (define (make-symbolic-link target link)
    (command "ln" "-sf" target link))

  )
