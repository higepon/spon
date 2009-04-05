(library (spon compat)
  (export current-system-name
          current-directory
          set-current-directory!
          do-cmd)
  (import (rnrs)
          (spon config)
          (only (mosh process) spawn waitpid pipe)
          (only (mosh) current-directory set-current-directory!))

  (define (current-system-name) "mosh")

  ;; todo replace with custom port
  (define (spawn2->null command args)
    (let-values ([(in out) (pipe)])
      (let-values ([(pid cin cout cerr) (spawn command args (list #f out out))])
        (close-port out)
        (close-port in)
        (waitpid pid))))

  (define (do-cmd cmd . args)
    (cond
     [(verbose?)
      (let*-values ([(pid . _) (spawn cmd args '(#f #f #f))]
                    [(pid status) (waitpid pid)])
        (zero? status))]
     [else
      (let-values ([(pid status) (spawn2->null cmd args)])
        (zero? status))]))

  )
