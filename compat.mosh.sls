(library (spon compat)
  (export do-cmd)
  (import (rnrs)
          (spon base)
          (only (mosh process) spawn waitpid pipe))

  ;; todo replace with custom port
  (define (spawn2->null command args)
    (let-values ([(in out) (pipe)])
      (let-values ([(pid cin cout cerr) (spawn command args (list #f #f out))])
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
