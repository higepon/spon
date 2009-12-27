(library (spon compat)
  (export implementation-name
          command
          current-directory
          )
  (import (rnrs)
          (srfi :39)
          (prefix (only (mosh) current-directory set-current-directory!) mosh:)
          (only (mosh process) spawn waitpid pipe)
          (spon config))

  (define (implementation-name) "mosh")

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

  (define current-directory
    (make-parameter (mosh:current-directory)
      (lambda (val)
        (mosh:set-current-directory! val)
        val)))
  )
