(library (spon compat)
  (export do-cmd)
  (import (rnrs)
      (spon aux)
      (only (mosh process) spawn waitpid))

  (define null-port
    (let ((np #f #;(make-custom-binary-output-port 'null
                          (lambda (bv s n) n)
                          #f #f #f)))
      (lambda () np)))

  (define (do-cmd cmd . args)
    (let*-values (((pid . _) (spawn cmd args
                     `(#f ,(if (verbose?) #f (null-port)) #f)))
          ((pid status) (waitpid pid)))
      (zero? status)))
  )
