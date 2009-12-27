(library (spon compat)
  (export implementation-name
          command
          file-copy
          make-directory
          make-symbolic-link
          current-directory
          )
  (import (rnrs)
          (spon config)
          (only (ikarus) current-directory)
          (ikarus ipc))

  (define (implementation-name) "ikarus")

  (define (command cmd . args)
    (let-values (([pid p-stdin p-stdout p-stderr] (apply process cmd args)))
      (zero?                            ; for now
       (wstatus-exit-status
        (cond ((quiet?)
               (waitpid pid))
              (else
               (let ((p-message (transcoded-port p-stdout (native-transcoder)))
                     (p-error (transcoded-port p-stderr (native-transcoder))))
                 (let loop ((status #f))
                   (when (verbose?)
                     (let ((message (get-string-all p-message)))
                       (unless (eof-object? message)
                         (display message)
                         (put-string (current-output-port) message))))
                   (let ((err (get-string-all p-error)))
                     (unless (eof-object? err)
                       (put-string (current-error-port) err)))
                   (or status (loop (waitpid pid #f)))))))))))

  ;; copied from compat.ypsilon.sls for now.
  ;; must be refactor.
  (define (file-copy src dst mode)
    (command "install" "-m" (number->string mode 8) src dst))

  (define (make-directory dir mode)
    (command "install" "-m" (number->string mode 8) "-d" dir))

  (define (make-symbolic-link target link)
    (command "ln" "-sf" target link))
  )
