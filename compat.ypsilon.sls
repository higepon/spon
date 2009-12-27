(library (spon compat)
  (export implementation-name
          command
          file-copy
          make-directory
          make-symbolic-link
          current-directory
          )
  (import (rnrs)
          (only (core)
                current-directory
                destructuring-bind
                process
                process-wait)
          (spon config))

  (define (implementation-name) "ypsilon")

  (define (command cmd . args)
    (destructuring-bind (pid p-stdin p-stdout p-stderr)
        (apply process cmd args)
      (zero?
       (cond ((quiet?)
              (process-wait pid #f)) ; nohang = #t
             ((verbose?)
              (let ((p-message (transcoded-port p-stdout (native-transcoder)))
                    (p-error (transcoded-port p-stderr (native-transcoder))))
                (let loop ((status #f))
                  (when (verbose?)
                    (let ((message (get-string-all p-message)))
                      (unless (eof-object? message)
                        (put-string (current-output-port) message))))
                  (let ((error (get-string-all p-error)))
                    (unless (eof-object? error)
                      (put-string (current-error-port) error)))
                  (or status (loop (process-wait pid #t)))))))))) ; nohang = #f
  ) ;[end]
