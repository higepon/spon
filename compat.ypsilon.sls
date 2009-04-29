(library (spon compat)
  (export current-implementation-name
          command
          file-copy
          make-directory
          make-symbolic-link
          current-directory
          set-current-directory!)
  (import (rnrs)
          (only (core)
                current-directory
                destructuring-bind
                process
                process-wait)
          (spon config))

  (define (current-implementation-name) "ypsilon")

  (define (command cmd . args)
    (destructuring-bind (pid p-stdin p-stdout p-stderr)
        (apply process cmd args)
      (zero?
       (cond ((verbose?)
              (let ((p-message (transcoded-port p-stdout (native-transcoder)))
                    (p-error (transcoded-port p-stderr (native-transcoder))))
                (let loop ((status #f))
                  (let ((message (get-string-all p-message)))
                    (unless (eof-object? message)
                      (put-string (current-output-port) message)))
                  (let ((error (get-string-all p-error)))
                    (unless (eof-object? error)
                      (put-string (current-error-port) error)))
                  (or status (loop (process-wait pid #t)))))) ; nohang = #t
             (else
              (process-wait pid #f)))))) ; nohang = #f

  (define (file-copy src dst mode)
    (command "install" "-m" (number->string mode 8) src dst))

  (define (make-directory dir mode)
    (command "install" "-m" (number->string mode 8) "-d" dir))

  (define (make-symbolic-link target link)
    (command "ln" "-sf" target link))

  (define (set-current-directory! dir)
    (current-directory dir))

  ) ;[end]
