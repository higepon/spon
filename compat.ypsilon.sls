;; (verbose? #t)でメッセージが出力されます
;; tools.slsの(srfi :48)はYpsilonには無いので、
;; (srfi :28)にして試してください。

(library (spon compat)
  (export do-cmd)
  (import (rnrs)
          (spon aux)
          (only (core) destructuring-bind process process-wait))

  (define (do-cmd cmd . args)
    (destructuring-bind (pid p-stdin p-stdout p-stderr)
        (apply process cmd args)
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
                 (or status (loop (process-wait pid #t))))))  ; nohang = #t
            (else
             (zero? (process-wait pid #f)))))) ; nohang = #f
  ) ;[end]
