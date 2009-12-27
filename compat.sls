(library (spon compat)
  (export implementation-name
          command
          file-copy
          make-directory
          make-symbolic-link
          current-directory
          )
  (import (rnrs)
          (spon config))

  (define (implementation-name) "scheme")

  ;; -- command :: (String, [String]) -> Boolean
  ;; Execute an external command `cmd' with arguments `args'.
  ;; If the command is successfully exited, returns #t,
  ;; otherwise returns #f.
  ;; When `verbose?' procedure (in the library (spon config)) returns #f,
  ;; standard output of the command is discarded.
  ;; When `silent?' procedure (also in the same library) returns #f,
  ;; standard error of the command is discarded.
  (define (command cmd . args)
    (raise (condition
        (make-implementation-restriction-violation)
        (make-who-condition 'command)
        (make-message-condition
             (string-append
              "Compatibility layer is not implemented. "
              (string-titlecase system-name)
              " seems to be not supported by your implementation. "
              "Please consult the author of your implementation."))
        (make-irritants-condition (cons cmd args)))))
  )
