(library (spon config)

  (export system-name
          verbose? quiet?
          download-uri base-path
          command-path library-path
          document-path source-path
          share-path temporary-path
          config-search-path
          load-config get-config)

  (import (rnrs)
          (srfi :39)
          (srfi :48)
          (srfi :98))

  (define system-name "spon")
  (define download-uri "http://scheme-users.jp/spon")
  (define base-path "/usr/local/share/spon")
  (define command-path "/usr/local/bin/spon")
  (define library-path "/usr/local/share/spon/lib")
  (define document-path "/usr/local/share/spon/doc")
  (define source-path "/usr/local/share/spon/src")
  (define share-path "/usr/local/share/spon/share")
  (define temporary-path "/tmp")

  (define config-search-path
    `(,@(cond
         ((get-environment-variable "HOME")
          => (lambda (home)
               (list (string-append home "/.spon"))))
         (else '()))
      ,(string-append base-path "/sponrc")
      "/usr/share/spon/sponrc"
      "/etc/sponrc"))

  (define (load-config)
    (let ((config (make-hashtable string-hash string=?))
          (config-path (find file-exists? config-search-path)))
      (when config-path
        (call-with-input-file config-path
          (lambda (in)
            (for-each
             (lambda (x)
               (if (not (pair? x))
                   (error 'load-config "invalid configuration" x)
                   (hashtable-set! config (format "~A" (car x)) (cdr x))))
             (read in)))))
      (letrec (($ (case-lambda
                   ((key)
                    ($ key key))
                   ((key default)
                    (hashtable-ref config key default)))))
        $)))

  (define (get-config)
    (let ((config #f))
      (lambda x
        (unless config
          (set! config (load-config)))
        (apply config x))))

  (define verbose? (make-parameter #f))

  ;; (quiet? #t) implies (verbose? #f).
  (define quiet? (make-parameter #f
                   (lambda (v)
                     (when v
                       (verbose? #f))
                     v)))
  )
