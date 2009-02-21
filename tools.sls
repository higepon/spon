(library (spon tools)
  (export download verify decompress install)
  (import (rnrs)
          (srfi :48)
          (srfi :98)
          (spon base)
          (spon compat))

  (define *default-spon-uri* "http://scheme-users.jp/spon")
  (define *default-spon-dir* "/usr/local/share/spon")

  (define *config-search-path*
    `(,@(cond
         ((get-environment-variable "HOME")
          => (lambda (home)
               (list (string-append home "/.spon"))))
         (else '()))
      "/usr/local/share/spon/sponrc"
      "/usr/share/spon/sponrc"
      "/etc/sponrc"))

  (define-syntax do-procs
    (syntax-rules ()
      ((_ (pre cmd ok ng) ...)
       (and (begin
              (format #t "----> ~A~%" pre)
              (let ((res cmd))
                (if res
                    (when ok
                      (format #t "----> ~A~%" ok))
                    (format #t "----> ERROR: ~A~%" ng))
                res))
            ...))))

  (define (load-config)
    (let ((config (make-hashtable string-hash string=?))
          (config-path (find file-exists? *config-search-path*)))
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
        (when (not config)
              (set! config (load-config)))
        (apply config x))))

  (define (get-options options-list)
    (let ((options (make-hashtable string-hash string=?)))
      (for-each
        (lambda (op)
          (if (pair? op)
            (hashtable-set! options (format "~A" (car op)) (cdr op))
            (hashtable-set! options (format "~A" op) #t)))
        options-list)
      (lambda (key default)
        (hashtable-ref options key default))))

  (define (cmd-wget uri dir)
    (do-cmd ((get-config) "wget") "-N" "-P" dir uri))

  (define (cmd-gpg signature file)
    (do-cmd ((get-config) "gpg") "--verify" signature file))

  (define (cmd-tar file dir)
    (do-cmd ((get-config) "tar") "-xvzf" file "-C" dir))

  (define (download package . options-list)
    (let* ((config (get-config))
           (options (get-options options-list))
           (spon-dir (config "spon-dir" *default-spon-dir*))
           (spon-uri (config "spon-uri" *default-spon-uri*))
           (arc-name (format "~A.tar.gz" package))
           (pkg-uri  (format "~A/~A" spon-uri arc-name))
           (sig-uri  (format "~A.asc" pkg-uri))
           (src-dir  (format "~A/src" spon-dir)))
      (do-procs
        ((format "Downloading package: ~A ..." pkg-uri)
         (cmd-wget pkg-uri src-dir)
         #f
         "failed to download package.")
        ((format "Downloading signature: ~A ..." sig-uri)
         (cmd-wget sig-uri src-dir)
         #f
         "failed to download signature."))))

  (define (verify package . options-list)
    (let* ((config (get-config))
           (options (get-options options-list))
           (spon-dir (config "spon-dir" *default-spon-dir*))
           (src-dir  (format "~A/src" spon-dir))
           (arc-name (format "~A.tar.gz" package))
           (pkg-file (format "~A/~A" src-dir arc-name))
           (sig-file (format "~A.asc" pkg-file)))
      (do-procs
        ("Veryfying package ..."
         (cmd-gpg sig-file pkg-file)
         #f
         "cannot verify package."))))

  (define (decompress package . options-list)
    (let* ((config (get-config))
           (options (get-options options-list))
           (spon-dir (config "spon-dir" *default-spon-dir*))
           (src-dir  (format "~A/src" spon-dir))
           (arc-name (format "~A.tar.gz" package))
           (pkg-file (format "~A/~A" src-dir arc-name)))
      (do-procs
        ("Decompressing package ..."
        (cmd-tar pkg-file spon-dir)
        #f
        "error in decompressing package"))))

  (define (install package . options-list)
    (let ((r (and
               (apply download (cons package options-list))
               (apply verify (cons package options-list))
               (apply decompress (cons package options-list)))))
      (if r
        (format #t "----> ~A is successfully installed.~%" package)
        (format #t "----> ~A install failed.~%" package))
      r))
  )
