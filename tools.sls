(library (spon tools)
  (export download verify decompress install verbose? system-name)
  (import (rnrs)
          (srfi :48)
          (srfi :98)
          (spon base)
          (spon compat))

  (define *config-search-path*
    `(,@(cond
         ((get-environment-variable "HOME")
          => (lambda (home)
               (list (string-append home "/.spon"))))
         (else '()))
      "/usr/local/share/spon/sponrc"
      "/usr/share/spon/sponrc"
      "/etc/sponrc"))

  (define *default-spon-uri* "http://scheme-users.jp/spon")
  (define *default-spon-dir* "/usr/local/share/spon")

  (define-syntax do-procs
    (syntax-rules ()
      ((_ (pre cmd ok ng) ...)
       (and (begin
              (format #t "---->  ~A~%" pre)
              (let ((res cmd))
                (if res
                    (when ok
                      (format #t "~A~%" ok))
                    (format #t "---->  ERROR: ~A~%" ng))
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

  (define (download wget uri dir)
    (do-cmd wget "-N" "-P" dir uri))

  (define (verify gpg signature file)
    (do-cmd gpg "--verify" signature file))

  (define (decompress tar file dir)
    (do-cmd tar "-xvzf" file "-C" dir))

  (define (install package)
    (let (($ (load-config)))
      (let ((wget ($ "wget"))
            (gpg  ($ "gpg"))
            (tar  ($ "tar"))
            (spon-dir ($ "spon-dir" *default-spon-dir*))
            (spon-uri ($ "spon-uri" *default-spon-uri*)))
        (let* ((src-dir  (format "~A/src" spon-dir))
               (arc-name (format "~A.tar.gz" package))
               (pkg-uri  (format "~A/~A" spon-uri arc-name))
               (sig-uri  (format "~A.asc" pkg-uri))
               (pkg-file (format "~A/~A" src-dir arc-name))
               (sig-file (format "~A.asc" pkg-file)))
          (do-procs
           ((format "Downloading package: ~A ..." pkg-uri)
            (download wget pkg-uri src-dir)
            #f
            "failed to download package")
           ((format "Downloading signature: ~A ..." sig-uri)
            (download wget sig-uri src-dir)
            #f
            "failed to download signature")
           ("Veryfying package ..."
            (verify gpg sig-file pkg-file)
            #f
            "cannot verify package")
           ("Decompressing package ..."
            (decompress tar pkg-file spon-dir)
            (format "done.\n~A is successfully installed.\n" package)
            "error in decompressing package")
           )))))
  )
