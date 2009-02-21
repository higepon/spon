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

  (define (print . strings)
    (for-each display strings)
    (newline))

  (define (print-if bool t f)
    (if bool
      (when t (print t))
      (when f (print f))))

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

  (define (download package)
    (let* ((config (get-config))
           (spon-dir (config "spon-dir" *default-spon-dir*))
           (spon-uri (config "spon-uri" *default-spon-uri*))
           (arc-name (format "~A.tar.gz" package))
           (pkg-uri  (format "~A/~A" spon-uri arc-name))
           (sig-uri  (format "~A.asc" pkg-uri))
           (src-dir  (format "~A/src" spon-dir)))
      (print (format "----> Downloading package: ~A ..." pkg-uri))
      (print-if (cmd-wget pkg-uri src-dir)
             #f
             "----> failed to download package.")
      (print (format "Downloading signature: ~A ..." sig-uri))
      (print-if (cmd-wget sig-uri src-dir)
             #f
             "----> failed to download signature.")))

  (define (verify package)
    (let* ((config (get-config))
           (spon-dir (config "spon-dir" *default-spon-dir*))
           (src-dir  (format "~A/src" spon-dir))
           (arc-name (format "~A.tar.gz" package))
           (pkg-file (format "~A/~A" src-dir arc-name))
           (sig-file (format "~A.asc" pkg-file)))
      (print "Veryfying package ...")
      (print-if (cmd-gpg sig-file pkg-file)
             #f
             "cannot verify package.")))

  (define (decompress package)
    (let* ((config (get-config))
           (spon-dir (config "spon-dir" *default-spon-dir*))
           (src-dir  (format "~A/src" spon-dir))
           (arc-name (format "~A.tar.gz" package))
           (pkg-file (format "~A/~A" src-dir arc-name)))
      (print "Decompressing package ...")
      (print-if (cmd-tar pkg-file spon-dir)
             (format "done.\n~A is successfully installed." package)
             "error in decompressing package")))

  (define (install package)
    (and
      (download package)
      (verify package)
      (decompress package)))
  )
