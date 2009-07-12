(library (spon tools)
  (export download verify decompress initialize setup install
          system-name verbose? quiet? download-error? download-error-uri
          current-implementation-name command
          file-copy make-directory make-symbolic-link
          current-directory)
  (import (rnrs)
          (srfi :39)
          (srfi :48)
          (spon config)
          (spon compat))

  (define-condition-type &spon &error
    make-spon-error spon-error?)

  (define-condition-type &download &spon
    make-download-error download-error?
    (uri download-error-uri))

  (define-record-type (version make-version version?)
    (fields
     (immutable v1 version-major)
     (immutable v2 version-minor)
     (immutable v3 version-patch)))

  (define-record-type (pkg-stat make-pkg-stat pkg-stat?)
    (fields
     (immutable status pkg-stat-status)
     (immutable version pkg-stat-version)))

  (define-record-type (pkg-info make-pkg-info pkg-info?)
    (fields
     (immutable name pkg-info-name)
     (immutable version pkg-info-version)
     (immutable depends pkg-info-depends)
     (immutable description pkg-info-description)
     (mutable status pkg-info-status pkg-info-status-set!)))

  (define-syntax do-procs
    (syntax-rules ()
      ((_ (pre cmd ok ng) ...)
       (and (begin
              (unless (quiet?)
                (format #t "----> ~A~%" pre))
              (let ((res cmd))
                (unless (quiet?)
                  (if res
                      (when ok
                        (format #t "----> ~A~%" ok))
                      (format #t "----> ERROR: ~A~%" ng)))
                res))
            ...))))

  (define (read-package-list)
    (define (get-version alis)
      (cond ((assq 'version alis)
             => (lambda (xs)
                  (and (= (length (cdr xs)) 3)
                       (for-all number? (cdr xs))
                       (apply make-version (cdr xs)))))
            (else #f)))
    (let ((ht (make-eq-hashtable)))
      (let ((file (format #f "~A/~A" base-path "package-list.sds")))
        (when (file-exists? file)
          (call-with-input-file file
            (lambda (in)
              (do ((i (read in) (read in)))
                  ((eof-object? i))
                (let ((name (car i))
                      (version (get-version (cdr i)))
                      (depends (cond ((assq 'depends (cdr i)) => cdr)
                                     (else #f)))
                      (description (let ((d (assq 'description (cdr i))))
                                     (and d (apply string-append (cdr d)))))
                      (status #f))
                  (hashtable-set! ht name
                                  (make-pkg-info name version depends description status))))))))
      (let ((file (format #f "~A/~A" base-path "install.sds")))
        (when (file-exists? file)
          (call-with-input-file file
            (lambda (in)
              (do ((i (read in) (read in)))
                  ((eof-object? i))
                (let ((name (car i))
                      (status (and (cadr i) #t))
                      (version (get-version (cddr i))))
                  (if (hashtable-contains? ht name)
                      (let ((p (hashtable-ref ht name)))
                        (pkg-info-status-set! p
                                              (make-pkg-stat status version))
                        (hashtable-set! ht name p))
                      (hashtable-set! ht name
                                      (make-pkg-info name #f #f #f
                                                     (make-pkg-stat status version))))))))))
      ht))

  (define (write-install-status pkg-ht)
    (let ((file (format #f "~A/~A" base-path "install.sds")))
      (call-with-output-file file
        (lambda (out)
          (let-values (((ks vs) (hashtable-entries pkg-ht)))
            (vector-for-each
             (lambda (key val)
               (let ((stat (pkg-info-status val)))
                 (when (pkg-stat? stat)
                   (write (cons*
                           key (pkg-stat-status stat)
                           (if (version? (pkg-stat-version stat))
                               (list (version->list (pkg-stat-version stat)))
                               '()))
                          out)
                   (newline out))))
             ks vs))))))

  (define (version<? v1 v2)
    (cond ((< (version-major v1) (version-major v2)) #t)
          ((> (version-major v1) (version-major v2)) #f)
          (else
           (cond ((< (version-minor v1) (version-minor v2)) #t)
                 ((> (version-minor v1) (version-minor v2)) #f)
                 (else
                  (cond ((< (version-patch v1) (version-patch v2)) #t)
                        (else #f)))))))

  (define (version->string version)
    (string-append (number->string (version-major version))
                   "." (number->string (version-minor version))
                   "." (number->string (version-patch version))))

  (define (version->list version)
    (list 'version
          (version-major version)
          (version-minor version)
          (version-patch version)))

  (define (package->symbol p)
    (cond
     ((symbol? p) p)
     ((string? p) (string->symbol p))
     ((pkg-info? p) (pkg-info-name p))
     (else #f)))

  (define (package->string p)
    (cond
     ((symbol? p) (symbol->string p))
     ((string? p) p)
     ((pkg-info? p)
      (if (version? (pkg-info-version p))
          (string-append (symbol->string (pkg-info-name p))
                         "-"
                         (version->string (pkg-info-version p)))
          (symbol->string (pkg-info-name p))))
     (else #f)))

  (define (package-installed? p ht)
    (cond
     ((pkg-info? p)
      (let ((stat (pkg-info-status p)))
        (if (pkg-stat? stat) (pkg-stat-status stat) stat)))
     ((symbol? p) (package-installed? (hashtable-ref ht p #f) ht))
     ((string? p) (package-installed? (string->symbol p) ht))
     (else #f)))

  (define (cmd-wget uri dir)
    (or (apply command
               ((get-config) "wget")
               "-N" "-P" dir uri (if (quiet?) '("-q") '()))
        (raise (make-download-error uri))))

  (define (cmd-gpg signature file)
    (let ((gpg ((get-config) "gpg" #f)))
      (or (not gpg)
          (apply command
                 gpg
                 `(,@(if (quiet?) '("-q") '()) "--verify" signature file)))))

  (define (cmd-tar file dir)
    (apply command
           ((get-config) "tar")
           "-xzf" file "-C" dir (if (quiet?) '() '("-v"))))

  (define (download package)
    (let* ((config (get-config))
           (download-uri (config "download-uri" download-uri))
           (pkg-uri  (format "~A/~A.tar.gz" download-uri (package->string package)))
           (sig-uri  (format "~A.asc" pkg-uri))
           (src-path (config "source-path" source-path)))
      (do-procs
       ((format "Downloading package: ~A ..." pkg-uri)
        (cmd-wget pkg-uri src-path)
        #f
        "Failed to download the package")
       ((format "Downloading signature: ~A ..." sig-uri)
        (cmd-wget sig-uri src-path)
        #f
        "Failed to download the signature"))))

  (define (verify package)
    (let* ((config (get-config))
           (src-path (config "source-path" source-path))
           (pkg-file (format "~A/~A.tar.gz" src-path (package->string package)))
           (sig-file (format "~A.asc" pkg-file)))
      (or (not (config "gpg" #f))
          (do-procs
           ("Veryfying package ..."
            (cmd-gpg sig-file pkg-file)
            #f
            "cannot verify package.")))))

  (define (decompress package)
    (let* ((config (get-config))
           (src-path (config "source-path" source-path))
           (pkg-file (format "~A/~A.tar.gz" src-path (package->string package))))
      (do-procs
       ("Decompressing package ..."
        (cmd-tar pkg-file src-path)
        #f
        "error in decompressing package"))))

  (define (initialize package mode)
    (let* ((config (get-config))
           (impl (current-implementation-name))
           (src-path (config "source-path" source-path))
           (pkg-path (format "~A/~A" source-path (package->string package)))
           (install.ss (format "~A/install.ss" pkg-path)))
      (do-procs
       ((format "Setup package for ~A ..." (string-upcase system-name))
        (parameterize ((current-directory pkg-path))
          (command impl install.ss (symbol->string mode)))
        #f
        (format "error in ~A" install.ss)))))

  (define (setup package mode)
    (let* ((config (get-config))
           (impl (current-implementation-name))
           (src-path (config "source-path" source-path))
           (pkg-path (format "~A/~A" source-path (package->string package)))
           (setup.ss (format "~A/setup.ss" pkg-path))
           (setup.impl.ss (format "~A/setup.~A.ss" pkg-path impl)))
      (do-procs
       ((format "Setup package to ~A ..." impl)
        (parameterize ((current-directory pkg-path))
          (command impl
                   (if (file-exists? setup.impl.ss) setup.impl.ss setup.ss)
                   (symbol->string mode)))
        #f
        (format "error in ~A" setup.ss)))))

  (define (install package)
    (define (install-rec package pkg-ht)
      (let ((pi (hashtable-ref pkg-ht (package->symbol package) #f)))
        (and (not (package-installed? pi pkg-ht))
             (begin
               (when (pkg-info? pi)
                 (let ((depends (pkg-info-depends pi)))
                   (when depends
                     (for-each (lambda (dep)
                                 (install-rec dep pkg-ht))
                               depends))))
               (let* ((p (if (pkg-info? pi) pi package))
                      (package-name (package->string p)))
                 (let ((r (do-procs
                           ((format "Installing ~A" package-name)
                            (and (download p)
                                 (verify p)
                                 (decompress p)
                                 (initialize p 'install)
                                 (setup p 'install))
                            (format "~A is successfully installed.~%" package-name)
                            (format "~A install failed.~%" package-name)))))
                   (when r
                     (if (pkg-info? pi)
                         (pkg-info-status-set! pi (make-pkg-stat #t (pkg-info-version pi)))
                         (hashtable-set! pkg-ht
                                         (package->symbol package)
                                         (make-pkg-info package #f #f #f (make-pkg-stat #t #f)))))
                   r))))))
    (let ((pkg-ht (read-package-list)))
      (if (package-installed? package pkg-ht)
          (begin
            (format #t "----> ~A is already installed.~%" (package->string package))
            #f)
          (let ((r (install-rec package pkg-ht)))
            (write-install-status pkg-ht)
            r))))

  (define (update package)
    (define (updatable? pi)
      (and (pkg-info? pi)
           (let ((stat (pkg-info-status pi)))
             (and (pkg-stat? stat)
                  (version<? (pkg-stat-version stat) (pkg-info-version pi))))))
    (define (update-rec package pkg-ht)
      (let* ((pi (hashtable-ref pkg-ht (package->symbol package) #f))
             (package-name (package->string pi)))
        (and (updatable? pi)
             (begin
               (let ((depends (pkg-info-depends pi)))
                 (when depends
                   (for-each (lambda (dep)
                               (update-rec dep pkg-ht))
                             depends)))
               (let ((r (do-procs
                         ((format "Updating ~A ..." package-name)
                          (and (download pi)
                               (verify pi)
                               (decompress pi)
                               (initialize pi 'update)
                               (setup pi 'update))
                          (format "~A is successfully update.~%" package-name)
                          (format "~A update failed.~%" package-name)))))
                 (when r
                   (if (pkg-info? pi)
                       (pkg-info-status-set! pi (make-pkg-stat #t (pkg-info-version pi)))
                       (hashtable-set! pkg-ht
                                       (package->symbol package)
                                       (make-pkg-info package #f #f #f (make-pkg-stat #t #f)))))
                 r)))))
    (let ((pkg-ht (read-package-list)))
      (let ((pi (hashtable-ref pkg-ht (package->symbol package) #f)))
        (if (updatable? pi)
            (let ((r (update-rec package pkg-ht)))
              (write-install-status pkg-ht)
              r)
            (begin
              (format #t "----> ~A can't update." (package->string package))
              #f)))))

  (define (uninstall package)
    #f)
  )
