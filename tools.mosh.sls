(library (spon tools)

    (export download verify decompress install)
    (import (rnrs)
            (only (mosh process) spawn waitpid))

    (define *config-search-path*
        '("~/.spon"
          "/usr/local/share/spon/sponrc"
          "/usr/share/spon/sponrc"
          "/etc/sponrc"))

    (define *default-spon-uri* "http://scheme-users.jp/spon/")
    (define *default-spon-dir* "/usr/local/share/spon")

    (define-syntax do-procs
        (syntax-rules ()
            ((_ (res ok err)) (let ((r res)) (display (if r ok err)) (newline) r))
            ((_ x y ...) (and (do-procs x) (do-procs y ...)))))

    (define (get-config-path)
        (let loop ((ls *config-search-path*))
            (if (pair? ls)
                (if (file-exists? (car ls))
                    (car ls)
                    (loop (cdr ls)))
                #f)))

    (define (get-config)
        (let ((config (make-hashtable string-hash string=?))
              (config-path (get-config-path)))
            (when config-path
                (call-with-input-file config-path
                    (lambda (in)
                        (let loop ((ls (read in)))
                            (when (and (pair? ls) (pair? (car ls)) (string? (car (car ls))))
                                (hashtable-set! config (car (car ls)) (cdr (car ls)))
                                (loop (cdr ls)))))))
            config))

    (define (download wget uri dir)
        (let-values (((pid cin cout cerr) (spawn wget (list "-P" dir uri) '(#f #f #f))))
            (let-values (((pid status) (waitpid pid))) (zero? status))))

    (define (verify gpg signature file)
        (let-values (((pid cin cout cerr) (spawn gpg (list "--verify" signature file) '(#f #f #f))))
            (let-values (((pid status) (waitpid pid))) (zero? status))))

    (define (decompress tar file dir)
        (let-values (((pid cin cout cerr) (spawn tar (list "-xvzf" file "-C" dir) '(#f #f #f))))
            (let-values (((pid status) (waitpid pid))) (zero? status))))

    (define (install package)
        (let ((config (get-config)))
            (let ((wget (hashtable-ref config "wget" "wget"))
                  (gpg (hashtable-ref config "gpg" "gpg"))
                  (tar (hashtable-ref config "tar" "tar"))
                  (spon-dir (hashtable-ref config "spon-dir" *default-spon-dir*))
                  (spon-uri (hashtable-ref config "spon-uri" *default-spon-uri*)))
                (let ((src-dir (string-append spon-dir "/src"))
                      (pkg-uri (string-append spon-uri package ".tar.gz"))
                      (sig-uri (string-append spon-uri package ".tar.gz.asc"))
                      (pkg-file (string-append spon-dir "/src/" package ".tar.gz"))
                      (sig-file (string-append spon-dir "/src/" package ".tar.gz.asc")))
                    (do-procs
                        ((download wget pkg-uri src-dir) "OK: package download." "ERROR: package download.")
                        ((download wget sig-uri src-dir) "OK: signature download." "ERROR: signature download.")
                        ((verify gpg sig-file pkg-file) "OK: verify." "ERROR: verify.")
                        ((decompress tar pkg-file spon-dir) "OK: decompress." "ERROR : decompress."))))))
    )
