#lang racket

(require racket/gui)
(require file/unzip)
(require file/zip)

;; convert individual line to 'always on' version
(define (process-line line)  
  (regexp-replaces line
                   '([#rx"display=\"b\"" "display=\"bd\""]
                     [#rx"(<.+?display=\"d\".+?\\>)" "<![CDATA[\\1]]>"]
                     [#rx"<(?i:watch name)=\"(.+?)\"" "<Watch name=\"(Patched) \\1\""]))
)

;; convert source watch file to 'always on' version
(define (process-file infile outfile)
  (for ([line (in-lines infile)])
        (displayln (process-line line) outfile)
  )
  (close-output-port outfile)
)

;; process package
(define (process-package package-path)
  (define package-name (file-name-from-path package-path)) ; <...>/<package-name>
  (define temp-path (make-temporary-file (string-append (path->string package-name) "-~a") 'directory))  
  (define watch-file (build-path temp-path "watch.xml"))
  (define tmp-watch-file (build-path temp-path "watch.xml.tmp"))
  (define patched-package-path (build-path (path-only package-path) (string-append (path->string package-name) ".patched.watch")))
  (unzip package-path (make-filesystem-entry-reader #:dest temp-path))
  (process-file (open-input-file watch-file)
                (open-output-file tmp-watch-file #:exists 'replace)
  )
  (rename-file-or-directory tmp-watch-file watch-file #t)
  (when (file-exists? patched-package-path)
      (delete-file patched-package-path))
  (current-directory temp-path)
  (zip patched-package-path "./")
)

(define (go-go-go)
  (define done #f)
  (let loop ()
    (when (equal? done #f)
      (define file-name (get-file "Select a watch file to convert" #f #f #f #f null '(("Any" "*.watch"))))
      (cond
        [(equal? file-name #f) (set! done #t)]
        [(not (equal? (regexp-match ".patched." file-name) #f))
         (message-box "All done." "Please do not select a file that was already patched.")]
        [else
         (process-package file-name)
         (message-box "All done." "Patched file generation complete.")
         (set! done #t)]
      )
      (loop)))
)

(go-go-go)