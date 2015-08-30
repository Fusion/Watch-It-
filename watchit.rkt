#lang racket

(require racket/gui)
(require file/unzip)
(require file/zip)

;; special case: dimmed color
(define (process-dim-color line)
  (define matched (regexp-match #rx"color=\"(.+?)\"" line))
  (cond
   [(equal? matched #f) line]
   [else
    (define bright-color (first (rest matched)))
    (regexp-replace #rx"color_dim=\".+?\"" line (string-append "color_dim=\"" bright-color "\""))]
  )
)

;; convert individual line to 'always on' version
(define (process-line line)
  (process-dim-color
   (regexp-replaces line
                   '([#rx"display=\"b\"" "display=\"bd\""]
                     [#rx"(<.+?display=\"d\".+?\\>)" "<![CDATA[\\1]]>"]
                     [#rx"<(?i:watch name)=\"(.+?)\"" "<Watch name=\"(Patched) \\1\""])
   )
 )
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
  (define patched-package-path
    (build-path (path-only package-path) (string-append
                                          (regexp-replace* #rx"\\." (path->string package-name) "-")
                                          "-patched.watch")))
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

;; select a package to process
(define (go-go-go)
  (define do-while-done #f)
  (let do-while ()
    (when (equal? do-while-done #f)
      (define file-name (get-file "Select a watch file to convert" #f #f #f #f null '(("Any" "*.watch"))))
      (cond
        [(equal? file-name #f) (set! do-while-done #t)]
        [(not (equal? (regexp-match "-patched." file-name) #f))
         (message-box "All done." "Please do not select a file that was already patched." #f '(stop))]
        [else
         (process-package file-name)
         (message-box "All done." "Patched file generation complete." #f '(ok))
         (set! do-while-done #t)]
      )
      (do-while)))
)

(define (about-app)
  (message-box "About Watch It!"
               #<<EOF
This application hastily put together by
Chris F Ravenscroft (cyansmoker)

This is for owners of smartwatches, such as the LG Watch,
that can keep displaying your watchface even in dimmed mode:
the app tries to make the 'dimmed' screen similar to the 'awake' one.

Note that this is very preliminary and will likely not work with LUA scripts.

The Racket source code should be available at http://github.com/fusion

EOF
               #f
               '(ok)
  )
)

(define (main-menu)
  (define do-while-done #f)
  (let do-while ()
    (when (equal? do-while-done #f)
      (define choice
        (message-box/custom "Watch It!"
                            "A small tool that attempts to keep your watch display interesting"
                            "Convert a watch file"
                            "About"
                            "Exit"
                            #f
                            '(default=1)
        )
      )
      (cond
        [(equal? choice 1) (go-go-go)]
        [(equal? choice 2) (about-app)]
        [else (set! do-while-done #t)]
      )
      (do-while)))
)

(main-menu)