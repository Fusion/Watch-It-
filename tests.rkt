#lang racket/base

(require "watchit.rkt")
(require rackunit)
(require rackunit/text-ui)

(define-test-suite main-suite
  (test-case
   "Replace dim colored with bright eqv"
   (check-equal? (process-dim-color "<Layer type=\"text\" color_dim=\"12345\" color=\"000000\" display=\"d\"/>")
                 "<Layer type=\"text\" color_dim=\"000000\" color=\"000000\" display=\"d\"/>"))
  (test-case
   "Replace bright only with bright-dim config"
   (check-equal? (process-line "<Layer type=\"shape\" display=\"b\"/>")
                 "<Layer type=\"shape\" display=\"bd\"/>"))
  (test-case
   "Comment out dim entry"
   (check-equal? (process-line "<Layer type=\"shape\" display=\"d\"/>")
                 "<![CDATA[<Layer type=\"shape\" display=\"d\"/>]]>"))                 
)

(run-tests main-suite)