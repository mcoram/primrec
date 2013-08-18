#lang racket

(require (planet dmac/spin))
(require web-server/templates)
(require "predict-extension.rkt")

(define (take-at-most lst num)
  (if (> (length lst) num) (take lst num) lst))

(get "/" (lambda (req) (include-template "web/index.html")))

;http://localhost:8000/pr_find?q=2%203
(get "/pr_find"
     (lambda (req)
       (let* ([q (params req 'q)]
              [v1 (list->vector (map string->number (string-split (string-normalize-spaces q) " ")))]
              [matchv (take-at-most (find-a1-matches v1) 10)]
              [sv (map (lambda (x) (format "~a" x)) matchv)])
         (string-join sv "<br>\n"))))

(run)
