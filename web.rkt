#lang racket

;To use, start it running and when the loading is done browse to: http://localhost:8000/

(require (planet dmac/spin))
(require web-server/templates)
(require "predict-extension.rkt")
(require "util.rkt")

(require (prefix-in srfi: srfi/48)) ; avoid shadowing racket's format

(require json)

(define (take-at-most lst num)
  (if (> (length lst) num) (take lst num) lst))

(define a1ctl (vector->list (vector-map length (vector-ref v-functions 1))))
(define (get-wt ix) (/ (* ix (+ ix 1))))
(define tmpsum 1)
(define a1wtv (list->vector (for/list ([depth (in-range (length a1ctl))]
                         [num a1ctl])
                (let* ([begix tmpsum]
                       [endix (+ tmpsum num)])
                  (set! tmpsum endix)
                  (let ([wt (apply + (map get-wt (range begix endix)))])
                    wt)))))
(define (accumwt l)
  (if (null? l)
      null
      (let ([last (first l)]
            [result null])
        (for ([x (cdr l)])
          (if (equal? (first x) (first last))
              (set! last (list (first last) (+ (second last) (second x))))
              (begin 
                (set! result (cons last result))
                (set! last x))))
        (reverse (cons last result)))))
        

(get "/" (lambda (req) (include-template "web/index.html")))

;http://localhost:8000/pr_find?q=2%203
;http://localhost:8000/pr_find?q=2%203&ct=3
(get "/pr_find"
     (lambda (req)
       (let* ([q (params req 'q)]
              [ct (string->number (params req 'ct))]
              [v1 (list->vector (map string->number (string-split (string-normalize-spaces q) " ")))]
              [ml (find-a1-matches v1)]
              [vnext (sort (map (lambda (x) (list (vector-ref (first x) (vector-length v1)) (vector-ref a1wtv (second x)))) ml) (lambda (x y) (< (first x) (first y))))]
              [vnextacc (accumwt vnext)]
              [totalwt (apply + (map second vnextacc))]
              [vnextp (for/list ([xx vnextacc]) (list (first xx) (/ (second xx) totalwt)))]
              [vnextps (for/list ([xx vnextp]) (format "~a w.p. ~a" (first xx) (srfi:format "~1,3F" (second xx))))]
              ;[vnextps (for/list ([xx vnextp]) (format "~a w.p. ~a" (first xx) (second xx)))]
              [matchv (take-at-most ml 30)]
              [sv (map (lambda (x) (format "~a" x)) matchv)])
         (printf "~a\n" q)
         (if ct 
             (jsexpr->string (list ct (string-join sv "<br>\n") (string-join vnextps ", ")))
             (string-join sv "<br>\n"))
         )))
(run)
