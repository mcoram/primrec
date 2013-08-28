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
                    (exact->inexact (if (> num 0) (/ wt num) 0)))))))
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

(define (pr->latex s)
  (match s
    [0 "0"]
    ['Z "Z"]
    ['S "S"]
    ['P11 "P^1_1"]
    ['P21 "P^2_1"]
    ['P22 "P^2_2"]
    ['P31 "P^3_1"]
    ['P32 "P^3_2"]
    ['P33 "P^3_3"]
    [`{R0 ,f ,g} (format "\\operatorname{PR}_0(~a,~a)" (pr->latex f) (pr->latex g)) ]
    [`{R1 ,f ,g} (format "\\operatorname{PR}_1(~a,~a)" (pr->latex f) (pr->latex g)) ]
    [`{C10 ,f ,g} (format "~a(~a)" (pr->latex f) (pr->latex g))]
    [`{C20 ,f ,g ,h} (format "~a(~a,~a)" (pr->latex f) (pr->latex g) (pr->latex h))]
    [`{C30 ,f ,g ,h ,i} (format "~a(~a,~a,~a)" (pr->latex f) (pr->latex g) (pr->latex h) (pr->latex i))]
    [`{C11 ,f ,g} (format "~a \\circ^1_1 ~a" (pr->latex f) (pr->latex g))]
    [`{C12 ,f ,g} (format "~a \\circ^1_2 ~a" (pr->latex f) (pr->latex g))]
    [`{C13 ,f ,g} (format "~a \\circ^1_3 ~a" (pr->latex f) (pr->latex g))]
    [`{C21 ,f ,g ,h} (format "~a \\circ^2_1 (~a,~a)" (pr->latex f) (pr->latex g) (pr->latex h))]
    [`{C22 ,f ,g ,h} (format "~a \\circ^2_2 (~a,~a)" (pr->latex f) (pr->latex g) (pr->latex h))]
    [`{C23 ,f ,g ,h} (format "~a \\circ^2_3 (~a,~a)" (pr->latex f) (pr->latex g) (pr->latex h))]
    [`{C31 ,f ,g ,h ,i} (format "~a \\circ^3_1 (~a,~a,~a)" (pr->latex f) (pr->latex g) (pr->latex h) (pr->latex i))]
    [`{C32 ,f ,g ,h ,i} (format "~a \\circ^3_2 (~a,~a,~a)" (pr->latex f) (pr->latex g) (pr->latex h) (pr->latex i))]
    [`{C33 ,f ,g ,h ,i} (format "~a \\circ^3_3 (~a,~a,~a)" (pr->latex f) (pr->latex g) (pr->latex h) (pr->latex i))]))    

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
              [vnextps (for/list ([xx vnextp]) 
                         (let ([p (second xx)]
                               [str (format "~a w.p. ~a" (first xx) (srfi:format "~1,3F" (second xx)))])
                           (if (> p .1) (format "<b>~a</b>" str)
                               (if (< p .001) (format "<small>~a</small>" str)
                                   str))))]
              [matchv (take-at-most ml 30)]
              [sv (map (lambda (x) (format "~a<br><span class=\"tab\"></span><font color=\"#00BB00\">~a</font><span class=\"tab\"></span><font color=\"#0000FF\">~a</font><span class=\"tab\"></span><font color=\"#999999\">~a ~a</font> \\(~a\\)" 
                                           (string-join (map (lambda (y) (format "~a" y)) (vector->list (first x))) " ")
                                           (second x)
                                           (third x)
                                           (fourth x)
                                           (fifth x)
                                           (pr->latex (third x)))) matchv)])
         (printf "~a\n" q)
         (if ct 
             (jsexpr->string (list ct (string-join sv "<br>\n") (string-join vnextps ", ")))
             (string-join sv "<br>\n"))
         )))
(run #:port 8000)
