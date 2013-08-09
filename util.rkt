#lang racket
(provide nth index-of initialize-vector flat-length)

(define nth list-ref)

(define (index-of lst ele)
  (let loop ((lst lst)
             (idx 0))
    (cond ((empty? lst) #f)
          ((equal? (first lst) ele) idx)
          (else (loop (rest lst) (add1 idx))))))

(define (initialize-vector n initializer) ; initializer: ix -> initial value
  (let ([v (make-vector n)])
    (begin
      (for ([ix (in-range n)])
        (vector-set! v ix (initializer ix)))
      v)))

(define (flat-length x)
  (cond 
        [(null? x) 0]
        [(pair? x) (+ (flat-length (car x)) (flat-length(cdr x)))]
        [#t 1]))