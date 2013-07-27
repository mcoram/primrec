#lang racket

(provide ksumj lazy-ksumj-4)

; List all the lists of k integers from 1..j that add to j exactly
(define (ksumj k j)
  (if (< k 1)
      null ; no results
      (if (equal? k 1) 
          (list(list j)) ; base-case
          (if (< j k)
              null ; empty list of results if j < k
              (for*/list ([i (in-range 1 (+ (- j k) 2))] ; range is 1..(j-k+1) b/c need at least (k-1) 1's for other entries
                          [r (ksumj (- k 1) (- j i))])
                (cons i r))))))

(define (test-ksumj-1)
  (displayln (ksumj 3 5))
  (displayln (ksumj 3 3))
  (displayln (ksumj 3 2))
  (displayln (ksumj 2 10))
  (displayln (ksumj 1 5)))

;I could do this lazy stuff with deeper memoization (using nested lazy-vectors) but naah
(require "lazy-vector.rkt")
(define lazy-ksumj-4-data (vector 
                      (make-lazy-vector (lambda (v j) (ksumj 1 j)))
                      (make-lazy-vector (lambda (v j) (ksumj 2 j)))
                      (make-lazy-vector (lambda (v j) (ksumj 3 j)))
                      (make-lazy-vector (lambda (v j) (ksumj 4 j)))))

(define (lazy-ksumj-4 k j) ; assert 1<=k<=4
  (lazy-vector-ref (vector-ref lazy-ksumj-4-data (- k 1)) j))

;(lazy-ksumj-4 3 10)