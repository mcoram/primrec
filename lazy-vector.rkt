#lang racket
;(require racket/vector)
 
;Warmup
(struct lazy-vector (v lastix extender) #:mutable #:transparent)

(define (force-extent v extent) ; Ensure the lazy-vector is constructed upto including index extent
  (when (< (lazy-vector-lastix v) extent)
    (begin
      (when (< (vector-length (lazy-vector-v v)) (+ extent 1)) ; ensure it's physically big enough to store the desired number of entries
        (let ([newv (make-vector (* 2 (max (vector-length (lazy-vector-v v)) extent)))])
          (begin
            (vector-copy! newv 0 (lazy-vector-v v) 0 (+ 1 (lazy-vector-lastix v)))
            (set-lazy-vector-v! v newv))))
      (for ([ix (in-range (+ 1 (lazy-vector-lastix v)) (+ 1 extent))]) ; build out the new values
        (vector-set! (lazy-vector-v v) ix ((lazy-vector-extender v) v ix)))
      (set-lazy-vector-lastix! v extent)))) ; store new extent

(define (lv-test-1)
  (begin
    (define v (lazy-vector (make-vector 4) -1 (lambda (v ix) ix)))
    (force-extent v 5)
    (force-extent v 15)
    (force-extent v 16)
    v))

(define (lazy-vector-ref v ix)
  (begin
    (when (< (lazy-vector-lastix v) ix)
      (force-extent v ix))
    (vector-ref (lazy-vector-v v) ix)))

(define (lv-test-2)
  (begin
    (define v (lazy-vector (make-vector 4) -1 (lambda (v ix) (if (equal? ix 0) 1 (* 3 (lazy-vector-ref v (- ix 1)))))))
    (displayln (lazy-vector-ref v 8))
    v))

(lv-test-2)