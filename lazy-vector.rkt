#lang racket
;(require racket/vector)
 
(provide make-lazy-vector lazy-vector-ref lazy-vector->vector set-lazy-vector-extender!)

(struct lazy-vector (v lastix extender) #:mutable #:transparent)

(define (make-lazy-vector extender) ; create a fresh lazy-vector; use null for extender if you will fill it in later
  (lazy-vector (make-vector 4) -1 extender))

; decided to just export the setter since it has the natural name anyway ; I'm exporting it to allow "mutual recursion"
;(define (update-lazy-vector-extender! v extender) ; the official way to update (does set-lazy-vector-extender! get exported?)
;  (set-lazy-vector-extender! v extender))

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
    ;(define v (lazy-vector (make-vector 4) -1 (lambda (v ix) (if (equal? ix 0) 1 (* 3 (lazy-vector-ref v (- ix 1))))))) ; deprecated: now we have an abstract way
    (define v (make-lazy-vector (lambda (v ix) (if (equal? ix 0) 1 (* 3 (lazy-vector-ref v (- ix 1)))))))

    (displayln (lazy-vector-ref v 8))
    v))

;(lv-test-2)

(define (lazy-vector->vector v)
  (let* ([vv (lazy-vector-v v)]
         [newv (make-vector (+ 1 (lazy-vector-lastix v)))])
    (begin
      (vector-copy! newv 0 vv 0 (+ 1 (lazy-vector-lastix v)))
      newv)))

;(lazy-vector->vector (lv-test-2))

(define (lv-test-3)
  (begin
    (define v1 (make-lazy-vector null))
    (define v2 (make-lazy-vector null))
    (set-lazy-vector-extender! v1 (lambda (v1 ix)
                                    (if (equal? ix 0)
                                        1
                                        (+ (* 4 (lazy-vector-ref v2 (- ix 1))) (lazy-vector-ref v1 (- ix 1))))))
    (set-lazy-vector-extender! v2 (lambda (v2 ix)
                                    (if (equal? ix 0)
                                        1
                                        (* 3 (lazy-vector-ref v1 (- ix 1))))))
    (displayln (lazy-vector-ref v1 10))
    (displayln (lazy-vector->vector v1))
    (displayln (lazy-vector->vector v2))
    ))

;(lv-test-3)