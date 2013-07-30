#lang racket
(require racket/serialize)

(define inname "out/functions")
(define ifile (open-input-file (string-append inname ".serial")))
(define ldat (deserialize (read ifile)))
(close-input-port ifile)
(define maxdepth (second ldat))
(define v-functions (third ldat))
(define l-slow (reverse (map (lambda (x) (list (third x) (second x) (first x))) (fourth ldat))))
;(define l-slow1 (filter (lambda (x) (equal? (pr-arity (third x)) 1)) l-slow))

;(vector-map length v-functions)
;(vector-ref v-functions 0)

(define (vector-initial-match v1 v2)
  (define l (min (vector-length v1) (vector-length v2)))
  (for/and ([ix (in-range l)]) (equal? (vector-ref v1 ix) (vector-ref v2 ix))))
(define (find-a1-match v1)
  (define lst (vector-ref v-functions 1))
  (for/or ([x lst])
    (if (vector-initial-match v1 (first x)) x #f)))
(define (find-a1-matches v1)
  (define lst (vector-ref v-functions 1))
  (filter (lambda (x) (vector-initial-match v1 (first x))) lst))
(define (find-s1-matches v1)
  (define lst l-slow) ; @@ should be l-slow1
  (filter (lambda (x) (vector-initial-match v1 (first x))) lst))


;(find-a1-matches '#(1 2 3))
;(find-a1-matches '#(1 2 4))
;(find-a1-matches '#(1 2 4 8))
;(find-a1-matches '#(1 3 9))
;(find-s1-matches '#(1 3 9))

(define s1 "")
(define v1 null)

(define (loop)
  (printf "Enter a sequence of non-negative integers, like \"0 1 2\"\n")
  (set! s1 (read-line))
  (if (not (equal? s1 ""))
      (begin
        (set! v1 (list->vector (map string->number (string-split (string-normalize-spaces s1) " "))))
        (printf "Normal matches:\n")
        (map (lambda (x) (displayln x)) (find-a1-matches v1))
        (printf "\nSlow matches:\n")
        (map (lambda (x) (displayln x)) (find-s1-matches v1))
        (loop))
      (printf "Bye!\n")))
(loop)
