#lang racket
(require "pr_primitives.rkt")
(provide compiled-forms)

; only for check
;(define-namespace-anchor anchor1)
;(define ns (namespace-anchor->namespace anchor1))
;(define (l_eval x) (eval x ns))

(define max-iter 10)

(define compiled-forms (make-hash))

(define (symbolic-s-compose foot c iter)
  (define result foot)
  (for ([i (in-range iter)]) (set! result (list c 'S result)))
  result)

;(symbolic-s-compose 'P21 'C12 5)

(define (symbolic-s-only-compose iter)
  (if (equal? iter 0)
      'P11
      (symbolic-s-compose 'S 'C11 (- iter 1))))

(define (register-solution-default case make-fun)
  (for* ([i (in-range max-iter)]
         [k (in-range max-iter)])
    (hash-set! compiled-forms (case i k) (make-fun i k))))

;these next two are cheap hacks; for proper versions of evaluation adopt the code from pr04
(define (get-arity s)
  (case (first s)
    [(R0) 1]
    [(R1) 2]))

(define (test-eval s f)
  (case (get-arity s)
    [(1) (for*/list ([i (in-range 25)]) (f i))]
    [(2) (for*/list ([i (in-range 5)] [j (in-range 5)]) (f i j))]))

;(define (register-solution-check case make-fun)
;  (for* ([i (in-range max-iter)]
;         [k (in-range max-iter)])
;    (let* ([case1 (case i k)]
;           [make1 (make-fun i k)]
;           [casef (l_eval case1)]
;           [res1 (test-eval case1 casef)]
;           [res2 (test-eval case1 make1)])
;      (if (not (equal? res1 res2))
;        (printf "Error on ~a:\n\t~a\n\t~a\n\n" case1 res1 res2)
;        void; (printf "Check on ~a:\n\t~a\n\t~a\n\n" case1 res1 res2)
;        )))
;  (register-solution-default case make-fun))


(define register-solution register-solution-default) ; no checking
;(define register-solution register-solution-check) ; checking

(define (case-R0-1 i k)
  (list 'R0  (symbolic-s-compose 'P21 'C12 i) (symbolic-s-compose 0 'C10 k)))
(define (make-fun-R0-1 i k)
  (lambda (a) (if (equal? a 0) k (+ a (- i 1)))))
(register-solution case-R0-1 make-fun-R0-1)

(define (case-R0-2 i k)
  (list 'R0  (symbolic-s-compose 'P22 'C12 i) (symbolic-s-compose 0 'C10 k)))
(define (make-fun-R0-2 i k)
  (lambda (a) (+ (* a i) k)))
(register-solution case-R0-2 make-fun-R0-2)

(define (case-R1-1 i k)
  (list 'R1  (symbolic-s-compose 'P31 'C13 i) (symbolic-s-only-compose k)))
(define (make-fun-R1-1 i k)
  (lambda (a b) (if (equal? a 0) (+ b k) (+ a (- i 1)))))
(register-solution case-R1-1 make-fun-R1-1)

(define (case-R1-2 i k)
  (list 'R1  (symbolic-s-compose 'P32 'C13 i) (symbolic-s-only-compose k)))
(define (make-fun-R1-2 i k)
  (lambda (a b) (+ (* a i) (+ b k))))
(register-solution case-R1-2 make-fun-R1-2)

(define (case-R1-3 i k)
  (list 'R1  (symbolic-s-compose 'P33 'C13 i) (symbolic-s-only-compose k)))
(define (make-fun-R1-3 i k)
  (lambda (a b) (if (equal? a 0) (+ b k) (+ i b))))
(register-solution case-R1-3 make-fun-R1-3)
