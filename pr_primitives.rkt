#lang racket

(provide Z S P11 P21 P22 P31 P32 P33 R0 R1 C10 C11 C12 C13 C20 C21 C22 C23 C30 C31 C32 C33 R0-diagnose R1-diagnose)

; Primitive operations
(define Z 0)
(define (S x) (+ 1 x))
(define (P11 x) x)
(define (P21 x y) x)
(define (P22 x y) y)
(define (P31 x y z) x)
(define (P32 x y z) y)
(define (P33 x y z) z)

; Primitive recursion operators
(define (R0-iter f g) ;  primitive recursion on an arity 2 and arity 0 (tech. I present the args to f in reverse classic order)
  (lambda (x)
    (let ([u g])
      (for ([i (in-range 0 x)])
        (set! u (f u i)))
      u)))
(define (R1-iter f g) ;  primitive recursion on an arity 3 and arity 1 (tech. I present the args to f in reverse classic order)
  (lambda (x y)
    (let ([u (g y)])
      (for ([i (in-range 0 x)])
        (set! u (f u i y)))
      u)))
(define (R0-rec f g) ;  primitive recursion on an arity 2 and arity 0 (tech. I present the args to f in reverse classic order)
  (letrec ([phi (lambda (x) 
                  (if (equal? x 0) 
                      g 
                      (let ([xm (- x 1)])
                        (f (phi xm) xm))))])
    phi))
(define (R1-rec f g) ;  primitive recursion on an arity 3 and arity 1 (tech. I present the args to f in reverse classic order)
  (letrec ([phi (lambda (x1 x2) 
                  (if (equal? x1 0) 
                      (g x2) 
                      (let ([x1m (- x1 1)])
                        (f (phi x1m x2) x1m x2))))]) 
    phi))
(define (R0-diagnose f g) ;  primitive recursion on an arity 2 and arity 0 (tech. I present the args to f in reverse classic order)
  (lambda (x)
    (let ([u g])
      (displayln (list 'R0-looping-to x 'initial 'val u))
      (for ([i (in-range 0 x)])
        (set! u (f u i))
        (displayln (list 'R0-looping-to x 'at i 'val u)))
      u)))
(define (R1-diagnose f g) ;  primitive recursion on an arity 2 and arity 0 (tech. I present the args to f in reverse classic order)
  (lambda (x y)
    (let ([u (g y)])
      (displayln (list 'R1-looping-to x 'initial 'val u 'argument y))
      (for ([i (in-range 0 x)])
        (set! u (f u i y))
        (displayln (list 'R1-looping-to x 'at i 'val u 'argument y)))
      u)))
(define R0 R0-iter)
(define R1 R1-iter)

; Composition operators; Cij composes one arity i function with i arity j functions that all recieve the same j arguments so that the result of the composition is an arity j function
(define (C10 f1 f2)
  (f1 f2))
(define (C11 f1 f2)
  (lambda (x1) (f1 (f2 x1))))
(define (C12 f1 f2)
  (lambda (x1 x2) (f1 (f2 x1 x2))))
(define (C13 f1 f2)
  (lambda (x1 x2 x3) (f1 (f2 x1 x2 x3))))
(define (C20 f1 f2 f3)
  (f1 f2 f3))
(define (C21 f1 f2 f3)
  (lambda (x1) (f1 (f2 x1) (f3 x1))))
(define (C22 f1 f2 f3)
  (lambda (x1 x2) (f1 (f2 x1 x2) (f3 x1 x2))))
(define (C23 f1 f2 f3)
  (lambda (x1 x2 x3) (f1 (f2 x1 x2 x3) (f3 x1 x2 x3))))
(define (C30 f1 f2 f3 f4)
  (f1 f2 f3 f4))
(define (C31 f1 f2 f3 f4)
  (lambda (x1) (f1 (f2 x1) (f3 x1) (f4 x1))))
(define (C32 f1 f2 f3 f4)
  (lambda (x1 x2) (f1 (f2 x1 x2) (f3 x1 x2) (f4 x1 x2))))
(define (C33 f1 f2 f3 f4)
  (lambda (x1 x2 x3) (f1 (f2 x1 x2 x3) (f3 x1 x2 x3) (f4 x1 x2 x3))))

; This example constructs the doubling function and applies it to 17 via primitive recursion
; (C10 (R0 (C12 S (C12 S P21)) 0) 17) 


; Speed test for the R0 and R1 operators; iter iter wins
; c.f. Eulerian numbers: http://oeis.org/A000295
;'(0 0 1 4 11 26 57 120 247 502 1013 2036 4083 8178 16369 32752 65519 131054 262125 524268)
;(time (for/list ([x (in-range 20)]) ((R0-iter (R1-iter (C13 S (C13 S P31)) P11) 0) x)))
;(time (for/list ([x (in-range 20)]) ((R0-rec (R1-rec (C13 S (C13 S P31)) P11) 0) x)))
;(time (for/list ([x (in-range 20)]) ((R0-iter (R1-rec (C13 S (C13 S P31)) P11) 0) x)))
;(time (for/list ([x (in-range 20)]) ((R0-rec (R1-iter (C13 S (C13 S P31)) P11) 0) x)))

;> (time (for/list ([x (in-range 25)]) ((R0-iter (R1-iter (C13 S (C13 S P31)) P11) 0) x)))
;cpu time: 4629 real time: 4627 gc time: 0
;> (time (for/list ([x (in-range 25)]) ((R0-rec (R1-rec (C13 S (C13 S P31)) P11) 0) x)))
;cpu time: 154281 real time: 161707 gc time: 77077

;Yay. The new R0 and R1 help. New challenge:
;(running (R0 (R1 (C13 S (C13 S (C13 S P31))) P11) 0) 11)
;(time (for/list ([x (in-range 18)]) ((R0 (R1 (C13 S (C13 S (C13 S P31))) P11) 0) x)))
;cpu time: 8660 real time: 8668 gc time: 26
;'(0 0 1 5 18 58 179 543 1636 4916 14757 44281 132854 398574 1195735 3587219 10761672 32285032)

;(R0 (R1 (C23 (R1 (C13 S P31) P11) P31 P31) P11) 0)
;(time (for/list ([x (in-range 6)]) ((R0 (R1 (C23 (R1 (C13 S P31) P11) P31 P31) P11) 0) x)))
; no result after a couple days of trying -- too huge?