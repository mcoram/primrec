#lang racket

(provide Z S P11 P21 P22 P31 P32 P33 R0 R1 C10 C11 C12 C13 C20 C21 C22 C23 C30 C31 C32 C33)

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
(define (R0 f g) ;  primitive recursion on an arity 2 and arity 0 (tech. I present the args to f in reverse classic order)
  (letrec ([phi (lambda (x) 
                  (if (equal? x 0) 
                      g 
                      (let ([xm (- x 1)])
                        (f (phi xm) xm))))])
    phi))
(define (R1 f g) ;  primitive recursion on an arity 3 and arity 1 (tech. I present the args to f in reverse classic order)
  (letrec ([phi (lambda (x1 x2) 
                  (if (equal? x1 0) 
                      (g x2) 
                      (let ([x1m (- x1 1)])
                        (f (phi x1m x2) x1m x2))))]) 
    phi))

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
