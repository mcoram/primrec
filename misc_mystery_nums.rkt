#lang racket
(define u 40)
(for {[i (in-range 39)]} (set! u (+ (* 2 u) 1)))
;u ; x1=(C10 (C21 (R1 (C23 (R1 (C23 (R1 (C13 S P32) S) P32 P32) S) P32 P32) S) S S) (C10 S 0))


;x1=(C10 (C21 (R1 (C23 (R1 (C23 (R1 (C13 S P32) S) P32 P32) S) P32 P32) S) S S) (C10 S 0))=22539988369407
;x2=(C10 (C21 (R1 (C23 (R1 (C23 (R1 (C13 S P32) S) P32 P32) S) P32 P33) S) S S) (C10 S 0))=8589934591
;x3=(C10 (C21 (R1 (C23 (R1 (C23 (R1 (C13 S P32) S) P32 P32) P11) P32 P32) S) S S) (C10 S 0))=68719476735
;x4=(C10 (C21 (R1 (C23 (R1 (C23 (R1 (C13 S P32) P11) P32 P32) S) P32 P32) S) S S) (C10 S 0))=141733920768
;x5=(C10 (C21 (R1 (C23 (R1 (C23 (R1 (C13 S P32) P11) P32 P32) P11) P32 P32) S) S S) (C10 S 0))=402653184
 
(define (h i j) (+ (+ i j) 1)) ;   (#(1 2 3 4 5 2 3 4 5 6 3 4 5 6 7 4 5 6 7 8 5 6 7 8 9) 5 (R1 (C13 S P32) S) 248 0)
(define (h0 i j) (+ i j)) ;        (#(0 1 2 3 4 1 2 3 4 5 2 3 4 5 6 3 4 5 6 7 4 5 6 7 8) 5 (R1 (C13 S P32) P11) 249 0)

(require "pr_primitives.rkt")

(define x1 (C10 (C21 (R1 (C23 (R1 (C23 h P32 P32) S) P32 P32) S) S S) (C10 S 0)))
(define x2 (C10 (C21 (R1 (C23 (R1 (C23 h P32 P32) S) P32 P33) S) S S) (C10 S 0)))
(define x3 (C10 (C21 (R1 (C23 (R1 (C23 h P32 P32) P11) P32 P32) S) S S) (C10 S 0)))
(define x4 (C10 (C21 (R1 (C23 (R1 (C23 h0 P32 P32) S) P32 P32) S) S S) (C10 S 0)))
(define x5 (C10 (C21 (R1 (C23 (R1 (C23 h0 P32 P32) P11) P32 P32) S) S S) (C10 S 0)))

;(#(666) 26 (C10 (R0 (R1 (C13 (R0 (R1 (C13 S P32) S) 0) (C23 (R1 P33 S) P31 P32)) S) 0) (C10 S (C10 S (C10 S 0)))) 224229036 0)

; These new mysteries at complexity 23 take >10min each to crack after the compiled forms are substituted so they need a new trick.
;(#(-1) 23 (C10 (C21 (R1 (C23 (R1 (C13 (R0 (R1 (C13 S P32) S) 0) P32) S) P32 P32) S) S S) (C10 S 0)) 20543027 -1)
;(#(-1) 23 (C10 (C21 (R1 (C23 (R1 (C13 (R0 (R1 (C13 S P32) S) 0) P32) S) P32 P32) S) S P11) (C10 S 0)) 20543028 -1)
;(#(-1) 23 (C10 (C21 (R1 (C23 (R1 (C13 (R0 (R1 (C13 S P32) S) 0) P32) S) P32 P33) S) S S) (C10 S 0)) 20543033 -1)
;(#(-1) 23 (C10 (C21 (R1 (C23 (R1 (C13 (R0 (R1 (C13 S P32) S) 0) P32) P11) P32 P32) P11) S S) (C10 S 0)) 20543052 -1)

;From the inside out, we have:
;(R1 (C13 S P32) S)          ; (a,b)->a+b+1
;(R0 (R1 (C13 S P32) S) 0)   ; a->a*(a+1)/2
;(R1 (C13 (R0 (R1 (C13 S P32) S) 0) P32) S)
;(#(1 2 3 4 5 2 3 4 5 6 3 4 5 6 7 4 5 6 7 8 5 6 7 8 9) 5 (R1 (C13 S P32) S) 248 0)
;(#(0 1 3 6 10 15 21 28 36 45 55 66 78 91 105 120 136 153 171 190 210 231 253 276 300) 7 (R0 (R1 (C13 S P32) S) 0) 489 0)
;(#(1 2 3 4 5 1 3 6 10 15 1 6 21 55 120 1 21 231 1540 7260 1 231 26796 1186570 26357430) 11 (R1 (C13 (R0 (R1 (C13 S P32) S) 0) P32) S) 32025 0)
; The code for the latter being f: (a,b)->
; let u=b+1
; a times: u=u*(u+1)/2

;Next out is: (R1 (C23 (R1 (C13 (R0 (R1 (C13 S P32) S) 0) P32) S) P32 P32) S)
; Which is code for which is g: (a,b)->
; let u=b+1
; a times: u=f(u,u)

; next out is x -> g(x+1,x+1)
; which is applied to 1
;(C10 (C21 (R1 (C23 (R1 (C13 (R0 (R1 (C13 S P32) S) 0) P32) S) P32 P32) S) S S) (C10 S 0))

; so the code calls g(2,2)
; i.e. let u=3
; u=f(u,u) ; i.e u=1540
; u=f(u,u)

; So the code is:
; u=1541
; 1540 times: u=f(u,u) ; i.e. iterated iterated squaring more or less. ... ok. that's big.
; Let's see, a lower bound for f(3,b) is (((b+1)^2/2)^2/2)^2/2=(b+1)^(2^3)/2^(2^3-1)
; or f(a,b) >= (b+1)^(2^a)/2^(2^a-1)=2*((b+1)/2)^(2^a) >= exp( 2^a * (log(b) - log(2))) = 2^(2^a*(log2(b)-1))
; which, for b>=4 is bigger than 2^(2^a) (generally much bigger)
; So the result is much bigger than 3080 iterations of x->2^x applied to 1541.
