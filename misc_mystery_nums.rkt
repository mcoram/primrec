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
