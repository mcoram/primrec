#lang racket
(require racket/match)

(provide peephole-check)

(define (peephole-check t)
  (match t
    [`{C10 P11 ,f} f]
    [`{C11 P11 ,f} f]
    [`{C11 ,f P11} f]
    [`{C21 P21 ,f ,a} f]
    [`{C21 P22 ,a ,f} f]
    [`{C31 P31 ,f ,a ,b} f]
    [`{C31 P32 ,a ,f ,b} f]
    [`{C31 P33 ,a ,b ,f} f]
    [`{C11 (R0 P21 0) (C11 S ,f)} f] ;decrement the increment of the result of f
    [_ null]))

; More candidates.
;[`{C22 P21 ,f ,a} f]
;[`{C22 P22 ,a ,f} f]
;[`{C33 P31 ,f ,a ,b} f]
;[`{C33 P32 ,a ,f ,b} f]
;[`{C33 P33 ,a ,b ,f} f]
; C23 and C32 also?
; C13 P11

; I nearly added this rule, but it's not true unless (f 0)==0. (it'd be true generally if (C10 ,f 0) was in place of the 0, but it's too complex)
;    [`{R0 (C12 S (C12 ,f P21)) 0} f]


;(peephole-check '(C11 P11 (C11 P11 (R0 (C12 S (C12 (R0 (R1 (C13 S P32) S) 0) P22)) 0))))
;(peephole-check '(R0 (C12 S (C12 (R0 (R1 (C13 S P32) S) 0) P22)) 0))
;(peephole-check '(R0 (C12 S (C12 (R0 (C12 (R0 (R1 (C13 S P32) S) 0) (C12 S P22)) 0) P21)) 0))

(define (test-peephole-check)
  (define fin (open-input-file "misc_peephole_cases.txt"))
  (define line null)
  (define (loop) 
    (set! line (read fin))
    (when (not (eof-object? line))
      (if (null? (peephole-check (third line))) 
          (printf "no peephole: ~a\n" (third line))
          (printf "   peephole: ~a\n" (third line)))
      (loop)))
  (loop))

;(test-peephole-check) ; we catch a lot of the repeats but may be missing quite a few yet.


