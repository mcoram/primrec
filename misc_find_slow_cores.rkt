#lang racket
(require "pr_primitives.rkt")
(require "predict-extension.rkt")
(require racket/set)
(require "util.rkt")
 
; The goal of this script was to find (R0 ...) and (R1 ...) cores (i.e. subtrees that do not include further R0 or R1 calls) that contribute to the slowdown of functions
; the thought is that replacing them with specialized code (that uses induction to bypass the loop) will speed things up substantially.
; Fortunately, it looks like all the relevant cores are of very simple types that I can optimize.

(define (find-subtrees-in t set1)
  (define result '())
  (if (list? t)
      (begin
        (when (symbol? (first t))
          (when (set-member? set1 (first t))
            (set! result (cons t result))))
        (apply append (cons result (map (lambda (x) (find-subtrees-in x set1)) t))))
      result))
(define (are-used-internally? t set1)
  (if (list? t)
      (for/or ([x (cdr t)]) (are-used? x set1))
      #f))
(define (are-used? t set1)
  (if (list? t)
      (for/or ([x t]) (are-used? x set1))
      (if (symbol? t) (set-member? set1 t) #f)))


;(define tst (third (nth a1l 50)))
;(define tstl (find-subtrees-in tst (list->set '(R0 R1))))
;(filter (lambda (x) (not (are-used-internally? x (list->set '(R0 R1))))) tstl)

(define (core-subtrees-in t set1)
  (filter (lambda (x) (not (are-used-internally? x set1))) (find-subtrees-in t set1)))

;(core-subtrees-in tst (list->set '(R0 R1)))

(define (make-corel l) (set->list (list->set (apply append (map (lambda (x) (core-subtrees-in (third x) (list->set '(R0 R1)))) l)))))
(define core0l (make-corel a0l))
;'((R1 (C13 S P32) S) (R1 (C13 S P32) P11) (R1 (C13 S (C13 S P32)) S) (R1 (C13 S (C13 S P32)) P11))
;!? that's it?
(define core1l (make-corel a1l))
(define a1sl (filter (lambda (x) (>= (abs (fifth x)) 1)) a1l))
(define core1sl (make-corel a1sl))

; The cases got totally out of hand here. sigh.
(define (cmp-t x y) 
  (if (equal? x 0)
      (if (equal? y 0)
          0
          -1)
      (if (equal? y 0)
          1
          (if (symbol? x)
              (if (symbol? y)
                  (let ([sx (symbol->string x)]
                        [sy (symbol->string y)])
                    (if (string<? sx sy) -1 (if (string>? sx sy) 1 0)))
                  -1)
              (if (symbol? y)
                  1
                  (if (not (equal? (length x) (length y)))
                      (let ([lx (length x)]
                            [ly (length y)])
                        (if (< lx ly) -1 (if (> lx ly) 1 0)))
                      (if (equal? (length x) 0)
                          0
                          (let ([c1 (cmp-t (car x) (car y))])
                            (if (equal? c1 0)
                                (cmp-t (cdr x) (cdr y))
                                c1)))))))))
      
(define (<-t x y) (< (cmp-t x y) 0))
(sort core1sl <-t)




;Compilation targets:

;  (R0 P21 0)
;  (R0 P21 (C10 S 0))
;  (R0 P22 0)
;  (R0 (C12 S (C12 S P21)) 0)
;  (R0 (C12 S (C12 S P22)) 0)
;  (R0 (C12 S (C12 S (C12 S P21))) 0)
;  (R0 (C12 S (C12 S (C12 S P22))) 0)
;  (R0 (C12 S (C12 S (C12 S (C12 S P21)))) 0)
;  (R0 (C12 S (C12 S (C12 S (C12 S P22)))) 0)
;  (R0 (C12 S (C12 S (C12 S (C12 S (C12 S P21))))) 0)
;  (R0 (C12 S (C12 S (C12 S (C12 S (C12 S P22))))) 0)
;  (R0 (C12 S (C12 S (C12 S (C12 S (C12 S (C12 S (C12 S P21))))))) 0)
;  (R1 P31 P11)
;  (R1 P31 S)
;  (R1 P31 (C11 S S))
;  (R1 P31 (C11 S (C11 S S)))
;  (R1 P31 (C11 S (C11 S (C11 S S))))
;  (R1 P31 (C11 S (C11 S (C11 S (C11 S S)))))
;  (R1 P33 S)
;  (R1 P33 (C11 S S))
;  (R1 P33 (C11 S (C11 S S)))
;  (R1 P33 (C11 S (C11 S (C11 S S))))
;  (R1 P33 (C11 S (C11 S (C11 S (C11 S S)))))
;  (R1 (C13 S P31) P11)
;  (R1 (C13 S P32) P11)
;  (R1 (C13 S P32) S)
;  (R1 (C13 S P33) P11)
;  (R1 (C13 S (C13 S P31)) P11)
;  (R1 (C13 S (C13 S P32)) P11)
;  (R1 (C13 S (C13 S P32)) S)
;  (R1 (C13 S (C13 S P33)) P11)
;  (R1 (C13 S (C13 S (C13 S P31))) P11)
;  (R1 (C13 S (C13 S (C13 S P32))) P11)
;  (R1 (C13 S (C13 S (C13 S P32))) S)
;  (R1 (C13 S (C13 S (C13 S P33))) P11)
;  (R1 (C13 S (C13 S (C13 S (C13 S P31)))) P11)
;  (R1 (C13 S (C13 S (C13 S (C13 S P32)))) P11)
;  (R1 (C13 S (C13 S (C13 S (C13 S P32)))) S)
;  (R1 (C13 S (C13 S (C13 S (C13 S P33)))) P11)
;  (R1 (C13 S (C13 S (C13 S (C13 S (C13 S P32))))) P11)
;  (R1 (C13 S (C13 S (C13 S (C13 S (C13 S P32))))) S)
;  (R1 (C13 S (C13 S (C13 S (C13 S (C13 S (C13 S P32)))))) P11)
;  (R1 (C13 S (C13 S (C13 S (C13 S (C13 S (C13 S P33)))))) P11))

  