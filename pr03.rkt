#lang racket
; In this version we see if it's faster if I avoid all those calls to eval and build the functions as I go
; Yes, it seems to be distinctly faster.
; For the next optim. I'll change C1j so that right associative only (this is to remove from the search alt. ways to assoc.)

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


(define gen0 ; a list of functions of each arity from 0..3; functions are presented as (list 'F F 1) for the symbolic version, the function, the "cost"
  (list
   (list (list 0 0 1) ) ; 0 is the only function of arity 0 initially
   (list (list 'S S 1) (list 'P11 P11 1)) ; S and P11 are arity 1
   (list (list 'P21 P21 1) (list 'P22 P22 1)) ; P21 and P22 are arity 2
   (list (list 'P31 P31 1) (list 'P32 P32 1) (list 'P33 P33 1))))

(define list-of-compose '((C10 C20 C30) (C11 C21 C31) (C12 C22 C32) (C13 C23 C33)))
(define list-of-compose-fun (list (list C10 C20 C30) (list C11 C21 C31) (list C12 C22 C32) (list C13 C23 C33)))

(define nth list-ref)
(define (index-of lst ele)
  (let loop ((lst lst)
             (idx 0))
    (cond ((empty? lst) #f)
          ((equal? (first lst) ele) idx)
          (else (loop (rest lst) (add1 idx))))))

(define (compose-extend gen arity updater)
  (let* 
      ([clist (nth list-of-compose arity)]
       [c1 (nth clist 0)]
       [c2 (nth clist 1)] 
       [c3 (nth clist 2)]
       [clistf (nth list-of-compose-fun arity)]
       [c1f (nth clistf 0)]
       [c2f (nth clistf 1)] 
       [c3f (nth clistf 2)]
       [gen1 (nth gen 1)]
       [gen2 (nth gen 2)]
       [gen3 (nth gen 3)]
       [gena (nth gen arity)])
    (begin
       (for* ([f1 gen1]
              #:unless (and (list? (first f1)) (equal? (first (first f1)) c1)) ; Force C1j constructions to right associate.
	      [f2 gena]
              #:unless (and (equal? c1 'C10) (list? (first f2)) (equal? (first (first f2)) c1)) ; Force C10 to be used only once (use a C11 chain as first arg.)
              )
         (updater (list c1 (first f1) (first f2)) 
                    (c1f (second f1) (second f2))
                    (+ 1 (third f1) (third f2))))
       (for* ([f1 gen2]
	      [f2 gena]
	      [f3 gena])
             (updater (list c2 (first f1) (first f2) (first f3)) 
		      (c2f (second f1) (second f2) (second f3))
		      (+ 1 (third f1) (third f2) (third f3))))
       (for* ([f1 gen3]
	      [f2 gena]
	      [f3 gena]
	      [f4 gena])
             (updater (list c3 (first f1) (first f2) (first f3) (first f4)) 
		      (c3f (second f1) (second f2) (second f3) (second f4))
		      (+ 1 (third f1) (third f2) (third f3) (third f4)))))))

(define (extendht updaters gen)
  (begin
    (compose-extend gen 0 (nth updaters 0))
    (compose-extend gen 1 (nth updaters 1)) 
    (for* ([f1 (nth gen 2)]
           [f2 (nth gen 0)])
      ((nth updaters 1) (list 'R0 (first f1) (first f2))
                        (R0 (second f1) (second f2))
			(+ 1 (third f1) (third f2))))
    (compose-extend gen 2 (nth updaters 2)) 
    (for* ([f1 (nth gen 3)]
           [f2 (nth gen 1)])
	  ((nth updaters 2) (list 'R1 (first f1) (first f2))
	                    (R1 (second f1) (second f2))
			    (+ 1 (third f1) (third f2))))
    (compose-extend gen 3 (nth updaters 3))))

(define (extendht1 updaters gen)
  (begin
    (compose-extend gen 0 (nth updaters 0))
    ))

(define run-depths (list (lambda (x depth1) x)
                         (lambda (f depth1) (for*/list ([i (in-range depth1)]) (f i)))
                         (lambda (f depth1) (for*/list ([i (in-range depth1)] [j (in-range depth1)]) (f i j)))
                         (lambda (f depth1) (for*/list ([i (in-range depth1)] [j (in-range depth1)] [k (in-range depth1)]) (f i j k)))))

; generate a list of hashes to hold the functions of each arity 0..3
(define (make-initial-hashes)
  (for/list ([x (in-range 4)]) (make-hash)))

(define (make-updater ht ok? to-key to-value selection)
  (let* 
      ([updater (lambda (x y z)
                    (when (ok? x y z)
                        (let* ([key (to-key x y z)]
                               [val (to-value x y z)]
                               [oval (hash-ref ht key null)])
                          (if (null? oval)
                              (hash-set! ht key val)
                              (hash-set! ht key (selection val oval))))))])
    updater))

(define (make-updaters genht limits depths)
  (for/list ([ix (in-range 4)]
             [limit1 limits]
             [depth1 depths]
             [run1 run-depths])
    (make-updater (nth genht ix)
                  (lambda (s f l) (<= l limit1)) ; ok?
                  (lambda (s f l) (run1 f depth1)) ; to-key
                  (lambda (s f l) (list s f l)) ; to-value
                  (lambda (v1 v2) (if (< (third v2) (third v1)) v2 v1)) ; selection
                  )))

(define (install-gen updaters gen)
  (for ([arity (in-range 4)]
	[gen1 gen]
	[updater1 updaters])
       (for ([x gen1]) (updater1 (first x) (second x) (third x)))))

(define (extractgen genht)
  (map (lambda (g) (hash-map g
                    (lambda (key value) value)))
       genht))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; globals (to escape the body of time; yuck)
(define genht1 null)
(define updaters1 null)
(define gen1 null)
(define gen2 null)
(define gen3 null)
(define updaters2 null)
(define gen3_2 null)
(define gen3_3 null)
(time
 (begin
   (set! genht1 (make-initial-hashes))
   ;(set! updaters1 (make-updaters genht1 '(10 10 10 10) '(0 25 5 3)))
   (set! updaters1 (make-updaters genht1 '(1000 1000 1000 1000) '(0 25 5 3)))
   (install-gen updaters1 gen0)
   (displayln (map length gen0))
   (extendht updaters1 gen0)
   (set! gen1 (extractgen genht1))
   (displayln (map length gen1))
   (extendht updaters1 gen1)
   (set! gen2 (extractgen genht1))
   (displayln (map length gen2))
   ;(extendht updaters1 gen2)
   ;(set! gen3 (extractgen genht1))
   ;(displayln (map length gen3))
   (set! gen3 gen2) ; hack

   (set! updaters2 (make-updaters genht1 '(100 10 10 10) '(0 25 5 3)))
   (extendht1 updaters2 gen3)
   (set! gen3_2 (extractgen genht1))
   (map length gen3_2);
   ; (7 . ((C10 (C21 (R1 (C13 S P31) S) S S) (C10 (C11 S S) 0)) 14))
   ; (C10 (C21 (R1 (C13 S P31) S) S S) (C10 (C11 S S) 0))

   (extendht1 updaters2 gen3_2)
   (set! gen3_3 (extractgen genht1))
   (map length gen3_3)

   ))
; (1 2 2 3)(2 5 7 6)(4 33 103 58)(9 6355 119999 50940)cpu time: 4552916 real time: 4554088 gc time: 28219 ; gen3

;(when #f 
;    (begin 
;      (first genht1)
;      ))

;(apply max (map (lambda (x) (apply max x)) (hash-keys (second genht1))))
;(filter (lambda (x) (equal? (apply max (first x)) 327)) (sort (hash->list (second genht1)) (lambda (x y) (< (fourth x) (fourth y)))) )
(sort (hash->list (first genht1)) (lambda (x y) (< (fourth x) (fourth y))))
;(take (sort (hash->list (second genht1)) (lambda (x y) (< (fourth x) (fourth y)))) 500)

;need to remove the functions first
;(require racket/serialize)
;(define ofile (open-output-file "genht3.rkts" #:exists 'replace))
;(write (serialize gen3) ofile)
;(close-output-port ofile)
