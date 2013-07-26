#lang racket

(define gen0 '( (0)
                (S P11)
                (P21 P22)
                (P31 P32 P33)))

(define-namespace-anchor anchor1)
(define ns (namespace-anchor->namespace anchor1))
(define (l_eval x) (eval x ns))

(define Z 0)
(define (S x) (+ 1 x))
(define (P11 x) x)
(define (P21 x y) x)
(define (P22 x y) y)
(define (P31 x y z) x)
(define (P32 x y z) y)
(define (P33 x y z) z)

(define nth list-ref)

(define-syntax-rule (C10 f1 f2)
  (f1 f2))
(define-syntax-rule (C11 f1 f2)
  (lambda (x1) (f1 (f2 x1))))
(define-syntax-rule (C12 f1 f2)
  (lambda (x1 x2) (f1 (f2 x1 x2))))
(define-syntax-rule (C13 f1 f2)
  (lambda (x1 x2 x3) (f1 (f2 x1 x2 x3))))
(define-syntax-rule (C20 f1 f2 f3)
  (f1 f2 f3))
(define-syntax-rule (C21 f1 f2 f3)
  (lambda (x1) (f1 (f2 x1) (f3 x1))))
(define-syntax-rule (C22 f1 f2 f3)
  (lambda (x1 x2) (f1 (f2 x1 x2) (f3 x1 x2))))
(define-syntax-rule (C23 f1 f2 f3)
  (lambda (x1 x2 x3) (f1 (f2 x1 x2 x3) (f3 x1 x2 x3))))
(define-syntax-rule (C30 f1 f2 f3 f4)
  (f1 f2 f3 f4))
(define-syntax-rule (C31 f1 f2 f3 f4)
  (lambda (x1) (f1 (f2 x1) (f3 x1) (f4 x1))))
(define-syntax-rule (C32 f1 f2 f3 f4)
  (lambda (x1 x2) (f1 (f2 x1 x2) (f3 x1 x2) (f4 x1 x2))))
(define-syntax-rule (C33 f1 f2 f3 f4)
  (lambda (x1 x2 x3) (f1 (f2 x1 x2 x3) (f3 x1 x2 x3) (f4 x1 x2 x3))))
(define-syntax-rule (R0 f g)
  (letrec ([phi (lambda (x) 
                  (if (equal? x 0) 
                      g 
                      (let ([xm (- x 1)])
                        (f (phi xm) xm))))])
    phi))
(define-syntax-rule (R1 f g)
  (letrec ([phi (lambda (x1 x2) 
                  (if (equal? x1 0) 
                      (g x2) 
                      (let ([x1m (- x1 1)])
                        (f (phi x1m x2) x1m x2))))]) 
    phi))

; This example constructs the doubling function (phi below) via primitive recursion (tech. I present the args to f in reverse classic order)
; Use of letrec to do the work seems kind of excessively "fancy", but I guess it'll do since I can't figure out "for"
;(let ([f (C12 S (C12 S P21))]
;      [g 0]) 
;  (letrec ([phi (lambda (x) 
;                  (let ([x2 (- x 1)])
;                    (if (equal? x 0) 
;                      g 
;                      (f (phi x2) x2))))]) 
;    (phi 17)))

;(C10 (R0 (C12 S (C12 S P21)) 0) 17) ; This is the one-liner version using our macros

(define list-of-compose '((C10 C20 C30) (C11 C21 C31) (C12 C22 C32) (C13 C23 C33)))
(define (compose-extend/list gen arity)
  (let* 
      ([clist (nth list-of-compose arity)]
       [c1 (nth clist 0)]
       [c2 (nth clist 1)] 
       [c3 (nth clist 2)]
       [l1 (for*/list ([f1 (nth gen 1)]
                       [f2 (nth gen arity)])
             `{,c1 ,f1 ,f2})]
       [l2 (for*/list ([f1 (nth gen 2)]
                       [f2 (nth gen arity)]
                       [f3 (nth gen arity)])
             `{,c2 ,f1 ,f2 ,f3})]
       [l3 (for*/list ([f1 (nth gen 3)]
                       [f2 (nth gen arity)]
                       [f3 (nth gen arity)]
                       [f4 (nth gen arity)])
             `{,c3 ,f1 ,f2 ,f3 ,f4})])
    (append l1 l2 l3)))
(define (compose-extend gen arity updater)
  (let* 
      ([clist (nth list-of-compose arity)]
       [c1 (nth clist 0)]
       [c2 (nth clist 1)] 
       [c3 (nth clist 2)]
       [l1 (for* ([f1 (nth gen 1)]
                       [f2 (nth gen arity)])
             (updater `{,c1 ,f1 ,f2}))]
       [l2 (for* ([f1 (nth gen 2)]
                       [f2 (nth gen arity)]
                       [f3 (nth gen arity)])
             (updater `{,c2 ,f1 ,f2 ,f3}))]
       [l3 (for* ([f1 (nth gen 3)]
                       [f2 (nth gen arity)]
                       [f3 (nth gen arity)]
                       [f4 (nth gen arity)])
             (updater `{,c3 ,f1 ,f2 ,f3 ,f4}))])
    null))

(define (raw-extend gen) 
  (list (compose-extend/list gen 0)
   (append (compose-extend/list gen 1) 
           (for*/list ([f1 (nth gen 2)]
                       [f2 (nth gen 0)])
             `{R0 ,f1 ,f2}))
   (append (compose-extend/list gen 2)
           (for*/list ([f1 (nth gen 3)]
                       [f2 (nth gen 1)])
             `{R1 ,f1 ,f2}))
   
   (compose-extend/list gen 3)))
 
(define (flat-length x)
  (cond 
        [(null? x) 0]
        [(pair? x) (+ (flat-length (car x)) (flat-length(cdr x)))]
        [#t 1]))


;(define gen1r (raw-extend gen0))
;(map l_eval (first gen1r))
;(map (lambda (f) (map f '(0 1 2 3 4 5))) (map l_eval (second gen1r)))

(define (make-updater ht ok? to-key to-value selection)
  (let* (
         [updater (lambda (x)
                    (if (ok? x)
                        (let* ([key (to-key x)]
                              [val (to-value x)]
                              [oval (hash-ref ht key null)])
                          (if (null? oval)
                              (hash-set! ht key val)
                              (hash-set! ht key (selection val oval))))
                        void))])
    updater))

(let* 
    ([gen gen0]
     [ht (make-hash)]
     [updater (make-updater ht
                                   (lambda (x) #t) ; ok?
                                   l_eval ; to-key
                                   (lambda (x) (list x (flat-length x))) ; to-value
                                   (lambda (v1 v2) (if (< (second v2) (second v1)) v2 v1)) ; selection
                                   )])
  (begin
    (for ([x (first gen)]) (updater x))
    (compose-extend gen 0 updater)
    ht))

(define run-depths (list (lambda (x depth1) (l_eval x))
                         (lambda (x depth1) (let ([f (l_eval x)]) (for*/list ([i (in-range depth1)]) (f i))))
                         (lambda (x depth1) (let ([f (l_eval x)]) (for*/list ([i (in-range depth1)] [j (in-range depth1)]) (f i j))))
                         (lambda (x depth1) (let ([f (l_eval x)]) (for*/list ([i (in-range depth1)] [j (in-range depth1)] [k (in-range depth1)]) (f i j k))))))

; generate a list of hashes of the same length (4) as gen
(define (make-initial-hashes)
  (for/list ([x (in-range 4)]) (make-hash)))

(define (make-updaters genht limits depths)
  (for/list ([ix (in-range 4)])
    (let ([limit1 (nth limits ix)]
          [depth1 (nth depths ix)]
          [run1 (nth run-depths ix)])
      (make-updater (nth genht ix)
                    (lambda (x) (<= (flat-length x) limit1)) ; ok?
                    (lambda (x) (run1 x depth1)) ; to-key
                    (lambda (x) (list x (flat-length x))) ; to-value
                    (lambda (v1 v2) (if (< (second v2) (second v1)) v2 v1)) ; selection
                    ))))

(define (install-gen updaters gen)
  (for ([arity (in-range 4)])
    (let ([gen1 (nth gen arity)]
          [updater1 (nth updaters arity)])
      (for ([x gen1]) (updater1 x)))))

(define (extendht updaters gen)
  (begin
    (compose-extend gen 0 (nth updaters 0))
    (compose-extend gen 1 (nth updaters 1)) 
    (for* ([f1 (nth gen 2)]
           [f2 (nth gen 0)])
      ((nth updaters 1) `{R0 ,f1 ,f2}))
    (compose-extend gen 2 (nth updaters 2)) 
    (for* ([f1 (nth gen 3)]
           [f2 (nth gen 1)])
      ((nth updaters 2)`{R1 ,f1 ,f2}))
    (compose-extend gen 3 (nth updaters 3))))
(define (extractgen genht)
  (map (lambda (g) (hash-map g
                    (lambda (key value) (first value))))
       genht))
(define (extendht1 updaters gen)
  (begin
    (compose-extend gen 0 (nth updaters 0))
    ))

(define genht1 (make-initial-hashes))
(define updaters1 (make-updaters genht1 '(10 10 10 10) '(0 25 5 3)))
(install-gen updaters1 gen0)
(map length gen0)
(extendht updaters1 gen0)
(define gen1 (extractgen genht1))
(map length gen1)
(extendht updaters1 gen1)
(define gen2 (extractgen genht1))
(map length gen2)
(extendht updaters1 gen2)
(define gen3 (extractgen genht1))
(map length gen3)

(define updaters2 (make-updaters genht1 '(100 10 10 10) '(0 25 5 3)))
(extendht1 updaters2 gen3)
(define gen3_2 (extractgen genht1))
(map length gen3_2)
; (7 . ((C10 (C21 (R1 (C13 S P31) S) S S) (C10 (C11 S S) 0)) 14))
; (C10 (C21 (R1 (C13 S P31) S) S S) (C10 (C11 S S) 0))

(extendht1 updaters2 gen3_2)
(define gen3_3 (extractgen genht1))
(map length gen3_3)
(first genht1)

