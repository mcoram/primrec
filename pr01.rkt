#lang racket

;;;(define gen0 '(((0)) ((S x1) (P11 x1)) ((P21 x1 x2) (P22 x1 x2)) ((P31 x1 x2 x3) (P32 x1 x2 x3) (P33 x1 x2 x3))))
(define gen0 '( (0)
                (S P11)
                (P21 P22)
                (P31 P32 P33)))

;(match `{with {x 1} {+ x 1}}
;    [`{with {,id ,rhs} ,body}
;     `{{lambda {,id} ,body} ,rhs}])

;(define ns (make-base-namespace))
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

  ;(eval (match '(C10 S 0)
;  [`{C10 ,f1 ,f2}
;   `{,f1 ,f2} ]) ns)
;(eval '(S 0) ns)

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


;(C10 S (C10 S 0))
;(eval '(C10 S 0) ns)
;(C10 (C11 (C11 S S) S) 0)

(require macro-debugger/expand)
;(syntax->datum
;   (expand-only #'(let ([x 1] [y 2]) (or (even? x) (even? y)))
;                (list #'or)))

;(syntax->datum
;   (expand-only #'(C10 S (C10 S 0))
;                (list #'C10)))
;(syntax->datum (expand-syntax #'(C10 S (C10 S 0))))

;(syntax->datum (expand-syntax #'(C10 (C11 (C11 S S) S) 0)))

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
(define (compose-extend gen arity)
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

(define (raw-extend gen) 
  (list (compose-extend gen 0)
   (append (compose-extend gen 1) 
           (for*/list ([f1 (nth gen 2)]
                       [f2 (nth gen 0)])
             `{R0 ,f1 ,f2}))
   (append (compose-extend gen 2)
           (for*/list ([f1 (nth gen 3)]
                       [f2 (nth gen 1)])
             `{R1 ,f1 ,f2}))
   
   (compose-extend gen 3)))
 
(define (flat-length x)
  (cond 
        [(null? x) 0]
        [(pair? x) (+ (flat-length (car x)) (flat-length(cdr x)))]
        [#t 1]))

(raw-extend gen0)
(map l_eval (first (raw-extend gen0)))
(map (lambda (f) (map f '(0 1 2 3 4 5))) (map l_eval (second (raw-extend gen0))))
