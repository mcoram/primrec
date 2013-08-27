#lang racket

(require "pr04.rkt")
(require "pr_compiled-forms.rkt")
(require "pr_primitives.rkt")
(require "util.rkt")

(provide get-arity pr-eval pr-compile)

(define-syntax hash-symbol->val
  (lambda (stx)
    (let ([hsym (cadr (syntax->datum stx))]
          [arg (caddr (syntax->datum stx))]
          [result null])
      (for ([a arg])
        (set! result (cons (list 'hash-set! hsym (list 'quote a) a) result)))
      (datum->syntax stx (cons 'begin (reverse result))))))
(define pr-hash (make-hash))
(hash-symbol->val pr-hash (Z S P11 P21 P22 P31 P32 P33 R0 R1 C10 C11 C12 C13 C20 C21 C22 C23 C30 C31 C32 C33))

(define pr-arity-hash #hash((Z . 0)(0 . 0)(S . 1)(P11 . 1)(P21 . 2)(22 . 2)(P31 . 3)(P32 . 3)(P33 . 3)(R0 . 1)(R1 . 2)(C10 . 0)(C11 . 1)(C12 . 2)(C13 . 3)(C20 . 0)(C21 . 1)(C21 . 1)(C22 . 2)(C23 . 3)(C30 . 0)(C31 . 1)(C32 . 2)(C33 . 3)))
(define (get-arity sexp) 
  (if (number? sexp) 0
      (if (symbol? sexp) (hash-ref pr-arity-hash sexp #f)
          (if (list? sexp)
              (if (null? sexp) #f
                  (get-arity (car sexp)))
              #f))))
(define (pr-compile sexp)
  (define c (hash-ref compiled-forms sexp null))
  (if (null? c)
      (if (number? sexp)
          sexp
          (if (list? sexp)
              (let ([sexp2 (map pr-compile sexp)])
                (apply (car sexp2) (cdr sexp2)))
              (let ([v (hash-ref pr-hash sexp null)])
                (if (null? v)
                    sexp
                    v))))
      c))

(define (pr-eval sexp timeout)
  (define arity (get-arity sexp))
  (define flst (list pr-compile sexp))
  (define vev (make-v-evaluators timeout))
  (define ev (vector-ref vev arity))
  (define l (flat-length sexp))
  (ev sexp flst l l))
  ;(values result result-time completed f)
  

;(C10 (C11 (C21 (R1 (C23 (R1 (C23 (hash-ref compiled-forms '(R1 (C13 S P32) P11)) P32 P32) P11) P32 P33) S) S P11) S) (C10 S 0))
;(pr-compile '(C10 (C11 (C21 (R1 (C23 (R1 (C23 (R1 (C13 S P32) P11) P32 P32) P11) P32 P33) S) S P11) S) (C10 S 0)))
;(get-arity '(C10 (C11 (C21 (R1 (C23 (R1 (C23 (R1 (C13 S P32) P11) P32 P32) P11) P32 P33) S) S P11) S) (C10 S 0)))
;(let-values ([(result result-time completed f) (pr-eval '(C10 (C11 (C21 (R1 (C23 (R1 (C23 (R1 (C13 S P32) P11) P32 P32) P11) P32 P33) S) S P11) S) (C10 S 0)) 10)])
;  result)


