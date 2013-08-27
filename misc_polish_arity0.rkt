#lang racket

(require "pr_primitives.rkt")
(require "pr_eval.rkt")
(require "predict-extension.rkt")
(require (prefix-in srfi: srfi/48)) ; avoid shadowing racket's format
  

;(C10 (C11 (C21 (R1 (C23 (R1 (C23 (hash-ref compiled-forms '(R1 (C13 S P32) P11)) P32 P32) P11) P32 P33) S) S P11) S) (C10 S 0))
;(pr-compile '(C10 (C11 (C21 (R1 (C23 (R1 (C23 (R1 (C13 S P32) P11) P32 P32) P11) P32 P33) S) S P11) S) (C10 S 0)))
;(get-arity '(C10 (C11 (C21 (R1 (C23 (R1 (C23 (R1 (C13 S P32) P11) P32 P32) P11) P32 P33) S) S P11) S) (C10 S 0)))
;(let-values ([(result result-time completed f) (pr-eval '(C10 (C11 (C21 (R1 (C23 (R1 (C23 (R1 (C13 S P32) P11) P32 P32) P11) P32 P33) S) S P11) S) (C10 S 0)) 10)])
;  result)

(define my-timeout 600)
(define result null)
(define numhash (make-hash))
(for ([x a0l])
  (let* ([x1 (vector-ref (first x) 0)]
         [sexp (third x)]
         [y (if (or (equal? x1 #f) (< x1 0))
                ; compute new x for y
                (let-values ([(result result-time completed f) (pr-eval sexp my-timeout)])
                  (list result (second x) sexp (fourth x) result-time))
                x)]
         [y1 (vector-ref (first y) 0)]
         [v (hash-ref numhash y1 null)])
    (if (or (equal? y1 #f) (< y1 0))
        (set! result (cons y result)) ; y1 is bogus so just store y
        (if (null? v) 
            ;y1 isn't bogus and it's novel
            (begin
              (set! result (cons y result))
              (hash-set! numhash y1 y))
            ;y1 isn't bogus or novel
            (displayln (list 'skip-non-novel y))))))
(set! result (reverse result))
(displayln result)

; this code is a bastardized copy of the predict-extension code
(define (complexity-cut2 result cut) (filter (lambda (x) (<= (second x) cut)) result))
(define (naturals-cut2 cut) (sort (map (lambda (x) (vector-ref (first x) 0)) (complexity-cut2 result cut)) <))
(define (dump-level-sets2 depth) (for ([i (in-range (+ 1 depth))]) (printf "~a\t~a\n" i (format "~a" (naturals-cut2 i)))))
(require racket/set)
(define (dump-natural-ordering2 depth) 
  (define last (list->set null))
  (define current null)
  (define temp null)
  (for ([i (in-range 1 (+ 1 depth))]) 
    (set! current (list->set (naturals-cut2 i)))
    (set! temp (sort (set->list (set-subtract current last)) <))
    (when (> (length temp) 0) (printf "~a \t <\n" temp))
    (set! last current)))
(dump-level-sets2 maxdepth)
(dump-natural-ordering2 maxdepth)

(displayln (list 'number-of-results (map (lambda (x) (length (naturals-cut2 x))) (range (+ 1 maxdepth)))))

(define (log10 x) (/ (log x) (log 10)))
;(map log10 (naturals-cut2 25))
(define (pow10 m) ; I'm building this b/c (expt 10 m) failed for m>156.
  (define x 1)
  (for ([i (in-range m)])
    (set! x (* 10 x)))
  x)

(define (big-format x)
  (if (< x 10000000000000000000000000) (format "~a" x)
      (let* ([m (inexact->exact (floor (log10 x)))]
             [x1 (/ x (pow10 m))])
        (format "~ae+~a" (srfi:format "~20,20F" x1) m))))

;(printf (string-join (map big-format (naturals-cut2 maxdepth)) " "))
(printf "\n\n\n\n")

(define (dump-level-sets3 depth) (for ([i (in-range (+ 1 depth))]) (printf "~a\t~a\n" i (string-join (map big-format (naturals-cut2 i)) " "))))
(dump-level-sets3 maxdepth)


