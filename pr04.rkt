#lang racket
(require "pr_primitives.rkt")
(require "lazy-vector.rkt")
(require "ksumj.rkt")
(require "util.rkt")


;evaluators of the primrecs of arity 0..3 on arguments from the set (0..(depth1-1))^arity
(define run-depths (vector (lambda (x depth1) x)
                           (lambda (f depth1) (for*/list ([i (in-range depth1)]) (f i)))
                           (lambda (f depth1) (for*/list ([i (in-range depth1)] [j (in-range depth1)]) (f i j)))
                           (lambda (f depth1) (for*/list ([i (in-range depth1)] [j (in-range depth1)] [k (in-range depth1)]) (f i j k)))))

; a moderately generic function to construct hashtable updaters with "programmable behavior"
(define (make-updater ht ok? to-key to-value prefer? on-new on-prefer) 
  (let* 
      ([updater (lambda (x y z)
                    (when (ok? x y z)
                        (let* ([key (to-key x y z)]
                               [val (to-value x y z)]
                               [oval (hash-ref ht key null)])
                          (if (null? oval)
                              (begin (hash-set! ht key val)
                                     (on-new key val))
                              (when (prefer? val oval)
                                (begin
                                  (hash-set! ht key val)
                                  (on-prefer key val oval)))))))])
    updater))

; builds 4 updaters to handle the four entries in v-ht below
(define (make-updaters v-ht v-accum depths)
  (for/list ([ix (in-range 4)]
             [ht1 v-ht]
             [depth1 depths]
             [run1 run-depths])
    (make-updater ht1
                  (lambda (s f l) #t) ; ok?
                  (lambda (s f l) (run1 f depth1)) ; to-key
                  (lambda (s f l) (list s f l)) ; to-value
                  (lambda (val oval) #f) ; prefer?
                  (lambda (key val) (vector-set! v-accum ix (cons val (vector-ref v-accum ix)))) ; on-new -- !side-effect on the accumulator!
                  (lambda (key val oval) void) ; on-prefer
                  )))

;module-level state

;make 4 list accumulators for functions of arities 0..3 in the current accumulation (i.e. one step of the lazy-vector extension)
(define v-accum (initialize-vector 4 (lambda (ix) null)))
;make 4 hashtables to store observably distinct functions of arities 0..3
(define v-ht (initialize-vector 4 (lambda (ix) (make-hash))))
;make 4 lazy-vectors to store observably distinct functions of each level of complexity; their extenders are set to null b/c they'll be filled in later
(define v-lv (initialize-vector 4 (lambda (ix) (make-lazy-vector null))))

(define v-updaters (list->vector (make-updaters v-ht v-accum '(0 25 5 3)))) ; these will be used to update v-ht primarily but will side effect on v-accum

; Test the updaters below; the updater's argument is of the form (symbolic-rep, function-rep, weight):
;((vector-ref v-updaters 1) '(C21 (R1 (C13 S P31) S) S S) (C21 (R1 (C13 S P31) S) S S) 8) ; this is a terse version of i -> 2*i+3
; and confirm that v-accum and v-ht are updated appropriately. check.

; Now let's install the extenders for the lazy-lists in v-lv. the idea is that the 4 lv's correspond to arities 0..3 and that entry ix of the vector is the list of observably distinct functions
; of the right arity that have complexity weight ix exactly (the 0 entries are null). The extenders will "mutually-recurse" with each other through references to the lazy list entries.

;install arity 0 extender
(set-lazy-vector-extender!
 (vector-ref v-lv 0) 
 (lambda (lvself ix)
   (let* ([arity 0]
         [myupdater (vector-ref v-updaters arity)])
     (cond 
       [(< ix 1) null]
       [(equal? ix 1)
        (let ([results (list (list 0 0 1) )])
          (begin
            (for ([x results]) (apply myupdater x))
            results
            ))] ; 0 is the only function of arity 0 initially; functions are presented as (list 'F F 1) for the symbolic version, the function, the "cost"
       [else
        (begin
          (vector-set! v-accum arity null) ; clear the accumulator
          ;... use updaters
          (vector-ref v-accum arity) ; the accumulator is our result
          )]))))

(lazy-vector-ref (vector-ref v-lv 0) 1)

(define gen0 ; a list of functions of each arity from 0..3; functions are presented as (list 'F F 1) for the symbolic version, the function, the "cost"
  (list
   (list (list 0 0 1) ) ; 0 is the only function of arity 0 initially
   (list (list 'S S 1) (list 'P11 P11 1)) ; S and P11 are arity 1
   (list (list 'P21 P21 1) (list 'P22 P22 1)) ; P21 and P22 are arity 2
   (list (list 'P31 P31 1) (list 'P32 P32 1) (list 'P33 P33 1))))
