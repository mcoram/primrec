#lang racket

(require "pr_primitives.rkt")
(require "lazy-vector.rkt")
(require "ksumj.rkt")
(require "util.rkt")
(require "run_with_timeout.rkt")
(require racket/serialize)

; Limits
(define timeout-per-eval 5) ; only allow this many seconds for an evaluation
(define evaluation-limits '(0 25 5 3)) ; These get a bit slow at depth 11 or so
;(define evaluation-limits '(0 4 4 3)) ; weak limits get it done
;Customize this to output as you go
(define (on-end-extender arity depth)
  (when (equal? arity 0)
    (printf "Dumping\n")
    (define outname "out/functions")
    (define ofile (open-output-file (string-append outname ".serial") #:exists 'replace))
    (write (serialize (list arity depth (dump-functions) (dump-slow))) ofile)
    (close-output-port ofile)
    (define ofile2 (open-output-file (string-append outname ".txt") #:exists 'replace))
    (write (dump-functions) ofile2)
    (write (dump-slow) ofile2)
    (close-output-port ofile2)
    ))


;Module-level state ;;;;;;;;;;;;;;;;;;

;make 4 list accumulators for functions of arities 0..3 in the current accumulation (i.e. one step of the lazy-vector extension)
(define v-accum (initialize-vector 4 (lambda (ix) null)))
;make 4 hashtables to store observably distinct functions of arities 0..3
(define v-ht (initialize-vector 4 (lambda (ix) (make-hash))))
;make 4 lazy-vectors to store observably distinct functions of each level of complexity; their extenders are set to null b/c they'll be filled in later
(define v-lv (initialize-vector 4 (lambda (ix) (make-lazy-vector null))))
;store slow functions for followup processing
(define l-slow null)
;keep track of update calls
(define update-count 0)

;build evaluators of the primrecs of arity 0..3 on arguments from the set (0..(depth1-1))^arity
(define-syntax-rule (make-evaluator result s f l depth result-size evaluation-body timeout)
  (lambda (s f l depth)
    (define rs result-size)
    (define result (make-vector rs))
    (define result-time -1)
    (define mythunk (lambda ()
                      (let ([run1 (lambda () evaluation-body)]) ; evaluation-body called for side-effect on result
                        (let-values ([(dummyres t1 t2 t3) (time-apply run1 null)])
                          (set! result-time t1)))))
    (for ([ix (in-range rs)]) (vector-set! result ix -1)) ; intialize result to impossible values
    ;(displayln (list 'running s l))
    (let ([completed (run-with-timeout mythunk timeout)])
      (when (not completed)
        (begin
          (displayln (list 'evaluation-timeout s l result))
          (set! l-slow (cons (list s f l result) l-slow)) ; !side-effect on l-slow! to store slow functions
          ))
      (values result result-time completed))))

(define v-evaluators
  (vector 
   (make-evaluator result s f l depth 1
                   (vector-set! result 0 f)
                   timeout-per-eval)
   (make-evaluator result s f l depth depth
                   (for* ([i (in-range depth)])
                     (vector-set! result i (f i)))
                   timeout-per-eval)
   (make-evaluator result s f l depth (* depth depth)
                   (let ([counter 0])
                     (for* ([i (in-range depth)]
                            [j (in-range depth)])
                       (vector-set! result counter (f i j))
                       (set! counter (+ 1 counter))))
                   timeout-per-eval)
   (make-evaluator result s f l depth (* depth depth depth)
                   (let ([counter 0])
                     (for* ([i (in-range depth)]
                            [j (in-range depth)]
                            [k (in-range depth)])
                       (vector-set! result counter (f i j k))
                       (set! counter (+ 1 counter))))
                   timeout-per-eval)))

; @@ I notice that evaluation for constants actually happens before the call to the evaluator in the induce step if arity=0; needs fix? would work in the macro version. hmm.

; a moderately generic function to construct hashtable updaters with "programmable behavior"
(define (make-updater ht ok? to-key to-value prefer? on-new on-prefer) 
  (let* 
      ([updater (lambda (x y z)
                  (when (ok? x y z)
                    (let* ([key (to-key x y z)]
                           [val (to-value x y z)]
                           [oval (hash-ref ht key null)])
                      (when (not (equal? key #f))
                        (if (null? oval)
                            (begin (hash-set! ht key val)
                                   (on-new key val))
                            (when (prefer? val oval)
                              (begin
                                (hash-set! ht key val)
                                (on-prefer key val oval))))))))])
    updater))

; builds 4 updaters to handle the four entries in v-ht below
(define (make-updaters v-ht v-accum depths)
  (for/list ([ix (in-range 4)]
             [ht1 v-ht]
             [depth1 depths]
             [run1 v-evaluators])
    (make-updater ht1
                  (lambda (s f l)  ; ok?
                    (begin
                      (set! update-count (+ 1 update-count)) ; !side-effect on update-count!
                      (when (equal? (modulo update-count 1000) 0) (displayln (list 'update-count update-count)))
                      #t))
                  (lambda (s f l) (let-values ([(result runtime completed) (run1 s f l depth1)]) (if completed result #f))) ; to-key
                  (lambda (s f l) (list s f l)) ; to-value
                  (lambda (val oval) #f) ; prefer?
                  (lambda (key val) ; on-new -- !side-effect on the accumulator!
                    (begin
                      (displayln (list 'on-new key val))
                      (vector-set! v-accum ix (cons val (vector-ref v-accum ix)))))
                  (lambda (key val oval) void) ; on-prefer
                  )))

(define v-updaters (list->vector (make-updaters v-ht v-accum evaluation-limits))) ; these will be used to update v-ht primarily but will side effect on v-accum



; Helpers to display the contents of the state
(define (function-counts) (vector-map (lambda (x) (length (hash-keys x))) v-ht))
(define (dump-functions) (vector-map (lambda (ht) (map (lambda (x) (list (first x) (fourth x) (second x))) (sort (hash->list ht) (lambda (x y) (< (fourth x) (fourth y)))))) v-ht))
(define (dump-slow) (map (lambda (row) (list (first row) (third row) (fourth row))) l-slow))

; Test the updaters below; the updater's argument is of the form (symbolic-rep, function-rep, weight):
;((vector-ref v-updaters 1) '(C21 (R1 (C13 S P31) S) S S) (C21 (R1 (C13 S P31) S) S S) 8) ; this is a terse version of i -> 2*i+3
; and confirm that v-accum and v-ht are updated appropriately. check.

; Now let's install the extenders for the lazy-lists in v-lv. the idea is that the 4 lv's correspond to arities 0..3 and that entry ix of the vector is the list of observably distinct functions
; of the right arity that have complexity weight ix exactly (the 0 entries are null). The extenders will "mutually-recurse" with each other through references to the lazy list entries.

(define pr-initial ; a list of functions of each arity from 0..3; functions are presented as (list 'F F 1) for the symbolic version, the function, the "cost"
  (vector
   (list (list 0 0 1) ) ; 0 is the only function of arity 0 initially
   (list (list 'S S 1) (list 'P11 P11 1)) ; S and P11 are arity 1
   (list (list 'P21 P21 1) (list 'P22 P22 1)) ; P21 and P22 are arity 2
   (list (list 'P31 P31 1) (list 'P32 P32 1) (list 'P33 P33 1))))


(define list-of-compose '((C10 C20 C30) (C11 C21 C31) (C12 C22 C32) (C13 C23 C33)))
(define list-of-compose-fun (list (list C10 C20 C30) (list C11 C21 C31) (list C12 C22 C32) (list C13 C23 C33)))

(define (pr-induce depth arity updater)
  (let* 
      ([clist (nth list-of-compose arity)]
       [c1 (nth clist 0)]
       [c2 (nth clist 1)] 
       [c3 (nth clist 2)]
       [clistf (nth list-of-compose-fun arity)]
       [c1f (nth clistf 0)]
       [c2f (nth clistf 1)] 
       [c3f (nth clistf 2)]
       [lv (vector-ref v-lv arity)]
       [lv0 (vector-ref v-lv 0)]
       [lv1 (vector-ref v-lv 1)]
       [lv2 (vector-ref v-lv 2)]
       [lv3 (vector-ref v-lv 3)])
    (begin 
      (for ([weights (ksumj 2 (- depth 1))]) ; C1j
        ;(displayln (list 'weights weights))
        (for* ([f1 (lazy-vector-ref lv1 (first weights))]
               #:unless (and (list? (first f1)) (equal? (first (first f1)) c1)) ; Force C1j constructions to right associate.
               [f2 (lazy-vector-ref lv (second weights))]
               #:unless (and (equal? c1 'C10) (list? (first f2)) (equal? (first (first f2)) c1)) ; Force C10 to be used only once (use a C11 chain as first arg.)
               )
          (updater (list c1 (first f1) (first f2)) 
                   (c1f (second f1) (second f2))
                   (+ 1 (third f1) (third f2)))))
      (for ([weights (ksumj 3 (- depth 1))]) ; C2j
        (for* ([f1 (lazy-vector-ref lv2 (first weights))]
               [f2 (lazy-vector-ref lv (second weights))]
               [f3 (lazy-vector-ref lv (third weights))]
               )
          (updater (list c2 (first f1) (first f2) (first f3)) 
                   (c2f (second f1) (second f2) (second f3))
                   (+ 1 (third f1) (third f2) (third f3)))))
      (for ([weights (ksumj 4 (- depth 1))]) ; C3j
        (for* ([f1 (lazy-vector-ref lv3 (first weights))]
               [f2 (lazy-vector-ref lv (second weights))]
               [f3 (lazy-vector-ref lv (third weights))]
               [f4 (lazy-vector-ref lv (fourth weights))]
               )
          (updater (list c3 (first f1) (first f2) (first f3) (first f4)) 
                   (c3f (second f1) (second f2) (second f3) (second f4))
                   (+ 1 (third f1) (third f2) (third f3) (third f4)))))
      (cond [(equal? arity 1)
             (for ([weights (ksumj 2 (- depth 1))]) ; R0
               (for* ([f1 (lazy-vector-ref lv2 (first weights))]
                      [f2 (lazy-vector-ref lv0 (second weights))]
                      )             
                 (updater (list 'R0 (first f1) (first f2))
                          (R0 (second f1) (second f2))
                          (+ 1 (third f1) (third f2)))))]
            [(equal? arity 2)
             (for ([weights (ksumj 2 (- depth 1))]) ; R1
               (for* ([f1 (lazy-vector-ref lv3 (first weights))]
                      [f2 (lazy-vector-ref lv1 (second weights))]
                      )             
                 (updater (list 'R1 (first f1) (first f2))
                          (R1 (second f1) (second f2))
                          (+ 1 (third f1) (third f2)))))]))))


(define (make-extender arity updater initial induce)
  (lambda (lvself ix)
    (displayln (list 'begin-extender arity ix 'counts (function-counts)))
    (let ([result(cond 
                   [(< ix 1) null]
                   [(equal? ix 1)
                    (begin
                      (for ([x initial]) (apply updater x))
                      initial
                      )] 
                   [else
                    (begin
                      (vector-set! v-accum arity null) ; clear the accumulator
                      (induce ix arity updater) ;... use updater
                      (vector-ref v-accum arity) ; the accumulator is our result
                      )])])
      (displayln (list 'end---extender arity ix 'counts (function-counts)))
      (on-end-extender arity ix)
      result
      )))

;install extenders
(for ([arity (in-range 4)]
      [updater v-updaters]
      [initial pr-initial])
  (set-lazy-vector-extender!
   (vector-ref v-lv arity) 
   (make-extender arity updater initial pr-induce)))

;(define dummy (time (lazy-vector-ref (vector-ref v-lv 0) 16))) ; worked got to 10
(define dummy (time (lazy-vector-ref (vector-ref v-lv 0) 50))) ; may take a LONG time; will kill when bored.

; Aha! Part of the slow-down is that some of these functions are getting complicated; here's two in 1 9:
; c.f. Eulerian numbers: http://oeis.org/A000295
;(on-new (0 0 1 4 11 26 57 120 247 502 1013 2036 4083 8178 16369 32752 65519 131054 262125 524268 1048555 2097130 4194281 8388584 16777191) ((R0 (R1 (C13 S (C13 S P31)) P11) 0) #<procedure:phi> 9))
;(on-new (0 1 4 11 26 57 120 247 502 1013 2036 4083 8178 16369 32752 65519 131054 262125 524268 1048555 2097130 4194281 8388584 16777191 33554406) ((R0 (R1 (C13 S (C13 S P31)) S) 0) #<procedure:phi> 9))
; DrRacket segfaults in 1 11 with 8192 memory.
;Yay. The new R0 and R1 help. New challenges:
;(running (R0 (R1 (C13 S (C13 S (C13 S P31))) P11) 0) 11)
;(running (R0 (R1 (C23 (R1 (C13 S P31) P11) P31 P31) P11) 0) 12)
;(running (R0 (R1 (C23 (R1 (C13 S P31) P11) P31 P31) S) 0) 12)

;(running (R1 (C13 (R0 (R1 (C13 S P31) S) 0) P31) S) 11)

;(vector-map (lambda (x) (lazy-vector->vector x)) v-lv)
(dump-functions)
(displayln l-slow)

;(define outname "out/functions15")
;(require racket/serialize)
;(define ofile (open-output-file (string-append outname ".serial") #:exists 'replace))
;(write (serialize (dump-functions)) ofile)
;(close-output-port ofile)
;(define ofile2 (open-output-file (string-append outname ".txt") #:exists 'replace))
;(write (dump-functions) ofile2)
;(close-output-port ofile2)
