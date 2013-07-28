#lang racket
; Warning: At decent depths (>9) this consumes gigs of memory and requires the 64bit version of Racket.

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
                  (lambda (s f l)  ; ok?
                    (begin
                      (set! update-count (+ 1 update-count)) ; !side-effect on update-count!
                      (when (equal? (modulo update-count 1000) 0) (displayln (list 'update-count update-count)))
                      #t))
                  (lambda (s f l) (run1 f depth1)) ; to-key
                  (lambda (s f l) (list s f l)) ; to-value
                  (lambda (val oval) #f) ; prefer?
                  (lambda (key val) ; on-new -- !side-effect on the accumulator!
                    (begin
                      (displayln (list 'on-new key val))
                      (vector-set! v-accum ix (cons val (vector-ref v-accum ix)))))
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
(define update-count 0)

; Helpers to display the contents of the state
(define (function-counts) (vector-map (lambda (x) (length (hash-keys x))) v-ht))
(define (dump-functions) (vector-map (lambda (ht) (map (lambda (x) (list (first x) (fourth x) (second x))) (sort (hash->list ht) (lambda (x y) (< (fourth x) (fourth y)))))) v-ht))

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
      result
      )))

;install extenders
(for ([arity (in-range 4)]
      [updater v-updaters]
      [initial pr-initial])
  (set-lazy-vector-extender!
   (vector-ref v-lv arity) 
   (make-extender arity updater initial pr-induce)))

;(time (lazy-vector-ref (vector-ref v-lv 0) 9))
;(time (lazy-vector-ref (vector-ref v-lv 0) 11)) ; DrScheme 32bit crashes at this point but racket32bit didn't
;(time (lazy-vector-ref (vector-ref v-lv 0) 12)) ; DrScheme 64bit made it here
(time (lazy-vector-ref (vector-ref v-lv 0) 14)) ; Sufficient to get "7" with (C10 (C21 (R1 (C13 S P31) S) S S) (C10 (C11 S S) 0))
;(time (lazy-vector-ref (vector-ref v-lv 0) 15)) ; racket crashes here with "Racket virtual machine has run out of memory; aborting"; 64bit doesn't crash but consumes +50gig swap

; Aha! Part of the slow-down is that some of these functions are getting complicated; here's two in 1 9:
; c.f. Eulerian numbers: http://oeis.org/A000295
;(on-new (0 0 1 4 11 26 57 120 247 502 1013 2036 4083 8178 16369 32752 65519 131054 262125 524268 1048555 2097130 4194281 8388584 16777191) ((R0 (R1 (C13 S (C13 S P31)) P11) 0) #<procedure:phi> 9))
;(on-new (0 1 4 11 26 57 120 247 502 1013 2036 4083 8178 16369 32752 65519 131054 262125 524268 1048555 2097130 4194281 8388584 16777191 33554406) ((R0 (R1 (C13 S (C13 S P31)) S) 0) #<procedure:phi> 9))
; DrRacket segfaults in 1 11 with 8192 memory.

;(vector-map (lambda (x) (lazy-vector->vector x)) v-lv)
(dump-functions)

(define outname "out/functions14")
(require racket/serialize)
(define ofile (open-output-file (string-append outname ".serial") #:exists 'replace))
(write (serialize (dump-functions)) ofile)
(close-output-port ofile)
(define ofile2 (open-output-file (string-append outname ".txt") #:exists 'replace))
(write (dump-functions) ofile2)
(close-output-port ofile2)
