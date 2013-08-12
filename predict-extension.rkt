#lang racket
(require racket/serialize)
(require file/gunzip)
(require "pr_primitives.rkt")
(require "util.rkt")
(provide main)

(printf "loading... ")
(define ldat 
  (let* 
      ([inname "out/functions-full.19"]
       [ifile1 (open-input-file (string-append inname ".serial.gz"))]
       [ifile (open-output-bytes)])
    (gunzip-through-ports ifile1 ifile)
    (close-input-port ifile1)
    (deserialize (read (open-input-bytes (get-output-bytes ifile))))))
(define maxdepth (second ldat))
(define v-functions (third ldat))
(define l-slow (reverse (fourth ldat)))
(define fun-ct (vector-map (lambda (v1) (apply + (vector->list (vector-map length v1)))) v-functions))
(printf "done.\nFunctions of arity 0,1,2,3 respectively are available to depth ~a,~a,~a,~a.\n" maxdepth (- maxdepth 2) (- maxdepth 3) (- maxdepth 4))
(apply printf `{"The total number of functions available are ~a, ~a, ~a, ~a, respectively.\n\n\n" ,@(vector->list fun-ct)})


;(vector-ref v-functions 0)
;(apply append (vector->list (vector-ref v-functions 0)))

(define a0l
  (apply append (vector->list (vector-ref v-functions 0))))

(define (complexity-cut a0l cut) (filter (lambda (x) (<= (second x) cut)) a0l))
(define (naturals-cut cut) (sort (map (lambda (x) (vector-ref (first x) 0)) (complexity-cut a0l cut)) <))
(define (dump-level-sets depth) (for ([i (in-range (+ 1 depth))]) (printf "~a\t~a\n" i (format "~a" (naturals-cut i)))))
(require racket/set)
(define (dump-natural-ordering depth) 
  (define last (list->set null))
  (define current null)
  (define temp null)
  (for ([i (in-range 1 (+ 1 depth))]) 
    (set! current (list->set (naturals-cut i)))
    (set! temp (sort (set->list (set-subtract current last)) <))
    (when (> (length temp) 0) (printf "~a \t <\n" temp))
    (set! last current)))
; (dump-natural-ordering maxdepth)



(define (vector-initial-match v1 v2)
  (define l (min (vector-length v1) (vector-length v2)))
  (for/and ([ix (in-range l)]) (equal? (vector-ref v1 ix) (vector-ref v2 ix))))
(define (find-matches v1 vl1)
  (define lst (apply append (vector->list vl1))) ; this is probably inefficient (esp. to do it repeatedly) but let's try.
  (filter (lambda (x) (vector-initial-match v1 (first x))) lst))
(define (find-a0-matches v1) (find-matches v1 (vector-ref v-functions 0)))
(define (find-a1-matches v1) (find-matches v1 (vector-ref v-functions 1)))
(define (find-a2-matches v1) (find-matches v1 (vector-ref v-functions 2)))
(define (find-s1-matches v1)
  (define lst l-slow) ; @@ should be l-slow1
  (filter (lambda (x) (vector-initial-match v1 (first x))) lst))
(define (escape-parens-regexp str)
  (regexp (string-replace (string-replace str "(" "\\(") ")" "\\)")))
(define (find-by-code-string lst codestr)
  (let ([r1 (escape-parens-regexp codestr)])
    (filter (lambda (x) 
              (regexp-match r1 (format "~a" (third x)))) lst)))
;e.g. to search for the body of 25 being used... (trying to figure out why isn't 26 found as a C11 S version of 25? If my associativity rule is screwing things up, I need to know.)
; edit: there was a bug caused by the associativity rules... it's fixed in this branch.
; (#(25) 21 (C10 (C11 (C21 (R1 (C23 (R1 (C13 S (C13 S P31)) S) P33 P31) S) S S) (C11 S S)) 0))
;(find-by-code-string (vector-ref v-functions 0) "(C21 (R1 (C23 (R1 (C13 S (C13 S P31)) S) P33 P31) S) S S)")
;(find-by-code-string (vector-ref v-functions 1) "(C21 (R1 (C23 (R1 (C13 S (C13 S P31)) S) P33 P31) S) S S)")
;(find-by-code-string l-slow "(C21 (R1 (C23 (R1 (C13 S (C13 S P31)) S) P33 P31) S) S S)")
; Curious. The function and its successor were found in A1:
; (#(5 13 25 41 61 85 113 145 181 221 265 313 365 421 481 545 613 685 761 841 925 1013 1105 1201 1301) 15 (C21 (R1 (C23 (R1 (C13 S (C13 S P31)) S) P33 P31) S) S S))
; (#(6 14 26 42 62 86 114 146 182 222 266 314 366 422 482 546 614 686 762 842 926 1014 1106 1202 1302) 17 (C11 S (C21 (R1 (C23 (R1 (C13 S (C13 S P31)) S) P33 P31) S) S S)))
; So why doesn't 26 enter via:
; (C10 (C11 (C11 S (C21 (R1 (C23 (R1 (C13 S (C13 S P31)) S) P33 P31) S) S S)) (C11 S S)) 0)
; Does the associativity prevent this? c.f. the #unless rules in pr04.rkt.
; Maybe it's the inner A1 function that can't get made, i.e. this guy:
; (C11 (C11 S (C21 (R1 (C23 (R1 (C13 S (C13 S P31)) S) P33 P31) S) S S)) (C11 S S))
; Yes, consider:
; """#:unless (and (list? (first f1)) (equal? (first (first f1)) c1)) ; Force C1j constructions to right associate."""
; In this invocation c1 takes the value 'C11 and it says you have to write instead:
; (C11 S (C11 (C21 (R1 (C23 (R1 (C13 S (C13 S P31)) S) P33 P31) S) S S) (C11 S S)))
; Ah! Oh no! But the necessary f2 isn't in our list because it's left version is. I.e. let's search for:
;  (C11 (C21 (R1 (C23 (R1 (C13 S (C13 S P31)) S) P33 P31) S) S S) (C11 S S))
;(find-by-code-string (vector-ref v-functions 1) "(C11 (C21 (R1 (C23 (R1 (C13 S (C13 S P31)) S) P33 P31) S) S S) (C11 S S))")
; Nothing. So that's it. Another bug. The well intentioned right associative rule together with my "keep only observationally unique" strategy are creating leaks. Bother.

; Find our new first generating function for 15.
;(find-by-code-string (apply append (vector->list (vector-ref v-functions 1))) "(C21 (R1 (C23 (R1 (C13 S P31) S) P31 P31) S) S S)")
; Find our new first generating function for 7. 
;(first (find-by-code-string (apply append (vector->list (vector-ref v-functions 1))) "(C21 (R1 (C13 S P31) S) S S)"))
;(first (find-by-code-string (apply append (vector->list (vector-ref v-functions 2))) "(R1 (C13 S P31) S)"))
;(first (find-by-code-string (apply append (vector->list (vector-ref v-functions 2)))"(R1 (C13 S (C13 S P31)) S)"))
;(first (find-by-code-string (apply append (vector->list (vector-ref v-functions 1))) "(C21 (R1 (C13 S (C13 S P31)) S) S S)"))
;(first (find-by-code-string (apply append (vector->list (vector-ref v-functions 1))) "(C21 (R1 (C13 (R0 (R1 (C13 S (C13 S P31)) S) 0) P31) S) S S)")) ;for 4083

(define (print-at-most num lst)
  (let ([sublst 
         (if (> (length lst) num) (take lst num) lst)])
    (map (lambda (x) (displayln x)) sublst)
    (when (> (length lst) num) (printf "... plus ~a more ...\n" (- (length lst) num)))))

;(find-a1-matches '#(1 2 3))
;(find-a1-matches '#(1 2 4))
;(find-a1-matches '#(1 2 4 8))
;(find-a1-matches '#(1 3 9))
;(find-s1-matches '#(1 3 9))

(define s1 "")
(define v1 null)
(define nprnt 15)

(define (loop)
  (printf "\n\nEnter a sequence of non-negative integers, like \"0 1 2\" or \"0 1 0 1\" and I'll try to find a function that matches your pattern.\n")
  (printf "Specifically, I'll search for a simple primitive recursive function that matches your sequence when it's evaluated at\n\t0 1 2 3 4 ... 24.\n")
  (printf "If you want to find a function of two non-negative integers, like addition, I evaluate functions of two arguments at\n\t (0,0) (0,1) (0,2) (0,3) (0,4) (1,0) (1,1) (1,2) (1,3) (1,4) (2,0) ... (4,4).\n")
  (printf "So, to find addition use e.g. \"0 1 2 3 4 1 2 3 4 5 2 3 4 5 6\" as the query which represents 0..4 plus 0, 1, and 2 respectively.\n")
  ;(printf "So, to find multiplication use e.g. \"0 0 0 0 0 0 1 2 3 4 0 2 4 6 8\" as the query which represents 0..4 times 0, 1, and 2 in sequentially.\n")
  (set! s1 (read-line))
  (if (not (equal? s1 ""))
      (begin
        (set! v1 (list->vector (map string->number (string-split (string-normalize-spaces s1) " "))))
        (when (equal? (vector-length v1) 1)
          (printf "Arity 0 matches:\n")
          (print-at-most nprnt (find-a0-matches v1)))
        (printf "\nArity 1 matches:\n")
        (print-at-most nprnt (find-a1-matches v1))
        (printf "\nArity 2 matches:\n")
        (print-at-most nprnt (find-a2-matches v1))
        ;(printf "\nSlow-to-compute matches:\n")
        ;(print-at-most nprnt (find-s1-matches v1))
        (loop))
      (printf "Bye!\n")))

(define (main . arglst) (loop))
