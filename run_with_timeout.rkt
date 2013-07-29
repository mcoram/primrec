#lang racket

(provide run-with-timeout)

; First try

;(define result (make-vector 20))
;(define sema1 (make-semaphore 0))
;(define thd1
;  (thread (lambda ()
;            (for ([i 20])
;              ;(sleep 2)
;              (sleep .01)
;              (vector-set! result i i)
;              (printf "thread 1\n"))
;            (semaphore-post sema1))))
;(when (not (sync/timeout 5 sema1)) 
;  (kill-thread thd1)
;  (printf "aborting\n"))
;(printf "after\n")

; Works, so moving on to abstraction stage.

(define (run-with-timeout thunk timeout)
  (define sema1 (make-semaphore 0))
  (define thd1
    (thread (lambda () 
              (thunk)
              (semaphore-post sema1))))
  (if (equal? (sync/timeout timeout sema1) #f)
      (begin ; timeout happened
        (kill-thread thd1)
        #f
        )
      #t ; thunk finished
      ))


; test it
(define (test-run-with-timeout) 
   (define result null)
   (define (makethunk delay)
     (lambda ()
       (for ([i 20])
         (sleep delay)
         (vector-set! result i (+ 1 i))
         (printf "thread 1\n"))
       ))
   (for ([comptime '(0.01 2)])
     (set! result (make-vector 20))
     (printf "testing with comptime=~s\n" comptime)
     (if (run-with-timeout (makethunk comptime) 5)
         (begin
           (printf "thunk finished\n")
           (displayln result))
         (begin
           (printf "thunk failed\n")
           (displayln result)))))
