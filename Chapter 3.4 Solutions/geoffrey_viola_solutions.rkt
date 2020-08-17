#lang racket

;; Exercise 3.38.
;; a
;; 3! = 6 total permutations 4 different values
;; 
;; Peter, Paul, Mary = $45
;; Paul, Peter, Mary = $45
;; 
;; Paul, Mary, Peter = $50
;; 
;; Peter, Mary, Paul = $35
;; 
;; Mary, Paul, Peter = $40
;; Mary, Peter, Paul = $40


;; b
;; Some actors can read the initial balance and write the final amount
;; Peter $110
;; Paul $80
;; Mary $50
;; 
;; Some actors can effectively get skipped
;; Peter, Mary = $60
;; Paul, Mary = $40
;; Mary, Peter = $60
;; Mary, Paul = $30

;; Exercise 3.39
;; 121: P1->P2
;; 101: P1->P2
;; 100: P1 x*x -> P2- >P1 set! with old 100 value
;; 11: P1 x*x -> P2 10 + 1 -> P1 set! x 100 -> P2 set! 11

;; Exercise 3.40
;; 1,000,000: P1->P2, P2->P1
;; 100: P1 read all -> P2 -> P2 write
;; 1000: P2 read all -> P1 -> P2 write
;; 10,000: P1 read first x -> P2 -> P1 10 * 1000, P2 reads two x's -> P1 -> P2 10 * 10 * 100
;; 100,000: P2 reads first x -> P1 -> P2 10 * 100 * 100

;; After serialization only 1000000 is possible

;; Exercise 3.41
;; I disagree. Reading the balance is atomic by definition.
;; It will either read before or after a modification.
;; Therefore, it is correct at the time it is called.

;; Exercise 3.42.
;; There shouldn't make a difference to the concurrency,
;; since the withdraw and deposit are protected.

;; Exercise 3.47 a
;; placeholder function
(define (make-mutex) false)

(define (make-semaphore n) 
  (let ((lock (make-mutex)) 
        (taken 0)) 
    (define (semaphore command) 
      (match command
        ['acquire (begin
                    (lock 'acquire) 
                    (if (< taken n) 
                        (begin (set! taken (+ taken 1)) (lock 'release)) 
                        (begin (lock 'release) (semaphore 'acquire))))]
        ['release (begin
                    (lock 'acquire) 
                    (set! taken (- taken 1)) 
                    (lock 'release))]))
    semaphore))
