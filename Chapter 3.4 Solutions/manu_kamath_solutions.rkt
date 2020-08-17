#lang sicp
;SICP 3.4

;Exercise 3.38

;a) Running sequentially - no interleavings 

;Peter: Pt
;Paul : P
;Mary : M
;
;Transaction:                                      Bank
;(Name, D/W, Amt)                                 Balance
;----------------------------------------------|-------------
;1. (Pt,D,10)->(P,W,20)->M(W,= 90/2)           |    45
;2. (Pt,D,10)->(M,W,=110/2)->(P,W,20)          |    35    
;3. (P,W,20)->(Pt,D,10)->(M,W,=90/2)           |    45
;4. (P,W,20)->(M,W,=80/2)->(Pt,D,10)           |    50
;5. (M,W,=100/2)->(Pt,D,10)->(P,W,20)          |    40
;6. (M,W,=100/2)->(P,W,20)->(Pt,D,10)          |    40

;b) Interleaving of reads and writes

;90 for example, if Peter reads in the balance as 100 and
;sets the value (100-10) at the end after M and Paul are done

;Exercise 3.39

(define x 10)
(define s (make-serializer))
(parallel-execute
 (lambda () (set! x ((s (lambda () (* x x))))))
 (s (lambda () (set! x (+ x 1)))))

; 121 --> line 33, then line 32
; 100 --> inner lambda of line 32 captures value 100, then line 33, then the set writes 100 to x
; 101 --> line 32, then line 33

;Exercise 3.40

(define x 10)
(parallel-execute (lambda () (set! x (* x x))) ; L1
                  (lambda () (set! x (* x x x)))) ; L2
; L1 acceses x twice, and sets after evaluating the expression
; L2 accesses x 3 times, then sets after evaluating the expression.
; Possible interleavings are:

; a) 100^3 = 10^6 : L1 then L2 sequentially, OR L2 then L1 sequentially,
;    OR L1 reads x=10, L2 reads x=10, then L1 evaluates and sets x=10*10, then L2 reads x=100, evaluates 100*10, then 1000*1000 and sets it
; b) 1000 : L1 reads x=10, then L2 reads x=10 and evaluates 10*10, then L1 evaluates 100*10 and finally sets it to 1000
; c) 100 : L1 evaluates 100 then sets it at the end
; d) 10&4 : L2 reads inner 2 xs as 10, then L1 evaluates and sets x = 100, then L2 evaluates 100*100 = 10000
; e) 10^5 : L2 reads innermost x=10, then L1 evaluates 10*10 and sets 100, L2 reads xs as =100 each, then L2 evaluates 100*100*10 = 100 000

(define x 10)
(define s (make-serializer))
(parallel-execute (s (lambda () (set! x (* x x)))) ; L1
                  (s (lambda () (set! x (* x x x))))) ; L2

; only 10^6, but note that it could be either L2->L1 or L1->L2 sequentially.

;Exercise 3.41

; Not necessary. Balance is just a read so no anomalous behaviour should arise.
; Ben's change makes deposits and withdrawals atomic, but in this case it's not needed

;Exercise 3.42

; It's safe and achieves the same behaviour of concurrency.


; Textbook's Mutex code

(define (make-serializer)
  (let ((mutex (make-mutex)))
    (lambda (p)
      (define (serialized-p . args)
        (mutex 'acquire)
        (let ((val (apply p args)))
          (mutex 'release)
          val))
      serialized-p)))

(define (make-mutex)
  (let ((cell (list false)))
    (define (the-mutex m)
      (cond ((eq? m 'acquire)
             (if (test-and-set! cell)
                 (the-mutex 'acquire))) ; retry
            ((eq? m 'release) (clear! cell))))
    the-mutex))
(define (clear! cell) (set-car! cell false))

; this is a strawman test-and-set, it must be supported by HW
(define (test-and-set! cell)
  (if (car cell) true (begin (set-car! cell true) false)))

;Exercise 3.47
; a) semaphore from mutexes

(define (make-semaphore N)
  (let ((mutex (make-mutex))
        (count 0))
    (define (the-semaphore m)
      (cond ((eq? m 'P)
             (mutex 'acquire)
             (if (< count N)
                 (begin (set! count (+ count 1))
                        (mutex 'release))
                 ; count = N so block thread/process and have it retry
                 (begin ((mutex) 'release)
                        (the-semaphore 'aquire)))
             ((eq? m 'V)
              (mutex 'acquire)
              (set! count (- count 1))
              (mutex 'release)))))
    the-semaphore))

; random note: from my understanding, a semaphore is not exactly the same
; as a generalization of a mutex. In other words, a binary semaphore is
; technically not the same as a mutex, because unlike a mutex it doesn't
; have the concept of ownership - multiple threads/processes can signal a semaphore
; Semaphores are used for synchronization, while mutex are used for mutual exclusion.
; (This is at least from a OS/Computer Architecture perspective. But I'm not sure
; if Dijkstra historically described his semaphore primitive the same way).