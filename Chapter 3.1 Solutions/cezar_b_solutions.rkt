#lang racket
(require rackunit)
(require racket/list)

; Ex 3.1
(define (make-accumulator current)
  (lambda (value)
    (begin
      (set! current (+ current value))
      current)))

(module+ test
  (begin
    (let ((f (make-accumulator 5)))
      (begin
        (check-equal? (f 10) 15)
        (check-equal? (f 10) 25)
        (check-equal? (f 10) 35)        
        )))
  )

; Ex 3.2
(define (make-monitored fn)
  (let ((num-calls 0))
    (define (mf arg)
      (cond ((eq? arg 'how-many-calls?) num-calls)
        ((eq? arg 'reset-count) (set! num-calls 0))
        (else (begin
                (set! num-calls (+ 1 num-calls))
                (fn arg)
                ))))
    mf))
    
(define (inc n) (+ n 1))

(module+ test
  (begin
    (let ((g (make-monitored (lambda (n) (+ n 1)))))
      (begin
        (displayln g)
        (check-equal? (map g '(1 2 3 4 5)) '(2 3 4 5 6))
        (check-equal? (g 'how-many-calls?) 5)
        (check-equal? (map g '(1 2 3 4 5)) '(2 3 4 5 6))
        (check-equal? (g 'how-many-calls?) 10)
        (g 'reset-count)
        (check-equal? (g 'how-many-calls?) 0)
        (check-equal? (map g '(1 2 3 4 5)) '(2 3 4 5 6))
        (check-equal? (g 'how-many-calls?) 5)
        )))
  )

; Ex 3.3 and 3.4
(define (make-account balance password)
  (define (call-the-cops) "No funds for cops")
  (define num-tries 0)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch key m)
    (if (eq? key password)
        (cond ((eq? m 'withdraw) withdraw)
              ((eq? m 'deposit) deposit)
              ((eq? m 'balance) (lambda () balance))
              (else (error "Unknown request: MAKE-ACCOUNT"
                           m)))
        (lambda (amount) (begin
                           (set! num-tries (+ num-tries 1))
                           (displayln num-tries)
                           (if (< num-tries 7)
                               "Incorrect password"                               
                               (call-the-cops))))))
  dispatch)

(module+ test
  (begin
    (let ((acc (make-account 100 '1234)))
      (begin
        (check-equal? ((acc '1234 'withdraw) 50) 50)
        (check-equal? ((acc '1234 'withdraw) 60) "Insufficient funds")
        (check-equal? ((acc '1234 'deposit) 40) 90)
        (check-equal? ((acc '1235 'withdraw) 60) "Incorrect password")
        (check-equal? ((acc '1234 'balance)) 90)
        (check-equal? ((acc '1235 'withdraw) 60) "Incorrect password")
        (check-equal? ((acc '1235 'withdraw) 60) "Incorrect password")
        (check-equal? ((acc '1235 'withdraw) 60) "Incorrect password")
        (check-equal? ((acc '1235 'withdraw) 60) "Incorrect password")
        (check-equal? ((acc '1235 'withdraw) 60) "Incorrect password")
        (check-equal? ((acc '1235 'withdraw) 60) "No funds for cops")
        (check-equal? ((acc '1235 'withdraw) 60) "No funds for cops")
        ))))


; Ex 3.5
(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1)
                 (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1)
                 trials-passed))))
  (iter trials 0))

(define (random-in-range l h)  
    (+ l (* (random) (- h l))))

(define (estimate-integral P x1 x2 y1 y2 trials)
  (define (experiment)
    (let ((x (random-in-range x1 x2))
          (y (random-in-range y1 y2)))
      (P x y)))
  (* 1.0 (monte-carlo trials experiment)))


(define (square x) (* x x))


(module+ test
  (begin
    (let (
          ; line f(x) = x => x^2/2 => ~1/2
          (a (estimate-integral (lambda (x y) (< y x)) 0.0 1.0 0.0 1.0 500000))
      
          ; parbola f(x) = x^3/2 => ~1/3
          (b (estimate-integral (lambda (x y) (< y (square x))) 0.0 1.0 0.0 1.0 500000))
          
          ; pi
          (c (* (estimate-integral (lambda (x y) (< (+ (square x) (square y)) 1.0))
                                   -1.0 1.0 -1.0 1.0 500000)
                4.0)))
      
      (check-within (abs (- a 0.5)) 0.0 0.1)
      (check-within (abs (- b 0.333)) 0.0 0.1)
      (check-within (abs (- c 3.14)) 0.0 0.1)
      )))
 
          
; Ex. 3.7

(define (make-joint parent-account parent-password password)
  (define (dispatch key m)
    (if (eq? key password)
        (parent-account parent-password m)
        (lambda (a . b) "Incorrect joint password")))
  dispatch)



(module+ test
  (let* ((acc0 (make-account 100 '1234))
         (acc1 (make-joint acc0 '1234 '5678)))
    (begin    
      (check-equal? ((acc0 '1234 'withdraw) 50) 50)
      (check-equal? ((acc1 '5678 'withdraw) 10) 40)
      (check-equal? ((acc0 '1234 'balance)) 40)
      (check-equal? ((acc1 'xyzt 'withdraw) 10) "Incorrect joint password")
      )))

; Ex 3.8
(define-syntax-rule (plus> a b)
  (let ((va a)
        (vb b))
    (+ va vb)))

(define-syntax-rule (plus< a b)
  (let ((vb b)
        (va a))
    (+ va vb)))

(define (make-f)
  (define f
    (let ((init null))
      (lambda (value)
        (if (null? init)
            (begin
              (set! init value) ; called the first time -> return the value
              value)
            0 ; called next time return 0
            ))))
  f)

(module+ test
  (let ((f0 (make-f))
        (f1 (make-f)))
    (begin
      (check-equal? (plus> (f0 0) (f0 1)) 0)
      (check-equal? (plus< (f1 0) (f1 1)) 1)
      )))
