#lang racket

(require rackunit
         threading)

;; 3.1
(define (make-accumulator acc)
  (lambda (inc) (begin
             (set! acc (+ acc inc))
             acc)))

(module+ test
  (let ((f (make-accumulator 5)))
    (begin
      (check-equal? (f 10) 15)
      (check-equal? (f 10) 25)
      (check-equal? (f 10) 35))))

;; 3.2

(define (make-monitored func)
  (let ([count 0])
    (lambda (arg) (match arg
               ['how-many-calls? count]
               ['reset-count (set! count 0)]
               [_ (begin
                    (set! count (+ count 1))
                    (func arg))]))))

(module+ test
  (define s (make-monitored sqrt))
  (check-equal? (s 'how-many-calls?) 0)
  (check-equal? (s 100) 10)
  (check-equal? (s 'how-many-calls?) 1)
  (s 'reset-count)
  (check-equal? (s 'how-many-calls?) 0))

;; 3.3 3.4
(define (make-account balance secret-password)
  (define (call-the-cops) "You are in trouble now")
  (define incorrect-access-count 0)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch password m)
    (cond
      ((not (eq? password secret-password))
       (lambda (_) (begin
                (set! incorrect-access-count (+ incorrect-access-count 1))
                (if (>= incorrect-access-count 7)
                    (call-the-cops)
                    "Incorrect password"))))
      ((eq? m 'withdraw) withdraw)
      ((eq? m 'deposit) deposit)
      (else (error "Unknown request: MAKE-ACCOUNT" m))))
  dispatch)

;;;; 3.3 tests
(module+ test
  (define acc (make-account 100 'secret-password))
  (check-equal? ((acc 'secret-password 'withdraw) 40) 60)
  (check-equal? ((acc 'some-other-password 'deposit) 50) "Incorrect password"))

;;;; 3.4 tests
(module+ test
  (define acc2 (make-account 100 'secret-password))
  (for ([i 6]) (check-equal? ((acc2 'some-other-password 'deposit) 50) "Incorrect password"))
  (check-equal? ((acc2 'some-other-password 'deposit) 50) "You are in trouble now"))

;; 3.5
;;;; From book
(define (random-in-range low high)
  (+ low (* (random) (- high low))))

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

;;;; My solution
(define (estimate-integral P x1 x2 y1 y2 trials)
  (monte-carlo
   trials
   (lambda ()
     (let ([x (random-in-range x1 x2)]
           [y (random-in-range y1 y2)])
       (P x y)))))

(module+ test
  (check-within (* (estimate-integral (lambda (x y) (< (+ (* x x) (* y y)) 1.0))
                           -1.0 1.0 -1.0 1.0 10000) 4.0) 3.1415926 0.1)
  )

;; 3.7
(define (make-joint acc acc-password new-password)
  (define (dispatch password m)
    (if (eq? password new-password)
        (acc acc-password m)
        (lambda (_) "Incorrect password")))
  dispatch
  )

(module+ test
  (define peter-acc (make-account 100 'open-sesame))
  (define paul-acc
    (make-joint peter-acc 'open-sesame 'rosebud))
  (check-equal? ((paul-acc 'rosebud 'withdraw) 40) 60)
  (check-equal? ((paul-acc 'badword 'withdraw) 40) "Incorrect password")
  (check-equal? ((peter-acc 'open-sesame 'deposit) 10) 70)
  )

;; 3.8
(define f
  (let ([c 2])
    (lambda (x)
      (if (eq? x 'reset)
          (set! c 2)
          (begin
            (set! c (- c 1))
            (* c x))))))

(module+ test
  (let ([left (f 0)]
        [right (f 1)])
    (check-equal? (+ left right) 0))
  (f 'reset)
  (let ([right (f 1)]
        [left (f 0)])
    (check-equal? (+ left right) 1)))
