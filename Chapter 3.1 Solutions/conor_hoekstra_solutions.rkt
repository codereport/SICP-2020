;; Exercise 3.1 (page 303-4)

(require rackunit)

(define (make-accumulator init)
  (let ((sum init))
    (λ (val) (begin (set! sum (+ sum val))
              sum))))

(define A (make-accumulator 5))

(check-equal? (A 10) 15)
(check-equal? (A 10) 25)

;; Exercise 3.2 (page 304)

;; only works for one parameter function f
(define (make-monitored f)
  (let ((count 0))
    (λ (arg-or-symbol)
      (cond ((eq? arg-or-symbol 'how-many-calls) count)
            ((eq? arg-or-symbol 'reset) (set! count 0))
            (else (set! count (+ count 1))
                  (f arg-or-symbol))))))

(define s (make-monitored sqrt))
(check-equal? (s 'how-many-calls) 0)
(check-equal? (s 100) 10)
(check-equal? (s 'how-many-calls) 1)

;; works for variadic number of parameters
(define (make-monitored f)
  (let ((count 0))
    (λ (head . tail)
      (cond ((eq? head 'how-many-calls) count)
            ((eq? head 'reset) (set! count 0))
            (else (set! count (+ count 1))
                  (apply f (cons head tail)))))))

(define s (make-monitored sqrt))
(check-equal? (s 'how-many-calls) 0)
(check-equal? (s 100) 10)
(check-equal? (s 'how-many-calls) 1)

(define p (make-monitored +))
(check-equal? (p 'how-many-calls) 0)
(check-equal? (p 1 2) 3)
(check-equal? (p 'how-many-calls) 1)

;; Exercise 3.3 (page 304-5)

;; original from book

(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount)) balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount)) balance)
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request: MAKE-ACCOUNT"
                       m))))
  dispatch)
  
;; modified

(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount)) balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount)) balance)
  (define (dispatch input-password m)
    (cond ((not (eq? password input-password)) (λ (_) "Incorrect password"))
          ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request: MAKE-ACCOUNT" m))))
  dispatch)  
  
(define acc (make-account 100 '123abc))
(check-equal? ((acc '123abc 'withdraw) 40) 60)
(check-equal? ((acc '456xyz 'withdraw) 40) "Incorrect password")
(check-equal? ((acc '123abc 'deposit)  10) 70)

;; Exercise 3.4 (page 305)

(define (make-account balance password)
  (let ((fail-count 0))
    (define (withdraw amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 (set! fail-count 0)
                 balance)
          "Insufficient funds"))
    (define (deposit amount)
      (set! balance (+ balance amount))
      (set! fail-count 0)
      balance)
    (define (dispatch input-password m)
      (cond ((> fail-count 7) (λ (_) "CALL THE COPS"))
            ((not (eq? password input-password))
             (λ (_) (begin (set! fail-count (add1 fail-count))
                           "Incorrect password")))
             ((eq? m 'withdraw) withdraw)
             ((eq? m 'deposit) deposit)
             (else (error "Unknown request: MAKE-ACCOUNT" m))))
  dispatch))

(define acc (make-account 100 '123abc))
(check-equal? ((acc '456xyz 'withdraw) 40) "Incorrect password")
(check-equal? ((acc '456xyz 'withdraw) 40) "Incorrect password")
(check-equal? ((acc '456xyz 'withdraw) 40) "Incorrect password")
(check-equal? ((acc '456xyz 'withdraw) 40) "Incorrect password")
(check-equal? ((acc '456xyz 'withdraw) 40) "Incorrect password")
(check-equal? ((acc '456xyz 'withdraw) 40) "Incorrect password")
(check-equal? ((acc '456xyz 'withdraw) 40) "Incorrect password")
(check-equal? ((acc '456xyz 'withdraw) 40) "Incorrect password")
(check-equal? ((acc '456xyz 'withdraw) 40) "CALL THE COPS")
