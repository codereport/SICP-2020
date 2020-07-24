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

;; Exercise 3.5 (page 309-11)

(require threading)
(require algorithms) ;; generate, sum

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (* range (/ (random 10000) 10000.0)))))

(define (estimate-interval P x1 x2 y1 y2 n)
  (let ((rect-area (* (- x2 x1) (- y2 y1)))
        (random-point (λ () (list (random-in-range x1 x2) (random-in-range y1 y2)))))
    (~>> (generate n random-point)
         (map (λ (p) (if (apply P p) 1.0 0)))
         (sum)
         (* (/ rect-area n)))))

(define (sq x) (* x x))
(estimate-interval (λ (x y) (< (+ (sq (- x 5)) (sq (- y 7))) 9))
                   2 8 4 10 100000)

(println "PI estimate")
(/ (estimate-interval (λ (x y) (< (+ (sq (- x 5)) (sq (- y 7))) 9))
                   2 8 4 10 100000) 9)

;; 28.18404
;; "PI estimate"
;; 3.1414400000000002

;; Exercise 3.7 (page 319-20)

(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount)) balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount)) balance)
  (define (make-joint joint-password)
    (dispatch joint-password))
  (define (dispatch account-password)
    (λ (input-password m)
      (cond ((not (eq? account-password input-password)) (λ (_) "Incorrect password"))
            ((eq? m 'withdraw) withdraw)
            ((eq? m 'deposit) deposit)
            ((eq? m 'make-joint) make-joint)
            (else (error "Unknown request: MAKE-ACCOUNT" m)))))
  (dispatch password))
  
(define acc (make-account 100 '123abc))
(check-equal? ((acc '123abc 'withdraw) 40) 60)
(check-equal? ((acc '456xyz 'withdraw) 40) "Incorrect password")
(check-equal? ((acc '123abc 'deposit)  10) 70)

(define acc2 ((acc '123abc 'make-joint) 'canIjoin))
(check-equal? ((acc2 'canIjoin 'withdraw) 70) 0)

;; Exercise 3.8 (page 320)

(define i 0)
(define vals '(-0.5 0.5))

(define (f x)
  (set! i (+ i x))
  (list-ref vals i))

(check-equal? (+ (f 0) (f 1)) 0.0)
(set! i 0)
(check-equal? (+ (f 1) (f 0)) 1.0)
