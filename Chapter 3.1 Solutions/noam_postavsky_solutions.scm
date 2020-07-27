;; - 3.1
(define (make-accumulator n)
  (lambda (x)
    (set! n (+ n x))
    n))

;; - 3.2
(define (make-monitored f)
  (let ((n 0))
    (lambda (x)
      (case x
        ((how-many-calls?) n)
        ((reset-count) (set! n 0))
        (else (set! n (+ n 1))
              (f x))))))

;; - 3.3
(define (make-account balance password)
  (lambda (pw op)
    (if (equal? pw password)
        (let ((opfun (case op
                       ((withdraw) -)
                       ((deposit) +)
                       (else (error "Unknown operation" op)))))
          (lambda (x)
            (set! balance (opfun balance x))
            balance))
        (lambda (_x) "Incorrect password"))))

;; - 3.4
(define (make-account balance password)
  (let ((wrong-pw-count 0))
    (lambda (pw op)
      (if (equal? pw password)
          (begin
            (set! wrong-pw-count 0)
            (let ((opfun (case op
                           ((withdraw) -)
                           ((deposit) +)
                           (else (error "Unknown operation" op)))))
              (lambda (x)
                (set! balance (opfun balance x))
                balance)))
          (begin (set! wrong-pw-count (+ wrong-pw-count 1))
                 (if (>= wrong-pw-count 7)
                     (call-the-cops)
                     (lambda (_x) "Incorrect password")))))))

;; - 3.5

(import (only chicken.random pseudo-random-real))
(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (* (pseudo-random-real) range))))

(define (estimate-integral P x₁ x₂ y₁ y₂ trials)
  (define (iter i successes)
    (if (= i trials) (exact->inexact (/ successes trials))
        (iter (+ i 1)
              (+ successes (if (P (random-in-range x₁ x₂)
                                  (random-in-range y₁ y₂))
                               1 0)))))
  (iter 0 0))

(define (in-unit-circle? x y)
  (>= 1 (+ (* x x) (* y y))))

;; Estimate of π.
(* 4 (estimate-integral in-unit-circle? -1 1 -1 1 10000))

;; - 3.7
(define (make-joint account original-password joint-password)
  (lambda (pw op)
    (account (if (equal? pw joint-password)
                 original-password #f)
             op)))

;; - 3.8

(define f
  (let ((first-arg #f))
    (lambda (x)
      (if first-arg first-arg
          (begin
            (set! first-arg x)
            x)))))

(+ (f 0) (f 1)) ;=> 0


