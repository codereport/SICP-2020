#lang racket

(require rackunit
         threading)

;; 1.11
(define (f-rec n)
  (if (< n 3)
      n
      (+ (f-rec (- n 1)) (* 2 (f-rec (- n 2))) (* 3 (f-rec (- n 3))))))

(define (f-itr n)
  (define (helper a b c n)
    (if (< n 1)
        a
        (helper b c (+ c (* 2 b) (* 3 a)) (- n 1))))
  (helper 0 1 2 n))

;;;; I hold a grudge against this piece, to be honest
;;;; But this is the best I can get
(define (f-fold n)
  (~>> (range n)
       (foldl (lambda (_ acc)
                (match-let ([(list a b c) acc])
                  (list b c (+ c (* 2 b) (* 3 a)))))
              (list 0 1 2))
      car))

;; 1.12

;;;; A C++ std::adjacent_difference-ish helper
(define (adjacent-map f xs)
  (for/list ([x xs] [y (cdr xs)])
    (f y x)))

(define (pascal-next-row l)
  (~>>
   (cons 0 l)
   (adjacent-map +)
   reverse
   (cons 1)))

(define (pascal n)
  (define (helper n2 row triangle)
    (if (<= n2 0)
        triangle
        (let ([next-row (pascal-next-row row)])
          (helper (- n2 1) next-row (cons row triangle)))))
  (reverse (helper (+ n 1) '(1) '())))

;; 1.16

(define (square x) (* x x))

(define (my-exp b n)
  (define (helper b n acc)
    (cond ((= n 0) 1)
          ((= n 1) (* b acc))
          ((even? n) (helper (square b) (/ n 2) acc))
          (else (helper b (- n 1) (* b acc)))
          ))
  (helper b n 1))

;;;; 1.17
(define (double x) (arithmetic-shift x 1))
(define (half x) (arithmetic-shift x -1))

(define (my-mult a b)
  (cond ((= b 0) 0)
        ((even? b) (my-mult (double a) (half b)))
        (else (+ (my-mult (double a) (half b)) a))))

;;;; 1.18
(define (my-mult-tail a b)
  (define (helper a b acc)
    (cond ((= b 0) acc)
        ((even? b) (helper (double a) (half b) acc))
        (else (helper (double a) (half b) (+ a acc)))))
  (helper a b 0))

;;;; 1.27
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder
          (square (expmod base (/ exp 2) m))
          m))
        (else
         (remainder
          (* base (expmod base (- exp 1) m))
          m))))

(define (is-prime? n)
  (define (try-it a)
    (= (expmod a n n) a))
  (define (try-n n)
    (if (< n 3)
        true
        (and (try-it (- n 1)) (try-n (- n 1)))))
  (try-n n))

;; Tests

;;;; 1.11
(check-eq? (f-rec 0) 0)
(check-eq? (f-rec 1) 1)
(check-eq? (f-rec 2) 2)
(check-eq? (f-rec 3) 4)

(check-eq? (f-itr 0) 0)
(check-eq? (f-itr 1) 1)
(check-eq? (f-itr 2) 2)
(check-eq? (f-itr 3) 4)

(check-eq? (f-fold 0) 0)
(check-eq? (f-fold 1) 1)
(check-eq? (f-fold 2) 2)
(check-eq? (f-fold 3) 4)

;;;; 1.12
(check-equal? (pascal 5)
              '((1)
                (1 1)
                (1 2 1)
                (1 3 3 1)
                (1 4 6 4 1)
                (1 5 10 10 5 1)))

;;;; 1.16

(check-eq? (my-exp 3 0) 1)
(check-eq? (my-exp 3 1) 3)
(check-equal? (my-exp 3 100)
           515377520732011331036461129765621272702107522001)

;;;; 1.17
(check-eq? (my-mult 3 0) 0)
(check-eq? (my-mult 3 3) 9)

;;;; 1.18
(check-eq? (my-mult-tail 3 0) 0)
(check-eq? (my-mult-tail 3 3) 9)

;;;; 1.27
(check-eq? (is-prime? 2) true)
(check-eq? (is-prime? 3) true)
(check-eq? (is-prime? 4) false)
(check-eq? (is-prime? 107) true)
;;;;;; Carmichael numbers fool the Fermat test
(check-eq? (is-prime? 561) true)
(check-eq? (is-prime? 1105) true)
(check-eq? (is-prime? 1729) true)
(check-eq? (is-prime? 2465) true)
(check-eq? (is-prime? 2821) true)
(check-eq? (is-prime? 6601) true)
