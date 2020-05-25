#lang racket

(require math/base
         rackunit
         threading
         plot)
(plot-new-window? #t)

(define (inc x)
  (+ x 1))
(define (cube x) (* x x x))

;; 1.30

(define (sum term a next b)
  (define (helper c acc)
    (if (> c b)
        acc
        (helper (next c) (+ (term c) acc))))
  (helper a 0))

;;;; 1.30 tests

(check-eq? (sum identity 1 inc 10) 55)
(check-eq? (sum cube 1 inc 3) 36)

;; 1.29

(define (simpson f a b n)
  (let ((h (/ (- b a) n))
        (coefficients
         (~>>
          (range (- (/ n 2) 1))
          (foldl (lambda (_ acc) (list* 4 2 acc)) '(1))
          (cons 1))))
    (~>
     (for/fold ([result 0])
               ([c coefficients]
                [x (~>>
                    (range n)
                    (map (lambda (k) (+ a (* k h))))
                    )])
       (+ result (* c (f x))))
     (* h)
     (/ 3)
     )))

(check-within (simpson cube 0.0 1.0 100) 0.25 0.1) ;; 0.2370
(check-within (simpson cube 0.0 1.0 1000) 0.25 0.01) ;; 0.2487

;; 1.31 a
(define (product term a next b)
  (define (helper c acc)
    (if (> c b)
        acc
        (helper (next c) (* (term c) acc))))
  (helper a 1))

(define (factorial x)
  (product identity 1 inc x))

(define (approx-pi n)
  (define (inc-2 x) (+ x 2))
  (* 4.0 (product (lambda (x) (/ (* x (+ x 2)) (* (+ x 1) (+ x 1))))
                  2 inc-2 (* n 2))
  ))

;;;; 1.31 a tests
(check-eq? (factorial 0) 1)
(check-eq? (factorial 1) 1)
(check-eq? (factorial 2) 2)
(check-eq? (factorial 3) 6)

(check-within (approx-pi 100) 3.1415926 1e-2)

;; 1.32 a
(define (accumulate combiner null-value term a next b)
  (define (helper c acc)
    (if (> c b)
        acc
        (helper (next c) (combiner (term c) acc))))
  (helper a null-value))

(define (sum-acc term a next b)
  (accumulate + 0 term a next b))

(define (prod-acc term a next b)
  (accumulate * 1 term a next b))

;;;; 1.32 a tests
(check-eq? (sum-acc identity 1 inc 10) 55)
(check-eq? (sum-acc (lambda (x) (* x x x)) 1 inc 3) 36)
(check-eq? (prod-acc identity 1 inc 10) 3628800)

;; 1.34
(define (f g) (g 2))

;; By substitution
;; (f f) =>
;; (f 2) =>
;; (2 2)
;; runtime type-error

;; 1.35
(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define golden-ratio (/ (+ 1 (sqrt 5)) 2))

;;;; 1.35 tests
(check-within (fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0) golden-ratio 1e-5
)

;; 1.36
(define (fixed-point-print f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (display next)
      (newline)
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(display "Finding fix point of x -> log_{x} 100\n")
(display "28 iterations starting from 4\n")
(fixed-point-print (lambda (x) (/ (log 1000) (log x))) 5.0)

(define (average x y) (/ (+ x y) 2))

(display "Finding fix point of x -> log_{x} 1000 with average damping\n")
(display "9 iterations starting from 4\n")
(fixed-point-print (lambda (x) (average x (/ (log 1000) (log x)))) 5.0)

;; 1.37 a
(define (cont-frac-rec n d k)
  (if (= k 1)
      (/ (n k) (d k))
      (/ (n k) (+ (d k) (cont-frac-rec n d (- k 1))))))

;;;; 1.37 a tests
(check-within (cont-frac-rec (lambda (i) 1.0) (lambda (i) 1.0) 100) (/ 1 golden-ratio) 1e-5)

;; 1.37 b
(define (cont-frac n d k)
  (define (helper acc k-prime)
    (if (= k-prime 1)
        acc
        (helper (/ (n k-prime) (+ (d k-prime) acc)) (- k-prime 1))))
  (helper (/ (n k) (d k)) k))

;;;; 1.37 b tests
(check-within (cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 100) (/ 1 golden-ratio) 1e-5)

;; 1.38
;; Broken, unknow why
;; (define e euler.0)

;; (check-within
;;  (cont-frac-rec
;;   (lambda (i) 1.0)
;;   (lambda (i) (if (= (remainder i 3) 2)
;;              (* 2 (+ 1 (quotient i 3)))
;;              1
;;              ))
;;   1000)
;;  (- e 2)
;;  1e-4)

;; 1.40
(define dx 1e-5)
(define (deriv g)
(lambda (x) (/ (- (g (+ x dx)) (g x)) dx)))

(define (newton-transform g)
(lambda (x) (- x (/ (g x) ((deriv g) x)))))
(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (cubic a b c)
  (lambda (x)
    (+ (* x x x)
       (* a x x)
       (* b x)
       c)))

;;;; 1.40 tests
;;;; Root calculated from Wolfram Alpha
(check-within (newtons-method (cubic 1 2 3) 0) -1.27568220365098 1e-10)

;; 1.41
(define (double f)
  (lambda (x) (f (f x))))

(check-eq? (((double (double double)) inc) 5) 21)

;; 1.42
(define (square x) (* x x))

(define (compose g f)
  (lambda (x)
    (g (f x))))

(check-eq? ((compose square inc) 6) 49)

;; 1.43
(define (repeat f n)
  (lambda (x) (~>>
          (range n)
          (foldl (lambda (_ acc) (f acc)) x))))

(check-eq? ((repeat square 2) 5) 625)

;; 1.44
(define (smooth f dx)
  (lambda (x) (/ (+ (f x) (f (+ x dx)) (f (- x dx))) 3)))

(define (smooth-n f dx n)
  (repeat (smooth f dx) n))

;;;; 1.44 Don't know what to test, so plot some graphs
(plot (function cube -1 1 #:label "y = sin(x)"))
(plot (function (smooth cube 0.01) -1 1  #:label "y = smooth(sin(x))"))
(plot (function (smooth-n cube 0.01 3) -1 1  #:label "y = smooth^{3}(sin(x))"))
