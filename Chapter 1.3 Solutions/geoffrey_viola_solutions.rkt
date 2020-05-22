#lang racket

; Before
(define (square x) (* x x))

; 1.3
(define (cube x) (* x x x))

; 1.3.1
(define (identity x) x)
(define (inc n) (+ n 1)) 

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

; exercise 1.29

; Taken from http://community.schemewiki.org/?sicp-ex-1.29
(define (simpson-2 f a b n) 
   (define h (/ (- b a) n))  
   (define (add-2h x) (+ x (* 2 h))) 
   (* (/ h 3.0) (+ (f a)  
                   (* 4.0 (sum f (+ a h) add-2h b))  
                   (* 2.0 (sum f (add-2h a) add-2h (- b h)))  
                   (f b))))

(simpson cube 0 1 10)

; 1.3.3

(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

; exercise 1.35 golden ratio
;(fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0)

; exercise 1.36
(define (fixed-point-chatty f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (display guess)
    (newline)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (average a b)
  (/ (+ a b) 2))
(define x-to-the-x-initial-guess 2.0)
(define (x-to-the-x-undampened)
  (fixed-point-chatty (lambda (x) (/ (log 1000) (log x)))
                      x-to-the-x-initial-guess))
(define (x-to-the-x-dampened)
 (fixed-point-chatty (lambda (y) (average y (/ (log 1000) (log y))))
                     x-to-the-x-initial-guess))
;(x-to-the-x-undampened)
;(x-to-the-x-dampened)

; exercise 1.37
(define (cont-frac n d k)
  (if (< k 0)
      0
      (/ (n k) (+ (d k) (cont-frac n d (- k 1))))))

;(cont-frac (lambda (i) 1.0)
;           (lambda (i) 1.0)
;           10)
;; 0.6180555555555556

; Section 1.3.4
(define (deriv g)
  (define dx 0.00001)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))
(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))
(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))
(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

; exercise 1.40
(define (cubic a b c)
  (lambda (x) (+ (cube x) (* a (square x)) (* b x) c)))
;(newtons-method (cubic 1 2 3) 1)

; exercise 1.41
(define (double f) (lambda (x) (f (f x))))
;((double inc) 0)
;2
;(((double double) inc) 0);
;4
;(((double (double double)) inc) 0)
;16
;(((double (double double)) inc) 5)
;21
