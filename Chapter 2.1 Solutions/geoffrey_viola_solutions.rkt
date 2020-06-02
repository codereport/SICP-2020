#lang racket

;; exercise 2.1

(define (better-make-rat n d)
  (if (< d 0) (cons (- n) (- d))  (cons n d)))

;; Tests
;;(better-make-rat 1 1)
;;(better-make-rat -1 1)
;;(better-make-rat 1 -1)
;;(better-make-rat -1 -1)

;; exercise 2.2
(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define (make-point x y) (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))
(define (make-segment p1 p2) (cons p1 p2))
(define (start-segment s) (car s))
(define (end-segment s) (cdr s))
(define (midpoint-segment s)
  (define (average a b) (/ (+ a b) 2.0))
  (make-point (average (x-point (start-segment s)) (x-point (end-segment s)))
              (average (y-point (start-segment s)) (y-point (end-segment s)))))


;; Test
;; (define my-start (make-point 0 0))
;; (define my-end (make-point 2 2))
;; (define my-segment (make-segment my-start my-end))
;; (print-point (midpoint-segment my-segment))

;; exercise 2.3

(define (make-rect min-x-min-y-pt w h) (list min-x-min-y-pt w h))
(define (min-x-min-y-pt rect) (car rect))
(define (width rect) (cdr (car rect)))
(define (length rect) (car (cdr (cdr rect))))
(define (perimeter rect) (+ (* (width rect) 2) (* 2 (length rect))))
(define (area rect) (* (width rect) (length rect)))

;; Test
;; (define my-unit-rect (make-rect (make-point 1 1) 1 1))
;; (perimeter my-unit-rect)
;; (area my-unit-rect)

;; exercise 2.4
(define (cons-1 x y)
  (lambda (m) (m x y)))

(define (car-1 z)
  (z (lambda (p q) p)))

(define (cdr-1 z)
  (z (lambda (p q) q)))

;; Test
(define my-pair (cons-1 1 2))
;; (car-1 my-pair)
;; (cdr-1 my-pair)
;; ((lambda (m) (m 1 2)) (lambda (p q) q))
;; ((lambda () ((lambda (p q) q) 1 2)))
;; ((lambda (p q) q) 1 2)))
;; 2

;; exercise 2.5
(define (cons-2-3 a b) (* (expt 2 a) (expt 3 b)))
(define (count-divisibility base divisor)
  (let-values ([(q r) (quotient/remainder base divisor)])
    (if (= 0 r) (+ 1 (count-divisibility q divisor)) 0)))
(define (car-2-3 a-b) (count-divisibility a-b 2))
(define (cdr-2-3 a-b) (count-divisibility a-b 3))

;; Test
;; (define a-b (cons-2-3 1 2))
;; (car-2-3 a-b)
;; (cdr-2-3 a-b)

;; Section 2.1.3
(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

;; exercise 2.6
(define one (lambda (f) (lambda (x) (f x))))
(define two (lambda (f) (lambda (x) (f (f x)))))
(define (add a b) (lambda (f) (lambda (x) ((a f) ((b f) x)))))
(define (num-add-1 x) (+ 1 x))

;; Tests
;; ((zero num-add-1) 0)
;; ((one num-add-1) 0)
;; ((two num-add-1) 0)
;; (((add one two) num-add-1) 0)
