#lang racket
(require rackunit)

;; Ex 2.1

(define (make-rat n d)
  (let ((g (gcd n d)))
    (cons (* (sgn d) (sgn g) (/ n g))
          (abs (/ d g)))))

(define (numer x) (car x))
(define (denom x) (cdr x))

(module+ test
  (let ((n0 (make-rat (+ 24) (- 36))))
    (begin
      (check-equal? (numer n0) (- 2))
      (check-equal? (denom n0) (+ 3)))))
  
;; Ex 2.2

(define make-point cons)
(define x-point car)
(define y-point cdr)

(define make-segment cons)
(define start-segment car)
(define end-segment cdr)
(define (segment-length s)
  (let ((x0 (x-point (start-segment s)))
        (y0 (y-point (start-segment s)))
        (x1 (x-point (end-segment s)))
        (y1 (y-point (end-segment s)))
        (square (lambda (x) (* x x))))
    (sqrt (+ (square (- x1 x0))
             (square (- y1 y0))))))
;; Ex 2.3

;; point A,B,C,D
(define make-rectangle list)
(define (rectangle-point r p)
  ((cond ((eq? p 'a) first)
        ((eq? p 'b) second)
        ((eq? p 'c) third)
        ((eq? p 'd) fourth)) r))

(define (rectangle-segment r p0 p1)
  (make-segment (rectangle-point r p0)
                (rectangle-point r p1)))

(define (rectangle-perimeter r)
  (+ (segment-length (rectangle-segment r 'a 'b))
     (segment-length (rectangle-segment r 'b 'c))
     (segment-length (rectangle-segment r 'c 'd))
     (segment-length (rectangle-segment r 'd 'a))))

(define (rectangle-area r)
  (* (segment-length (rectangle-segment r 'a 'b))
     (segment-length (rectangle-segment r 'a 'd))))

(module+ test
  (let ((r0 (make-rectangle (make-point 0 0)
                            (make-point 20 0)
                            (make-point 20 10)
                            (make-point 0 10))))
    (begin
      (check-equal? (rectangle-area r0) 200)
      (check-equal? (rectangle-perimeter r0) 60)
      )))
        
                

;; Ex 2.4

; \m.m x y
(define (xcons x y)
  (lambda (m) (m x y)))

; \z.z (\p.\q. p)
(define (xcar z)
  (z (lambda (p q) p)))

; \z.z (\p.\q. p)
(define (xcdr z)
  (z (lambda (p q) q)))

; (xcar (xcons 1 2)) beta reduction
; (\z.z (\p.\q. p)) (\m. m 1 2) -> (\m. m 1 2) (\p.\q. p) -> (\p.\q. p) 1 2 -> 1

; (xcdr (xcons 1 2)) beta reduction
; (\z.z (\p.\q. p)) (\m. m 1 2) -> (\m. m 1 2) (\p.\q. q) -> (\p.\q. p) 1 2 -> 2

;; Ex 2.5

(define (gcons a b)
  (define (times a m)
    (if (= m 0)
        1
        (* a (times a (- m 1)))))
  (* (times 2 a) (times 3 b)))

(define (gcons-powerof n a)
  (if (= 0 (remainder n a))
      (+ 1 (gcons-powerof (/ n a) a))
      0))

(define (gcar n) (gcons-powerof n 2))
(define (gcdr n) (gcons-powerof n 3))


(module+ test
  (let ((n0 (gcons 2 4 )))
    (begin
      (check-equal? (gcar n0) 2)
      (check-equal? (gcdr n0) 4)
      )))

;; Ex 2.6

; TAPL 5.2
; c0 = \s. \z. z;
; c1 = \s. \z. s z;
; c2 = \s. \z. s (s z);
; c3 = \s. \z. s (s (s z));
; plus = \m. \n. \s. \z. m s (n s z);
(define (c0 s z) z)

(define (c1 s z) (s z))

(define (c2 s z) (s (s z)))


(define (plus m n s z)
  (m s (n s z)))

(module+ test
  (define (succ x) (+ x 1))
  (begin
    (check-equal? (plus c1 c1 succ 0) 2)))
