#lang racket
(require racket/stream)
(require math/number-theory)
(require rackunit)

; SICP 3.5

; Section 3.5.1

;book code adapted for Racket
;(define (stream-filter pred stream)
;  (cond ((stream-empty? stream) empty-stream)
;        ((pred (stream-first stream))
;         (stream-cons (stream-first stream)
;                      (stream-filter
;                       pred
;                       (stream-rest stream))))
;        (else (stream-filter pred (stream-rest stream)))))

;(define (stream-enumerate-interval low high)
;  (if (> low high)
;      empty-stream
;      (stream-cons
;       low
;       (stream-enumerate-interval (+ low 1) high))))

;Exercise 3.50

(define (stream-map proc . argstreams)
  (if (stream-empty? (car argstreams))
      empty-stream
      (stream-cons
       (apply proc (map stream-first argstreams))
       (apply stream-map
              (cons proc (map stream-rest argstreams))))))

;;Exercise 3.51
;(define (display-line x) (newline) (display x))
;
;(define (show x)
;  (display-line x)
;  x)
;
;(define x (stream-map show
;                      (stream-enumerate-interval 0 10)))
;
;(stream-ref x 5)
;;prints 0 1 2 3 4 5 ,
;;x = 5
;(stream-ref x 7)
;;prints 6 7, returns x = 7

;Section 3.5.2

;book code defining stream implicitly
(define ones (stream-cons 1 ones))
(define integers
  (stream-cons 1 (add-streams ones integers)))
(define (add-streams s1 s2) (stream-map + s1 s2))

;constructing fibonacci numbers' stream
(define fibs
  (stream-cons
   0
   (stream-cons 1 (add-streams (stream-rest fibs) fibs)))) ; refer to the book's addition diagram on pg 446

;Exercise 3.54
(define (mul-streams s1 s2) (stream-map * s1 s2))

(define factorials
  (stream-cons 1 (mul-streams (stream-rest integers) factorials)))

(module+ test
  (begin
    (check-equal? (stream->list (stream-take factorials 6))
                  '(1 2 6 24 120 720))))

;Exercise 3.55
; at any given point we have the (n+1)th element of the stream S and the nth partial sum
(define (partial-sums S)
  (stream-cons (stream-first S)
               (add-streams (stream-rest S) (partial-sums S))))

(module+ test
  (begin
    (check-equal? (stream->list (stream-take (partial-sums integers) 5))
                  '(1 3 6 10 15))))

; book code for scale-streams
(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor))
              stream))

;Exercise 3.56
(define (merge s1 s2)
  (cond ((stream-empty? s1) s2)
        ((stream-empty? s2) s1)
        (else
         (let ((s1car (stream-first s1))
               (s2car (stream-first s2)))
           (cond ((< s1car s2car)
                  (stream-cons
                   s1car
                   (merge (stream-rest s1) s2)))
                 ((> s1car s2car)
                  (stream-cons
                   s2car
                   (merge s1 (stream-rest s2))))
                 (else
                  (stream-cons
                   s1car
                   (merge (stream-rest s1)
                          (stream-rest s2)))))))))

(define S (stream-cons 1 (merge (scale-stream S 5) (merge (scale-stream S 3) (scale-stream S 2)))))

;Section 3.5.3

; book code
(define (average x y)
  (/ (+ x y) 2))

(define (sqrt-improve guess x)
  (average guess (/ x guess)))

(define (sqrt-stream x)
  (define guesses
    (stream-cons
     1.0
     (stream-map (lambda (guess) (sqrt-improve guess x))
                 guesses)))
  guesses)

(define (make-tableau transform s)
  (stream-cons s (make-tableau transform (transform s))))

(define (accelerated-sequence transform s)
  (stream-map stream-first (make-tableau transform s)))

;Exercise 3.64
(define (stream-limit S tolerance)
  (if (< (abs (- (stream-first S)
                 (stream-first (stream-rest S))))
         tolerance)
      (stream-first (stream-rest S))
      (stream-limit (stream-rest S) tolerance)))

;Testing sqrt
(define (custom-sqrt x tolerance)
  (stream-limit (sqrt-stream x) tolerance))

(custom-sqrt 5 0.001) ; 2.236067977499978
(sqrt 5) ; 2.23606797749979

;Exercise 3.65
; approximating ln 2 similar to the book code
(define (ln-summands n)
  (stream-cons (/ 1.0 n)
               (stream-map - (ln-summands (+ 1 n)))))
(define ln-stream
  (partial-sums (ln-summands 1)))

; determining speed of convergence
;pass

;Section 3.5.3 - Infinite streams of pairs

;book code
(define (interleave s1 s2)
  (if (stream-empty? s1)
      s2
      (stream-cons (stream-first s1)
                   (interleave s2 (stream-rest s1)))))

(define (pairs s t)
  (stream-cons
   (list (stream-first s) (stream-first t))
   (interleave
    (stream-map (lambda (x) (list (stream-first s) x))
                (stream-rest t))
    (pairs (stream-rest s) (stream-rest t)))))

; generalized prime-sum-pairs for infinite stream of integers
;(stream-filter
; (lambda (pair) (prime? (+ (car pair) (cadr pair))))
; pairs)
;
;Exercise 3.66 - Hard
;
;(pairs integers integers)
;
;pairs starts off with (1,1) and then interleaves t with stream-car of s
;and then recursing on the stream-cdrs of s and t. So the pairs will be in the order:
; (1,1) -- 1st pair
; (1,2)
; (2,2) -- 3rd pair
; (1,3)
; (2,3)
; (1,4)
; (3,3) -- 7th pair
; (1,5)
; (2,4)
; (1,6)
; (3,4)
; (1,7)
; (2,5)
; (1,8)
; (4,4) -- 15th pair
;
; a) after (1,1) (1, t_j) occurs every other element, so for (1,100) the number of elements preceding it
;    is 98*2 + 1 = 197.
;
; b) (100,100): following the pattern of 2^n - 1 for (n, n) , this is probably (2^100 - 1) - 1 preceding pairs
; c) (99,100): ???
;
;No idea how to represent the position of the pair (i,j) mathematically

;Exercise 3.67
; the list now has four (recursive) parts
(define (all-pairs s t)
  (stream-cons
   (list (stream-first s) (stream-first t)) ; top left corner
   (interleave
    (stream-map (lambda (x) (list (stream-first s) x))
                (stream-rest t)) ; top row
    (interleave
     (stream-map (lambda (x) (list x (stream-first t)))
                 (stream-rest s)) ; left column
     (all-pairs (stream-rest s) (stream-rest t)))))) ; rest

;Test
(stream->list (stream-take (all-pairs integers integers) 25))
; '((1 1) (1 2) (2 1) (1 3) (2 2) (1 4) (3 1) (1 5) (2 3) (1 6) (4 1) (1 7) (3 2) (1 8) (5 1) (1 9) (2 4) (1 10) (6 1) (1 11) (3 3) (1 12) (7 1) (1 13) (2 5))

; Exercise 3.70 - not tested

(define (merge-weighted s1 s2 weight)
  (cond ((stream-empty? s1) s2)
        ((stream-empty? s2) s1)
        (else
         (let ((s1car (stream-first s1))
               (s2car (stream-first s2)))
           (let ((w1 (weight s1car))
                (w2 (weight s2car)))
             (cond ((< w1 w2)
                    (stream-cons
                     s1car
                     (merge-weighted (stream-rest s1) s2 weight)))
                   ((> w1 w2)
                    (stream-cons
                     s2car
                     (merge-weighted s1 (stream-rest s2) weight)))
                   (else ; if w1 = w2 then add both of the heads of the streams
                    (stream-cons
                     s1car
                     (stream-cons
                      s2car
                      (merge-weighted (stream-rest s1)
                                      (stream-rest s2)
                                      weight))))))))))

(define (all-weighted-pairs s t weight)
  (stream-cons
   (list (stream-first s) (stream-first t)) ; top left corner
   (merge-weighted
    (stream-map (lambda (x) (list (stream-first s) x))
                (stream-rest t)) ; top row
    (merge-weighted
     (stream-map (lambda (x) (list x (stream-first t)))
                 (stream-rest s)) ; left column
     (all-weighted-pairs (stream-rest s) (stream-rest t) weight)
     weight)
    weight))) ; rest
; part a - pass

; Exercise 3.71
(define (sum-of-cubes pair)
  (+ (expt (car pair) 3) (expt (cdr pair) 3)))

(define (ramanujan-numbers-helper s prev)
  (let ((num (sum-of-cubes (stream-first s))))
    (cond ((= num prev)
           (stream-cons num
                        (ramanujan-numbers-helper (stream-rest s) num)))
          (else (ramanujan-numbers-helper (stream-rest s) num)))))

(define (ramanujan-pairs) (all-weighted-pairs integers integers sum-of-cubes))
(define (ramanujan-numbers) (ramanujan-numbers-helper ramanujan-pairs 0))
(ramanujan-numbers)

; fails because ramanujan-pairs does not return a stream??? 
(module+ test
  (begin
    (check-equal? (stream->list (stream-take ramanujan-numbers 6))
                  '(1729 4104 13832 20683 32832 39312))))