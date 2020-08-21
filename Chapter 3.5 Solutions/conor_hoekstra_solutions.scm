;; #lang sicp

;; Code from the book

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

; (define (stream-map proc s)
;   (if (stream-null? s)
;       the-empty-stream
;       (cons-stream (proc (stream-car s))
;                    (stream-map proc (stream-cdr s)))))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))

(define (display-stream s) (stream-for-each display-line s))
(define (display-line x) (newline) (display x))

(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (stream-enumerate-interval (+ low 1) high))))

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter
                       pred
                       (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))

;; Exercise 3.50 (page 440)

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))

;; Exercise 3.51 (page 440-1)

(define (show x)
  (display-line x)
  x)

(define x (stream-map show (stream-enumerate-interval 0 10)))
;; 0

(stream-ref x 5)
;; 1
;; 2
;; 3
;; 4
;; 5

(stream-ref x 7)
;; 6
;; 7

;; Exercise 3.53 (page 447)

;; 1 2 4 8 ...

;; Exercise 3.54 (page 447-8)

;; Code from book
(define ones (cons-stream 1 ones))
(define (add-streams s1 s2) (stream-map + s1 s2))
(define integers (cons-stream 1 (add-streams ones integers)))

;; Solution
(define (mul-streams s1 s2) (stream-map * s1 s2))
(define factorials (cons-stream 1 (mul-streams factorials (stream-cdr integers))))

;; Test
(stream-ref factorials 9) ; 3628800

;; Exercise 3.55 (page 448)

(define (partial-sums S)
  (cons-stream (stream-car S)
               (add-streams (partial-sums S) (stream-cdr S))))

;; Test
(stream-ref (partial-sums (stream-enumerate-interval 1 5)) 4) ; 15

;; Exercise 3.56 (page 448-49)

;; code from the book

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor))
              stream))

(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (cond ((< s1car s2car)
                  (cons-stream
                   s1car
                   (merge (stream-cdr s1) s2)))
                 ((> s1car s2car)
                  (cons-stream
                   s2car
                   (merge s1 (stream-cdr s2))))
                 (else
                  (cons-stream
                   s1car
                   (merge (stream-cdr s1)
                          (stream-cdr s2)))))))))

(define S (cons-stream 1 (merge (scale-stream S 2)
                                (merge (scale-stream S 3)
                                       (scale-stream S 5)))))

;; Test
(map (lambda (x) (stream-ref S x))
     '(0 1 2 3 4 5 6 7 8 9 10)) ; (1 2 3 4 5 6 8 9 10 12 15)

;; Pi from alternating series

(define (pi-summands n)
  (cons-stream (/ 1.0 n)
               (stream-map - (pi-summands (+ n 2)))))

(define pi-stream
  (scale-stream (partial-sums (pi-summands 1)) 4))

;; Pi with Euler acceleration

(define (square x) (* x x))

(define (euler-transform s)
  (let ((s0 (stream-ref s 0))  ; S n -1
        (s1 (stream-ref s 1))  ; S n
        (s2 (stream-ref s 2))) ; S n+1
    (cons-stream (- s2 (/ (square (- s2 s1))
                          (+ s0 (* -2 s1) s2)))
                 (euler-transform (stream-cdr s)))))

;; Pi with super-"tableu" acceleration

(define (make-tableau transform s)
  (cons-stream s (make-tableau transform (transform s))))

(define (accelerated-sequence transform s)
  (stream-map stream-car (make-tableau transform s)))

(stream-ref pi-stream 1000)       ; 3.1425916543395442

(stream-ref (euler-transform
             pi-stream) 1000)     ; 3.1415926538383

(stream-ref (accelerated-sequence
             euler-transform
             pi-stream) 9)        ; 3.141592653589795

;; Exercise 3.64

;; Code from book

(define (avg a b) (/ (+ a b) 2.0))

(define (sqrt-improve guess x)
  (avg guess (/ x guess)))

(define (sqrt-stream x)
  (define guesses
    (cons-stream
     1.0
     (stream-map (lambda (guess) (sqrt-improve guess x))
                 guesses)))
  guesses)

(define (sqrt x tolerance)
  (stream-limit (sqrt-stream x) tolerance))

;; Solution

(define (stream-limit stream tolerance)
  (let* ((prev   (stream-ref stream 0))
         (result (stream-ref stream 1))
         (delta (abs (- result prev))))
  (if (< delta tolerance)
      result
      (stream-limit (stream-cdr stream) tolerance))))

;; Test
(sqrt 5 0.1)   ; 2.238095238095238
(sqrt 5 0.001) ; 2.236067977499978

;; Exercise 3.65 (page 459)

;; ln(2) from alternating series

(define (ln2-summands n)
  (cons-stream (/ 1.0 n)
               (stream-map - (ln2-summands (+ n 1)))))

(define ln2-stream
  (partial-sums (ln2-summands 1)))

(stream-ref ln2-stream 1000)       ; 0.6936464315588232

(stream-ref (euler-transform
             ln2-stream) 1000)     ; 0.6931471806840143

(stream-ref (accelerated-sequence
             euler-transform
             ln2-stream) 9)        ; 0.6931471805599454
