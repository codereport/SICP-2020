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

;; Exercise 3.66 (page 462)

;; code from the book

(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))

(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (pairs (stream-cdr s) (stream-cdr t)))))

;; Initially didn't have any intuition - thought (1, 100) would be around n * (n + 1) / 2
;; but played around and saw it was really closer to ~200

(define ii (pairs integers integers))
(stream-ref ii 197) ; (1 100)

;; after this my guess was ~ 2 * sum(x, y) -> but after trying a couple examples
;; it was clear that was going to be wrong

;; probably more like x * y
;; then i coded this procedure ... which was too inefficient

(define (find-ref s p)
  (define (iter n)
    (if (equal? p (stream-ref s n))
        n
        (iter (+ n 1))))
  (iter 0))

;; then i thought this would be more efficient

(define (find-ref s p)
  (define (iter s-inner n)
    (if (equal? p (stream-car s-inner))
        n
        (begin (display (stream-car s-inner))
               (iter (stream-cdr s-inner) (+ n 1)))))
  (iter s 0))

;; (268581)(2 134292)(1 268582)(3 67148)(1 268583)(2 134293)
;; (1 268584)(5 16791)(1 268585)(2 134294)(1 268586)(3 67149)
;; (1 268587)(2 134295)(1 268588)(4 33577)(1 268589)(2 134296)
;; (1 268590)(3 67150)(1 268591)(2 134297)(1 268592)(6 8399)
;; (1 268593)(2 134298)(1 268594)(3 67151)(1 268595)(2 134299)(1 268596)

;; this is from http://community.schemewiki.org/?sicp-ex-3.66

;; (1,100):198;
;; (100,100):2^100 - 1;
;; ---------------
;; f(n,m) m>=n (m,n is Z+)
;; (m-n=0): 2^n - 1
;; (m-n=1): (2^n - 1) + 2^(n - 1)
;; (m-n>1): (2^n - 1) + 2^(n - 1) + (m - n - 1) * 2^n
;; ------------------------------------
;;   1   2   3   4   5   6   7   8   9  ...  100   
;; 1 1   2   4   6   8  10  12  14  16       198 
;; 2     3   5   9  13  17  21  25  29        
;; 3         7  11  19  27  35  43  51
;; 4            15  23  39  .....
;; 5                31  .........
;; .
;; .
;; 100 ------------------------------------- (2^100 - 1)

;; Exercise 3.67 (page 462)

;; orginal
(define (pairs-orig s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (pairs-orig (stream-cdr s) (stream-cdr t)))))

;; modified
(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (pairs (stream-cdr s) t))))

;; Test
(map (lambda (x) (stream-ref ii x)) '(0 1 2 3 4 5))
;; ((1 1) (1 2) (2 1) (1 3) (2 2) (1 4))

;; Exercise 3.70 (page 474)

(define (merge-weighted s t weight)
  (cond ((stream-null? s) t)
        ((stream-null? t) s)
        (else
         (let ((s-car (stream-car s))
               (t-car (stream-car t)))
           (cond ((< (weight s-car) (weight t-car))
                  (cons-stream s-car (merge-weighted (stream-cdr s) t weight)))
                 ((< (weight t-car) (weight s-car))
                  (cons-stream t-car (merge-weighted (stream-cdr t) s weight)))
                 (else (cons-stream
                        s-car
                        (cons-stream t-car (merge-weighted (stream-cdr s)
                                                           (stream-cdr t) weight)))))))))
                  
(define (weighted-pairs s t weight)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (merge-weighted (stream-map (lambda (x) (list (stream-car s) x)) (stream-cdr t))
                   (weighted-pairs (stream-cdr s) (stream-cdr t) weight)
                   weight)))

;; a)
(define wp (weighted-pairs integers integers (lambda (p) (apply + p))))
(map (lambda (x) (stream-ref wp x)) '(0 1 2 3 4 5 6 7 8 9))
;; ((1 1) (1 2) (1 3) (2 2) (1 4) (2 3) (1 5) (2 4) (3 3) (2 5))

;; Exercise 3.71 (page 464-5)

;; Add this click to recording: https://www.youtube.com/watch?v=Qi4SDDjgHdU

(define (ramanujan-numbers n)
  (let* ((cube (lambda (x) (* x x x)))
         (ram-w (lambda (p) (apply + (map cube p)))))
    (define (iter s left)
      (cond ((= left 0) 'done)
            (else
             (let ((a (stream-car s))
                   (b (stream-car (stream-cdr s))))
               (if (= (ram-w a) (ram-w b))
                   (begin (display (list (ram-w a) a b))
                          (newline)
                          (iter (stream-cdr s) (- left 1)))
                   (iter (stream-cdr s) left))))))
    (iter (weighted-pairs integers integers ram-w) n)))

;; Test
(ramanujan-numbers 5)
;; (1729 (9 10) (1 12))
;; (4104 (9 15) (2 16))
;; (13832 (18 20) (2 24))
;; (20683 (19 24) (10 27))
;; (32832 (18 30) (4 32))
;; done
