#lang racket
(require racket/stream)
(require rackunit)

; Ex. 3.50 - needed for 3.54
(define (stream-map proc . argstreams)
  (if (stream-empty? (car argstreams))
      empty-stream
      (stream-cons
       (apply proc (map stream-first argstreams))
       (apply stream-map
              (cons proc (map stream-rest argstreams))))))

(define (stream-filter pred stream)
  (cond ((null? stream) empty-stream)
        ((pred (stream-first stream))
         (stream-cons (stream-first stream)
                      (stream-filter
                       pred
                       (stream-rest stream))))
        (else (stream-filter pred (stream-rest stream)))))

; Ex. 3.54

(define (mul-streams s1 s2) (stream-map * s1 s2))
(define (add-streams s1 s2) (stream-map + s1 s2))
(define (constant-stream c) (stream-cons c (constant-stream c)))
(define ones (constant-stream 1))
(define integers (stream-cons 1 (add-streams ones integers)))
(define nats (stream-cons 0 (add-streams ones nats)))

; A000142 X(n+1) = X(n) * n
(define factorials
  (stream-cons 1 (mul-streams factorials integers)))
         
(module+ test
  (begin
    (check-equal? (stream->list (stream-take factorials 23))
                 '(1 1 2 6 24 120 720 5040 40320 362880 3628800 39916800 479001600 6227020800 87178291200 1307674368000 20922789888000 355687428096000 6402373705728000 121645100408832000 2432902008176640000 51090942171709440000 1124000727777607680000))))

; Ex. 3.55 - A000217 - Triangular numbers
; s0 s0 s0 s0 s0
; +
; 0 s1 (s1 + s2) (s1 + s2 + s3) == (partial-sums s[1:])

(define (partial-sums s)
  (add-streams (constant-stream (stream-first s))
               (stream-cons 0 (partial-sums (stream-rest s)))))


(module+ test
  (begin
    (check-equal? (stream->list (stream-take (partial-sums integers) 53))
                                '(1 3 6 10 15 21 28 36 45 55 66 78 91 105 120 136 153 171 190 210 231 253 276 300 325 351 378 406 435 465 496 528 561 595 630 666 703 741 780 820 861 903 946 990 1035 1081 1128 1176 1225 1275 1326 1378 1431))))


; Ex. 3.56

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor))
              stream))

(define (merge s1 s2)
  (cond ((null? s1) s2)
        ((null? s2) s1)
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

; A051037 5-smooth numbers
(define S (stream-cons 1 (merge (scale-stream S 2)
                                (merge (scale-stream S 3)
                                       (scale-stream S 5)))))

(module+ test
  (begin
    (check-equal? (stream->list (stream-take S 62))
                  '(1 2 3 4 5 6 8 9 10 12 15 16 18 20 24 25 27 30 32 36 40 45 48 50 54 60 64 72 75 80 81 90 96 100 108 120 125 128 135 144 150 160 162 180 192 200 216 225 240 243 250 256 270 288 300 320 324 360 375 384 400 405))))
                          

(define (average a b) (/ (+ a b) 2.0))
(define (sqrt-improve guess x) (average guess (/ x guess)))
(define (sqrt-stream x)
  (define guesses
    (stream-cons 1.0
                 (stream-map (lambda (guess) (sqrt-improve guess x))
                             guesses)))
  guesses)

; Ex. 3.64

; s0 s1 s2 s3 s4
; -s1 -s2 -s3 -s4 -s5    : (scale-stream (stream-rest s) -1)
; (s0 - s1) (s1 - s2)    : add-streams
; |s0 - s1| |s1 - s2|    : take abs
; (s0, |s0 - s1|), (s1, |s1 - s2|) : make a stream with pairs because we need to keep the element
; filter ( cdr(el) < tol)
; take car of the first element of this stream

(define (stream-limit s tol)
  (car (stream-first (stream-filter (lambda (x) (< (cdr x) tol))
                             (stream-map cons s
                                         (stream-map abs
                                                     (add-streams s (scale-stream (stream-rest s) -1))))))))

(define (sqrt-sicp x tolerance)
  (stream-limit (sqrt-stream x) tolerance))

(module+ test
  (begin
    (check-within (sqrt 2) (sqrt-sicp 2 0.0001) 0.0001)))
     
; Ex. 3.65
(define (ln2-summands n)
  (stream-cons (/ 1.0 n)
               (stream-map - (ln2-summands (+ n 1)))))

(define ln2-stream (partial-sums (ln2-summands 1)))

(module+ test
  (begin
    (check-within (log 2) (stream-ref ln2-stream 500) 0.001)))

; Ex 3.66

; (1, 100) at position 201
; (99, 100) at position 64333267961582642125957687672830
; (100, 100) at position 128666535923165284251915375345662
; More below:

(define (interleave s1 s2)
  (if (null? s1)
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

; [ 0]  (s0, t0)
; [ 1]  (s0, t1)
; [ 2]  pairs(s1, t1)[0] =  (s1, t1)
; [ 3]  (s0, t2)
; [ 4]  pairs(s1, t1)[1] = (s1, t2)
; [ 5]  (s0, t3)
; [ 6]  pairs(s1, t1)[2] = pairs(s2, t2)[0] = (s2, t2)
; [ 7]  (s0, t4)
; [ 8]  pairs(s1, t1)[3] = (s1, t3)
; [ 9]  (s0, t5)
; [10]  pairs(s1, t1)[4] = pairs(s2, t2)[1] = (s2, t3)
; [11]  (s0, t6)
; [12]  pairs(s1, t1)[5] = (s1, t4)

; Pairs (0, k) appear every 2 - denote this seqence as A0
; Pairs (1, k) appear every 4 - denote this seqence as A1
; Pairs (2, k) appear every 8 - denote this seqence as A2


;(map car (stream->list (stream-take (pairs nats nats) 35)))
; (0 0 1 0 1 0 2 0 1 0 2 0 1 0 3 0 1 0 2 0 1 0 3 0 1 0 2 0 1 0 4 0 1 0 2)
;      x when A0 finishes 2 steps writes an item from A1 instead of an items from A0
;              x when A1 finishes 2 steps (at distance 4)  writes an item from A2 instead of an items from A1
;                              x when A2 finishes 2 steps (at distance 8)  writes an item from A3 instead of an items from A2

; A0[0] is at 0, A1[0] is at 2 , A2[0] is at 6, A3[0] is at 14, A4[0]
; A0[0] = 0, A1[0] = A0[0] + 2 = 2, A2[0] = A1[0] + 2^2 = 6, A3[0] = A2[0] + 2^3 = 14
; => Am[0] = 0 + 2^1 + 2^2 + ... + 2^m = 2^(m+1) - 2
;    Am[1] = Am[0] + 2^(m-1)
;    Am[2] = Am[1] + 2^m
;    Am[3] = Am[1] + (3 - 1) * 2^m

(define (pair-pos m n)
  (let* ((x0 (- (expt 2 (+ m 1)) 2))
         (x1 (+ x0 (expt 2 (- m 1)))))
    (cond ((= n 0) x0)
          ((= n 1) x1)
          (else (+ x1 (* (- n 1) (expt 2 m)))))))

; Ex 3.67
(define (all-pairs s t)
  (stream-cons
   (list (stream-first s) (stream-first t))
   (interleave
    (interleave
     (stream-map (lambda (x) (list (stream-first s) x))
                 (stream-rest t))
     (stream-map (lambda (x) (list x (stream-first t)))
                 (stream-rest s)))
     (all-pairs (stream-rest s) (stream-rest t)))))

; Ex. 3.70

(define (merge-weighted s1 s2 weight)
  (cond ((null? s1) s2)
        ((null? s2) s1)
        (else
         (let* ((s1car (stream-first s1))
                (w1 (weight s1car))
                (s2car (stream-first s2))
                (w2 (weight s2car))
                )
           (cond ((<= w1 w2)
                     (stream-cons
                      s1car
                      (merge-weighted (stream-rest s1) s2 weight)))
                  (else
                   (stream-cons
                    s2car
                    (merge-weighted s1 (stream-rest s2) weight))))))))


; generic function - the helper know how to merge/filter 
(define (all-pairs-with-merge-helper s t helper)    
  (let ((s1 (stream-cons (list (stream-first s) (stream-first t))
                         (all-pairs-with-merge-helper (stream-rest s) (stream-rest t) helper)))
        (s2 (stream-map (lambda (x) (list (stream-first s) x)) (stream-rest t)))
        (s3 (stream-map (lambda (x) (list x (stream-first t))) (stream-rest s))))
    
        (helper s1 (helper s2 s3))))

(define (weight-of-pair-a p)
  (let ((i (car p))
        (j (cadr p)))
    (+ i j)))

(define (weight-of-pair-b p)
  (let ((i (car p))
        (j (cadr p)))
    (+ (* 2 i) (* 5 j) (* 5 i j))))

(define (helper-a s1 s2) (merge-weighted s1 s2 weight-of-pair-a))

(define (helper-b s1 s2)
  (define (filter-div x)
    (or (= 0 (modulo x 3))
        (= 0 (modulo x 2))
        (= 0 (modulo x 5))))
  (define (filter-div-pair p)
    (or (filter-div (car p)) (filter-div (cadr p))))
          
  (stream-filter filter-div-pair (merge-weighted s1 s2 weight-of-pair-b)))

; Ex 3.70 - a
(define (all-pairs-a s t)
  (all-pairs-with-merge-helper s t helper-a))

; Ex 3.70 - b
(define (all-pairs-b s t)
  (all-pairs-with-merge-helper s t helper-b))


(module+ test
  (begin
    (check-true
     (apply <= (map weight-of-pair-a (stream->list (stream-take (all-pairs-a integers integers) 200)))))
    (check-true
     (apply <= (map weight-of-pair-b (stream->list (stream-take (all-pairs-b integers integers) 200)))))
    ))

; Ex. 3.71  A001235 Taxi-cab numbers: sums of 2 cubes in more than 1 way.
; 1729 4104 13832 20683 32832
(define (weight-of-ramanujan-pair p)
    (let ((i (car p))
          (j (cadr p)))
      (+ (* i i i) (* j j j))))

(define (helper-ramanujan s1 s2)
  (merge-weighted s1 s2 weight-of-ramanujan-pair))

; use distinct pairs
(define (pairs-with-merge-helper s t helper)    
  (let ((s1 (stream-cons (list (stream-first s) (stream-first t))
                         (pairs-with-merge-helper (stream-rest s) (stream-rest t) helper)))
        (s2 (stream-map (lambda (x) (list (stream-first s) x)) (stream-rest t))))
    (helper s1 s2)))

(define ramanujan
  ; ramanujan-stream is the stream of pairs weighted using ramanujan metric
  ; combine a ramanujan-stream with a delayes ramanujan-stream (stream rest) using cons
  ; filter the stream for car = cdr to detect equal consecutive numbers
  ; and take the car only
  (let ((ramanujan-stream (pairs-with-merge-helper integers integers helper-ramanujan)))
    (stream-map car
                (stream-filter (lambda (p) (= (car p) (cdr p)))
                               (stream-map (lambda (p1 p2) (cons (weight-of-ramanujan-pair p1)
                                                                 (weight-of-ramanujan-pair p2)))
                                           ramanujan-stream (stream-rest ramanujan-stream))))))
    

(module+ test
  (begin
    (check-equal? (stream->list (stream-take ramanujan 35))
                  '(1729 4104 13832 20683 32832 39312 40033 46683 64232 65728 110656 110808 134379 149389 165464 171288 195841 216027 216125 262656 314496 320264 327763 373464 402597 439101 443889 513000 513856 515375 525824 558441 593047 684019 704977))))
