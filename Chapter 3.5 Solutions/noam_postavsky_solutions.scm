;; SRFI (Scheme Request For Implementation) 41 - Streams.
(import (rename srfi-41
                (stream-cons cons-stream)
                (stream-null the-empty-stream))
        (only srfi-41 stream-car stream-cdr stream-null?
              stream-map stream-for-each stream-ref
              stream->list
              stream-match))

;; SRFI-41 should have a stream-merge, but it fails to import (old version?)
(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (cond ((< s1car s2car)
                  (cons-stream s1car (merge (stream-cdr s1) s2)))
                 ((> s1car s2car)
                  (cons-stream s2car (merge s1 (stream-cdr s2))))
                 (else
                  (cons-stream s1car
                               (merge (stream-cdr s1)
                                      (stream-cdr s2)))))))))

(define (add-streams s1 s2)
  (stream-map + s1 s2))
(define (mul-streams s1 s2)
  (stream-map * s1 s2))

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

(define ones (cons-stream 1 ones))

(define integers
  (cons-stream 1 (add-streams ones integers)))

;; - 3.54

(define factorials
  (cons-stream 1 (mul-streams factorials (stream-cdr integers))))

(stream->list 5 factorials) ;=> (1 2 6 24 120)

;; - 3.55

(define (partial-sums s)
  (define sums
    (if (stream-null? s)
        the-empty-stream
        (cons-stream (stream-car s)
                     (add-streams (stream-cdr s) sums))))
  sums)

(stream->list 5 (partial-sums integers)) ;=> (1 3 6 10 15)

;; - 3.56

(define S (cons-stream 1 (merge (scale-stream S 2)
                                (merge (scale-stream S 3)
                                       (scale-stream S 5)))))

(stream->list 20 S) ;=> (1 2 3 4 5 6 8 9 10 12 15 16 18 20 24 25 27 30 32 36)

;; - 3.64

(define (stream-limit s tolerance)
  (stream-match
   s
   ((s1 s2 . _)                         ; Pattern.
    (< (abs (- s2 s1)) tolerance)       ; Fender (aka guard).
    s2)                                 ; Result.
   ((_ . srest)
    (stream-limit srest tolerance))
   (_ (error "Stream too short"))))

(define (sqrt x tolerance)
  (define (sqrt-improve guess x)
    (define (average x1 x2)
      (/ (+ x1 x2) 2))
    (average guess (/ x guess)))
  (define (sqrt-stream x)
    (define guesses
      (cons-stream 1.0
                   (stream-map (lambda (guess)
                                 (sqrt-improve guess x))
                               guesses)))
    guesses)

  (stream-limit (sqrt-stream x) tolerance))

(sqrt 2 1) ;=> 1.5
(sqrt 2 0.5) ;=> 1.41666666666667
(sqrt 2 0.05) ;=> 1.41421568627451

;; - 3.65

(define (euler-transform s)
  (define (square x) (* x x))
  (let ((s0 (stream-ref s 0))           ; S_(n-1)
        (s1 (stream-ref s 1))           ; S_n
        (s2 (stream-ref s 2)))          ; S_(n+1)
    (cons-stream (- s2 (/ (square (- s2 s1))
                          (+ s0 (* -2 s1) s2)))
                 (euler-transform (stream-cdr s)))))


(define (accelerated-sequence transform s)
  (define (make-tableau transform s)
    (cons-stream s
                 (make-tableau transform
                               (transform s))))
  (stream-map stream-car
              (make-tableau transform s)))

(define (log2-summands n)
  (cons-stream (/ 1 n)
               (stream-map - (log2-summands (+ n 1)))))

(stream->list 5 (log2-summands 1)) ;=> (1 -1/2 1/3 -1/4 1/5)

(define log2-stream
  (partial-sums (log2-summands 1.0)))

(stream->list 8 log2-stream) ;=> (1.0 0.5 0.833333333333333 0.583333333333333 0.783333333333333
                             ;    0.616666666666667 0.759523809523809 0.634523809523809)

(stream->list 8 (euler-transform log2-stream))
                                        ;=> (0.7 0.69047619047619 0.694444444444444 0.692424242424242
                                        ;    0.693589743589744 0.692857142857143
                                        ;    0.693347338935574 0.693003341687552)


(stream->list 8 (accelerated-sequence euler-transform log2-stream))
                                        ;=> (1.0 0.7 0.69327731092437 0.693148869332925
                                        ;    0.693147196073549 0.693147180663564
                                        ;    0.693147180560404 0.693147180559945)

;; - 3.66

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

(stream->list 10 (pairs integers integers))
                                        ;=> ((1 1) (1 2)
                                        ;    (2 2) (1 3)
                                        ;    (2 3) (1 4)
                                        ;    (3 3) (1 5)
                                        ;    (2 4) (1 6))


;; Q: About how many pairs precede the pair (1,100)?
;; A: Roughly 200 (every other pair is of the form (1,n)).
;; Pair (1,n) is the (2n-3)th pair.

(stream-ref (pairs integers integers) 197) ;=> (1 100)
(stream-ref (pairs integers integers) 1997) ;=> (1 1000)

;; (99,100)?  Something exponential?

(stream->list 100 (pairs integers integers))
;; ((1 1) (1 2) ; 2
;;  (2 2) (1 3) (2 3) ; 3
;;  (1 4) (3 3) (1 5) (2 4) (1 6) (3 4) ; 6
;;  (1 7) (2 5) (1 8) (4 4) (1 9) (2 6) (1 10) (3 5) (1 11) (2 7) (1 12) (4 5) ; 12
;;                                         ; 24
;;  (1 13) (2 8) (1 14) (3 6) (1 15) (2 9) (1 16) (5 5) (1 17) (2 10) (1 18) (3 7) (1 19) (2 11) (1 20) (4 6) (1 21) (2 12) (1 22) (3 8) (1 23) (2 13) (1 24) (5 6)
;;                                         ; 48
;;  (1 25) (2 14) (1 26) (3 9) (1 27) (2 15) (1 28) (4 7) (1 29) (2 16) (1 30) (3 10) (1 31) (2 17) (1 32) (6 6) (1 33) (2 18) (1 34) (3 11) (1 35) (2 19) (1 36) (4 8) (1 37) (2 20) (1 38) (3 12) (1 39) (2 21) (1 40) (5 7) (1 41) (2 22) (1 42) (3 13) (1 43) (2 23) (1 44) (4 9) (1 45) (2 24) (1 46) (3 14) (1 47) (2 25) (1 48) (6 7)
;;  (1 49) (2 26) (1 50) (3 15) (1 51))

;; - 3.67

(define (all-pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (interleave (stream-map (lambda (x) (list (stream-car s) x))
                            (stream-cdr t))
                (stream-map (lambda (x) (list x (stream-car t)))
                            (stream-cdr s)))
    (all-pairs (stream-cdr s) (stream-cdr t)))))

(stream->list 10 (all-pairs integers integers))
                                        ;=> ((1 1) (1 2)
                                        ;    (2 2) (2 1)
                                        ;    (2 3) (1 3)
                                        ;    (3 3) (3 1)
                                        ;    (3 2) (1 4))


;; - 3.70

(define (merge-weighted s1 s2 weight)
  (define (merge s1 s2)
   (cond ((stream-null? s1) s2)
         ((stream-null? s2) s1)
         (else
          (let* ((s1car (stream-car s1))
                 (s1weight (weight s1car))
                 (s2car (stream-car s2))
                 (s2weight (weight s2car)))
            (cond ((< s1weight s2weight)
                   (cons-stream s1car (merge (stream-cdr s1) s2)))
                  ((> s1weight s2weight)
                   (cons-stream s2car (merge s1 (stream-cdr s2))))
                  (else
                   (cons-stream s1car
                                (merge (stream-cdr s1)
                                       (stream-cdr s2)))))))))
  (merge s1 s2))

(define (weighted-pairs s t weight)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (merge-weighted
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (weighted-pairs (stream-cdr s) (stream-cdr t) weight)
    weight)))

;; This does not work?
(stream->list 10 (weighted-pairs integers integers
                                 (lambda (pair) (+ (car pair) (cadr pair)))))
                                        ;=> ((1 1) (1 2) (1 3) (1 4) (1 5) (1 6) (1 7) (1 8) (1 9) (1 10))

;; - 3.71
