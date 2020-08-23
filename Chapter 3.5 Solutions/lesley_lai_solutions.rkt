#lang racket

(require rackunit
         threading)

;; Code for stream pattern matching
;; From https://stackoverflow.com/questions/60569711/pattern-matching-with-streams-in-racket
(require syntax/parse/define (prefix-in * racket/stream))
(define (stream-empty? v) (and (stream? v) (*stream-empty? v)))
(define (stream-cons? v) (and (stream? v) (not (*stream-empty? v))))

(define-match-expander stream-cons
  (syntax-parser
    [(_ fst rst) #'(? stream-cons? (app stream-first fst) (app stream-rest rst))])
  (syntax-parser
    [(_ fst rst) #'(*stream-cons fst rst)]))

(define-match-expander stream*
  (syntax-parser
    [(_ rst) #'rst]
    [(_ fst more ... rst) #'(stream-cons fst (stream* more ... rst))])
  (syntax-parser
    [(_ init ... rst) #'(*stream* init ... rst)]))

(define-match-expander stream
  (syntax-parser
    [(_) #'(? stream-empty?)]
    [(_ elem ...) #'(stream* elem ... (? stream-empty?))])
  (syntax-parser
    [(_ elem ...) #'(*stream elem ...)]))

;; A stream-map that accept variadic numbers of argument
(define (stream-map proc . argstreams)
  (if (stream-empty? (car argstreams))
      empty-stream
      (stream-cons
       (apply proc (map stream-first argstreams))
       (apply stream-map
              (cons proc (map stream-rest argstreams))))))

;; 3.54
(define (mul-stream xs ys)
  (stream-map * xs ys))

(define factorials
  (stream-cons 1 (mul-stream factorials (in-naturals 1))))

(module+ test
  (check-equal? (~>>
                 (mul-stream (in-range 10) (in-range 10))
                 stream->list)
                (~>>
                 (in-range 10)
                 (stream-map (lambda (x) (* x x)))
                 stream->list))

  (check-equal?
   (stream->list (stream-take factorials 10))
   '(1 1 2 6 24 120 720 5040 40320 362880))
)

;; 3.55
(define (partial-sums s)
  (if (stream-empty? s)
      empty-stream
      (let ([first (stream-first s)])
       (stream-cons first
                    (~>>
                     (stream-rest s)
                     (partial-sums)
                     (stream-map (curry + first)))))))

(module+ test
  (check-true (stream-empty? (partial-sums empty-stream)))

  (check-equal?
   (~>
    (partial-sums (in-naturals 1))
    (stream-take 5)
    (stream->list))
   '(1 3 6 10 15)))

;; 3.56
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

(define S
  (let ([sn (lambda (n) (stream-map (curry * n) (in-naturals 1)))])
    (let ([s2 (sn 2)]
          [s3 (sn 3)]
          [s5 (sn 5)])
      (stream-cons 1 (merge s2 (merge s3 s5))))))


(module+ test
  (check-equal?
   (~> (stream-take S 10)
       (stream->list))
   '(1 2 3 4 5 6 8 9 10 12)))

;; 3.64
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

(define (stream-limit s tolerance)
  (match s
    [(stream) 'f]
    [(stream-cons hd tail)
     (match tail
       [(stream) 'f]
       [(stream-cons hd2 _)
        (if (< (abs (- hd2 hd)) tolerance)
        hd2
        (stream-limit tail tolerance))])
     ]))

(module+ test
  (check-within
   (sqrt 2)
   (stream-limit (sqrt-stream 2) 1e-10)
   1e-10))

;; 3.65
(define (ln2-summands n)
  (stream-cons (/ 1.0 n)
               (stream-map - (ln2-summands (+ n 1)))))

(define ln2-stream
  (partial-sums (ln2-summands 1)))

(module+ test
  (check-within
   (log 2)
   (stream-limit ln2-stream 1e-2)
   1e-2))

;; 3.66
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

;;(stream->list (stream-take (pairs (in-naturals) (in-naturals)) 20))

;; TODO

;; 3.67
(define (pairs-67 s t)
  (stream-cons
   (list (stream-first s) (stream-first t))
   (interleave
    (stream-map (lambda (x) (list x (stream-first s)))
                 (stream-rest t))
    (interleave
     (stream-map (lambda (x) (list (stream-first s) x))
                 (stream-rest t))
     (pairs-67 (stream-rest s) (stream-rest t))))))

(module+ test
  (check-equal?
   (~>
    (pairs-67 (in-naturals) (in-naturals))
    (stream-take 10)
    (stream->list)
    )
   '((0 0) (1 0) (0 1) (2 0) (1 1) (3 0) (0 2) (4 0) (2 1) (5 0))))

;; 3.70
(define (merge-weight s1 s2 weight)
  (cond ((stream-empty? s1) s2)
        ((stream-empty? s2) s1)
        (else
         (let* ([s1car (stream-first s1)]
                [s2car (stream-first s2)]
                [w1 (weight s1car)]
                [w2 (weight s2car)])
           (cond ((< w1 w2)
                  (stream-cons
                   s1car
                   (merge-weight (stream-rest s1) s2 weight)))
                 ((> w1 w2)
                  (stream-cons
                   s2car
                   (merge-weight s1 (stream-rest s2) weight)))
                 (else
                  (let ([merged-tail (lambda ()
                                       (merge-weight (stream-rest s1)
                                                     (stream-rest s2)
                                                     weight))])
                    (if (equal? s1car s2car)
                        (stream-cons s2car (merged-tail))
                        (stream-cons s1car (stream-cons s2car (merged-tail))))
                    )))))))

(define (pairs-weighted s t weight)
  (stream-cons
   (list (stream-first s) (stream-first t))
   (merge-weight
    (stream-map (lambda (x) (list (stream-first s) x))
                (stream-rest t))
    (pairs-weighted (stream-rest s) (stream-rest t) weight)
    weight)))

;;;; 3.70 a
(define pairs-70a
  (pairs-weighted
   (in-naturals 1)
   (in-naturals 1)
   (lambda (pair) (+ (car pair) (cadr pair)))))

(module+ test
  (check-equal?
   (~>
    pairs-70a
    (stream-take 10)
    stream->list)
   '((1 1) (1 2) (1 3) (2 2) (1 4) (2 3) (1 5) (2 4) (3 3) (1 6))))

;;;; 3.70 b
(define (not-divisible-by by num)
  (not (eq? (remainder num by) 0)))

(define s-3-70-b
  (~>>
   (in-naturals 1)
   (stream-filter (curry not-divisible-by 2))
   (stream-filter (curry not-divisible-by 3))
   (stream-filter (curry not-divisible-by 5))))

(define pairs-70b
  (pairs-weighted
   s-3-70-b
   s-3-70-b
   (lambda (pair) (let ([i (car pair)]
                   [j (cadr pair)])
               (+ (* 2 i) (* 3 j) (* 5 i j))))))

(module+ test
  (check-equal?
   (~>
    pairs-70b
    (stream-take 10)
    stream->list)
   '((1 1) (1 7) (1 11) (1 13) (1 17) (1 19) (1 23) (1 29) (1 31) (7 7))))

;; 3.71
(define (cube-weight pair)
  (let ([i (car pair)]
        [j (cadr pair)])
    (+ (* i i i) (* j j j))))

(define s-cube-weighted
  (pairs-weighted
   (in-naturals 1)
   (in-naturals 1)
   cube-weight))

;; STL style adjacent-find algorithm for streams
(define (stream-adjacent-find condition s)
    (match s
      [(stream-cons x1 (stream-cons x2 tail))
       (let ([find-in-tail
              (lambda () (stream-adjacent-find condition (stream-cons x2 tail)))])
        (if (condition x1 x2)
            (stream-cons x1 (find-in-tail))
            (find-in-tail)))]
      [_ empty-stream]))

(define ramanujan-numbers (~>>
    s-cube-weighted
    (stream-adjacent-find (lambda (x1 x2) (equal? (cube-weight x1) (cube-weight x2))))
    (stream-map cube-weight)))

(module+ test
  (check-equal?
   (~>>
    ramanujan-numbers
    (stream-take _ 5)
    stream->list)
   '(1729 4104 13832 20683 32832)))
