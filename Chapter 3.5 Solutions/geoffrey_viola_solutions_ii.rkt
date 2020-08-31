#lang racket

(require rackunit
         threading)

;; Exercises 3.74
(define (sign-change-detector x y)
  (cond ((and (< x 0) (> y 0)) 1)
        ((and (> x 0) (< y 0)) -1)
        (else 0)))

;; the assignment is horendous, but it works
(define (zero-crossings input-stream)
  (let  ((prev-value 0)
         (s input-stream))
    (stream-map (lambda (x) (define ret (curry sign-change-detector prev-value x))
                  (if (not (null? s)) (begin
                                        (set! prev-value (stream-first s))
                                        (set! s (stream-rest s))) #f)
                  ret)
                input-stream)))

;; lazy =
(define (my-stream-= a b)
  (if (stream-empty? a) (if (stream-empty? b) #t #f)
      (begin
        (if (= (stream-first a) (stream-first b))
            (my-stream-= (stream-rest a) (stream-rest b))
            #f))))

(module+ test
  (begin
    (define sense-data (stream 1 2 1.5 1 0.5 -0.1 -2 -3 -2 -0.5 0.2 3 4))
    (define sense-data-change (stream 0 0 0 0 0 -1 0 0 0 0 1 0 0))
    (check-equal? (my-stream-= (stream* sense-data-change) (zero-crossings sense-data)) #t)
    ))

;; Exercises 3.77
(define the-empty-stream (stream))
(define (integral delayed-integrand initial-value dt)
  (stream-cons initial-value
               (let ((integrand (force delayed-integrand)))
                 (if (stream-empty? integrand)
                     the-empty-stream
                     (integral (delay (stream-rest integrand))
                               (+ (* dt (stream-first integrand))
                                  initial-value)
                               dt)))))

(module+ test
  (begin
    (check-equal? (~>
                   (stream 1 1 1)
                   (integral 0 10)
                   stream->list
                   last)
                  30)
    (check-equal? (~>
                   (stream 1 2 3)
                   (integral 5 20)
                   stream->list
                   last)
                  125)
    ))

;; Exercise 3.81
(define (rand-stream original-input-stream)
  (random-seed 0)
  (define (rand-stream-helper input-stream)
    (if (stream-empty? input-stream)
        empty-stream
        (let ((command (stream-first input-stream)))
          (case command
            ['reset
             (random-seed 0)
             (rand-stream-helper (stream-rest input-stream))]
            ['generate
             (let ((r (random 4294967087)))
               (stream-cons r (rand-stream-helper (stream-rest input-stream))))]))))
  (rand-stream-helper original-input-stream))

(module+ test
  (begin
    (check-equal? (~>
                   (stream 'generate 'generate 'generate)
                   rand-stream
                   stream->list)
                  '(3681460455 2832193900 883280686))
    (check-equal? (~>
                   (stream 'reset 'generate 'generate)
                   rand-stream
                   stream->list)
                  '(3681460455 2832193900))
    (check-equal? (~>
                   (stream 'generate 'reset 'generate)
                   rand-stream
                   stream->list)
                  '(3681460455 3681460455))
    ))
