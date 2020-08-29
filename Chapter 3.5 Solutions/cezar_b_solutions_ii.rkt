#lang racket
(require racket/stream)
(require rackunit)
(require math/base)
(require racket/match)

;- some utilities
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

(define (mul-streams s1 s2) (stream-map * s1 s2))
(define (add-streams s1 s2) (stream-map + s1 s2))
(define (constant-stream c) (stream-cons c (constant-stream c)))
(define ones (constant-stream 1))
(define integers (stream-cons 1 (add-streams ones integers)))
(define nats (stream-cons 0 (add-streams ones nats)))
(define (list->stream els)
  (if (null? els)
      empty-stream
      (stream-cons (car els) (list->stream (stream-rest els)))))
(define (sign n)
  (cond ((positive? n) 1)
          ((negative? n) -1)
          (else 0)))
;-

; Ex. 3.74


(define (sign-change-detector a b)
  (let ((sa (sign a))
        (sb (sign b)))
    (cond ((= sa sb) 0)
          ((positive? sb) -1)
          (else 1))))

(define sense-data (list->stream '(1 2 1.5 1 0.5 -0.1 -2 -3 -2 -0.5 0.2 3 4)))
(define zero-crossings
  (stream-map sign-change-detector
              sense-data
              (stream-cons (stream-first sense-data) sense-data)))

(module+ test
  (begin
    (check-equal? (stream->list zero-crossings)
                  '(0 0 0 0 0 -1 0 0 0 0 1 0 0))))

; Ex. 3.77
(define (integral delayed-integrand initial-value dt)
  (stream-cons
   initial-value
   (let ((integrand (force delayed-integrand)))
     (if (stream-empty? integrand)
         empty-stream
         (integral (stream-rest integrand)
                   (+ (* dt (stream-first integrand))
                      initial-value)
                   dt)))))

(define (solve f y0 dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (stream-map f y))
  y)

(module+ test
  (begin
    (check-within euler.0     
                  (stream-ref (solve (lambda (y) y)
                                     1
                                     0.001)
                              1000)
                  0.01)))

; Ex. 3.81

(define (random-stream seed requests)
  ; next randon number is just an increment
  (if (stream-empty? requests)                  
      empty-stream
      (let ((next  (match (stream-first requests)
                     ['get (+ 1 seed)] ; next number
                     [(list 'reset new-seed) new-seed])))
        (stream-cons next (random-stream next (stream-rest requests))))))

(module+ test
  (begin
    (check-equal? (stream->list (random-stream 0 '((reset 0) get get get (reset 0) get get)))
                  '(0 1 2 3 0 1 2))))

