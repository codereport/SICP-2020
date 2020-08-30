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

;; 3.74
(define (make-zero-crossings input-stream last-value)
  (define (sign-change-detector x y)
    (cond ((and (>= x 0) (< y 0)) 1)
          ((and (< x 0) (>= y 0)) -1)
          (else 0)))

  (stream-map sign-change-detector
              input-stream
              (stream-cons (stream-first input-stream) input-stream)
             ))

(module+ test
  (check-equal? (~>
                 (stream 1 2 1.5 1 0.5 -0.1 -2 -3 -2 -0.5 0.2 3 4)
                 (make-zero-crossings 0)
                 stream->list
                 )
                '(0 0 0 0 0 -1 0 0 0 0 1 0 0)))

;; 3.77
(define (integral delayed-integrand initial-value dt)
  (stream-cons
   initial-value
   (let ([integrand (force delayed-integrand)])
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

;; dy/dt = y
;; Solution: y = c_0 e^t
;; Explicit Euler diverges really rapidly on exponentials :-(
(module+ test
  (for-each (curry check-within 0.1)
            (~>
             (solve identity 1 0.01)
             (stream-take 10)
             (stream->list))
            (~>>
             (range 0 0.099 0.01)
             (map exp))))

;; 3.81
;;;; rand-update implementation steal from
;;;; https://github.com/klutometis/sicp/blob/master/rand-update.scm
(define modulus (make-parameter (expt 2 64)))
(define multiplier (make-parameter 6364136223846793005))
(define increment (make-parameter 1442695040888963407))
(define (rand-update x)
  (modulo (+ (* (multiplier) x) (increment)) (modulus)))

(define (random-numbers seed)
  (stream-cons
   seed
   (stream-map rand-update (random-numbers seed))))

(define (rng seed requests)
  (define initial-randoms (random-numbers 0))
  (define (helper randoms requests_)
    (match requests_
      [(stream) empty-stream]
      [(stream-cons 'generate tail)
       (stream-cons
        (stream-first randoms)
        (helper (stream-rest randoms) tail))]
      [(stream-cons 'reset tail) (helper initial-randoms tail)]))
  (helper initial-randoms requests))


(module+ test
  (define seed 0)
  (define (randoms num)
    (~>
     (random-numbers seed)
     (stream-take num)
     stream->list))

  (check-equal?
   (~>>
    (stream 'generate 'generate 'generate 'generate 'generate
            'reset 'generate 'generate 'generate 'generate
            'reset 'generate
            'reset 'generate 'generate 'generate 'generate 'generate)
    (rng seed)
    stream->list)
   (append (randoms 5) (randoms 4) (randoms 1) (randoms 5))))
