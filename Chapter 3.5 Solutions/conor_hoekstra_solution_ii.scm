;; Exercise 3.74 (page 467-9)

;(define sense-data '(1 2 1.5 1 0.5 -0.1 -2 -3 -2 -0.5 0.2 3 4))
(define sense-data (cons-stream 1 (cons-stream -1 1)))

(define (sign-change-detector curr prev)
  (cond ((and (> prev 0) (< curr 0)) -1)
        ((and (< prev 0) (> curr 0))  1)
        (else 0)))

(define (make-zero-crossings input-stream last-value)
  (cons-stream
   (sign-change-detector
    (stream-car input-stream)
    last-value)
   (make-zero-crossings
    (stream-cdr input-stream)
    (stream-car input-stream))))

(define zero-crossings
  (make-zero-crossings sense-data -1))

(stream-ref zero-crossings 0) ;  1
(stream-ref zero-crossings 1) ; -1

(define zero-crossings2
  (stream-map sign-change-detector
              sense-data
              (cons-stream -1 sense-data)))

(stream-ref zero-crossings2 0) ;  1
(stream-ref zero-crossings2 1) ; -1
