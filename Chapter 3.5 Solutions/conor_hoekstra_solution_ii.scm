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

;; Exercise 3.77 (page 473)

;; code from the book

(define (integral delayed-integrand initial-value dt)
  (define int
    (cons-stream
     initial-value
     (let ((integrand (force delayed-integrand)))
       (add-streams (scale-stream integrand dt) int))))
  int)

(define (solve f y0 dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (stream-map f y))
  y)

(stream-ref (solve (lambda (x) x) 1 0.001) 1000)
; y: undefined;
; cannot use before initialization

;; original
(define (integral integrand initial-value dt)
  (cons-stream
   initial-value
   (if (stream-null? integrand)
       the-empty-stream
       (integral (stream-cdr integrand)
                 (+ (* dt (stream-car integrand))
                    initial-value)
                 dt))))

;; modified
(define (integral delayed-integrand initial-value dt) 
  (cons-stream
   initial-value 
   (let ((integrand (force delayed-integrand))) 
     (if (stream-null? integrand) 
         the-empty-stream 
         (integral (delay (stream-cdr integrand)) 
                   (+ (* dt (stream-car integrand)) 
                      initial-value) 
                   dt)))))

;; Exercise 3.81 (page 481)

(define random
  (let ((a 69069)
        (c 1.0)
        (m (expt 2 32.0))
        (seed 19380110)
        (orig-seed 19380110))
    (lambda (args)
      (if (list? args)
          (set! seed (cadr args))
          (set! seed (modulo (+ (* seed a) c) m)))
      (/ seed m))))

(define (random-stream input-stream)
  (stream-map random input-stream))

(define temp (cons-stream '(reset 19380110) (cons-stream 'new temp)))
(define r (random-stream temp))

;; Test
(map (lambda (x) (stream-ref r x))
     '(0 1 2 3 4 5 6 7 8 9 10))

; (0.004512283485382795
;  0.6589080521371216
;  0.004512283485382795
;  0.6589080521371216
;  0.004512283485382795
;  0.6589080521371216
;  0.004512283485382795
;  0.6589080521371216
;  0.004512283485382795
;  0.6589080521371216
;  0.004512283485382795)
