#lang racket

(define table (make-hash))
(define (put key1 key2 value) (hash-set! table (list key1 key2) value))
(define (get key1 key2)       (hash-ref  table (list key1 key2) #f))

;; from previous chapter

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error "No method for these types: APPLY-GENERIC"
                 (list op type-tags))))))

;; from the book
(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

(define (install-ordinary-package)
  (define (tag x) (attach-tag 'number x))
  (put 'add '(number number)
       (λ (x y) (tag (+ x y))))
  (put 'sub '(number number)
       (λ (x y) (tag (- x y))))
  (put 'mul '(number number)
       (λ (x y) (tag (* x y))))
  (put 'div '(number number)
       (λ (x y) (tag (/ x y))))
  (put 'make 'number (λ (x) (tag x)))
  'done)

(define (make-number n)
  ((get 'make 'number) n))

(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))
  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (λ (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (λ (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (λ (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (λ (x y) (tag (div-rat x y))))
  (put 'make 'rational
       (λ (n d) (tag (make-rat n d))))
  'done)

(define (make-rational n d)
  ((get 'make 'rational) n d))

(define (install-complex-package)
  ;; imported procedures from rectangular and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))
  ;; internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
                         (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                       (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                       (- (angle z1) (angle z2))))
  ;; interface to rest of the system
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (λ (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (λ (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (λ (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (λ (z1 z2) (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex
       (λ (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (λ (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

;; Exercise 2.78 (page 261)

 (define (attach-tag type-tag contents) 
   (if (number? contents) 
       contents 
       (cons type-tag contents))) 
  
 (define (type-tag datum) 
   (cond ((number? datum) 'number) 
         ((pair? datum) (car datum)) 
         (else (error "Wrong datum -- TYPE-TAG" datum)))) 
  
 (define (contents datum) 
   (cond ((number? datum) datum) 
         ((pair? datum) (cdr datum)) 
         (else (error "Wrong datum -- CONTENTS" datum))))

(install-ordinary-package)
(install-rational-package)
(install-complex-package)

(require rackunit)

(check-equal? (add 1 2) 3)
(check-equal? (add (make-rational 1 2) (make-rational 1 4)) '(rational 3 . 4))
