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

;; Exercise 2.79 (261)

(put 'equ? '(number number) =)                                           ; put in number package
(put 'equ? '(rational rational) (λ (x y)                                 ; put in rational package
                                  (= (* (numer x) (denom y))
                                     (* (numer y) (denom x)))))    
(put 'equ? '(complex complex) (λ (x y)                                   ; put in complex package
                                (and (= (real-part x) (real-part y))
                                     (= (imag-part x) (imag-part y)))))
                                     
(define (equ? x y) (apply-generic 'equ? x y))

(check-equal? (equ? 1 1) #t)
(check-equal? (equ? 1 2) #f)
(check-equal? (equ? (make-rational 1 2) (make-rational 2 4)) #t)
(check-equal? (equ? (make-rational 1 2) (make-rational 1 3)) #f)
(check-equal? (equ? (make-complex-from-real-imag 1 2)
                    (make-complex-from-real-imag 1 2)) #t)

;; Exercise 2.83 (page 272)

(put 'raise 'integer  (λ (x) (make-rational x 1)))
(put 'raise 'rational (λ (x) (make-real (/ (numer x) (denom x)))))
(put 'raise 'real     (λ (x) (make-from-real-imag x 0)))

(define (raise x) (apply-generic 'raise x)) 

;; Exercise 2.84 (page 272)

(define (do-raise a b)
  (let ((a-type (type-tag a))
        (b-type (type-tag b)))
    (cond ((equal? a-type b-type) a)
          ((get 'raise a-type)
           (do-raise ((get 'raise a-type) (contents a)) b))
          (else #f))))

(define (apply-generic op . args)            
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((a (car args))
                    (b (cadr args)))
                (cond ((do-raise a b) (apply-generic op (do-raise a b) b))
                      ((do-raise b a) (apply-generic op a (do-raise b a)))
                      (else (error "Not supported" (list op type-tags)))))
              (error "Not supported" (list op type-tags)))))))

(check-equal? (do-raise 2 3) 2)
(check-equal? (do-raise 2 (make-rational 3 1)) (make-rational 2 1))
(check-equal? (add 2 (make-rational 3 1)) (make-rational 5 1))
(check-equal? (mul 2 (make-rational 3 1)) (make-rational 6 1))
