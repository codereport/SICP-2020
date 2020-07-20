#lang racket

(require rackunit)

(define (square x) (* x x))

(define *the-table* (make-hash))
(define (put key1 key2 value) (hash-set! *the-table* (list key1 key2) value))
(define (get key1 key2) (hash-ref *the-table* (list key1 key2)))

;; 2.78
(define (attach-tag type-tag contents)
  (if (number? contents)
      contents
      (cons type-tag contents)))
(define (type-tag datum)
  (cond ((number? datum) 'scheme-number)
        ((pair? datum) (car datum))
        (error "Bad tagged datum -- TYPE-TAG" datum)))
(define (contents datum)
  (cond ((number? datum) datum)
        ((pair? datum) (cdr datum))
        (error "Bad tagged datum -- CONTENTS" datum)))

(module+ test
  (begin
    (check-equal? (attach-tag '() 1) 1)
    (check-equal? (type-tag 1) 'scheme-number)
    (check-equal? (contents 1) 1)
    ))

;; 2.79
;; book code
(define (install-rational-package)
  (begin
    ;; internal procedures
    (define (make-rat n d)
      (let ((g (gcd n d)))
        (list (/ n g) (/ d g))))
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
         (lambda (x y) (tag (add-rat x y))))
    (put 'sub '(rational rational)
         (lambda (x y) (tag (sub-rat x y))))
    (put 'mul '(rational rational)
         (lambda (x y) (tag (mul-rat x y))))
    (put 'div '(rational rational)
         (lambda (x y) (tag (div-rat x y))))
    (put 'make 'rational
         (lambda (n d) (tag (make-rat n d))))
    ))
(define (numer x) (cadr x))
(define (denom x) (caddr x))
(define (make-rational n d)
  ((get 'make 'rational) n d))

(define (install-complex-package)
  (begin
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
         (lambda (z1 z2) (tag (add-complex z1 z2))))
    (put 'sub '(complex complex)
         (lambda (z1 z2) (tag (sub-complex z1 z2))))
    (put 'mul '(complex complex)
         (lambda (z1 z2) (tag (mul-complex z1 z2))))
    (put 'div '(complex complex)
         (lambda (z1 z2) (tag (div-complex z1 z2))))
    (put 'make-from-real-imag 'complex
         (lambda (x y) (tag (make-from-real-imag x y))))
    (put 'make-from-mag-ang 'complex
         (lambda (r a) (tag (make-from-mag-ang r a))))
    ))
(define (rectangular? z)
  (eq? (type-tag z) 'rectangular))
(define (polar? z)
  (eq? (type-tag z) 'polar))
(define (real-part z)
  (cond ((rectangular? z) 
         (real-part-rectangular (contents z)))
        ((polar? z)
         (real-part-polar (contents z)))
        (else (error "Unknown type -- REAL-PART" z))))
(define (imag-part z)
  (cond ((rectangular? z)
         (imag-part-rectangular (contents z)))
        ((polar? z)
         (imag-part-polar (contents z)))
        (else (error "Unknown type -- IMAG-PART" z))))
(define (real-part-rectangular z) (car z))
(define (imag-part-rectangular z) (cadr z))
(define (magnitude-rectangular z)
  (sqrt (+ (square (real-part-rectangular z))
           (square (imag-part-rectangular z)))))
(define (angle-rectangular z)
  (atan (imag-part-rectangular z)
        (real-part-rectangular z)))
(define (make-from-real-imag-rectangular x y)
  (attach-tag 'rectangular (list x y)))
(define (make-from-mag-ang-rectangular r a) 
  (attach-tag 'rectangular
              (list (* r (cos a)) (* r (sin a)))))
(define (real-part-polar z)
  (* (magnitude-polar z) (cos (angle-polar z))))
(define (imag-part-polar z)
  (* (magnitude-polar z) (sin (angle-polar z))))
(define (magnitude-polar z) (car z))
(define (angle-polar z) (cdr z))
(define (make-from-real-imag-polar x y) 
  (attach-tag 'polar
              (list (sqrt (+ (square x) (square y)))
                    (atan y x))))
(define (make-from-mag-ang-polar r a)
  (attach-tag 'polar (list r a)))
(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))

;; my code
;; integer and real numbers
(define (install-scheme-number-equ-package)
  (begin
    (put 'equ '(scheme-number scheme-number) =)
    ))
(define (equ-num? x y) ((get 'equ '(scheme-number scheme-number)) x y))

(module+ test
  (begin
    (install-scheme-number-equ-package)
    (check-equal? (equ-num? 1 1) #t)
    (check-equal? (equ-num? 1 2) #f)
    (check-equal? (equ-num? 1.1 1.1) #t)
    (check-equal? (equ-num? 1.1 2.1) #f)
    ))

;; rational
(define (install-rational-equ-package)
  (begin
    (put 'equ '(rational rational)
         (λ (x y)
           (and (equal? (car x) 'rational) (equal? (car y) 'rational)
                (= (numer x) (numer y)) (= (denom x) (denom y)))))))
(define (equ-rat? x y) ((get 'equ '(rational rational)) x y))

(module+ test
  (begin
    (install-rational-package)
    (install-rational-equ-package)
    (check-equal? (equ-rat? (make-rational 1 2) (make-rational 1 2)) #t)
    (check-equal? (equ-rat? (make-rational 1 2) (make-rational 1 3)) #f)
    ))

;; complex
(define (install-complex-equ-package)
  (begin
    (put 'equ '(complex complex)
         (λ (x y)
           (and (or (equal? (car x) 'rectangular) (equal? (car x) 'polar))
                (equal? (car x) (car y))
                (= (real-part x) (real-part y))
                (= (imag-part x) (imag-part y)))))))
(define (equ-complex? x y) ((get 'equ '(complex complex)) x y))

(module+ test
  (begin
    (install-complex-package)
    (install-complex-equ-package)
    (check-equal? (equ-complex?
                   (make-from-real-imag-rectangular 1 2)
                   (make-from-real-imag-rectangular 1 2)) #t)
    (check-equal? (equ-complex?
                   (make-from-real-imag-rectangular 1 2)
                   (make-from-real-imag-rectangular 1 3)) #f)
    ))

; 2.83
(put 'raise 'integer
  (λ (x) (make-rational x 1)))
(put 'raise 'rational
  (λ (x) (/ (numer x) (denom x))))
(put 'raise 'real
  (λ (x) (make-from-real-imag-rectangular x 0)))
(module+ test
  (begin
    (check-equal? (equ-rat? ((get 'raise 'integer) 1) (make-rational 1 1)) #t)
    (check-equal? (equ-rat? ((get 'raise 'integer) 1) (make-rational 1 2)) #f)
    (check-equal? (= ((get 'raise 'rational) (make-rational 1 2)) 0.5) #t)
    (check-equal? (= ((get 'raise 'rational) (make-rational 1 2)) 1) #f)
    (check-equal? (equ-complex? ((get 'raise 'real) 1.5)
                     (make-from-real-imag-rectangular 1.5 0)) #t)
    (check-equal? (equ-complex? ((get 'raise 'real) 1.5)
                     (make-from-real-imag-rectangular 1.5 1)) #f)

    ))


; 2.84
; skipped
