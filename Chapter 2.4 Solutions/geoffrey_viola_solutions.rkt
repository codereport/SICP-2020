#lang racket
(require rackunit)

(define *the-table* (make-hash))
(define (put key1 key2 value) (hash-set! *the-table* (list key1 key2) value)) 
(define (get key1 key2) (hash-ref *the-table* (list key1 key2)))

;; 2.73 (a-d)
;; 2.73 a
;; All the operations are refactored into their own functions.
;; number? and same-variable? don't take a corresponding "deriv" function
;; The keys would be unbounded

;; 2.73 b
(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else ((get 'deriv (operator exp)) (operands exp)
                                           var))))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

(define (=number? exp num) 
  (and (number? exp) (= exp num))) 
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))
(define (addend s) (car s)) 
(define (augend s) (cadr s)) 
(define (deriv-sum operand var)
  (make-sum (deriv (addend operand) var) (deriv (augend operand) var))) 
(put 'deriv '+ deriv-sum)
(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))
(define (multiplier p) (car p))
(define (multiplicand p) (cadr p))
(define (deriv-product exp var)
  (make-sum
   (make-product (multiplier exp)
                 (deriv (multiplicand exp) var))
   (make-product (deriv (multiplier exp) var)
                 (multiplicand exp))))
(put 'deriv '* deriv-product)

(module+ test
  (begin
    (check-equal? (deriv '(+ x 0) 'x) 1)
    (check-equal? (deriv '(+ 2 x) 'x) 1)
    (check-equal? (deriv '(+ x x) 'x) 2)
    (check-equal? (deriv '(* 2 x) 'x) 2)
    ;; Note that this should be 3, but only 2 args are considered
    (check-equal? (deriv '(+ x x x) 'x) 2)))

;; 2.74 c
(define (base s) (car s))
(define (exponent s) (cadr s))
(define (make-exponentiation base exponent)
  (cond ((=number? base 1) 1) 
        ((=number? exponent 1) base) 
        ((=number? exponent 0) 1)
        (else  
         (list '** base exponent))))
(define (deriv-exponentiation exp var)
  (make-product
   (make-product (exponent exp)
                 (make-exponentiation (base exp)
                                      (make-sum (exponent exp) -1)))
   (deriv (base exp) var)))
(put 'deriv '** deriv-exponentiation)

(module+ test
  (begin
    (check-equal? (deriv '(** x 0) 'x) 0)
    (check-equal? (deriv '(** x 1) 'x) 1)
    (check-equal? (deriv '(** x 2) 'x) '(* 2 x))
    (check-equal? (deriv '(** x 3) 'x) '(* 3 (** x 2)))))

;; d
;; Switch the put statement

;; 2.74 (a-d)

;; a
;; get must have the function for (list division 'record)
(define (get-record division employee-name files) 
  ((get division 'record) employee-name files))

;; b
;; get must have the function for (list division 'salary)
(define (get-salary division record files) 
  ((get division 'salary) record files))

;; c
;; Each division must supploy this getter
(define (get-name file employee-names) '())
(define (find-employee-record division employee-name files)
  (if (= (length files) 0) (error "file not found")
      (let ((file (car files)))
        (if (get-name file employee-name)
            file
            (find-employee-record division employee-name file)))))

;; d
;; The preconditions above must be met
 
;; 2.75
(define (make-from-mag-ang x y)
  (define (dispatch op)
    (cond ((eq? op 'real-part) (* x (cos y)))
          ((eq? op 'imag-part) (* x (sin y)))
          ((eq? op 'magnitude) x)
          ((eq? op 'angle) y)
          (else
           (error "Unknown op -- MAKE-FROM-REAL-IMAG" op))))
  dispatch)
