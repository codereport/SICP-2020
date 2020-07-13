(import (only chicken.plist get put!))

;;; - 2.73 (a-d)

;;; a. There is no place to put a type tag on variables and numbers,
;;; so they can't be handled by data-directed dispatch.

;;; b.

(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))
(define (make-sum a1 a2) (list '+ a1 a2))
(define (make-product m1 m2) (list '* m1 m2))
(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))
(define (addend s) (cadr s))
(define (augend s) (caddr s))
(define (product? x)
  (and (pair? x) (eq? (car x) '*)))
(define (multiplier p) (cadr p))
(define (multiplicand p) (caddr p))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else ((get 'deriv (operator exp)) (operands exp)
               var))))

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

(define (install-deriv-sum)
  (define (deriv-sum terms var)
    (make-sum
     (deriv (car terms) var)
     (deriv (cadr terms) var)))
  (put! 'deriv '+ deriv-sum))

(define (install-deriv-product)
  (define (deriv-product factors var)
    (let ((left (car factors))
          (right (cadr factors)))
      (make-sum
       (make-product left (deriv right var))
       (make-product (deriv left var) right))))
  (put! 'deriv '* deriv-product))

(install-deriv-product)
(install-deriv-sum)

;;; c.

(define (exponentiation? expr)
  (and (pair? expr) (eq? (car expr) '**)))
(define (base exp)
  (cadr exp))
(define (exponent exp)
  (caddr exp))
(define (make-exponentiation base exponent)
  (list '** base exponent))

(define (install-deriv-exponentiation)
  (define (deriv-exponentiation args var)
    (let ((u (car args))
          (n (cadr args)))
      (make-product (make-product n
                                  (make-exponentiation u (make-sum n -1)))
                    (deriv u var))))
  (put! 'deriv '** deriv-exponentiation))

(install-deriv-exponentiation)

;;; d.

;; Use (put! OPERATOR 'deriv) instead of (put! 'deriv OPERATOR)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; - 2.74 (a-d)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; a.
;; PERSONNEL-FILE should be of the form (TYPE-TAG . DATA)
(define (get-record personnel-file employee-id)
  ((get (car personnel-file) 'get-record)
   employee-id))

;;; b.
;; EMPLOYEE-RECORD should be of the form (TYPE-TAG . DATA)
(define (get-salary employee-record)
  ((get (car employee-record) 'get-salary)
   employee-record))

;;; c.
(define (find-employee-record employee-name personnel-files)
  (define (find predicate sequence)
    (cond ((null? sequence) #f)
          ((predicate (car sequence))
           (car sequence))
          (else (find (cdr sequence)))))
  (find (lambda (file)
          (get-record file employee-name))
        personnel-files))

;;; d.
;; A new company would have to add a 'get-record' operation for their
;; personnel-file type-tag, and a 'get-salary' for the
;; personnel-file's type-tag.

;;; - 2.75

(define (make-from-mag-ang r a)
  (define (dispatch op)
    (cond ((eq? op 'real-part) (* r (cos a)))
          ((eq? op 'imag-part) (* r (sin a)))
          ((eq? op 'magnitude) r)
          ((eq? op 'angle) a)
          (else
           (error "Unknown op -- MAKE-FROM-MAG-ANG" op))))
  dispatch)
