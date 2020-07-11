;; Exercise 2.73 (page 248-50)

;; originally

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum (make-product
                    (multiplier exp)
                    (deriv (multiplicand exp) var))
                   (make-product
                    (deriv (multiplier exp) var)
                    (multiplicand exp))))
        (else (error "unknown expression type: DERIV" exp))))

;; revised (using "data-directed" style)

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else ((get 'deriv (operator exp))
               (operands exp) var))))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

;; a) What was done? dispath of sum?, product?, etc were consolidated into
;;      the else ((get 'deriv ... clause of the conditional expression. Lookup
;;      is now done based on the operator using a selector
;;    Why can't we assimilate the predicates number? and variable into the
;;      data-directed dispatch? No "tag" to dispatch on

