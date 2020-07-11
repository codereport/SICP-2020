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

;; b)

;; sum

(define (install-sum-package) 
  (define make-sum cons) 
  (define addend cadr) 
  (define augend caddr) 
  (define (deriv-sum expr var)
    (make-sum (deriv (addend expr) var)
              (deriv (augend expr) var)))   
  (define (tag x) (attach-tag '+ x))
  
  (put 'deriv '(+) deriv-sum) 
  (put 'make-sum '+ (λ (x y) (tag (make-sum x y))))
  
  'done) 
  
(define (make-sum x y) ((get 'make-sum '+) x y))

;; product

(define (install-product-package) 
  (define make-product cons) 
  (define multiplier cadr) 
  (define multiplicand caddr) 
  (define (deriv-product expr var) 
    (make-sum 
     (make-product (multiplier expr) 
                   (deriv (multiplicand expr) var)) 
     (make-product (deriv (multiplier expr) var) 
                   (multiplicand expr)))) 
  
  (define (tag x) (attach-tag '* x))
  
  (put 'deriv '(*) deriv-product) 
  (put 'make-product '* (λ (x y) (tag (make-product x y))))
  
  'done) 
  
(define (make-product x y) ((get 'make-product '*) x y)) 

;; boilerplate for get & put

 (define table (make-hash))
 (define (put key1 key2 value) (hash-set! table (list key1 key2) value))
 (define (get key1 key2) (hash-ref table (list key1 key2) #f))

(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))
(define (=number? exp num) (and (number? exp) (= exp num)))

(define (attach-tag type-tag contents)
  (cons type-tag contents))

;; (require racketunit)

(install-sum-package)
(install-product-package)
