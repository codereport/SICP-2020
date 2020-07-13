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

;; b) & c)

;; boilerplate for get & put

(define table (make-hash))
(define (put key1 key2 value) (hash-set! table (list key1 key2) value))
(define (get key1 key2)       (hash-ref  table (list key1 key2) #f))

;; Code from 2.3

(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))
(define (=number? exp num) (and (number? exp) (= exp num)))
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2))
         (+ a1 a2))
        (else (list '+ a1 a2))))
(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))
(define (addend s) (cadr s))
(define (augend s) (caddr s))
(define (multiplier p) (cadr p))
(define (multiplicand p) (caddr p))

(define (make-exponentiation base exp) 
  (cond ((=number? base 1) 1) 
        ((=number? exp 1) base) 
        ((=number? exp 0) 1) 
        (else (list '^ base exp))))

(define base cadr)
(define exponent caddr)

;; Modified code

(define (deriv-sum expr var)
  (make-sum (deriv (addend expr) var)
            (deriv (augend expr) var)))

(define (deriv-product expr var)
  (make-sum
   (make-product (multiplier expr)
                 (deriv (multiplicand expr) var))
   (make-product (deriv (multiplier expr) var)
                 (multiplicand expr))))

(define (deriv-exponentiation expr var)
  (make-product  
   (make-product
    (exponent expr)
    (make-exponentiation (base expr)
                         (make-sum (exponent expr) -1)))
   (deriv (base expr) var)))

(put 'deriv '+ deriv-sum)
(put 'deriv '* deriv-product)
(put 'deriv '^ deriv-exponentiation)

;; New deriv procedure

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else ((get 'deriv (operator exp)) (operands exp) var))))

(define (operator exp) (car exp))
(define (operands exp) exp) ;; note this change

(define (attach-tag type-tag contents) (cons type-tag contents))

(check-equal? (deriv '(+ x 3) 'x) 1)
(check-equal? (deriv '(* x 3) 'x) 3)
(check-equal? (deriv '(^ x 3) 'x) '(* 3 (^ x 2)))

;; d) Just swap key1 & key2 in your get proceduce implementation

;; Exercise 2.74 (page 250-1)

(define table (make-hash))
(define (put key1 key2 value) (hash-set! table (list key1 key2) value))
(define (get key1 key2)       (hash-ref  table (list key1 key2) #f))

(define (attach-tag type-tag contents) (cons type-tag contents))

(define divA-record-set '(("Bob" (address "123 Lane")
                                 (salary  100000))
                          ("Jen" (salary  150000)
                                 (adresss "456 Drive"))))

(define (findf proc lst)
  (let ((temp (memf proc lst)))
  (if temp (car temp) #f)))

(define (car=? x lst) (equal? x (car lst)))

;; name | address | salary
(define divB-record-set '(("Jim" "789 Street" 200000)
                          ("Ann" "000 Ave"    250000)))

;; a)

(define (get-recordA name) (findf (curry car=? name) divA-record-set))
(define (get-recordB name) (findf (curry car=? name) divB-record-set))

(put 'A 'record get-recordA)
(put 'B 'record get-recordB)

;; Assume unique names for indexing

(define (get-record div name)
  ((get div 'record) name))

;; > (get-record 'A "Jen")
;; '("Jen" (salary 150000) (adresss "456 Drive"))
;; > (get-record 'B "Jim")
;; '("Jim" "789 Street" 200000)

;; b)

(require threading)

(put 'A 'salary (λ (name) (~>> (get-recordA name)
                               (cdr)
                               (findf (curry car=? 'salary))
                               (cadr))))
(put 'B 'salary (λ (name) (caddr (get-recordB name))))

(define (get-salary div employee-name)
  ((get div 'salary) employee-name))

;; > (get-salary 'A "Jen")
;; 150000
;; > (get-salary 'B "Ann")
;; 250000

;; c)

(define (find-employee-record name div-list)
  (if (null? div-list)
      "Failed to find employee"
      (let* ((div (car div-list))
             (temp (get-record div name)))
        (if temp
            temp
            (find-employee-record name (cdr div-list))))))
            
;; > (find-employee-record "Jim" '(A B))
;; '("Jim" "789 Street" 200000)
;; > (find-employee-record "Jen" '(A B))
;; '("Jen" (salary 150000) (adresss "456 Drive"))

;; d) Add get-record, get-salary + corresponding put methods
