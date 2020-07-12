#lang racket
(require rackunit)
(require racket/trace)


;-- <BOOK> --
(module sicp-calculus racket
  (require racket/trace)
  (provide (all-defined-out))
  (define (variable? x) (symbol? x))
  (define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))

  (define (addend s) (car s))
  (define (augend s) (cadr s))
  (define (multiplier p) (car p))
  (define (multiplicand p) (cadr p))
  (define (base s) (car s))
  (define (exponent s) (cadr s))  
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
  (define (make-exponentiation base exp) 
    (cond ((=number? base 1) 1) 
          ((=number? exp 1) base) 
          ((=number? exp 0) 1) 
          (else (list '** base exp))))  
  (define (=number? exp num) (and (number? exp) (= exp num)))
  (define ops (make-hash))
  (define (opkey op type) (format "~s|~s" op type))
  (define (put op type item) (hash-set! ops (opkey op type) item))    
  (define (get op type) (hash-ref ops (opkey op type)))
  (define (deriv exp var)
    (cond ((number? exp) 0)
          ((variable? exp) (if (same-variable? exp var) 1 0))
          (else ((get 'deriv (operator exp))
                 (operands exp) var))))
  (define (operator exp) (car exp))
  (define (operands exp) (cdr exp))
  
  )
;-- <BOOK> --

; Ex. 2.73 (a-d)

; a) number? is a primitive, exp is not a tagged type

; b) 
(require (prefix-in c: 'sicp-calculus))  

(define (deriv-sum terms var)
  (c:make-sum (c:deriv (c:addend terms) var)
              (c:deriv (c:augend terms) var)))

(define (deriv-product terms var)
  (c:make-sum (c:make-product
               (c:multiplier terms)
               (c:deriv (c:multiplicand terms) var))
              (c:make-product
               (c:deriv (c:multiplier terms) var)
               (c:multiplicand terms))))

; c)
(define (deriv-exponentiation terms var)
  (c:make-product  
   (c:make-product
    (c:exponent terms)
    (c:make-exponentiation (c:base terms)
                           (c:make-sum (c:exponent terms) -1)))
   (c:deriv (c:base terms) var)))

(define (install-calculus-package)
  (c:put 'deriv '+ deriv-sum)
  (c:put 'deriv '* deriv-product)
  (c:put 'deriv '** deriv-exponentiation)
  )

; d) only hash table access
(module+ test
  (begin
    (install-calculus-package)
    (check-equal? (c:deriv '(** x 3) 'x) '(* 3 (** x 2)))
    ))


; Ex. 2.74 (a-d)

; specific personnel files
(define acme-records
  (list
   '("Deanne" "Charter" "dcharter0@photobucket.com" "22 Huxley Way" 2881)
   '("Vernon" "Riddal" "vriddall1@barnesandnoble.com" "5480 Sullivan Terrace" 4197)
   ))


(define globex-records
  (list
   '(("Gusta" "Blaymires")	("2309 Calypso Court"	1081))
   '(("Orlando" "Ledster")  ("74141 Declaration Alley" 3559))
  ))

; acme specific api
(define (acme-get-name record) (format "~a ~a" (cadr record) (car record)))
(define (acme-get-salary record) (car (cddddr record)))
(define (acme-get-record company-records fullname)
  (let ((entry (filter (lambda (record)
                         (equal? fullname (acme-get-name record)))
                       company-records)))
   (if (not (null? entry))
        (car entry)
        null)))

; globex specific api
(define (globex-get-name record) (format "~a ~a" (cadar record) (caar record)))
(define (globex-get-salary record) (cadadr record))
(define (globex-get-record company-records fullname)
  (let ((entry (filter (lambda (record)
                         (equal? fullname (globex-get-name record)))
                       company-records)))
    (if (not (null? entry))
        (car entry)
        null)))

; merge acme and globex into insatiable
(define acme-database (cons 'acme acme-records))
(define globex-database (cons 'globex globex-records))


(define company-ops (make-hash))
(define (company-key op company) (format "~s|~s" op company))
(define (company-put op company item) (hash-set! company-ops (company-key op company) item))    
(define (company-get op company) (hash-ref company-ops (company-key op company)))

(define (merge-insatiable)
  (company-put 'get-record 'acme acme-get-record)
  (company-put 'get-record 'globex globex-get-record)
  (company-put 'get-salary 'acme   acme-get-salary)
  (company-put 'get-salary 'globex globex-get-salary)
  (company-put 'get-name 'acme   acme-get-name)
  (company-put 'get-name 'globex globex-get-name)    
  )

; A generic record is a pair (specific-record, database) i.e. the record carries the database handle with it.
(define (get-record db fullname)
  (let ((record ((company-get 'get-record (car db))
                 (cdr db)
                 fullname)))
    (if (not (null? record)) (cons record db)
        null)))

(define (get-field-value fieldname record default)
  (if (not (null? record))
      (let* ((specific-record (car record))
             (db (cdr record)))
        ((company-get fieldname (car db)) specific-record))
      default))

(define (get-salary record)
  (get-field-value 'get-salary record 0))

(define (get-name record)
  (get-field-value 'get-name record '()))

(define (find-employee-record fullname databases)
  (let ((records(filter (lambda (record) (not (null? record)))
                        (map (lambda (db)
                               (let ((record (get-record db fullname)))
                                 record))              
                             databases))))
    (if (not (empty? records))
        (car records)
        null)))
        
(module+ test
  (begin
    ; test specific api
    (check-equal? (acme-get-salary (acme-get-record acme-records "Charter Deanne")) 2881)
    (check-equal? (globex-get-salary (globex-get-record globex-records "Ledster Orlando")) 3559)
    ; merge companies
    (merge-insatiable)
    ; test get-record
    (check-equal? (car (get-record acme-database "Charter Deanne")) '("Deanne" "Charter" "dcharter0@photobucket.com" "22 Huxley Way" 2881))
    (check-equal? (car (get-record globex-database "Ledster Orlando")) '(("Orlando" "Ledster") ("74141 Declaration Alley" 3559)))
    (check-equal? (get-record acme-database "Ledster Orlando") '())
    ; test get-salary
    (check-equal? (get-salary (get-record acme-database "Charter Deanne")) 2881)
    ; test 
    (check-equal? (get-salary (find-employee-record "Ledster Orlando" (list acme-database globex-database))) 3559)
    ))


; Ex. 2.75

(define (make-from-mag-ang mag ang)
  (define (dispatch op)
    (cond ((eq? op 'real-part) (* mag (cos ang)))
          ((eq? op 'imag-part) (* mag (sin ang)))
          ((eq? op 'magnitude) mag)
          ((eq? op 'angle) ang)
          (else (error "Unknown op: MAKE-FROM-MAG-ANG" op))))
    dispatch)

(define (square x) (* x x))

(define (make-from-real-imag x y)
  (define (dispatch op)
    (cond ((eq? op 'real-part) x)
          ((eq? op 'imag-part) y)
          ((eq? op 'magnitude) (sqrt (+ (square x) (square y))))
          ((eq? op 'angle) (atan y x))
          (else (error "Unknown op: MAKE-FROM-REAL-IMAG" op))))
  dispatch)

(define (apply-generic op arg) (arg op))

(define c0 (make-from-mag-ang 1 (/ 3.14 3)))
(define c1 (make-from-real-imag (apply-generic 'real-part c0)
                                (apply-generic 'imag-part c0)))

(module+ test
  (begin
    (check-within (apply-generic 'real-part c0)
                  (apply-generic 'real-part c1)
                  0.01)))
