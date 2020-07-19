#lang racket
(require rackunit)
(require racket/list)


; Ex. 2.77, 2.78 and 2.79 

(define table (make-hash))
(define (put key1 key2 value) (hash-set! table (list key1 key2) value))
(define (get key1 key2)       (hash-ref  table (list key1 key2) #f))
(define (attach-tag tag value) (cons tag value))

(define (type-tag datum)
  (cond ((pair? datum) (car datum))
        ((number? datum) 'scheme-number)
        (error "Bad tagged datum: TYPE-TAG" datum)))

(define (contents datum)
  (cond ((pair? datum) (cdr datum))
        ((number? datum) datum)
        (error "Bad tagged datum: TYPE-TAG" datum)))

(define square *)

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args))
        (values (map contents args))
        )
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          ; can we convert ?
          (if (member op (list 'add 'sub 'mul 'div 'equ?))
              ; try to find a common type (assume only binary ops ...)          
              (let ((root-type (common-supertype type-tags)))
                (if (not (null? root-type))
                    ; convert all values to supertype
                    (let ((values (map (lambda (arg)
                                         (convert-from-to-value (type-tag arg) root-type
                                                                (contents arg)))
                                       args)))
                      ; apply operation to
                      (apply (get op
                                  (build-list (length type-tags) (lambda (x) root-type)))
                             values))
                    (error "No conversion: APPLY-GENERIC" (list op type-tags)))
                )
              (error "No method for these types: APPLY-GENERIC" (list op type-tags))
              )))))

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (equ? x y) (apply-generic 'equ? x y))

; -- scheme-number --
(define (install-scheme-number-package)
  (define (tag x) (attach-tag 'scheme-number x))
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put 'make 'scheme-number (lambda (x) (tag x)))
  (put 'equ? '(scheme-number scheme-number)
       (lambda (x y) (= x y)))
  'done)

(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

; -- rational --
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
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))

  (put 'equ? '(rational rational)
       (lambda (x y) (and (= (* (numer x) (denom y))
                             (* (numer y) (denom x))))))

  (put 'numer '(rational) (lambda (q) (numer q)))
  (put 'denom '(rational) (lambda (q) (denom q)))
  
  'done)

(define (numer q) (apply-generic 'numer q))
(define (denom q) (apply-generic 'denom q))


; --complex --
(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) (tag (make-from-mag-ang r a))))
  (put 'equ? '(rectangular rectangular)
       (lambda (z1 z2) (and (= (real-part z1) (real-part z2))
                            (= (imag-part z1) (imag-part z2)))))
  
  'done)

(define (install-polar-package)
  ;; internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z) (* (magnitude z) (cos (angle z))))
  (define (imag-part z) (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) (tag (make-from-mag-ang r a))))
  (put 'equ? '(polar polar)
       (lambda (z1 z2) (and (= (magnitude z1) (magnitude z2))
                            (= (angle z1) (angle z2)))))
  
  'done)

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))
(define (complex-equ? z1 z2) (apply-generic 'equ? z1 z2))

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

  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)

  (put 'equ? '(complex complex)
       (lambda (z1 z2) (complex-equ? z1 z2)))
  'done)

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))


(define (make-rational n d)
  ((get 'make 'rational) n d))

(define (install-all)
  (install-scheme-number-package)
  (install-rational-package)
  (install-rectangular-package)
  (install-polar-package)
  (install-complex-package)
  )

(module+ test
  (begin
    (install-all)
    (check-equal? (add 1 2) (make-scheme-number 3))
    (check-equal? (equ? 1 1) #t)
    (check-equal? (equ?  (make-rational 1 2) (make-rational 6 12)) #t)
    (check-equal? (equ?  (make-complex-from-real-imag 1 2) (make-complex-from-real-imag 1 2)) #t)
    ))


; Ex. 2.83 and 2.84
(define (raise-scheme-number-to-rational n)
  (make-rational (contents n) 1))

(define (raise-rational-to-complex n)
  (make-complex-from-real-imag (/ (numer n) (denom n))
                               0))

; supertypes is a list of pairs (supertype . raiser)
(define (raise from supertypes)
  (put 'raise from supertypes))

(define (install-raise)
  (raise 'scheme-number
         (list (cons 'rational raise-scheme-number-to-rational)))
  
  (raise 'rational
         (list (cons 'complex raise-rational-to-complex)))
  )


(define (install-geometry-raise)
  (define (dummy a) (println (format "[~a]" a)) a)

  (raise 'triangle (list (cons 'polygon dummy)))
  (raise 'isosceles-triangle (list (cons 'triangle dummy)))
  (raise 'quadrilateral-triangle (list (cons 'isosceles-triangle dummy)))
  (raise 'isosceles-right-triangle (list (cons 'isosceles-triangle dummy)
                                         (cons 'right-triangle dummy)))
  (raise 'right-triangle (list (cons 'triangle dummy)))  
  (raise 'quadrilateral (list (cons 'polygon dummy)))
  (raise 'trapezoid (list (cons 'quadrilateral dummy)))
  (raise 'parallelogram (list (cons 'trapezoid dummy)))
  (raise 'rectangle (list (cons 'parallelogram dummy)))
  (raise 'square (list (cons 'rhombus dummy)
                       (cons 'rectangle dummy)))
  
  (raise 'rhombus (list (cons 'parallelogram dummy)
                        (cons 'kite dummy)))  
  (raise 'kite (list (cons 'quadrilateral dummy)))
  )

(define (id x) x)

(define (zip-with fn a b)
  (cond ((null? a) '())
        (else (cons (fn (car a) (car b)) (zip-with fn (cdr a) (cdr b))))))

(define (flatmap fn a)
  (foldl (lambda (elem init) (append init elem)) '() (map fn a)))

; return a list of paths from type A to the root type
(define (supertype-chains A)
  (define (try-extend-chains chains)
    (define uptypes (map (lambda (path)
                           (let ((uptype (get 'raise (car (last path)))))
                             (if uptype uptype '())))
                         chains))
    (define extended-chains (zip-with (lambda (path uptypes)
                                        
                                        (if (not (null? uptypes))
                                            (map (lambda (uptype) (append path (list uptype)))
                                                 uptypes)
                                            (list path)))
                                      
                                      chains
                                      uptypes))
    
    (define next-chains (flatmap id extended-chains))

    (if (null? (foldl append '() uptypes))
        chains
        (try-extend-chains next-chains)))
  
  (try-extend-chains (list (list (cons A id)))))

(define (is-subtype a b)
  (not (null? (flatmap (lambda (path)
                         (if (member b (map car path))
                             (list b)
                             '()))
                       (supertype-chains a)))))

(define (common-supertype  types)
  (define (common-type A B)
    (define pa (supertype-chains A))
    (define pb (supertype-chains B))
    (define (common-chain-node a b)
      (cond ((or (null? a) (null? b)) '())
            ((eq? (caar a) (caar b)) (list (caar a)))
            (else (let ((xa (common-chain-node (cdr a) b))
                        (xb (common-chain-node a (cdr b))))
                    (append xa xb)))))
    (define types (sort (remove-duplicates (flatmap (lambda (a)
                                                      (flatmap (lambda (b)
                                                                 (common-chain-node a b)) pb)) pa))
                        is-subtype))
    (if (not (null? types))
        (car types)
        '()))
  (foldl common-type (car types) (cdr types)))

(define (convert-from-to-value from to value)
  (define (convert-chain path value)
    (define next-value ((cdar path) value))
    (if (eq? (caar path) to)
        next-value
        (convert-chain (cdr path) next-value)))
    
  (car (flatmap (lambda (path)
                  (if (member to (map car path))
                      (list (convert-chain path value))
                      '())) (supertype-chains from))))
        

(module+ test
  (begin
    (install-all)
    (install-raise)
    (check-equal? (equ? (raise-scheme-number-to-rational 3) (make-rational 6 2)) #t)
    (check-equal? (equ? (convert-from-to-value 'scheme-number 'complex 2)
                        (make-complex-from-real-imag 2 0))
                  #t
                  )
    (check-equal? (equ? (add (make-complex-from-real-imag 2 3) (make-scheme-number 4))
                        (make-complex-from-real-imag 6 3))
                  #t)
    ))
  

(module+ test
  (begin
    (install-geometry-raise)
    (check-equal? (common-supertype (list 'rectangle 'kite 'trapezoid)) 'quadrilateral)
    ))

