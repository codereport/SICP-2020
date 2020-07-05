#lang racket

(require rackunit)
(require racket/trace)

; Ex 2.54

(define (equal? a b)
  (cond ((null? a) (null? b))
        ((null? b) (null? a))
        ; can I use (eq? a b) ? - yes for numbers, symbols and strings
        ((foldr (lambda (f z) (or z (and (f a) (f b)))) #f (list symbol? number? string?)) (eq? a b))
        ((and (list? a) (list? b)) (and (equal? (car a) (car b))
                                        (equal? (cdr a) (cdr b))))
        (else #f)))

(module+ test
  (begin
    (check-equal? (equal? '(this is a list) '(this is a list)) #t)
    (check-equal? (equal? '(this is a list) '(this (is a) list)) #f)
    (check-equal? (equal? '(1 2 3) '(1 2 3)) #t)
    (check-equal? (equal? '() '()) #t)
    (check-equal? (equal? '() '(a)) #f)
    (check-equal? (equal? '("hello" "world") '("hello" "world")) #t)
    ))

; Ex. 2.56
; Ex. 2.57 - make-sum/product with multiple operands
;-------------------------------------------------------------- 
(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))
(define (=number? n value) (and (number? n) (eq? n value)))
(define (sum? x) (and (pair? x) (eq? (car x) '+)))
(define (product? x) (and (pair? x) (eq? (car x) '*)))
;--------------------------------------------------------------


(define (make-binop op els)
  ;  (make-binop '* '(2 3 4 5 6)) -> '(* 6 (* 5 (* 4 (* 3 2))))
  (cond ((null? (cdr els)) (car els))
        (else (foldl (lambda (n z) (list op n z)) (car els) (cdr els)))))

(define (make-sum . els)
  (let* ((nums (filter number? els))
         (exprs (filter-not number? els))
         (z (foldr + 0 nums)))
    (cond ((= z 0) (if (null? exprs) 0 (make-binop '+ exprs)))
          (else (make-binop '+ (cons z exprs))))))
          
(define (make-product . els)
  (let* ((nums (filter number? els))
         (exprs (filter-not number? els))
         (z (foldr * 1 nums)))
    (cond ((= z 0) 0)
          ((= z 1) (if (null? exprs) 1 (make-binop '* exprs)))
          (else (make-binop '* (cons z exprs))))))

(define (addend s) (cadr s))
(define (augend s)
  ; (augend '(+ x y)) -> y
  ; (augend '(+ x y z t))-> '(+ y z t)
  (if (= 3 (length s))
      (caddr s)
      (cons '+ (cddr s))))
                       
(define (multiplier p) (cadr p))
(define (multiplicand s)
  (if (= 3 (length s))
      (caddr s)
      (cons '* (cddr s))))


(define (make-exponentiation base exponent)
  (cond ((=number? exponent 0) 1)
        ((=number? exponent 1) base)
        (else (list '** base exponent))))

(define (exponentiation? x) (and (pair? x) (eq? (car x) '**)))
(define (base e) (cadr e))
(define (exponent e) (caddr e))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        ((sum? exp) (make-sum (deriv (addend exp) var)
                              (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        ((exponentiation? exp)
         (make-product (exponent exp)
                       (make-exponentiation (base exp)
                                            (make-sum (exponent exp) -1))
                       (deriv (base exp) var)))
                                            
        (else
         (error "unknown expression type: DERIV" exp))))

(module+ test
  (begin
    (check-equal? (deriv '(** x 3) 'x) '(* (** x 2) 3))
    ))
                  

; Ex. 2.59
;-------------------------------------------------------------- 
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))
;--------------------------------------------------------------

(define (union-set A B)
  (cond ((null? A) B)
        ((null? B) A)
        ((element-of-set? (car A) B) (union-set (cdr A) B))
        (else (cons (car A)
                    (union-set (cdr A) B)))))

(module+ test
  (begin
    (check-equal? (sort (union-set '(1 2 3 4 5 6 8) '(3 4 5 6)) <) '(1 2 3 4 5 6 8))
    (check-equal? (sort (union-set '(1 2 3 4 5 6 8) '()) <) '(1 2 3 4 5 6 8))
    (check-equal? (sort (union-set '() '()) <) '())
    ))

; Ex. 2.62

(define (merge-sorted-lists A B)
  ; always eliminate the minimum
  (cond ((null? A) B)
        ((null? B) A)
        (else (let ((x1 (car A))
                    (x2 (car B)))
                (cond ((= x1 x2) (cons x1 (merge-sorted-lists (cdr A) (cdr B))))
                      ((< x1 x2) (cons x1 (merge-sorted-lists (cdr A) B)))
                      (else (cons x2 (merge-sorted-lists A (cdr B)))))))))

(define union-set-ol merge-sorted-lists)

(module+ test
  (begin
    (check-equal? (union-set-ol '(1 2 3 4 5 6 8) '(3 4 5 6)) '(1 2 3 4 5 6 8))
    (check-equal? (union-set-ol '(1 2 3 4 5 6 8) '()) '(1 2 3 4 5 6 8))
    ))


; Ex 2.63

; TL1(T) = APPEND (TL1(L(T)),  CONS(X(T), TL1(R(T))))
; CTL(T, e) = CTL(L(T), CONS(X(T), CTL(R(T), e)))

; a) Yes Left, Root, Right
; b) Same # of steps - at each step both functions will access a root node (CONS)


; Ex. 2.65
;--------------------------------------------------------------

(module treeset racket
  (provide (all-defined-out))
  
  (define (entry tree) (car tree))
  (define (left-branch tree) (cadr tree))
  (define (right-branch tree) (caddr tree))
  (define (make-tree entry left right)
    (list entry left right))
  
  (define (list->tree elements)
    (car (partial-tree elements (length elements))))
  (define (partial-tree elts n)
    (if (= n 0)
        (cons '() elts)
        (let ((left-size (quotient (- n 1) 2)))
          (let ((left-result
                 (partial-tree elts left-size)))
            (let ((left-tree (car left-result))
                  (non-left-elts (cdr left-result))
                  (right-size (- n (+ left-size 1))))
              (let ((this-entry (car non-left-elts))
                    (right-result
                     (partial-tree
                      (cdr non-left-elts)
                      right-size)))
                (let ((right-tree (car right-result))
                      (remaining-elts
                       (cdr right-result)))
                  (cons (make-tree this-entry
                                   left-tree
                                   right-tree)
                        remaining-elts))))))))

  (define (tree->list-1 tree)
    (if (null? tree)
        '()
        (append (tree->list-1 (left-branch tree))
                (cons (entry tree)
                      (tree->list-1
                       (right-branch tree))))))
  )
;-------------------------------------------------------------- 
(require (prefix-in ts: 'treeset))
(define (union-set-tree A B)
  (ts:list->tree (merge-sorted-lists (ts:tree->list-1 A)
                                       (ts:tree->list-1 B))))



(module+ test
  (begin
    (check-equal? (ts:tree->list-1 (union-set-tree (ts:list->tree '(1 2 3 4 5 6 8))
                                                   (ts:list->tree '(3 4 5 6))))
                  '(1 2 3 4 5 6 8))
    ))

; Ex. 2.67
;--------------------------------------------------------------
(module huffman racket
  (provide (all-defined-out))
  (define (make-leaf symbol weight) (list 'leaf symbol weight))
  (define (leaf? object) (eq? (car object) 'leaf))
  (define (symbol-leaf x) (cadr x))
  (define (weight-leaf x) (caddr x))
  (define (make-code-tree left right)
    (list left
          right
          (append (symbols left) (symbols right))
          (+ (weight left) (weight right))))
  (define (left-branch tree) (car tree))
  (define (right-branch tree) (cadr tree))
  (define (symbols tree)
    (if (leaf? tree)
        (list (symbol-leaf tree))
        (caddr tree)))
  (define (weight tree)
    (if (leaf? tree)
        (weight-leaf tree)
        (cadddr tree)))

  (define (decode bits tree)
    (define (decode-1 bits current-branch)
      (if (null? bits)
          '()
          (let ((next-branch
                 (choose-branch (car bits) current-branch)))
            (if (leaf? next-branch)
                (cons (symbol-leaf next-branch)
                      (decode-1 (cdr bits) tree))
                (decode-1 (cdr bits) next-branch)))))
    (decode-1 bits tree))

  (define (choose-branch bit branch)
    (cond ((= bit 0) (left-branch branch))
          ((= bit 1) (right-branch branch))
          (else (error "bad bit: CHOOSE-BRANCH" bit))))

  )
;--------------------------------------------------------------
(require (prefix-in h: 'huffman))
(define sample-tree
  (h:make-code-tree (h:make-leaf 'A 4)
                    (h:make-code-tree
                     (h:make-leaf 'B 2)
                     (h:make-code-tree
                      (h:make-leaf 'D 1)
                      (h:make-leaf 'C 1)))))
(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(module+ test
  (begin
    (check-equal? (h:decode sample-message sample-tree) '(A D A B B C A))
    ))


; Ex: 2.68
(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (encode-symbol sym tree)
  (cond ((h:leaf? tree) (if (eq? (h:symbol-leaf tree) sym) '() (error "Unknown Symbol")))
        ((memq sym (h:symbols (h:left-branch tree))) (cons 0 (encode-symbol sym (h:left-branch tree))))
        ((memq sym (h:symbols (h:right-branch tree))) (cons 1 (encode-symbol sym (h:right-branch tree))))
        (else (error "Unknown Symbol"))
        ))

(module+ test
  (begin
    (check-equal? (encode '(A D A B B C A) sample-tree) '(0 1 1 0 0 1 0 1 0 1 1 1 0))
    ))
