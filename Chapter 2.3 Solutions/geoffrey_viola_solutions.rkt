#lang racket
(require rackunit)
(require point-free)

;; Exercise 2.54.
(define (rec-equal? item x)
  (cond ((null? x) false)
        ((list? (car x))
         (or (rec-equal? (car item) (car x)) (rec-equal? (car item) (cdr x))))
        ((eq? (car item) (car x)) #t)
        (else (rec-equal? (cdr item) (cdr x)))))

(check-equal? (rec-equal? '(this is a list) '(this is a list)) #t)
(check-equal? (rec-equal? '(this is a list) '(this (is a) list)) #t)

;; 2.3.2  Example: Symbolic Differentiation
;; Exercise 2.56.
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        ;; start my code
        ((exponentiation? exp)
         (make-product
          (make-product (exponent exp)
                        (make-exponentiation (base exp)
                                             (make-sum (exponent exp) -1)))
          (deriv (base exp) var)))
        ;; end my code
        (else
         (error "unknown expression type -- DERIV" exp))))

(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))
(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))
(define (addend s) (cadr s))
(define (augend s) (caddr s))
(define (product? x)
  (and (pair? x) (eq? (car x) '*)))
(define (multiplier p) (cadr p))
(define (multiplicand p) (caddr p))
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))
(define (=number? exp num)
  (and (number? exp) (= exp num)))
(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

;; start my code
(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**)))
(define (base s) (cadr s))
(define (exponent s) (caddr s))
(define (make-exponentiation base exponent)
  (cond ((=number? base 1) 1) 
        ((=number? exponent 1) base) 
        ((=number? exponent 0) 1)
        (else  
         (list '** base exponent))))

(check-equal? (deriv '(** x 0) 'x) 0)
(check-equal? (deriv '(** x 1) 'x) 1)
(check-equal? (deriv '(** x 2) 'x) '(* 2 x))
; Should be 2^(2x+1) * x^(2x) * (log(x) + 1 + log(2))
; but good enough for this exercise
(check-equal? (deriv '(** (* 2 x) (* 2 x)) 'x)
              '(* (* (* 2 x) (** (* 2 x) (+ (* 2 x) -1))) 2))

;; Exercise 2.59.
(define (union-set-unordered-no-hash set1 set2)
  (~> set1
      (curry append set2)
      remove-duplicates))

(check-equal? (union-set-unordered-no-hash '() '()) '())
(check-equal? (union-set-unordered-no-hash '(1) '()) '(1))
(check-equal? (union-set-unordered-no-hash '() '(1)) '(1))
(check-equal? (union-set-unordered-no-hash '(1 2 3 5) '(1 2 3 4)) '(1 2 3 4 5))

;; Exercise 2.62.
(define (union-set-ordered-list set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else (let ((x1 (car set1))
                    (x2 (car set2)))
                (cond ((= x1 x2)
                       (cons x1 (union-set-ordered-list
                                 (cdr set1) (cdr set2))))
                      ((< x1 x2)
                       (cons x1 (union-set-ordered-list
                                 (cdr set1) set2)))
                      ((< x2 x1)
                       (cons x2 (union-set-ordered-list
                                 set1 (cdr set2)))))))))

(check-equal? (union-set-ordered-list '() '()) '())
(check-equal? (union-set-ordered-list '(1) '()) '(1))
(check-equal? (union-set-ordered-list '() '(1)) '(1))
(check-equal? (union-set-ordered-list '(1 2 3 5) '(1 2 3 4)) '(1 2 3 4 5))

;; Exercise 2.63.
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))
(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1 (right-branch tree))))))
(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))

;; Exercise 2.63.a.
(define fig2-16-1 '(7 (3 (1 () ()) (5 () ())) (9 () (11 () ())))) 
(define fig2-16-2 '(3 (1 () ()) (7 (5 () ()) (9 () (11 () ()))))) 
(define fig2-16-3 '(5 (3 (1 () ()) ()) (9 (7 () ()) (11 () ())))) 
(check-equal? (tree->list-1 fig2-16-1) (tree->list-2 fig2-16-1))
(check-equal? (tree->list-1 fig2-16-2) (tree->list-2 fig2-16-2))
(check-equal? (tree->list-1 fig2-16-3) (tree->list-2 fig2-16-3))
;; Both algorithms return the same result. It's an in-order traversal.

;; Exercise 2.63.b.
;; Assuming the tree is balanced, the Big O of the first one is O(nlog(n))
;; The issue with the first one is that the append is O(n)
;; The second one would be O(log(n)), because it only calls cons

;; References
;; According to this user, append is O(n)
;; https://stackoverflow.com/a/45454767
;; The wl user, still questions whether the second one is O(nlog(n))
;; from copying
;; http://community.schemewiki.org/?sicp-ex-2.63

;; Exercise 2.65.
(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts)
                                              right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-elts))))))))

(define (union-set-ordered-tree set1 set2)
  (~> set1
      tree->list-2
      (curry append (tree->list-2 set2))
      remove-duplicates
      list->tree))

(check-equal? (tree->list-2 (union-set-ordered-tree fig2-16-1 fig2-16-1))
              (tree->list-2 fig2-16-1))
(check-equal? (tree->list-2 (union-set-ordered-tree fig2-16-1 '(2 () ())))
              '(2 1 3 5 7 9 11))

;; Exercise 2.66.
(define (lookup x set)
  (cond ((null? set) '())
        ((= x (entry set)) (entry set))
        ((< x (entry set))
         (lookup x (left-branch set)))
        ((> x (entry set))
         (lookup x (right-branch set)))))

(check-equal? (lookup 1 fig2-16-1) 1)
(check-equal? (lookup 2 fig2-16-1) '())

;; Exercise 2.67.
(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define (leaf? object)
  (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))
(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))
(define (left-branch-htree tree) (car tree))
(define (right-branch-htree tree) (cadr tree))
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
  (cond ((= bit 0) (left-branch-htree branch))
        ((= bit 1) (right-branch-htree branch))
        (else (error "bad bit -- CHOOSE-BRANCH" bit))))
(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree (make-leaf 'D 1)
                                   (make-leaf 'C 1)))))
(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(check-equal? (decode sample-message sample-tree) '(A D A B B C A))

;; Exercise 2.68.
(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

; http://community.schemewiki.org/?sicp-ex-2.68
(define (encode-symbol sym tree) 
  (if (leaf? tree) 
      (if (eq? sym (symbol-leaf tree)) 
          '() 
          (error "missing symbol: ENCODE-SYMBOL" sym)) 
      (let ((left (left-branch-htree tree))) 
        (if (member sym (symbols left)) 
            (cons 0 (encode-symbol sym left)) 
            (cons 1 (encode-symbol sym (right-branch-htree tree))))))) 

(check-equal? (encode '(A D A B B C A) sample-tree) sample-message)
