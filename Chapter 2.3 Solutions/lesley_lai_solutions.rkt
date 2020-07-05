#lang racket

(require rackunit
         threading)

;; 2.54
(define (my-equal? a b)
  (match* (a b)
    [('() '()) #t]
    [((cons ah at) (cons bh bt))
     (if (eq? ah bh)
         (my-equal? at bt)
         #f)]
    [(_ _) #f]))

;;;; 2.54 tests
(module+ test
  (check-true (my-equal? '() '()))
  (check-false (my-equal? '(this is a list) '()))
  (check-false (my-equal? '() '(this is a list)))
  (check-true (my-equal? '(this is a list) '(this is a list)))
  (check-false (my-equal? '(this is a list) '(this (is a) list))))

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


;; 2.56
(define (make-exponential b e)
  (match* (b e)
    [(0 _) 0]
    [(1 _) 1]
    [(_ 0) 1]
    [(_ 1) b]
    [((? number?) (? number?)) (expt b e)]
    [(_ _) (list '** b e)]))

(define (deriv expr var)
  (match expr
    [(? number?) 0]
    [(? variable?) (if (same-variable? expr var) 1 0)]
    [(list '+ a1 a2) (make-sum (deriv a1 var) (deriv a2 var))]
    [(list '* a1 a2) (make-sum
                      (make-product (deriv a1 var) a2)
                      (make-product a1 (deriv a2 var)))]
    [(list '** b (? number? e)) (make-product
                                 e
                                 (make-exponential b (make-sum e (- 1))))]))

;;;; 2.56 tests
(module+ test
  (check-equal? (make-exponential 0 'x) 0)
  (check-equal? (make-exponential 1 'x) 1)
  (check-equal? (make-exponential 'x 0) 1)
  (check-equal? (make-exponential 'x 1) 'x)
  (check-equal? (make-exponential 2 3) 8)
  (check-equal? (make-exponential 'x 'y) '(** x y))

  (check-equal? (deriv 5 'x) 0)
  (check-equal? (deriv 'x 'x) 1)
  (check-equal? (deriv 'y 'x) 0)
  (check-equal? (deriv '(+ x 3) 'x) 1)
  (check-equal? (deriv '(* x y) 'x) 'y)
  (check-equal? (deriv '(** x 1) 'x) 1)
  (check-equal? (deriv '(** x 2) 'x) '(* 2 x))
  (check-equal? (deriv '(** x 5) 'x) '(* 5 (** x 4))))

;; 2.59

(define (element-of-set-1? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set-1? x (cdr set)))))

(define (adjoin-set-1 x set)
  (if (element-of-set-1? x set)
      set
      (cons x set)))

(define (union-set-1 set1 set2)
  (foldl
   (lambda (elem acc)
     (if (element-of-set-1? elem set2) acc (cons elem acc)))
   set2 set1))

 ;;;; 2.59 tests
(module+ test
  (let* [(set1 (adjoin-set-1 1 (adjoin-set-1 2 (adjoin-set-1 3 (adjoin-set-1 4 (adjoin-set-1 5 '()))))))
        (set2 (adjoin-set-1 42 '()))
        (set3 (union-set-1 set1 set2))]
  (check-true (element-of-set-1? 1 set3))
  (check-true (element-of-set-1? 2 set3))
  (check-true (element-of-set-1? 5 set3))
  (check-true (element-of-set-1? 42 set3))
  ))

(define (element-of-set-2? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-set-2? x (cdr set)))))

  ;; 2.62
(define (union-set-2 set1 set2)
  (match* (set1 set2)
    [('() _) set2]
    [(_ '()) set1]
    [((cons hd1 tl1) (cons hd2 tl2))
     (cond ((= hd1 hd2) (cons hd1 (union-set-2 tl1 tl2)))
           ((< hd1 hd2) (cons hd1 (union-set-2 tl1 set2)))
           (else (cons hd2 (union-set-2 set1 tl2))))]))

(module+ test
  (let [(set1 '(1 2 4 5))
        (set2 '(3 42))]
  (check-equal? (union-set-2 set1 '()) '(1 2 4 5))
  (check-equal? (union-set-2 '() set2) '(3 42))
  (check-equal? (union-set-2 set1 set2) '(1 2 3 4 5 42))))

;; 2.63

;;;; a. Same result
;;;; b. The first procedural uses `append` at each leaf, so it is an
;;;; O(n log(n)) algorithm. The first one performs one cons for each
;;;; leaf, so it is O(n).


(define (entry tree) (car tree))
(define (left-branch-1 tree) (cadr tree))
(define (right-branch-1 tree) (caddr tree))
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

(define (tree->list tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch-1 tree)
                      (cons (entry tree)
                            (copy-to-list
                             (right-branch-1 tree)
                             result-list)))))
  (copy-to-list tree '()))

;; 2.65 (only union-set)
(define (union-set-bbt set1 set2)
  (list->tree (union-set-2 (tree->list set1) (tree->list set2))))

;;;; 2.65 tests
(module+ test
  (check-equal?
   (tree->list
    (union-set-bbt (list->tree '(1 2 3)) (list->tree '(4 5))))
   '(1 2 3 4 5)
  ))


;; 2.67
;;;; Functions from book
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

(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree
                    (make-leaf 'D 1)
                    (make-leaf 'C 1)))))
(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))
(define sample-decoded '(A D A B B C A))

;;;; 2.67 test
(module+ test
  (check-equal?
   (decode sample-message sample-tree)
   sample-decoded))

;; 2.68
(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (encode-symbol symbol tree)
  (define (helper subtree acc) (match tree
     [(list 'leaf s _)
      (if (= s symbol) acc (error "Unrecognized symbol"))]
     [(list l r _ _)
      (if (memq symbol (symbols l))
          (helper l (cons 0 acc))
          (helper r (cons 1 acc)))]))
  (reverse (helper tree '()))
  )

(module+ test
    (check-equal?
     (encode sample-decoded sample-tree) sample-message))
