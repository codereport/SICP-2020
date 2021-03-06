;; Exercise 2.54 (page 196)

(require rackunit)

(define (equal? a b)
  (cond ((and (null? a) (null? b)) #t)
        ((or (null? a) (null? b)) #f)
        ((eq? (car a) (car b)) (equal? (cdr a) (cdr b)))
        (else #f)))
     
(check-equal? (equal? '(0 1 2) (range 3)) #t)
(check-equal? (equal? '(0 1 2) (range 2)) #f)

;; Symbolic Differentiation (from the book)

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
(define (sum? x) (and (pair? x) (eq? (car x) '+)))
(define (addend s) (cadr s))
(define (augend s) (caddr s))
(define (product? x) (and (pair? x) (eq? (car x) '*)))
(define (multiplier p) (cadr p))
(define (multiplicand p) (caddr p))

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
        (else
         (error "unknown expression type: DERIV" exp))))

;; Exercise 2.56 (page 203)

(define (make-exponentiation base exp) 
  (cond ((=number? base 1) 1) 
        ((=number? exp 1) base) 
        ((=number? exp 0) 1) 
        (else (list '^ base exp))))

(define base cadr)
(define exponent caddr)

(define (exponentiation? exp) 
  (and (list? exp) (eq? (car exp) '^))) 

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
         (make-product  
          (make-product
           (exponent exp)
           (make-exponentiation (base exp)
                                (make-sum (exponent exp) -1)))
          (deriv (base exp) var)))
        (else
         (error "unknown expression type: DERIV" exp))))

(check-equal? (deriv '(^ x 3) 'x) '(* 3 (^ x 2)))

;; Exercise 2.59 (page 207)

(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((equal? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))

(check-equal? (element-of-set? 1 '(1 2 3)) #t)
(check-equal? (element-of-set? 4 '(1 2 3)) #f)

(define (union-set a b)
  (cond ((null? b) a)
        ((element-of-set? (car b) a) (union-set a (cdr b)))
        (else (union-set (cons (car b) a) (cdr b)))))

(check-equal? (union-set '(1 2 3) '(4 5 6)) '(6 5 4 1 2 3))
(check-equal? (union-set '(1 2 3) '(2 3 4)) '(4 1 2 3 ))

;; Exercise 2.62 (page 210)

(define (union-set a b)
  (define (iter a b c)
    (cond ((null? a) (append (reverse b) c))
          ((null? b) (append (reverse a) c))
          ((= (car a) (car b)) (iter (cdr a) (cdr b) (cons (car a) c)))
          ((< (car a) (car b)) (iter (cdr a)    b    (cons (car a) c)))
          ((> (car a) (car b)) (iter    a    (cdr b) (cons (car b) c)))))          
  (reverse (iter a b '())))   
  
(check-equal? (union-set '(1 2 3) '(4 5 6)) '(1 2 3 4 5 6))
(check-equal? (union-set '(4 5 6) '(1 2 3)) '(1 2 3 4 5 6))
(check-equal? (union-set '(1 2 3) '(2 3 4)) '(1 2 3 4))
(check-equal? (union-set '(1 2 3) '(3 4)) '(1 2 3 4))
(check-equal? (union-set '(1 3) '(2 3 4)) '(1 2 3 4))

;; Exercise 2.63 (page 213/14)

;; a) the same (in-order traversal)
;; b) append vs cons = O(nlogn) vs O(n)

;; Exercise 2.65 (page 216)

;; From book

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (tree->list tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list
                             (right-branch tree)
                             result-list)))))
  (copy-to-list tree '()))

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

;; union-set via balanced-tree

(define (union-set a b)
  (list->tree
   (remove-duplicates
    (append (tree->list a)
            (tree->list b)))))

;; > (union-set (list->tree '(1 2 3 5 7 9 46))  
;;              (list->tree '(5 6 10 11 20 23 46))) 
;; '(9
;;   (3 (1 () (2 () ())) (5 () (7 () ())))
;;   (10 (46 () (6 () ())) (20 (11 () ()) (23 () ()))))

;; Code from book

(define (make-leaf symbol weight) (list 'leaf symbol weight))
(define (leaf? object) (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch
         tree) (car
                tree))
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

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)
                               (cadr pair))
                    (make-leaf-set (cdr pairs))))))

;; Exercise 2.67 (page 226)

(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree
                    (make-leaf 'D 1)
                    (make-leaf 'C 1)))))

;;    root
;; 0  /  \  1
;;   A    .
;;       / \
;;   0  B   .  1
;;         / \
;;     0  D   c  1

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))
;;                       A D     A B   B   C     A

;; Exercise 2.68 (page 226/7)

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (encode-symbol s tree)
  (if (leaf? tree)
      (if (eq? s (symbol-leaf tree))
          '()
          (error "fail"))
      (if (memq s (symbols (left-branch tree)))
          (cons 0 (encode-symbol s (left-branch tree)))
          (cons 1 (encode-symbol s (right-branch tree))))))

;; > (encode '(A D A B B C A) sample-tree) 
;; '(0 1 1 0 0 1 0 1 0 1 1 1 0)
