;; - 2.54

(module ex2.54 (equal?)
  (import (only scheme
                define if cond and quote
                eq?
                car cdr pair?))
  (define (equal? x y)
    (cond
     ((eq? x y) #t) ;; NOTE: using eqv? would give correct results for numbers too.
     ((and (pair? x) (pair? y))
      (and (equal? (car x) (car y))
           (equal? (cdr x) (cdr y))))
     (else #f))))

;; - 2.56


(module ex2.56 (deriv)
  (import (only scheme
                define cond if let
                and symbol? eq? pair? quote number?
                list car cdr cons cadr caddr)
          (only chicken.base error))
  (define (variable? x) (symbol? x))
  ;; Two variables are the same if the symbols representing them are
  ;; `eq?':
  (define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))
  ;; Sums and products are constructed as lists:
  (define (make-sum a1 a2) (list '+ a1 a2))
  (define (make-product m1 m2) (list '* m1 m2))
  ;; A sum is a list whose first element is the symbol `+':
  (define (sum? x)
    (and (pair? x) (eq? (car x) '+)))
  ;; The addend is the second item of the sum list:
  (define (addend s) (cadr s))
  ;; The augend is the third item of the sum list:
  (define (augend s) (caddr s))
  ;; A product is a list whose first element is the symbol `*':
  (define (product? x)
    (and (pair? x) (eq? (car x) '*)))
  ;; The multiplier is the second item of the product list:
  (define (multiplier p) (cadr p))
  ;; The multiplicand is the third item of the product list:
  (define (multiplicand p) (caddr p))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Exponentiation representation (** BASE EXPONENT)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define (exponentiation? expr)
    (and (pair? expr) (eq? (car expr) '**)))
  (define (base exp)
    (cadr exp))
  (define (exponent exp)
    (caddr exp))
  (define (make-exponentiation base exponent)
    (list '** base exponent))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; Exponentiation derivative rule
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ((exponentiation? exp)
           (let ((u (base exp))
                 (n (exponent exp)))
             (make-product n
                           (make-exponentiation u (make-sum n -1))
                           (deriv u))))
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          (else
           (error "unknown expression type -- DERIV" exp)))))

;; - 2.59

(module chapter2.2 (accumulate accumulate-n)
  (import (only scheme
                define if
                null? cons car cdr
                map))
  (define (accumulate op initial sequence)
    (define (iter sequence acc)
      (if (null? sequence)
          acc
          (iter (cdr sequence)
                (op (car sequence) acc))))
    (iter sequence initial))

  (define (accumulate-n op init seqs)
    (define (iter seqs acc)
      (if (null? (car seqs))
          acc
          (iter (map cdr seqs)
                (cons (accumulate op init (map car seqs)) acc))))
    (iter seqs init)))

(module set.unordered-list (element-of-set? adjoin-set intersection-set union-set)
  (import (only scheme
                define if cond and or quote
                equal?
                null? cons car cdr)
          (only chapter2.2 accumulate))

  (define (element-of-set? x set)
    ;; Book used 'false' and 'true' instead of #f and #t.
    (cond ((null? set) #f)
          ((equal? x (car set)) #t)
          (else (element-of-set? x (cdr set)))))

  (define (adjoin-set x set)
    (if (element-of-set? x set)
        set
        (cons x set)))

  (define (intersection-set set1 set2)
    (cond ((or (null? set1) (null? set2)) '())
          ((element-of-set? (car set1) set2)
           (cons (car set1)
                 (intersection-set (cdr set1) set2)))
          (else (intersection-set (cdr set1) set2))))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define (union-set set1 set2)
    (accumulate adjoin-set set1 set2))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  )

;; - 2.62

(module set.ordered-list (;; element-of-set? adjoin-set intersection-set
                          union-set)
  (import (only scheme
                define if cond and or quote let
                equal?
                null? cons car cdr append reverse
                <= < = >))
  (define (union-set set1 set2)
    (define (iter set1 set2 acc)
      (cond ((null? set1) (append (reverse acc) set2))
            ((null? set2) (append (reverse acc) set1))
            (else (let ((x1 (car set1))
                        (x2 (car set2)))
                    (iter (if (<= x1 x2) (cdr set1) set1)
                          (if (<= x2 x1) (cdr set2) set2)
                          (cons (if (<= x1 x2) x1 x2) acc))))))
    (iter set1 set2 '())))

;; - 2.63

;;; Both procedures return the same result (the list of numbers in
;;; ascending order), but tree->list-1 make log(n) calls to append, so
;;; it has a runtime complexity of θ(nlog(n)).  tree->list-2 makes
;;; exactly n calls to cons, so its complexity is θ(n).

;; Figure 2.16 in lisp notation.
'(7 (3 (1 () ()) (5 () ()))
    (9 () (11 () ())))

'(3 (1 () ())
    (7 (5 () ())
       (9 () (11 () ()))))

'(5 (3 (1 () ()) ())
    (9 (7 () ()) (11 () ())))


;; - 2.65 (only union-set)

(module set.tree (union-set)
  (import (only scheme
                define if cond and or quote let
                equal?
                list length null? cons car cdr append
                cadr caddr
                quotient remainder - + <= < = >)
          (prefix set.ordered-list olist:))

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

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define (union-set tree1 tree2)
    ;; This feels like cheating...
    (list->tree (olist:union-set (tree->list-2 tree1)
                                 (tree->list-2 tree2))))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  )


(module ex2.67.68 (encode decode)
  (import (only scheme
                define if cond and or quote let
                equal? eq?
                list length null? cons car cdr
                reverse append memq
                cadr caddr cadddr
                quotient remainder - + <= < = >)
          (only chicken.base error))

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
          (else (error "bad bit -- CHOOSE-BRANCH" bit))))

  (define (adjoin-set x set)
    (cond ((null? set) (list x))
          ((< (weight x) (weight (car set))) (cons x set))
          (else (cons (car set)
                      (adjoin-set x (cdr set))))))

  (define sample-tree
    (make-code-tree (make-leaf 'A 4)
                    (make-code-tree
                     (make-leaf 'B 2)
                     (make-code-tree (make-leaf 'D 1)
                                     (make-leaf 'C 1)))))

  (define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; - 2.67
  (decode sample-message sample-tree) ;=> (A D A B B C A)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (encode message tree)
    (if (null? message)
        '()
        (append (encode-symbol (car message) tree)
                (encode (cdr message) tree))))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; - 2.68
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define (encode-symbol symbol tree)
    (define (iter tree bits)
      (cond ((leaf? tree) (reverse bits))
            ((memq symbol (symbols (left-branch tree)))
             (iter (left-branch tree) (cons 0 bits)))
            (else
             (iter (right-branch tree) (cons 1 bits)))))
    (if (memq symbol (symbols tree))
        (iter tree '())
        (error "Non-encodable symbol")))

  (equal? (encode (decode sample-message sample-tree) sample-tree)
          sample-message))


