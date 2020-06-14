#lang racket

(require rackunit
         threading)

;; 2.17
(define (last-pair l)
  (match l
    [(list e) (list e)]
    [(cons hd tl) (last-pair tl)]
    ))

;;;; 2.17 test
(check-equal? (last-pair '(23 72 149 34)) '(34))

;; 2.18
(define (reverse l)
  (define (helper lst acc)
    (match lst
      ['() acc]
      [(cons hd tl) (helper tl (cons hd acc))]))
  (helper l '())
  )

;;;; 2.18 test
(check-equal? (reverse (list 1 4 9 16 25)) '(25 16 9 4 1))

;; 2.20

(define (same-parity fst . rst)
  (let ([mod (remainder fst 2)])
    (define (helper l acc)
      (match l
        ['() acc]
        [(cons hd tl)
         (if (= (remainder hd 2) mod)
             (helper tl (cons hd acc))
             (helper tl acc))]))
    (cons fst (reverse (helper rst '())))))

;;;; 2.20 test
(check-equal? (same-parity 1 2 3 4 5 6 7) '(1 3 5 7))
(check-equal? (same-parity 2 4 6) '(2 4 6))

;; 2.21
(define (square x) (* x x))

(define (square-list-rec items)
  (if (null? items)
      '()
      (cons (square (car items)) (square-list-rec (cdr items)))))

(check-equal? (square-list-rec (list 1 2 3 4)) '(1 4 9 16))

(define (square-list items)
  (map square items))

;;;; 2.21 test
(check-equal? (square-list (list 1 2 3 4)) '(1 4 9 16))

;; 2.23
(define (for-each f lst)
  (if (null? lst)
      void
      (begin
        (f (car lst))
        (for-each f (cdr lst)))))

;;;; 2.23 test
(begin
  (define res 0)
  (for-each (lambda (x) (set! res (+ res x))) (range 11))
  (check-equal? res 55))

;; 2.25
(check-equal?
 (cadr (caddr '(1 3 (5 7) 9)))
 7)

(check-equal?
 (car (car '((7))))
 7)

(check-equal?
 (cadr (cadr (cadr (cadr (cadr (cadr '(1 (2 (3 (4 (5 (6 7))))))))))))
 7)

;; 2.27
(define (deep-reverse l)
  (define (helper lst acc)
    (match lst
      ['() acc]
      [(cons hd tl) (helper tl (cons (deep-reverse hd) acc))]
      [e e]))
  (helper l '())
  )

;;;; 2.27 tests
(check-equal?
 (deep-reverse (list (list 1 2) (list 3 4)))
 (list (list 4 3) (list 2 1)))

;; 2.28
(define (fringe lst)
  (match lst
      ['() '()]
      [(cons hd tl) (if (list? hd)
                        (append (fringe hd) (fringe tl))
                        (cons hd (fringe tl)))]))

;;;; 2.28 tests
(define x (list (list 1 2) (list 3 4)))
(check-equal? (fringe x) '(1 2 3 4))

;; 2.30
(define (square-tree-direct tree)
  (match tree
      ['() '()]
      [(cons hd tl) (cons (square-tree-direct hd) (square-tree-direct tl))]
      [x (* x x)]))

(define (square-tree-map tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (square-tree-map sub-tree)
             (* sub-tree sub-tree))) tree))

;;;; 2.30 tests
(define original
  (list 1
        (list 2 (list 3 4) 5)
        (list 6 7)))

(define expected
  (list 1
        (list 4 (list 9 16) 25)
        (list 36 49))
  )

(check-equal?
 (square-tree-direct original) expected)

(check-equal?
 (square-tree-map original) expected)


;; 2.31
(define (tree-map f tree)
  (match tree
      ['() '()]
      [(cons hd tl) (cons (tree-map f hd) (tree-map f tl))]
      [x (f x)]))

;;;; 2.31 test
(define (square-tree tree) (tree-map square tree))
(check-equal? (square-tree original) expected)

;; 2.32
(define (subsets s)
  (if (null? s)
      (list '())
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda x (cons (car s) (car x))) rest)))))

;;;; 2.32 test
(check-equal? (subsets '(1 2 3))
                    '(() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3)))
