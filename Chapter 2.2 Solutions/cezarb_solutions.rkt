#lang racket

(require rackunit)

; Ex 2.17

(define (last-pair alist)
  (define (last-pair-1 alist atail)
    (if (null? alist)
        atail
        (last-pair-1 (cdr alist) (list (car alist)))))
  (last-pair-1 alist '()))

(module+ test
  (begin
    (check-equal? (last-pair (list 1 2 3 4)) '(4))
    (check-equal? (last-pair '()) '())
    ))


; Ex 2.18

(define (reverse-a alist)
  (foldl cons '() alist))

(define (reverse-b alist)
  (if (null? alist)
      '()
      (append (reverse (cdr alist)) (list (car alist)))))

(module+ test
  (begin
    (check-equal? (reverse-a (list 1 2 3 4)) (list 4 3 2 1))
    (check-equal? (reverse-a '()) '())
    (check-equal? (reverse-b (list 1 2 3 4)) (list 4 3 2 1))
    (check-equal? (reverse-b '()) '())
    ))

; Ex 2.20

(define (same-parity h . t)
  (filter
   (lambda (e) (= (remainder h 2) (remainder e 2)))
   (cons h  t)))

(module+ test
  (begin
    (check-equal? (same-parity 1 2 3 4 5 6 7) '(1 3 5 7))
    (check-equal? (same-parity  2 3 4 5 6 7) '(2 4 6))
    ))

; Ex 2.21
(define (square-list-1 items)
  (if (null? items)
      null
      (cons (* (car items) (car items)) (square-list-1 (cdr items)))))

(define (square-list-2 items)
  (map (lambda (x) (* x x)) items))

(module+ test
  (begin
    (check-equal? (square-list-1 (list 1 2 3 4)) '(1 4 9 16))
    (check-equal? (square-list-2 (list 1 2 3 4)) '(1 4 9 16))
    ))

; Ex 2.23
(define (for-each fn items)
   (if (null? items)
      #t
      (let ((x (fn (car items))))
        (for-each fn (cdr items)))))

(module+ test
  (let ((p (open-output-string)))
    (begin
      (for-each (lambda (x) (display x p)) '(1 2 3 4)) 
      (check-equal? (get-output-string p) "1234"))))

; 2.27
(define (deep-reverse alist)
  (if (null? alist)
      '()
      (append (deep-reverse (cdr alist))
              (let ((h (car alist)))
                (if (list? h)
                    (list (deep-reverse h))
                    (list h))))))
(module+ test
  (begin
    (check-equal? (deep-reverse (list 1 2 3 4)) (list 4 3 2 1))
    (check-equal? (deep-reverse '()) '())
    (check-equal? (deep-reverse (list (list 1 2)  2 (list 3 4))) '((4 3) 2 (2 1)))
    ))

; 2.25
(module+ test
  (define l1 '(1 3 (5 7) 9))
  (define l2 '((7)))
  (define l3 '(1 (2 (3 (4 (5 (6 7)))))))
  (check-equal? (car (cdaddr l1)) 7)
  (check-equal? (caar l2) 7)
  (check-equal? (cadadr (cadadr (cadadr l3))) 7)
  )

; 2.28
(define (fringe tree)
  (if (list? tree)
      (foldr append '() (map fringe tree))
      (list tree)))

(module+ test
  (begin
    (define x (list (list 1 2) (list 3 4)))
    (check-equal? (fringe x) '(1 2 3 4))
    (check-equal? (fringe (list x x)) '(1 2 3 4 1 2 3 4))
    ))

; 2.30

(define (square-tree-1 tree)
  (cond ((null? tree) null)
        ((pair? tree) (cons (square-tree-1 (car tree))
                            (square-tree-1 (cdr tree))))
        (else (* tree tree))))

(define (square-tree-2 tree)
  (define (square-tree-2-helper el)
    (cond ((null? el) null)
          ((pair? el) (square-tree-2 el))
          (else (* el el))))
  (map square-tree-2-helper tree))
          


; 2.31
(define (tree-map fn tree)
  (define (tree-map-helper el)
    (cond ((null? el) null)
          ((pair? el) (tree-map fn el))
          (else (fn el))))
  (map tree-map-helper tree))

(define (square-tree tree) (tree-map (lambda (x) (* x x)) tree))


(module+ test
  (begin
    (define t (list 1
                    (list 2 (list 3 4) 5)
                    (list 6 7)))
    
    (check-equal? (square-tree-1 t) '(1 (4 (9 16) 25) (36 49)))
    (check-equal? (square-tree-2 t) '(1 (4 (9 16) 25) (36 49)))
    (check-equal? (square-tree t) '(1 (4 (9 16) 25) (36 49)))
    ))

; 2.32
(define (subsets s)
  (if (null? s)
      (list null)
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (el) (cons (car s) el)) rest)))))

(module+ test
  (begin
    (define s (list 1 2 3))
    (check-equal? (subsets s) '(() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3)))
    ))


