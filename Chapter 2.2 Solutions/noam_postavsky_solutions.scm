;; - 2.17

(define (last-pair lst)
  (let ((rest (cdr lst)))
    (if (null? rest)
        lst
        (last-pair rest))))

;; - 2.18

(module ex.2.18 (reverse)
  (import (only scheme define
                if
                list null? cons car cdr))
  (define nil (list))
  (define (reverse lst)
    (define (iter lst acc)
      (if (null? lst)
          acc
          (iter (cdr lst) (cons (car lst) acc))))
    (iter lst nil)))

;; - 2.20

(module ex.2.20 (same-parity)
  (import (only scheme define if let
                even? odd?
                if quote
                reverse list null? cons car cdr))
  (define (same-parity x . xs)
    (let ((keep? (if (even? x) even? odd?)))
      (define (iter lst acc)
        (if (null? lst)
            acc
            (iter (cdr lst)
                  (let ((head (car lst)))
                    (if (keep? head)
                        (cons head acc)
                        acc)))))
      (reverse (iter xs (list x))))))

;; - 2.21

(module ex.2.21.square-list-recursive (square-list)
  (import (only scheme define if
                list null? cons car cdr))
  (define nil (list))
  (define (square-list items)
    (if (null? items)
        nil
        (let ((item (car items)))
          (cons (* item item)
                (square-list (cdr items)))))))

(module ex.2.21.square-list-with-map (square-list)
  (import (only scheme define
                map lambda *))
  (define (square-list items)
    (map (lambda (x) (* x x)) items)))

;; - 2.23

(module ex.2.23 (for-each)
  (import (only scheme define
                cond
                null? car cdr))
  (define (for-each f list)
    (cond ((null? list)
           #t)
          (else
           (f (car list))
           (for-each f (cdr list))))))

;; - 2.25

(car (cdr (car (cdr (cdr '(1 3 (5 7) 9))))))
(car (car '((7))))
(car (cdr
      (car (cdr
            (car (cdr
                  (car (cdr
                        (car (cdr
                              (car (cdr
                                    '(1 (2 (3 (4 (5 (6 7))))))))))))))))))

;; - 2.27

(module ex.2.27 (deep-reverse)
  (import (only scheme define
                if let
                list null? pair? cons car cdr))
  (define (deep-reverse lst)
    (define (iter lst acc)
      (if (null? lst) acc
          (let ((item (car lst)))
            (iter (cdr lst)
                  (cons (if (pair? item)
                            (deep-reverse item)
                            item)
                        acc)))))
    (iter lst (list))))

;; - 2.28

(module ex.2.28 (fringe)
  (import (only scheme define
                if let
                list null? pair? cons car cdr
                reverse))
  (define (fringe tree)
    (define (iter tree acc)
      (if (null? tree) acc
          (iter (cdr tree)
                (let ((item (car tree)))
                  (if (pair? item)
                      (iter item acc)
                      (cons item acc))))))
    (reverse (iter tree (list)))))

;; - 2.30

(module ex.2.30.square-tree-direct (square-tree)
  (import (only scheme define
                if let
                list null? pair? cons car cdr
                reverse
                *))
  (define (square-tree tree)
    (define (iter tree acc)
      (if (null? tree) (reverse acc)
          (let ((item (car tree)))
            (iter (cdr tree)
                  (cons (if (pair? item)
                            (square-tree item)
                            (* item item))
                        acc)))))
    (iter tree (list))))

(module ex.2.30.square-tree-with-map (square-tree)
  (import (only scheme define
                map lambda * if pair?))
  (define (square-tree tree)
    (map (lambda (sub-tree)
           (if (pair? sub-tree)
               (square-tree sub-tree)
               (* sub-tree sub-tree)))
         tree)))

;; - 2.31

(module ex.2.31 (tree-map)
  (import (only scheme define
                map lambda if pair?))
  (define (tree-map f tree)
    (map (lambda (sub-tree)
           (if (pair? sub-tree)
               (tree-map f sub-tree)
               (f sub-tree sub-tree)))
         tree)))

;; - 2.32

(module ex.2.32 (subsets)
  (import (only scheme define let
                map lambda if
                list null? cons cdr car
                append))
  (define nil (list))

  (define (subsets s)
    (if (null? s)
        (list nil)
        (let ((rest (subsets (cdr s))))
          (append rest (map (lambda (subset)
                              (cons (car s) subset))
                            rest))))))
