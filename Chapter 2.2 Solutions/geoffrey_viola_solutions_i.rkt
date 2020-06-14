#lang racket

;; Exercise 2.17
(define (last-pair x)
  (define (last-pair-non-nil x)
    (let ((next (cdr x)))
      (if (null? next)
          x
          (last-pair next))))
  ; handle empty list
  (if (null? x)
      '()
      (last-pair-non-nil x)))

;; tests
;;(last-pair (list ))
;;(last-pair (list 1))
;;(last-pair (list 1 2))


;; Exercise 2.18
(define (reverse x)
  (define (reverse-iter in out)
    (if (null? in)
        out
        (reverse-iter (cdr in) (cons (car in) out))))
  (reverse-iter x '()))

;; tests
;(reverse (list ))
;(reverse (list 1 4 9 16 25))


;; Exercise 2.20
(define (same-parity x . y)
  (letrec ((lsb (bitwise-and x 1))
           (same-parity-iter
            (lambda (in out)
              (if (null? in)
                  out
                  (let* ((next-value (car in))
                         (next-out
                          ; filter predicate
                          (if (= (bitwise-and next-value 1) lsb)
                              ; filter accept
                              (append out (list next-value))
                              ; filter discard
                              out)))
                    (same-parity-iter (cdr in) next-out))))))
    (same-parity-iter (cons x y) '())))

;; tests
;(same-parity 1 2 3 4 5 6 7)
;(same-parity 2 3 4 5 6 7)


;; Exercise 2.21
(define (square x) (* x x))
;; non-map
;(define (square-list items)
;  (if (null? items)
;      '()
;      (cons (* (car items) (car items)) (square-list (cdr items)))))
;; map
(define (square-list items)
  (map square items))

;; test
;(square-list (list 1 2 3 4))


;; Exercise 2.22
(define (square-list-iter-fixed items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things) 
              (append answer (list (square (car things)))))))
  (iter items '()))

;; test
;(square-list-iter-fixed (list 1 2 3 4))

;; cons was in the wrong order in the first implementation.
;; cons operates on a list as the first argument
;; in the second implementation.


;; Exercise 2.23
(define (for-each proc elements)
  (define (iter proc in out)
    (if (null? in)
        out
        (iter proc
              (cdr in)
              (cons (proc (car in)) (cdr in)))))
  (iter proc elements '()))

;; test
;(for-each (lambda (x) (newline) (display x))
;          (list 57 321 88))


;; Exercise 2.25
(define e-2-25-1 (list 1 3 (list 5 7) 9))
;; There isn't a cadaddr
;(car (cdaddr e-2-25-1))

(define e-2-25-2 (list (list 7)))
;(caar e-2-25-2)

(define e-2-25-3 (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))
;; There isn't a cadadadadadadr
;(cadadr(cadadr (cadadr e-2-25-3)))


;; Exercise 2.27
(define (deep-reverse x)
  (define (iter in out)
    (if (null? in)
        out
        (let* ((next-in (car in))
          (next-out (if (pair? next-in)
              (deep-reverse next-in)
              next-in)))
          (iter (cdr in) (cons next-out out)))))
  (iter x '()))

;; tests
;(define e-2-27 (list (list 1 2) (list 3 4)))
;(deep-reverse e-2-27)
;(define nested-list (list 1 (list 2) (list 3 4) 5))
;(deep-reverse nested-list)


;; Exercie 2.28
; fringe = flatten
(define (fringe x)
  (define (iter in out)
    (if (null? in)
        out
        (let* ((next-in (car in))
          (new-out (if (pair? next-in)
              (append out (fringe next-in))
              (append out (list next-in)))))
          (iter (cdr in) new-out))))
  (iter x '()))

;(define e-2-28 (list (list 1 2) (list 3 4)))
;(fringe e-2-28)


;; Exercise 2.30
(define (square-tree-rec tree)
  (cond ((null? tree) '())
        ((not (pair? tree)) (square tree))
        (else (cons (square-tree-rec (car tree))
                    (square-tree-rec (cdr tree))))))

(define (square-tree-map tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (square-tree-map sub-tree)
             (square sub-tree)))
       tree))

;; tests
(define e-2-30 (list 1 (list 2 (list 3 4) 5) (list 6 7)))
;(square-tree-map e-2-30)
;(square-tree-rec e-2-30)


;; Exercise 2.31
(define (tree-map proc tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (square-tree-map sub-tree)
             (proc sub-tree)))
       tree))

;; tests
(define (square-tree-test tree) (tree-map square tree))
;(square-tree-test e-2-30)


;; Exercise 2.32
(define (subsets s)
  (if (null? s)
      (list '())
      (let ((rest (subsets (cdr s))))
        (append rest (map
                      (lambda (x) (cons (car s) x))
                      rest)))))

;; tests
;(subsets (list 1 2 3))

;; When the list is empty. it returns an empty list
;; Otherwise, it returns the accumulated list appended with
;; the first element prepended to each item in the accumulated list
