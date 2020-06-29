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

;;;;;;;;;;;;;;;;;;

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define nil (list))

;; - 2.33

(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) nil sequence))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length sequence)
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))

;; - 2.34

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
                (+ this-coeff (* x higher-terms)))
              0
              coefficient-sequence))

(horner-eval 2 (list 1 3 0 5 0 1)) ;=> 79

;; - 2.36

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

(accumulate-n + 0 '((1 2 3) (4 5 6) (7 8 9) (10 11 12))) ;=> (22 26 30)

;; - 2.37

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (mᵢ)
         (dot-product mᵢ v))
       m))

(define (transpose mat)
  (accumulate-n cons (list) mat))

(transpose '((1 2 3 4)
             (4 5 6 6)
             (6 7 8 9)))
;; =>
;; ((1 4 6)
;;  (2 5 7)
;;  (3 6 8)
;;  (4 6 9))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (mᵢ) (matrix-*-vector cols mᵢ)) m)))

;; - 2.38

(foldr / 1 (list 1 2 3)) ;=> 3/2

(foldl / 1 (list 1 2 3)) ;=> 1/6

(foldr list nil (list 1 2 3)) ;=> (1 (2 (3 ())))

(foldl list nil (list 1 2 3)) ;=> (((() 1) 2) 3)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; To get the same result for foldr and fold, op should be
;; commutative: ∀x,y (op x y) == (op y x)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; - 2.40

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (prime? n)
  (define (square x)
    (* x x))
  (define (find-divisor n test-divisor)
    (cond ((> (square test-divisor) n) n)
          ((divides? test-divisor n) test-divisor)
          (else (find-divisor n (+ test-divisor 1)))))
  (define (divides? a b)
    (= (remainder b a) 0))
  (define (smallest-divisor n)
    (find-divisor n 2))

  (= n (smallest-divisor n)))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (unique-pairs n)
  (flatmap (lambda (i)
             (map (lambda (j) (list i j))
                  (enumerate-interval 1 (- i 1))))
           (enumerate-interval 2 n)))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum? (unique-pairs n))))

(prime-sum-pairs 6)
;=> ((2 1 3) (3 2 5) (4 1 5) (4 3 7) (5 2 7) (6 1 7) (6 5 11))

;; - 2.41

(define (unique-triples n)
  (flatmap (lambda (i)
             (map (lambda (jk)
                    (cons i jk))
                  (unique-pairs (- i 1))))
           (enumerate-interval 3 n)))

(define (triples-with-sum n s)
  (filter (lambda (ijk)
            (= (+ (car ijk) (cadr ijk) (caddr ijk)) s))
          (unique-triples n)))

(triples-with-sum 5 8) ;=> ((4 3 1) (5 2 1))

;; - 2.44

;; ?
(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))

;; - 2.45

(define (split smaller-split bigger-split)
  (lambda (painter n)
    (if (= n 0)
        painter
        (let ((smaller (up-split painter (- n 1))))
          (bigger-splot painter (smaller-split smaller smaller))))))
