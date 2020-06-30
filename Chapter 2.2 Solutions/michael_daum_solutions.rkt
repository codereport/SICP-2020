#lang racket

;;
;; 2.17
;;
(define (last-pair lst)
  (if (null? (cdr lst))
      lst
      (last-pair (cdr lst))))

;;
;; 2.18
;;
(define (reverse lst)
  (define (reverse_helper forward_lst reversed_lst)
    (if (null? forward_lst)
        reversed_lst
        (reverse_helper (cdr forward_lst) (cons (car forward_lst) reversed_lst))))
  (reverse_helper lst null))

(define primes '(1 2 3 5 7 11 13 17 19))
(define list1 '(1))
(define list2 '(1 2))
(define list3 '(1 2 3))

(reverse list1)
(reverse list2)
(reverse list3)
(reverse primes)

;;
;; 2.19
;;
(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))
(define no-more? null?)
(define except-first-denomination cdr)
(define first-denomination car)
(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination
                 coin-values))
            (cc (- amount
                   (first-denomination
                    coin-values))
                coin-values)))))
(cc 100 us-coins)
(cc 100 uk-coins)

;;
;; 2.20
;;
(define (same-parity firstint . restints)
  (define pred? (if (even? firstint) even? odd?))
  (define (iter unfiltered filtered)
    (if (null? unfiltered)
        filtered
        (iter (cdr unfiltered) (if (pred? (car unfiltered))
                                   (cons (car unfiltered) filtered)
                                   filtered))))
  (cons firstint (reverse (iter restints null))))

(same-parity 1 3 2 5 6 3 2 3 1 2  5 4 12)
(same-parity 2 5 6 3 2 3 1 2  5 4 12 2)

;;
;; 2.21
;;
(define (square x) (* x x))
(define (square-list-items-a lst)
  (if (null? lst) null
      (let* [(fst (car lst))
             (fst2 (square fst))]
        (cons fst2 (square-list-items-a (cdr lst))))))
(square-list-items-a (list 1 2 3 4))

(define (square-list-items-b lst)
  (map square lst))
(square-list-items-b (list 1 2 3 4))

(define ((mapf f) lst)
  (map f lst))
(define square-list-items-c (mapf square))

(square-list-items-c (list 1 2 3 4))

;;
;; 2.22
;;
;; The first part is right out of the definition of reverse above
;; The second part is because it will create a cons cell with a list as car and the car as cdr, so the resulting data structure won't be a list at all, it's a vlist
(define (bad-square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer
                    (square (car things))))))
  (iter items null))
(bad-square-list (list 1 2 3 4)) ;; Not a list, it's a vlist '((((() . 1) . 4) . 9) . 16)

;|    |  16|
;[ |  |    ]
;  V
;|    |   9|
;[ |  |    ]
;  V
;|    |   4|
;[ |  |    ]
;  V
;|   /|   1|
;[ /  |    ]

;;
;; 2.23
;;
(define (for-each f lst)
  (if (null? lst) null
      (let ((discard (f (car lst))))
        (for-each f (cdr lst)))))
(for-each display (list 1 2 3 4))

;;
;; 2.24
;;
(list 1 (list 2 (list 3  4))) ;; Expression

`( 1 ( 2 ( 3 4 )))            ;; Shown in Repl

;; [   |   ] -> [   | \ ]     ;; Box Diagram
;;   |            |
;;   V            V
;;   1          [   |   ] -> [   | \ ]
;;                |            |
;;                V            V
;;                2          [   |   ] -> [  | \ ]
;;                             |             |
;;                             V             V
;;                             3             4
;;                  /\        ;; Tree Representation
;;                 /  \
;;                1   /\
;;                   2  \
;;                      /\
;;                     3  4

;;
;; 2.25
;;
(define (cadaddr lst)
  (car (cdaddr lst)))
(define (cadadadadadadr lst)
  (cadadr (cadadr (cadadr lst))))
(cadaddr `(1 3 (5 7 9)))
(caar `((7)))
(cadadadadadadr `(1 ( 2 ( 3 ( 4 ( 5 (6 7)))))))

;;
;; 2.26
;;
(define x (list 1 2 3))
(define y (list 4 5 6))
(append x y) ;; '(1 2 3 4 5 6)
(cons x y)   ;; '((1 2 3) . 4 5 6) ;; ( I got this wrong ;) )
(list x y)   ;; '((1 2 3) (4 5 6))

;;
;; 2.27
;;
(define (deep-reverse lst)
  (define (iter forward_lst reversed_lst)
    (if (null? forward_lst)
        reversed_lst
        (iter (cdr forward_lst) (cons (deep-reverse (car forward_lst)) reversed_lst))))
  (if (list? lst)
      (iter lst null)
      lst))

(deep-reverse '((1 (2 3))))

;;
;; 2.28
;;
(define (fringe lst)
  (define (iter lst accum)
    (cond [(null? lst) accum]
          [(not (pair? lst)) (cons lst accum)]
          [else (iter (cdr lst) (iter (car lst) accum))]))
  (iter lst '()))

(fringe '(3 4 (1 2)))
(fringe '((1 2) (3 4)))

;;
;; 2.29
;;
(define (make-mobile left right)
  (list left right))
(define (make-branch lngth structure)
  (list lngth structure))
(define (left-branch mobile)
  (car mobile))
(define (right-branch mobile)
  (cadr mobile))
(define (branch-length branch)
  (car branch))
(define (branch-structure branch)
  (cadr branch))

(define (total-weight mobile)
  (if (pair? mobile)
      (+ (total-weight (branch-structure (left-branch mobile)))
         (total-weight (branch-structure (right-branch mobile))))
      mobile))

(define (mobile-balanced? mobile)
  (define (balanced-and-weight mobile)
    (if (not (pair? mobile))
        (list #t mobile)
        (let* [(l-branch (left-branch mobile))
               (r-branch (right-branch mobile))
               (bw-left (balanced-and-weight (branch-structure l-branch)))
               (bw-right (balanced-and-weight (branch-structure r-branch)))]
          (list (and (eq? (* (branch-length l-branch) (cadr bw-left))
                          (* (branch-length r-branch) (cadr bw-right)))
                     (car bw-left)
                     (car bw-right))
                (+ (cadr bw-left) (cadr bw-right))))))
  (car (balanced-and-weight mobile)))

(mobile-balanced? 3)
(mobile-balanced? (make-mobile (make-branch 4 3) (make-branch 3 4)))
(total-weight )

;;
;; 2.30
;;
(define (square-tree-direct tree)
  (cond [(null? tree) null]
        [(pair? tree) (cons (square-tree-direct (car tree)) (square-tree-direct (cdr tree)))]
        [(* tree tree)]))
(square-tree-direct null)
(square-tree-direct 3)
(square-tree-direct '(3 3))
(square-tree-direct '(3 (3 2 1)))
(define (square-tree-map tree)
  (map (lambda (tree)
         (if (pair? tree)
             (cons (square-tree-direct (car tree)) (square-tree-direct (cdr tree)))
             (* tree tree)))
       tree))
(square-tree-map null)
;; (square-tree-map 3) Fails on map version bc not tree
(square-tree-map '(3 3))
(square-tree-map '(3 (3 (2 11) 1)))

;;
;; 2.31
;;
(define ((tree-map func) tree)
  (cond [(null? tree) null]
        [(pair? tree) (cons ((tree-map func) (car tree)) ((tree-map func) (cdr tree)))]
        [(func tree)]))
(define square-tree-from-tree-map
  (tree-map (lambda (x) (* x x))))
(square-tree-from-tree-map null)
(square-tree-from-tree-map 3)
(square-tree-from-tree-map '(3))
(square-tree-from-tree-map '(3 3))
(square-tree-from-tree-map '(3 (3 (2 11) 1)))

;;
;; 2.32
;;
(define (subsets s)
  (if (null? s)
      (list null)
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (lst) (cons (car s) lst)) rest)))))
(subsets null)
(subsets '(2 3 4 5))

;;
;; 2.33
;;
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op
                      initial
                      (cdr sequence)))))
(accumulate + 0 (list 1 2 3 4 5))

(define (accum-map p sequence)
  (accumulate (lambda (x y) (cons (p x) y))
              null sequence))
(define (accum-append seq1 seq2)
  (accumulate cons seq2 seq1))
(define (accum-length sequence)
  (accumulate (lambda (x y) (+ y 1)) 0 sequence))

;;
;; 2.34
;;
(define (horner-eval x coefficient-sequence)
  (accumulate
   (lambda (this-coeff higher-terms)
     (+ this-coeff (* x higher-terms)))
   0
   coefficient-sequence))
(horner-eval 3 '(1 1 1))

;;
;; 2.35
;;
(define (accum-count-leaves t)
  (accumulate
   (lambda (x y)
     (+ (if (pair? x)
            (accum-count-leaves x)
            1)
        y))
   0
   t))

;;
;; 2.36
;;
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      null
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))
(accumulate-n + 0 '((1 2 3) (4 5 6) (7 8 9) (10 11 12)))

;;
;; 2.37
;;
(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (x) (dot-product x v)) m))

(define (transpose mat)
  (accumulate-n cons '() mat))

(define (matrix-*-matrix m n)
  (let ([cols (transpose n)])
    (map (lambda (mv) (map (lambda (nv) (dot-product mv nv)) cols)) m)))

;;
;; 2.38
;;
(define fold-right accumulate)
(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))
(fold-right / 1 (list 1 2 3)) ;; (/ 1 (/ 2 ( / 3 1))) == 3/2
(fold-left / 1 (list 1 2 3))  ;; (/ (/ (/ 1 1) 2) 3)
(fold-right list null '(1 2 3)) ;; (list 1 (list 2 (list 3 '()))) == '(1 (2 (3 ())))
(fold-left list null '(1 2 3)) ;;  (list (list (list '() 1) 2) 3) == '(((() 1) 2) 3)
(list (list (list '() 1) 2) 3)

;;
;; 2.39
;;
(define (reverse-foldr sequence)
  (fold-right
   (lambda (x y) (append y (list x)))
   null sequence))
(define (reverse-foldl sequence)
  (fold-left (lambda (x y) (cons y x)) null sequence))

;;
;; 2.40
;;
(define (flatmap proc seq)
  (accumulate append null (map proc seq)))

(define (enumerate-interval low high)
  (if (> low high)
      null
      (cons low
            (enumerate-interval
             (+ low 1)
             high))))

(define (unique-pairs n)
  (flatmap (lambda (j)
         (map (lambda (i) (list j i))
              (enumerate-interval 1 (- j 1))))
       (enumerate-interval 1 n )))

(require math/number-theory) ;; for prime?
(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair)
        (cadr pair)
        (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter
        prime-sum?
        (unique-pairs n))))

(prime-sum-pairs 15)

;;
;; 2.41
;;
(define (unique-triples n)
  (flatmap (lambda (ij-pair)
             (let ([i (car ij-pair)]
                   [j (cadr ij-pair)])
               (map (lambda (k) (list k j i))
                    (enumerate-interval 1 (- j 1)))))
           (unique-pairs n)))
(define (sum lst)
  (accumulate + 0 lst))
(define (triples-sum-to-s s n)
  (filter (lambda (lst) (eq? (sum lst) s))
          (unique-triples n)))

;;
;; 2.42
;;
(define empty-board '())

(define (adjoin-position row col board)
  (cons (list row col) board))

(define (safe-1? candidate queen)
  (let ([cx (cadr candidate)]
        [cy (car candidate)]
        [qx (cadr queen)]
        [qy (car queen)])
    (not (or (eq? cy qy)
             (eq? (abs (- cx qx)) (abs (- cy qy)))))))

(define (safe? col positions)
  (let ([candidate (car positions)]
        [rest-of-queens (cdr positions)])
    (accumulate (lambda (x y) (and x y)) #t
                (map (lambda (position)
                       (safe-1? candidate position))
                     rest-of-queens))))

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions)
           (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position
                    new-row
                    k
                    rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

;;
;; 2.43
;;
;; It's very bad because it has to re-solve the N-1 sub-problem for each new candidate row!
;; If the original solves in time T, then the slow one should be T * board-size! (factorial)
