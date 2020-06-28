;; Hack

(define enumerate-tree flatten)
(define accumulate foldr)
(define square sq)

;; Example from the book

(define (sum-odd-squares tree)
  (accumulate
   + 0 (map square (filter odd? (enumerate-tree tree)))))

;; With Threading Macro

(require threading)

(define (sum-odd-squares tree)
  (~>> tree
       (enumerate-tree)
       (filter odd?)
       (map square)
       (accumulate + 0)))
       
;; Exercise 2.33 (page 161)

(define (map p sequence)
  (accumulate (λ (x y) (cons (p x) y)) `() sequence))

(define (append seq1 seq2)
  (accumulate cons seq1 seq2))

(define (length sequence)
  (accumulate (λ (_ acc) (+ acc 1)) 0 sequence))

;; Exercise 2.34 (page 162-3)

(define (horner-eval x coefficient-sequence)
  (accumulate (λ (coeff acc) (+ coeff (* x acc)))
              0
              coefficient-sequence))

;; > (horner-eval 2 (list 1 3 0 5 0 1))
;; 79

;; Exericse 2.36 (page 163)

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      '()
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

;; > (accumulate-n + 0 '((0 1) (1 2) (2 3)))
;; '(3 6)

;; TODO get map + apply solution working

;; Exercise 2.37 (page 163-5)

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

;; > (dot-product (range 4) (range 4))
;; 14

(define (matrix-*-vector m v)
  (map (λ (row) (dot-product v row)) m))

;; > (define mat '((0 1 2) (1 2 3) (2 3 4)))
;; > (matrix-*-vector mat (range 3))
;; '(5 8 11)

(define (transpose mat)
  (accumulate-n cons '() mat))

;; > (define mat2 '((0 0 0) (1 1 1) (2 2 2)))
;; > (transpose mat2)
;; '((0 1 2) (0 1 2) (0 1 2))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (λ (row) (matrix-*-vector cols row))  m)))

;; > (matrix-*-matrix mat mat2)
;; '((5 5 5) (8 8 8) (11 11 11))
;; > (matrix-*-matrix mat2 mat)
;; '((0 0 0) (3 6 9) (6 12 18))

;; Exercise 2.38 (page 165)

;; > (foldr / 1 '(1 2 3))
;; 1 1/2
;; > (foldl / 1 '(1 2 3))
;; 1 1/2
;; > (foldr list '() '(1 2 3))
;; '(1 (2 (3 ())))
;; > (foldl list '() '(1 2 3))
;; '(3 (2 (1 ())))

;; asssociativity / commutative

;; Exercise 2.39 (page 166)

(define (reverser sequence)
  (foldr (λ (x y) (append (list x) y)) '() sequence))

(define (reversel sequence)
  (foldl (λ (x y) (cons x y)) '() sequence))

;; Exercise 2.40 (169)

(require algorithms)
(require threading) 

(define (unique-pairs n)
  (let ((lst (range 1 (+ n 1))))
        (~>> lst
             (cartesian-product lst)
             (filter increasing?)
             (remove-duplicates))))

;; > (unique-pairs 4)
;; '((1 2) (1 3) (1 4) (2 3) (2 4) (3 4))

;; Attempt #1 at increasing

(define (increasing? lst)
  (~>> lst
       (reverse)
       (adjacent-map _ -)
       (andmap positive?)))

;; Attempt #2 at increasing

(define (increasing? lst)
  (~>> lst
       (adjacent-map _ <)
       (andmap identity)))

;; Attempt #3 at increasing

(define (all? lst)
  (andmap identity lst))

(define (increasing? lst)
  (~>> lst
       (adjacent-map _ <)
       (all?)))

;; Exercise 2.41

(require algorithms)
(require threading) 

(define (triplets-sum-k n k)
  (let ((lst (range 1 (+ n 1))))
        (~>> lst
             (cartesian-product lst lst)
             (filter increasing?)
             (filter (λ (t) (= (sum t) k))))))

;; > (triplets-sum-k 10 10)
;; '((1 2 7) (1 3 6) (1 4 5) (2 3 5))

;; Code brom book for pict language

(#%require sicp-pict)

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))

;; Exercise 2.44 (page 179)

(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))

(define (right-split painter n) 
  (if (= n 0) 
      painter 
      (let ((smaller (right-split painter (- n 1)))) 
        (beside painter (below smaller smaller)))))

;; Exercise 2.45 (page 182)

(define (split f g)
  (define (rec painter n)
    (if (= n 0) 
        painter 
        (let ((smaller (rec painter (- n 1)))) 
          (f painter (g smaller smaller)))))
  rec)

(define right-split (split beside below))
(define up-split (split below beside))
