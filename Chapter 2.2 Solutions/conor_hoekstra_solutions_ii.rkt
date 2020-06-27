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
       
;; Exercise 2.33

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

;; Exericse 2.36

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      '()
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

;; > (accumulate-n + 0 '((0 1) (1 2) (2 3)))
;; '(3 6)
