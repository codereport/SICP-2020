#lang racket

(require rackunit
         threading)

;; 2.33
(module+ two-33
  (require rackunit)
  (define (map p sequence)
    (foldr (lambda (x acc) (cons (p x) acc)) '() sequence))

  (define (append seq1 seq2)
    (foldr cons seq2 seq1))

  (define (length sequence)
    (foldl (lambda (_ acc) (+ 1 acc)) 0 sequence))

  ;;;; 2.33 tests
  (check-equal?
   (map (lambda (x) (* x x)) '(1 2 3 4 5))
   '(1 4 9 16 25)
   )

  (check-equal?
   (append '(1 2 3 4 5) '(6 7 8 9 10))
   '(1 2 3 4 5 6 7 8 9 10))

  (check-equal?
   (length '(1 2 3 4 5)) 5)
  )

;; 2.34
(define (horner-eval x coefficient-sequence)
  (foldr (lambda (this-coeff higher-terms)
           (+ (* higher-terms x) this-coeff))
         0
         coefficient-sequence))

;;;; 2.34 tests
(check-equal?
 (horner-eval 2 (list 1 3 0 5 0 1)) 79)


;; 2.36
;;;; Implementation follows the book skeleton
(define (accumulate-n-book op init seqs)
  (if (null? (car seqs))
      '()
      (cons (foldl op init (map car seqs))
            (accumulate-n-book op init (map cdr seqs)))))

;;;; My implementation
;;;; This "curry" precedural in Racket is pretty cool!
(define (accumulate-n op init seqs)
  (match seqs
    ['() '()]
    [(cons hd _) (foldl (curry map op)
                        (build-list (length hd) (const init)) seqs)]))

;;;; 2.36 tests

(~>>
 (list accumulate-n-book accumulate-n)
 (for-each
  (lambda (proc)
   (check-equal?
    (proc + 0 '((1 2 3) (4 5 6) (7 8 9) (10 11 12)))
    '(22 26 30)
    ))))


;; 2.37
(define (dot-product v w)
  (foldr + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (curry dot-product v) m))

(define (transpose mat)
  (~>>
   mat
   (accumulate-n-book cons '())
   (map reverse))
  )

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (curry matrix-*-vector cols) m)))

;;;; 2.37 tests
(check-equal?
 (dot-product '(1 2 3) '(4 5 6)) 32)

(define mat1
  '((1 2 3)
    (4 5 6)
    (7 8 9)))

(define mat2
  '((10 19 13)
    (14 11 16)
    (17 18 12)))

(define vec1 '(1 2 3))

(check-equal?
 (matrix-*-vector mat1 vec1)
 '(14 32 50))

(check-equal?
 (transpose mat1)
 '((1 4 7)
   (2 5 8)
   (3 6 9)))

(check-equal?
 (matrix-*-matrix mat1 mat2)
 '((89 95 81)
   (212 239 204)
   (335 383 327)))

;; 2.38
;; (fold-right / 1 (list 1 2 3)) = 3/2
;; (fold-left / 1 (list 1 2 3)) = 3/2
;; (fold-right list nil (list 1 2 3)) = (1 (2 (3 ())))
;; (fold-left list nil (list 1 2 3)) = (3 (2 (1 ())))

;; Fold left and fold right produces the same result when the
;; operation is associative (order does not matter)

(define (flatmap proc seq)
  (foldr append '() (map proc seq)))

;; 2.40
;;;; A bunch of functions copied from book
(define (square x) (* x x))

(define (smallest-divisor n) (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b) (= (remainder b a) 0))

(define (prime? n)
(= n (smallest-divisor n)))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

;;;; My implementation below
(define (unique-pair n)
  (~>>
   (range 2 (+ n 1))
   (flatmap (lambda (i)
              (~>>
               (range 1 i)
               (map (curry list i))
               )))))

(define (prime-sum-pairs n)
  (~>> (unique-pair n)
       (filter prime-sum?)
       (map make-pair-sum)))

;;;; 2.40 tests
(check-equal?
 (unique-pair 3)
 '((2 1) (3 1) (3 2))
 )

(check-equal?
 (prime-sum-pairs 6)
 '((2 1 3) (3 2 5) (4 1 5) (4 3 7) (5 2 7) (6 1 7) (6 5 11))
 )

;; 2.41
(define (same-sum-triples n s)
  (~>>
   (for*/list ([i (in-range 1 (+ n 1))]
               [j (in-range 1 i)]
               [k (in-range 1 j)])
     (list i j k))
   (filter (lambda (triple) (= s (foldl + 0 triple))))))

(check-equal?
 (same-sum-triples 10 20)
 '((8 7 5) (9 6 5) (9 7 4) (9 8 3) (10 6 4) (10 7 3) (10 8 2) (10 9 1))
 )
