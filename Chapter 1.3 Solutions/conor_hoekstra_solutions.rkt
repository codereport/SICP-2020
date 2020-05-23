;; Calculate pi

;; Original

(~>> (range 1000)
     (map (λ (x) (+ 1 (* 2 x))))
     (chunks-of _ 2)
     (map (λ (x) (foldl * 1 x)))
     (map (λ (x) (/ 1.0 x)))
     (foldl + 0)
     (* 8))

;; 3.1405926538397897

;; Cleaned up

(define (sum xs)  (foldl + 0 xs))
(define (prod xs) (foldl * 1 xs))

(define (first-n-odds n)
  (~>> (range n)
       (map (λ (x) (+ 1 (* 2 x))))))

(~>> (first-n-odds 10000)
     (chunks-of _ 2)
     (map prod)
     (map (λ (x) (/ 8.0 x)))
     (sum))

;; 3.1414926535900367

#| 
Haskell Solution
================

import Data.List.Split (chunksOf)

let firstNOdds n = map (((-)1) . (*2)) $ [1..n]

let pi = sum 
       . map ((8/) . product) 
       . chunksOf 2 
       . firstNOdds
|#

;; Exercise 1.29 (page 80)

(require threading)
(require algorithms)

(define (cube x) (* x x x))

(define (simpsons-integral f a b n)
  (let* ((h (/ (+ b a) n))
         (k (- (/ n 2) 1))
         (coefficients (flatten
                         (append '(1)
                                 (make-list k '(4 2))
                                 '(4 1)))))
    (~>> (range a (+ b h) h)
         (map f)
         (zip-with * coefficients)
         (sum)
         (* (/ h 3.0)))))

;; > (simpsons-integral cube 0 1 100)
;; 0.25
;; > (simpsons-integral cube 0 1 1000)
;; 0.25

;; Exercise 1.30 (page 80)

(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))

(define (inc n) (+ n 1))
(define (identity n) n)

;; > (sum identity 0 inc 10)
;; 55

;; Exercise 1.31 a) (page 80/81)

(define (product term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
  (iter a 1))

;; > (product identity 1 inc 10)
;; 3628800

;; Exercise 1.32 a) (page 81/82)

(define (accumulate combiner init term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner result (term a)))))
  (iter a init))

(define (product term a next b) (accumulate * 1 term a next b))
(define (sum     term a next b) (accumulate + 0 term a next b))

;; Exercise 1.34 (page 88)

(define (f proc) (proc 2))

(f f)
(f 2)
(2 2) <- 2 is not a procedure

;; Exercise 1.35 (page 94)

(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

;; > (fixed-point (λ (x) (+ 1 (/ 1 x))) 1.0)
;; 1.6180327868852458

;; Golden Ratio (from Google) = 1.61803398875

;; Exercise 1.36 (page 94)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (newline)
      (display next)
      (if (close-enough? guess next)
          (next)
          (try next))))
  (try first-guess))

;; > (fixed-point (λ (x) (+ 1 (/ 1 x))) 1.0)

;; 2.0
;; 1.5
;; 1.6666666666666665
;; 1.6
;; 1.625
;; 1.6153846153846154
;; 1.619047619047619
;; 1.6176470588235294
;; 1.6181818181818182
;; 1.6179775280898876
;; 1.6180555555555556
;; 1.6180257510729614
;; 1.6180371352785146
;; 1.6180327868852458. . application: not a procedure;
;;  expected a procedure that can be applied to arguments
;;   given: 1.6180327868852458
;;   arguments...: [none]

;; > (fixed-point (λ (x) (/ (log 1000) (log x))) 2.0)

;; 9.965784284662087
;; 3.004472209841214
;; 6.279195757507157
;; 3.759850702401539
;; 5.215843784925895
;; 4.182207192401397
;; 4.8277650983445906
;; 4.387593384662677
;; 4.671250085763899
;; 4.481403616895052
;; 4.6053657460929
;; 4.5230849678718865
;; 4.577114682047341
;; 4.541382480151454
;; 4.564903245230833
;; 4.549372679303342
;; 4.559606491913287
;; 4.552853875788271
;; 4.557305529748263
;; 4.554369064436181
;; 4.556305311532999
;; 4.555028263573554
;; 4.555870396702851
;; 4.555315001192079
;; 4.5556812635433275
;; 4.555439715736846
;; 4.555599009998291
;; 4.555493957531389
;; 4.555563237292884
;; 4.555517548417651
;; 4.555547679306398
;; 4.555527808516254
;; 4.555540912917957
;; 4.555532270803653. . application: not a procedure;
;;  expected a procedure that can be applied to arguments
;;   given: 4.555532270803653
;;   arguments...: [none]

;; Exercise 1.37 a) (page 94-96)

; Note: this recursive solution on works when n and d ignore k :p
(define (cont-frac n d k)
  (if (= k 1)
      (/ (n k) (d k))
      (/ (n k) (+ (d k) (cont-frac n d (- k 1))))))

;; > (/ 1 (cont-frac (λ (i) 1.0)
;;                   (λ (i) 1.0)
;;                   13))
;; 1.6180257510729614

;; Exercise 1.37 b) (page 96)

;; Iterative solution

(define (cont-frac n d k)
  (define (cf-iter i)
    (if (= i k)
        (/ (n i) (d i))
        (/ (n i) (+ (d i) (cf-iter (+ i 1))))))
  (cf-iter 1))

;; > (/ 1 (cont-frac (λ (i) 1.0)
;;                   (λ (i) 1.0)
;;                   13))
;; 1.6180257510729614

;; Exercise 1.38 (page 96)

(+ 2 (cont-frac (λ (i) 1.0)
                (λ (i) (if (= (remainder i 3) 2)
                           (* (/ (+ i 1) 3) 2)
                           1))
                13))

;; 2.718281828735696

;; Exercise 1.40 (page 103)

(define (cubic a b c)
  (λ (x) (+ (* x x x)
            (* a x x)
            (* b x)
            c)))

(define (solve a b c)
  (newtons-method (cubic a b c) 1.0))

;; > (solve 2 .1 1)
;; 1.0
;; 0.4225392779084478
;; -0.21155118275627272
;; 1.51885268517555
;; 0.8110472066463499
;; 0.26001093281328136
;; -0.6177913600448666
;; 0.5776126518208842
;; 0.015453931264681087
;; -6.148866770905981
;; -4.389308952753587
;; -3.262125292465215
;; -2.58984446006954
;; -2.2638356408221454
;; -2.1737146551311786
;; -2.1668727239099552
;; -2.1668344861311106
;; -2.1668344852504697

;; Exercise 1.41 (page 103)

(define (double f)
  (λ (x) (f (f x))))
  
;; > (((double (double double)) inc) 5)
;; 21

;; Exercise 1.42 (page 103)

(define (compose f g)
  (λ (x) (f (g x))))

;; > ((compose (λ (x) (* x x))
;;             (λ (x) (+ 1 x)))
;;    6)
;; 49

;; Exercise 1.43 (page 104)

(define (repeated f n)
  (if (= n 0)
      (λ (x) x)
      (compose f (repeated f (- n 1)))))

;; Exercise 1.44 (page 104)

(define (smoothed-f f dx)
  (λ (x) (~>> (list x (+ x dx) (- x dx))
              (map f)
              (average))))
                 
;; > (map (smoothed-f (λ (x) (* x x)) 1.0) (range 10))
;; '(0.6666666666666666
;;   1.6666666666666667
;;   4.666666666666667
;;   9.666666666666666
;;   16.666666666666668
;;   25.666666666666668
;;   36.666666666666664
;;   49.666666666666664
;;   64.66666666666667
;;   81.66666666666667)

