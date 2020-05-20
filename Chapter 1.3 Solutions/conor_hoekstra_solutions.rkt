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
