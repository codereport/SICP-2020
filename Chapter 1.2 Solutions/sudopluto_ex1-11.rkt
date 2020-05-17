#lang sicp

;; 1.11

(define (f-recur n)
  (if (< n 3)
      n
      (+ (f-recur (- n 1))
         (* 2 (f-recur (- n 2)))
         (* 3 (f-recur (- n 3))))))

(define (f-iter n)
  
  (define (calc num1 num2 num3)
    (+ num1 (* 2 num2) (* 3 num3)))
  
  (define (f-impl cnt sub1 sub2 sub3)
    (if (= 3 cnt)
        (calc sub1 sub2 sub3)
        (f-impl (- cnt 1) (calc sub1 sub2 sub3) sub1 sub2)))
  
  (if (< n 3)
      n
      (f-impl n 2 1 0)))

;; small nums (cond 1)
(f-recur 0)
(f-iter 0)

(f-recur 2)
(f-iter 2)


;; big nums (cond 2)
(f-recur 5)
(f-iter 5)

(f-recur 15)
(f-iter 15)
