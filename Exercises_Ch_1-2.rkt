; Exercise 1.11 (page 53)

; Recursive solution

(define (f n)
  (if (< n 3)
      n
      (+ (f (- n 1))
         (* 2 (f (- n 2)))
         (* 3 (f (- n 3))))))

; > (map f (range 10))
; '(0 1 2 4 11 25 59 142 335 796)

; Iterative solution

(define (f n)
  (f-iter 0 1 2 n))

(define (f-iter a b c count)
  (cond ((= 0 count) a)
        (else (f-iter b c (+ c (* 2 b) (* 3 a)) (- count 1)))))

; > (map f (range 10))
; '(0 1 2 4 11 25 59 142 335 796)

; Exercise 1.12

(require threading)
; (require algorithms) ;; TODO

(define (pascals-triangle n)
  (if (= 1 n)
      '(1)
      (pt-iter '((1 1) (1)) (- n 2))))

(define (pt-iter acc n)
  (if (= 0 n)
      (reverse acc)
      (pt-iter (cons (pt-next-row (first acc)) acc) (- n 1))))

(define (pt-next-row row)
  (~> row
      (sliding 2 1 _)
      (map (λ (x) (foldl + 0 x)) _)
      (append '(1) _ '(1))))

; > (pascals-triangle 10)
; '((1)
;   (1 1)
;   (1 2 1)
;   (1 3 3 1)
;   (1 4 6 4 1)
;   (1 5 10 10 5 1)
;   (1 6 15 20 15 6 1)
;   (1 7 21 35 35 21 7 1)
;   (1 8 28 56 70 56 28 8 1)
;   (1 9 36 84 126 126 84 36 9 1))

; Exponentiation (page 57/8)

; Recursive

(define (expt b n)
  (if (= n 0)
      1
      (* b (expt b (- n 1)))))

; Iterative

(define (expt b n)
  (define (expt-iter acc base count)
    (if (= count 0)
        acc
        (expt-iter (* acc base) base (- count 1))))
  (expt-iter 1 b n))

; Algorithms + Threading

(require threading)
; (require algorithms) ;; TODO

(define (expt b n)
  (~> (repeat-n n b) ; TODO 
      (foldl * 1 _)))
