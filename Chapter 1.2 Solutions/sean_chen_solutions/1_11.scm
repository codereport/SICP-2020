; Recursive process
; (define (fn n)
;     (if (< n 3)
;     n
;     (+ 
;         (fn (- n 1)) 
;         (* (fn (- n 2)) 2) 
;         (* (fn (- n 3)) 3))
;     )
; )

; Iterative process
(define (fn n) 
    (define (iter a b c count) 
        (if (= count 0) 
            a 
            (iter b c (+ c (* 2 b) (* 3 a)) (- count 1)))) 
    (iter 0 1 2 n)) 

(display (fn 5))