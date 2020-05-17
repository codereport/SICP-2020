(define (square n) (* n n))

; Recursive fast exponent procedure
; (define (rec-fast-expt b n)
;     (cond ((= n 0) 1)
;         ((even? n) (square (rec-fast-expt b (/ n 2))))
;         (else (* b (rec-fast-expt b (- n 1))))))

; Iterative fast exponent procedure
(define (iter-fast-expt b n)
    ; z holds the answer we've computed thus far in the procedure
    (define (iter x y z)
        (cond ((= x 0) z)
        ((even? x) (iter (/ x 2) (square y) z))
        (else (iter (- x 1) y (* y z)))))
    (iter b n 1))

(display (iter-fast-expt 5 5))
