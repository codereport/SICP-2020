(define (double x) (* x 2))

(define (halve x) (/ x 2))

(define (fast-mult a b)
    (define (iter x y z)
        (cond ((= y 0) z)
            ((even? y) (iter (double x) (halve y) z))
            (else (iter x (- y 1) (+ z x)))))
    (iter a b 0))

(display (fast-mult 2 4))
