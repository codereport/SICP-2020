(define (inc x) (+ x 1))
(define (dec x) (- x 1))

; This is a recursive process since every recursive
; step is defined in terms of the result of another 
; recursive step that happens further down the line.
; In other words, each recursive step is deferred 
; until its own recursive call is resolved and 
; returns a result that the caller can then use
; to calculate its own result. 
(define (foo a b)
    (if (= a 0)
    b
    (inc (foo (dec a) b))))

; This is an iterative process since the 
; recursive call is the "outermost" operation.
; In other words, each recursive call is 
; "independent" of any others since none of the
; calls strictly depend on the result of a 
; subsequent recursive call. 
(define (bar a b)
    (if (= a 0)
    b
    (bar (dec a) (inc b))))

(display (foo 4 5))

(display (bar 4 5))