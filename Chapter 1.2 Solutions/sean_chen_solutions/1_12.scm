(define (pascal row col)
    (cond ((or (< row col)
               (< col 1)) 0 )
        ((or (= col 1)
               (= col row)) 1 )
        (else (+ (pascal (- row 1) (- col 1))
                   (pascal (- row 1) col )))))

(display (pascal 1 1))