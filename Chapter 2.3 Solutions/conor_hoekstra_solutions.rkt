;; Exercise 2.54 (page 196)

(require rackunit)

(define (equal? a b)
  (cond ((and (null? a) (null? b)) #t)
        ((or (null? a) (null? b)) #f)
        ((eq? (car a) (car b)) (equal? (cdr a) (cdr b)))
        (else #f)))
     
(check-equal? (equal? '(0 1 2) (range 3)) #t)
(check-equal? (equal? '(0 1 2) (range 2)) #f)
