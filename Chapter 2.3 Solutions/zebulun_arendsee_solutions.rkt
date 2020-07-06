#lang racket/base


; 2.54 ------------------------------------------------------------------------
(define
  (equal? xs ys)
  (cond
    ( (and (null? xs) (null? ys)) #t )
    ( (and (pair? xs) (pair? ys)) (and (equal? (car xs) (car ys)) (equal? (cdr xs) (cdr ys))) )
    ( (and (not (pair? xs)) (not (pair? ys))) (eq? xs ys) )
    ( #t #f )
  )
)
(equal? '(a b c) '(a b c))   ; #t
(equal? '(a b) '(a b c))     ; #f
(equal? '(a b c) '((a b) c)) ; #f
(equal? '() '())             ; #t
(equal? 'a 'a)               ; #t
(equal? '(a) '(a))           ; #t
(equal? 'a 'b)               ; #f

; 2.55 ------------------------------------------------------------------------
; ''abracadabra == (quote abracadabra)
; '''abracadabra == (quote (quote abracadabra))
; '...'abracadabra == (quote ... (quote abracadabra))
; ergo, car ''abracadabra == quote


; 2.59 ------------------------------------------------------------------------
(define
  (member? x xs)
  ( cond
    ((null? xs) #f)
    ((equal? x (car xs)) #t)
    (#t (member? x (cdr xs)))
  )
)

; O(n^2) time
(define
  (union xs ys)
  ( cond
    ((or (null? xs)) ys)
    ((member? (car xs) ys) (union (cdr xs) ys))
    (#t (cons (car xs) (union (cdr xs) ys)))
  )
)
;;; in haskell (also O(n^2) time)
; union :: [a] -> [a] -> [a]
; union [] ys = ys
; union (x:xs) ys
;  | elem x ys = union xs ys
;  | otherwise = x : union xs ys
