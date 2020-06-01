;; Code from book needed for Exercise 2.1

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(define (make-rat n d)
  (let ((g (gcd n d)))
    (cons (/ n g) (/ d g))))

(define numer car)
(define denom cdr)

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

;; Exercise 2.1 (page 118)

(define (make-rat n d)
  (let ((g ((if (< d 0) - +) (abs (gcd n d)))))
    (cons (/ n g) (/ d g))))
    
 ;; Exercise 2.2 (page 121/2)
 
(define make-segment cons)
(define start-segment car)
(define end-segment cdr)

(define make-point cons)
(define x-point car)
(define y-point cdr)

(define (segment-midpoint segment)
  (let ((x1 (x-point (start-segment segment)))
        (x2 (x-point (end-segment segment)))
        (y1 (y-point (start-segment segment)))
        (y2 (y-point (end-segment segment))))
  (make-point (/ (+ x1 x2) 2.0)
              (/ (+ y1 y2) 2.0))))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

;; > (define a (make-point -1 -1))
;; > (define b (make-point 5 2))
;; > (define line (make-segment a b))
;; > (print-point (segment-midpoint line))
;; 
;; (2.0,0.5)

;; Exercise 2.3 (page 122)

;; Note: technically you can implement make-rectangle with only 3 points but
;;       for the sake of simplicity I will use 4 (and assume clockwise)

(define (make-rectangle a b c d)
  (list a b c d))

(define first-point first)                             ; car
(define (second-point rect) (first (rest rect)))       ; cadr
(define (third-point rect) (first (rest (rest rect)))) ; caddr

(define (sq x) (* x x))

(define (segment-length segment)
  (let* ((x1 (x-point (start-segment segment)))
         (x2 (x-point (end-segment segment)))
         (y1 (y-point (start-segment segment)))
         (y2 (y-point (end-segment segment)))
         (xdiff (- x1 x2))
         (ydiff (- y1 y2)))
    (cond ((= xdiff 0) (abs ydiff))
          ((= ydiff 0) (abs xdiff))
          (else (sqrt (+ (sq xdiff)
                         (sq ydiff)))))))

(define (rectangle-width rect)
  (segment-length
   (make-segment (first-point rect)
                 (second-point rect))))

(define (rectangle-height rect)
  (segment-length
   (make-segment (second-point rect)
                 (third-point rect))))

(define (rectangle-area rect)
  (* (rectangle-width rect)
     (rectangle-height rect)))

(define (rectangle-perimeter rect)
  (* 2 (+ (rectangle-width rect)
          (rectangle-height rect))))

(define a (make-point 0 0))
(define b (make-point 0 3))
(define c (make-point 4 3))
(define d (make-point 4 0))
(define r (make-rectangle a b c d))

(define aa (make-point 0 0))
(define bb (make-point -1 1))
(define cc (make-point 0 2))
(define dd (make-point 1 1))
(define rr (make-rectangle aa bb cc dd))

;; > (rectangle-area r)
;; 12
;; > (rectangle-perimeter r)
;; 14
;; > (rectangle-area rr)
;; 2.0000000000000004
;; > (rectangle-perimeter rr)
;; 5.656854249492381 ;; verify 4 * sqrt 2 = 5.65685424949

;; Exercise 2.4 (page 125)

(car (cons x y))
(car (lambda (m) (x y)))
(lambda (lambda (p q) p) (x y))
(lambda (x y) x)
(x) ;; :) 

(define (cdr z)
  (z (lambda (p q) q)))
  
;; Exercise 2.5 (page 125)

(require algorithms) ;; repeat, product
(require threading)

(define (positive-pow base exp)
  (~>> (repeat exp base)
       (product)))

(define (make-pair a b)
  (* (positive-pow 2 a)
     (positive-pow 3 b)))

(define (factor-out n factor)
  (define (iter N acc)
    (if (< 0 (remainder N factor))
        acc
        (iter (/ N factor) (+ 1 acc))))
  (iter n 0))
        
(define (pair-fst pair) (factor-out pair 2))
(define (pair-snd pair) (factor-out pair 3))

;; > (define p (make-pair 4 6))
;; > (pair-fst p)
;; 4
;; > (pair-snd p)
;; 6

;; Exercise 2.6

(define one (lambda (f) (lambda (x) (f x)))) 
(define two (lambda (f) (lambda (x) (f (f x))))) 

;; taken from http://community.schemewiki.org/?sicp-ex-2.6
 (define (add a b) 
   (lambda (f) 
     (lambda (x) 
       ((a f) ((b f) x))))) 
