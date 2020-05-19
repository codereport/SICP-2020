;;; 1.11

(define (frec n)
  (if (< n 3) n
      (+ (frec (- n 1))
         (* 2 (frec (- n 2)))
         (* 3 (frec (- n 3))))))

;; f(n)   = f(n - 1) + 2f(n - 2) + 3f(n - 3)
;; f(n+3) = f(n + 2) + 2f(n + 1) + 3f(n)

(define (f-iterative n)
  (define (iter fi+0 fi+1 fi+2 i+2)
    (if (= i+2 n) fi+2
        (iter fi+1 fi+2
              (+ fi+2
                 (* 2 fi+1)
                 (* 3 fi+0))
              (+ i+2 1))))
  (if (< n 3) n
      (iter 0 1 2 2)))


;;; 1.12

(define (pascal-triangle level i)
  (cond ((or (< i 0) (< level i)) 0)
        ((= level 0) 1)
        (#t
         (+ (pascal-triangle (- level 1) (- i 1))
            (pascal-triangle (- level 1) i)))))

(define (number-sequence n)
  (define (iter i list)
    (if (= i n) list
        (iter (+ i 1) (cons i list))))
  (iter 0 '()))

(define (pascal-row level)
  (map (lambda (i) (pascal-triangle level i))
       (number-sequence (+ level 1))))


;;; 1.16

(define (fast-iter-expt b n)
  (define (iter b n acc)
    (cond
     ((= n 0) acc)
     ((even? n) (iter (* b b) (/ n 2) acc))
     (#t (iter b (- n 1) (* b acc)))))
  (iter b n 1))


;;; 1.17

(define (mult a b)
  (define (double x) (+ x x))
  (define (halve x) (/ x 2))
  (cond
   ((= b 0) 0)
   ((= b 1) a)
   ((even? b) (double (mult a (halve b))))
   (#t (+ a (mult a (- b 1))))))


;;; 1.18

(define (mult a b)
  (define (double x) (+ x x))
  (define (halve x) (/ x 2))
  (cond
   ((= b 0) 0)
   ((= b 1) a)
   ((even? b) (mult (double a) (halve b)))
   (#t (mult (+ a b) (- b 1)))))


;;; 1.27

;; (define (expmod base exp m)
;;   (define (square x) (* x x))
;;   (cond ((= exp 0) 1)
;;         ((even? exp)
;;          (remainder (square (expmod base (/ exp 2) m))
;;                     m))
;;         (else
;;          (remainder (* base (expmod base (- exp 1) m))
;;                     m))))

(define (expmod b n m)
  (define (iter b n acc)
    (cond
     ((= n 0) acc)
     ((even? n) (iter (remainder (* b b) m) (/ n 2) acc))
     (#t (iter b (- n 1) (remainder (* b acc) m)))))
  (iter b n 1))

(define (fermat-thorough-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (define (all i)
    (cond
     ((>= i n) #t)
     ((try-it i)
      (all (+ i 1)))
     (else #f)))
  (all 1))


;;; 1.28

(define (expmod-sqrt1 b n m)
  (define (iter b n acc)
    (cond
     ((= n 0) acc)
     ((even? n)
      (let ((sq (remainder (* b b) m)))
        (if (and (= sq 1)
                 (not (= b 1)) (not (= b (- m 1))))
            0 ;; nontrivial square root of 1 mod m
            (iter sq (/ n 2) acc))))
     (#t (iter b (- n 1) (remainder (* b acc) m)))))
  (iter b n 1))

(import (only (chicken random) pseudo-random-integer))

(define (miller-rabin n)
  (define (try-it a)
    (= 1 (expmod-sqrt1 a (- n 1) n)))
  (define (iter times)
    (if (= times 0) #t
        (and (try-it (+ 1 (pseudo-random-integer (- n 1))))
             (iter (- times 1)))))
  (iter 10))
