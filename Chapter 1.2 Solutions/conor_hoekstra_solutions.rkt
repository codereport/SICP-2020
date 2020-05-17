; Factorial (page 41-43)

; Recursive solution

(define (fact n)
  (if (= n 0)
      1
      (* n (fact (- n 1)))))

; Iterative

(define (fact n)
  (define (fact-iter acc n)
    (if (= n 0)
        acc
        (fact-iter (* n acc) (- n 1))))
  (fact-iter 1 n))

; Algorithms + Threading

(require threading)

(define (fact n)
  (~> (range 1 (+ n 1))
      (foldl * 1 _)))

; Fibonacci (page 47-50)

; Recursive

(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))

; Iterative

(define (fib n)
  (fib-iter 1 0 n))

(define (fib-iter a b count)
  (if (= count 0)
      b
      (fib-iter (+ a b) a (- count 1))))

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
        (else (f-iter b 
                      c 
                      (+ c 
                         (* 2 b) 
                         (* 3 a)) 
                      (- count 1)))))

; > (map f (range 10))
; '(0 1 2 4 11 25 59 142 335 796)

; Exercise 1.12

(require threading)
(require algorithms)

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
      (sliding 2)
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
(require algorithms)

(define (expt b n)
  (~> (repeat n b)
      (foldl * 1 _)))

; Fast exponentiation

; Recursive

(define (square x) (* x x))

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

; Exercise 1.16 (59/60)

; Iterative

(define (fast-expt b n)
  (define (iter squares extra N)
    (cond ((= N 1) (* squares extra))
          ((even? N) (iter (square squares) extra (/ N 2)))
          (else (iter squares (* extra squares) (- N 1)))))
  (iter b 1 n))

; Exercise 1.17

; Recursive

; there is a bug in this code I originally wrote
(define (multiply a b)
  (cond ((= a 1) b)
        ((even? a) (multiply (/ a 2) (double b)))
        (else (+ b (multiply (- a 1) b)))))

; corrected code
(define (multiply a b)
  (cond ((= a 0) 0)
        ((even? a) (multiply (/ a 2) (double b)))
        (else (+ b (multiply (- a 1) b)))))

; Exercise 1.18

; this is totally broken
(define (multiply a b)
  (define (iter A B acc)
    (cond ((= B 0) acc)
          ((even? B) (iter A (/ B 2) (double A)))
          (else (iter A (- B 1) (+ acc A)))))
  (iter a b 0))

; corrected code (after looking at http://community.schemewiki.org/?sicp-ex-1.18)
(define (multiply a b)
  (define (iter A B acc)
    (cond ((= B 0) acc)
          ((even? B) (iter (double A) (/ B 2) acc)) ; very interesting that acc isn't modified in this iteration
          (else (iter A (- B 1) (+ acc A)))))
  (iter a b 0))

; GCD

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

; Exercise 1.22 (page 70/71)

(define (smallest-divisor n)
  (define (iter i)
    (cond ((= (remainder n i) 0) i)
          ((> (* i i) n) n)
          (else (iter (+ i 1)))))
  (iter 2))

(define (prime? n)
  (= (smallest-divisor n) n))

(define (timed-prime-test n)
  (start-prime-test n (current-milliseconds)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (current-milliseconds) start-time) n)
      (display "")))

(define (report-prime elapsed-time n)
  (display n)
  (display " *** ")
  (display elapsed-time)
  (newline))
  
; > (timed-prime-test 10000000000037)
; 10000000000037 *** 116

; Exercise 1.27 (page 73)

; Copied from text

; prime? from above

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder
          (square (expmod base (/ exp 2) m))
          m))
        (else
         (remainder
          (* base (expmod base (- exp 1) m))
          m))))

(define (square x) (* x x))

; Modified

; Don't want a random number (pass it in)
(define (fermat-test n a)
  (= (expmod a n n) a))

; My code

(define (is-carmichael-number? n)
  (define (fermat-test-all n)
    (~> (range 1 n)
        (andmap (λ (a) (fermat-test n a)) _)))
  (and (fermat-test-all n) (not (prime? n))))

; > (map is-carmichael-number? '(561 1105 1729 2465 2821 6601))
; '(#t #t #t #t #t #t)
