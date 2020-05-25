;;; Solutions here are vanilla r⁵rs scheme, apart from srfi-64 for
;;; testing.  And I avoid using features not yet introduced (e.g.,
;;; don't use "let" before exercise 1.34).

(import srfi-64) ;; "A Scheme API for test suites "
(test-runner-current (test-runner-simple))


;;; 1.19 (From section 2)

;; Tpq(a,b) = { a <- bq + aq + ap;
;;              b <- bp + aq }
;;
;; Tp'q'(a,b) = Tpq(Tpq(a,b))
;;            = Tpq(bq + aq + ap, bp + aq)
;;            = (bp + aq)q + (bq + aq + ap)q + (bq + aq + ap)p , (bp + aq)p + (bq + aq + ap)q
;;            = bpq + aqq + bqq + aqq + apq + bpq + apq + app  , bpp + apq + bqq + aqq + apq
;;            = 2bpq + 2aqq + bqq + 2apq + app                 , bpp + 2apq + bqq + aqq
;;            = b(2pq + qq) + a(2qq + 2pq + pp)                , b(pp + qq) + a(2pq + qq)
;;            = b(2pq + qq) + a((2pq + qq) + (qq + pp))        ,
;;            = b(2pq + qq) + a(2pq + qq) + a(pp + qq)         ,
;; p' = pp + qq
;; q' = 2pq + qq


(define (fib n)
  (define (fib-iter a b p q count)
    (cond ((= count 0) b)
          ((even? count)
           (fib-iter a
                     b
                     (+ (* p p) (* q q))   ; p'
                     (+ (* 2 p q) (* q q)) ; q'
                     (/ count 2)))
          (else (fib-iter (+ (* b q) (* a q) (* a p))
                          (+ (* b p) (* a q))
                          p
                          q
                          (- count 1)))))
  (fib-iter 1 0 0 1 n))

(test-group
 "ex19 - fib logarithmic"
 (define (fib-linear n)
   (define (fib-iter a b count)
     (if (= count 0)
         b
         (fib-iter (+ a b) a (- count 1))))
   (fib-iter 1 0 n))

 ;; Check that both computation methods give the same result.
 (define (test-fibs n)
   (test-eqv (fib-linear n) (fib n)))
 (define (test-fibs-upto from to)
   (test-fibs from)
   (if (< from to)
       (test-fibs-upto (+ from 1) to)))
 (test-fibs-upto 0 20))



;;; 1.29

(define (simpson-integral f a b n)
  (define h (/ (- b a) n))
  (define (y k)
    (f (+ a (* k h))))
  (define (sum-term n)
    (* (if (even? n) 2 4)
       (y n)))
  (define (iter i acc)
    (if (< i n)
        (iter (+ i 1) (+ acc (sum-term i)))
        acc))
  (* h 1/3 (+ (y 0) (iter 1 0) (y n))))

(define (cube x) (* x x x))

;; Even n = 2 gives exactly the correct answer.
(simpson-integral cube 0 1 2) ;=> 1/4


;;; 1.30
(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ (term a) result))))
  (iter a 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (tests-for-sum sum testname)
  (test-group
   testname

   (define (inc x) (+ x 1))

   (test-eqv "sum cubes" (sum cube 1 inc 10) 3025)
   (test-eqv "sum integers" (sum identity 1 inc 10) 55)

   (define (pi-sum a b)
     (define (pi-term x)
       (/ 1.0 (* x (+ x 2))))
     (define (pi-next x)
       (+ x 4))
     (sum pi-term a pi-next b))
   (test-approximate "approx pi"
     (* 8 (pi-sum 1 1000))
     3.139592655589783
     0.000000000000005)

   (define (integral f a b dx)
     (define (add-dx x) (+ x dx))
     (* (sum f (+ a (/ dx 2.0)) add-dx b)
        dx))
   (test-approximate "integrate cubic, step .01"
     (integral cube 0 1 0.01)
     .24998750000000042
     .0000005)
   (test-approximate "integrate cubic, step .001"
     (integral cube 0 1 0.001)
     .249999875000001
     .000000005)))

(tests-for-sum sum "ex1.30 - sum")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; 1.31

(define (product-recursive f a next b)  ; 1.31a
  (if (> a b)
      1
      (* (f a) (product f (next a) next b))))

(define (product f a next b)            ; 1.31b
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* (f a) result))))
  (iter a 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (tests-for-product product testname)
  (define (inc x) (+ x 1))

  (define (factorial n)
    (product identity 1 inc n))

  (define (pi-product upto)
    (define (² x) (* x x))
    (define (f n)
      (define x (* 2.0 n))
      (/ (² x)
         (* (- x 1) (+ x 1))))
    (* 2 (product f 1 inc upto)))

  (test-group
   testname
   (test-eqv "5! = 120" (factorial 5) 120)

   (test-approximate "approx pi"
     (pi-product 1000)
     3.141
     0.005)))

(tests-for-product product-recursive "ex1.31a - product recursive")
(tests-for-product product "ex1.31b - product iterative")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; 1.32

(define (accumulate-recursive combine null-value term a next b) ; 1.32a
  (if (> a b)
      null-value
      (combine (term a) (accumulate-recursive combine null-value term (next a) next b))))

(define (product-accumulate-recursive term a next b)
  (accumulate-recursive * 1 term a next b))
(define (sum-accumulate-recursive term a next b)
  (accumulate-recursive + 0 term a next b))

(define (accumulate combine null-value term a next b) ; 1.32b
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combine (term a) result))))
  (iter a null-value))

(define (product-accumulate term a next b)
  (accumulate * 1 term a next b))
(define (sum-accumulate term a next b)
  (accumulate + 0 term a next b))

(tests-for-product product-accumulate-recursive "ex1.32a - product accumulate recursive")
(tests-for-product product-accumulate "ex1.32b - product accumulate iterative")
(tests-for-sum sum-accumulate-recursive "ex1.32a - sum accumulate recursive")
(tests-for-sum sum-accumulate "ex1.32a - sum accumulate iterative")


;;; 1.34

(define (f g)
  (g 2))

;; (f f) => (f 2) => (2 2) => Error: call of non-procedure: 2


;;; 1.35

(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

;;      ϕ = (1 + √5)/2 ≈ 1.6180
;; is the "golden ratio", which satisfies the equation
;;      ϕ² = ϕ + 1
;;
;;      ϕ = ϕ/ϕ + 1/ϕ
;;      ϕ = 1 + 1/ϕ

(test-group
 "ex1.35 - fixed-point ϕ"
 (test-approximate "ϕ"
   (fixed-point (lambda (ϕ) (+ 1 (/ 1 ϕ))) 1.0)
   1.618033988749
   tolerance))


;;; 1.36

(define (fixed-point-verbose f first-guess)
  (define tolerance 0.00001)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (display next)
      (newline)
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(fixed-point-verbose (lambda (x) (/ (log 1000) (log x))) 2.0)

;; 9.96578428466209
;; 3.00447220984121
;; 6.27919575750716
;; 3.75985070240154
;; 5.2158437849259
;; 4.1822071924014
;; 4.82776509834459
;; 4.38759338466268
;; 4.6712500857639
;; 4.48140361689505
;; 4.6053657460929
;; 4.52308496787189
;; 4.57711468204734
;; 4.54138248015145
;; 4.56490324523083
;; 4.54937267930334
;; 4.55960649191329
;; 4.55285387578827
;; 4.55730552974826
;; 4.55436906443618
;; 4.556305311533
;; 4.55502826357355
;; 4.55587039670285
;; 4.55531500119208
;; 4.55568126354333
;; 4.55543971573685
;; 4.55559900999829
;; 4.55549395753139
;; 4.55556323729288
;; 4.55551754841765
;; 4.5555476793064
;; 4.55552780851625
;; 4.55554091291796
;; 4.55553227080365

(define (average x y)
  (/ (+ x y) 2))

(fixed-point-verbose (lambda (x) (average x (/ (log 1000) (log x)))) 2.0)

;; 5.98289214233104
;; 4.92216872130834
;; 4.62822431819546
;; 4.56834651313624
;; 4.5577305909237
;; 4.55590980904513
;; 4.55559941161062
;; 4.55554655214737
;; 4.55553755199982


;;; 1.37

(define (cont-frac-recursive n d k) ;; 1.37a
  (define (rec i)
    (let ((nᵢ (n i))
          (dᵢ (d i)))
      (/ nᵢ
         (if (= i k) dᵢ
             (+ dᵢ (rec (+ i 1)))))))
  (rec 1))

(define (cont-frac n d k) ;; 1.37b
  (define (iter i acc)
    (if (= i 0)
        acc
        (iter (- i 1)
              (/ (n i)
                 (+ (d i) acc)))))
  (iter k 0.0))

(test-group
 "ex1.37 - continued fraction"
 ;; 12 iterations required to get 4 digit accuracy.
 (test-eqv "continued fraction, recursive and iterative"
   (cont-frac (lambda (_) 1.0) (lambda (_) 1.0) 12)
   (cont-frac-recursive (lambda (_) 1.0) (lambda (_) 1.0) 12))
 (test-approximate "1/ϕ as continued fraction, accurate to 4 decimals"
   (/ 1 (cont-frac (constantly 1.0) (constantly 1.0) 12))
   1.61803
   0.00005))


;;; 1.38

(define (e-approx k)
  (+ 2 (cont-frac (lambda (_) 1.0)
                  (lambda (i)
                    (if (= (remainder i 3) 2)
                        (* (/ (+ i 1) 3) 2)
                        1))
                  k)))

(test-group
 "ex1.38 - Euler approximation for e"
 (test-approximate (e-approx 10) (exp 1) 0.00005))


;;; 1.40

(define dx 0.00001)

(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (cubic a b c)
  (lambda (x)
    (+ (* x x x)
       (* a x x)
       (* b x)
       c)))


;;; 1.41

(define (double f)
  (lambda (x) (f (f x))))

(define (inc n) (+ n 1))
(((double (double double)) inc) 5) ; => 21


;;; 1.42

(define (square x) (* x x))

(define (compose f g)
  (lambda (x) (f (g x))))

(test-group
 "ex1.42 - compose"
 (test-eqv 49 ((compose square inc) 6)))


;;; 1.43

(define (repeated-iter f n)
  (define (iter i acc)
    (if (= i n)
        acc
        (iter (+ i 1) (f acc))))
  (lambda (x)
    (iter 0 x)))

(define (repeated f n)
  (accumulate compose identity (lambda (_) f) 1 inc n))

(test-group
 "ex1.43 - repeated application"
 (test-eqv "repeated-iter" 625 ((repeated-iter square 2) 5))
 (test-eqv "repeated-accumulate" 625 ((repeated square 2) 5)))


;;; 1.44

(define (smooth f)
  (define (average x y z)
    (/ (+ x y z) 3))
  (lambda (x)
    (average (f (- x dx))
             (f x)
             (f (+ x dx)))))

(define (smooth-n f n)
  ((repeated smooth n) f))

(define (heaviside-step n)
  (if (< n 0) 0.0 1.0))

(heaviside-step -0.1)                   ;=> 0
(heaviside-step 0)                      ;=> 1

;; ((smooth heaviside-step) 0) 0.666666666666667
;; ((smooth-n heaviside-step 2) 0) 0.666666666666667
;; ((smooth-n heaviside-step 3) 0) 0.62962962962963
;; ((smooth-n heaviside-step 4) 0) 0.617283950617284
;; ((smooth-n heaviside-step 5) 0) 0.604938271604938
;; ((smooth-n heaviside-step 10) 0) 0.565394841572254

;; Approaching 0.5.  Too expensive to increase repeats much further,
;; because implementation calls f 3ⁿ times to evaluate n-times
;; smoothed f.

;; 3sf(x-dx) = f(x-2dx) + f(x-dx) + f(x)
;; 3sf(x)    =            f(x-dx) + f(x) + f(x+dx)
;; 3sf(x+dx) =                      f(x) + f(x+dx) + f(x+2dx)

;; Some repeated evaluatations of f, perhaps only 3 + 2n evaluations
;; really required?

;; 3ssf(x) = sf(x-dx) + sf(x) + sf(x+dx)
;; 9ssf(x) = f(x-2dx) + 2f(x-dx) + 3f(x) + 2f(x+dx) + f(x+2dx)
;; 3sssf(x)  = ssf(x-dx) + ssf(x) + ssf(x+dx)
;; 9sssf(x)  = sf(x-2dx) + 2sf(x-dx) + 3sf(x) + 2sf(x+dx) + sf(x+2dx)
;; 27sssf(x) = f(x-3dx) +  f(x-2dx) +  f(x-dx)
;;                      + 2f(x-2dx) + 2f(x-dx) + 2f(x)
;;                                  + 3f(x-dx) + 3f(x) + 3f(x+dx)
;;                                             + 2f(x) + 2f(x+dx) + 2f(x+2dx)
;;                                                     +  f(x+dx) +  f(x+2dx) + f(x+3dx)
;;           = f(x-3dx) + 3f(x-2dx) + 6f(x-dx) + 7f(x) + 6f(x+dx) + 3f(x+2dx) + f(x+3dx)
;;
;; 3ⁿsⁿf(x) = ?
