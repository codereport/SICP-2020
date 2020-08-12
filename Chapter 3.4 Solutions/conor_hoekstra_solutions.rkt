;; Exercise 3.38 (page 409-10)

a) 

10+÷∘2⊣¯20+100 ⍝ 50
÷∘2⊣¯20+10+100 ⍝ 45
¯20+10+÷∘2⊣100 ⍝ 40
¯20+÷∘2⊣10+100 ⍝ 35

b) worst case would be:

¯20+÷∘2⊣100 ⍝ 30

;; Exercise 3.39 (page 414)

;; YES -> 101: P 1 sets x to 100 and then P 2 increments x to 101.
;; YES -> 121: P 2 increments x to 11 and then P 1 sets x to x * x .
;; NO  -> 110: P 2 changes x from 10 to 11 between the two times that
;;             P 1 accesses the value of x during the evaluation of (* x x) .
;; YES -> 11:  P 2 accesses x , then P 1 sets x to 100, then P 2 sets x . -> lambda could eval (* x x) and then +1 op could get 10 before set! take place
;; NO  -> 100: P 1 accesses x (twice), then P 2 sets x to 11, then P 1 sets x .

;; Exercise 3.40 (page 414)

;; a) 100 1K 10K 100K 1M
;; b) 1M

;; Exercise 3.41 (page 414-5)

;; Not a mutator so no need to serialize

;; Exercise 3.42 (page 415-6)

;; They extra let expression will create an additional frame in the environment model 
;; - but other than that they are the same

;; make-serializer / make-mutex from book

(define (make-serializer)
  (let ((mutex (make-mutex)))
    (lambda (p)
      (define (serialized-p . args)
        (mutex 'acquire)
        (let ((val (apply p args)))
          (mutex 'release)
          val))
      serialized-p)))

(define (make-mutex)
  (let ((cell (list false)))
    (define (the-mutex m)
      (cond ((eq? m 'acquire)
             (if (test-and-set! cell)
                 (the-mutex 'acquire))) ; retry
            ((eq? m 'release) (clear! cell))))
    the-mutex))

(define (clear! cell) (set-car! cell false))

(define (test-and-set! cell)
  (if (car cell)
      true
      (begin (set-car! cell true)
             false)))

;; Exercise 3.47 a) (page 425)

;; original (incorrect) solution

(define (make-semaphore n)
  (let ((count 0))
    (define (the-semaphore msg)
      (cond ((eq? msg 'acquire)
             (if (>= count n)
                 (the-semaphore 'acquire)
                 (set! count (+ count 1))))
            ((eq? msg 'release)
             (set! (count (- count 1))))))
    the-semaphore))
    
;; modified (correct) solution

(define (make-semaphore n)
  (let ((count 0)
        (lock (make-mutex)))
    (define (the-semaphore msg)
      (cond ((eq? msg 'acquire)
             (lock 'acquire)
             (if (>= count n)
                 (begin (lock 'release) (the-semaphore 'acquire))
                 (begin (set! count (+ count 1) (lock 'release)))))
            ((eq? msg 'release)
             (lock 'aquire)
             (set! (count (- count 1)))
             (lock 'release))))
    the-semaphore))
