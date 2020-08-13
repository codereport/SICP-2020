#lang racket
(require rackunit)
(require racket/list)
(require control)

; Ex 3.38

; a)
; peter paul mary=45
; paul peter mary=45
; peter mary paul=35
; mary peter paul=40
; paul mary peter=50
; mary paul peter=40

; b)
; ->peter->paul->peter->mary->paul->mary->mary = 25
; ->paul->mary->paul->mary->peter->peter->mary = 30
; ->peter->peter->mary->paul->paul->mary->mary = 35
; ->mary->mary->mary->paul->paul->peter->peter = 40
; ->paul->paul->peter->peter->mary->mary->mary = 45
; ->paul->paul->peter->mary->peter->mary->mary = 50
; ->peter->peter->paul->mary->mary->paul->mary = 55
; ->paul->peter->mary->paul->peter->mary->mary = 60
; ->peter->paul->paul->mary->peter->mary->mary = 70
; ->peter->mary->mary->paul->mary->peter->paul = 80
; ->mary->peter->peter->paul->mary->mary->paul = 90
; ->mary->paul->mary->peter->paul->mary->peter = 110

(define (peter-paul-mary)
  
  (define balance 100)
  
  (define peter-state-mach
    (let ((tmp 0)
          (state 'initial))
      (list
       (cons 'reset (lambda ()  (set! tmp 0) (set! state 'initial)))
       (cons 'initial (lambda ()  (set! tmp (+ balance 10)) (set! state 's0)))
       (cons 's0 (lambda () (set! balance tmp) (set! state 'done)))
       (cons 'query (lambda () state))
       (cons 'done (lambda () '()))
       )
      ))
  
  (define paul-state-mach
    (let ((tmp 0)
          (state 'initial))
      (list
       (cons 'reset (lambda () (set! tmp 0) (set! state 'initial)))
       (cons 'initial (lambda () (set! tmp (- balance 20)) (set! state 's1)))
       (cons 's1 (lambda () (set! balance tmp) (set! state 'done)))
       (cons 'query (lambda () state))
       (cons 'done (lambda () '()))
       )
      ))
    

  (define mary-state-mach
    (let ((tmp 0)
          (state 'initial))
      (list
       (cons 'reset (lambda ()  (set! tmp 0) (set! state 'initial)))
       (cons 'initial (lambda () (set! tmp (/ balance 2)) (set! state 's1)))
       (cons 's1 (lambda () (set! tmp (- balance tmp)) (set! state 's2)))       
       (cons 's2 (lambda () (set! balance tmp) (set! state 'done)))
       (cons 'query (lambda () state))
       (cons 'done (lambda () '()))
       )
      ))
  (lambda (op)    
    (cond ((eq? op 'reset) (set! balance 100))
          ((eq? op 'get-jobs)
           (list (cons 'peter peter-state-mach)
                 (cons 'paul paul-state-mach)
                 (cons 'mary mary-state-mach)))
          ((eq? op 'result) balance)
          )
    ))
  
(define (job-get-state-and-trans job trans-name)
  (car (filter (lambda (t) (eq? (car t) trans-name) ) (cdr job))))

(define (job-get-name job)
  (car job))

(define (job-get-state job)
  ((cdr (job-get-state-and-trans job 'query))))
  
(define (job-reset! job)
  ((cdr (job-get-state-and-trans job 'reset))))

(define (job-step! job)
  (let* ((state (job-get-state job))
         (st (job-get-state-and-trans job state)))
    ((cdr st))))

(define (complete-job! job)
  (while (not (eq? (job-get-state job) 'done))
    (job-step! job)))
  
(define (run-sequential get-system)
  (define system (get-system))
  (define runs (permutations (system 'get-jobs)))
  (for ([seq runs])
    (system 'reset)
    (printf ";")
    (for ([job seq])
      (job-reset! job)
      (printf " ~a" (job-get-name job))      
      (complete-job! job)
      )
    (printf "=~a\n" (system 'result) )    
    ))


;(run-sequential peter-paul-mary)

(define (run-random get-system ntimes)
  (define system (get-system))
  (define jobs (system 'get-jobs))
  (define results '())
  (while (>= ntimes 0)
    (system 'reset)
    (for-each job-reset! jobs)
    (define sequence "")
    (while (not (andmap (lambda (job) (eq? (job-get-state job) 'done)) jobs))
      (let ((job (car (shuffle jobs))))
        (unless (eq? (job-get-state job) 'done)
          (set! sequence (string-append sequence
                                        (format "->~a" (job-get-name job))))
          (job-step! job))))
    (set! results (cons (cons (system 'result) sequence) results))
    (set! ntimes (- ntimes 1))
    )
  
  (for ([result (sort (remove-duplicates results
                                         (lambda (a b) (= (car a) (car b))))
                      (lambda (a b) (< (car a) (car b))))
                ])
    (printf "; ~a = ~a\n" (cdr result) (car result)))
  )

;(run-random peter-paul-mary 5000)


; Ex. 3.39

; (define x 10)

; Case 1
;(parallel-execute
; (lambda () (set! x (* x x)))
; (lambda () (set! x (+ x 1))))

; Case 2
; (define s (make-serializer))
;(parallel-execute
; (lambda () (set! x ((s (lambda () (* x x))))))
; (s (lambda () (set! x (+ x 1)))))

; YES - 101: P1 sets x to 100 and then P2 increments x to 101.
; YES - 121: P2 increments x to 11 and then P1 sets x to x * x.
; NO - 110: P2 changes x from 10 to 11 between the two times that
; P1 accesses the value of x during the evaluation of (* x x).
; NO - 11: P2 accesses x, then P1 sets x to 100, then P2 sets x.
; YES - 100: P1 accesses x (twice), then P2 sets x to 11, then P1 sets x.

; Ex 3.40
; (define x 10)
; (lambda () (set! x (* x x)))
;  (lambda () (set! x (* x x x)))
; a) 10^6, 10^2 , 10^3 , 10^4, 10^5
; b) 10^6


; Ex. 3.41
; NO because balance is set only once per call (and Ben knows this)
; This could be dangerous:
;(define (deposit amount)
; (set! balance (+ balance (/ amount / 2))
; Ben Bitdiddle reads balance here:
; (set! balance (+ balance (/ amount / 2))      
;  balance)

; Ex. 3.42
; Safe - coarse grained lock 


; Ex. 3.47 a

#|
(define (make-semaphore n)
  (let ((sync (make-mutex)))    
    (lambda (op)
      (cond ((eq? op 'wait) (let ((spin #t))
                              (while spin
                                (sync 'acquire)
                                (when (>= (- n 1) 0)
                                  (set! n (- n 1))
                                  (set! spin #f))
                                (sync 'release))))
            ((eq? op 'post) (begin
                              (sync 'acquire)
                              (set! n (+ n 1))
                              (sync 'release)))))))
|#

