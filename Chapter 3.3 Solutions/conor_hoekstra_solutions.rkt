;; Exercise 3.12 (page 345-6)

;; Response from (cdr x) after append
'(b)

;; Response from (cdr x) after append!
'(b c d) ; equivalent of passing something by reference and then modifying

;; Exercise 3.14 (page 347)

;; mystery reverses a list

;; Exercise 3.16 (350-1)

(count-pairs '(1 2 3)) ; -> 3

(define x (cons 1 '()))
(define y (cons x x))
(define z (cons y '()))
(count-pairs z) ; -> 4

(define x (cons 1 '()))
(define y (cons x x))
(define z (cons y y))
(count-pairs z) ; -> 7

;; for box and pointer diagrams, see conor_hoekstra_solutions.md

;; Exercise 3.17 (page 351)

(define seen '())

(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         (if (memq x seen)
             0
             (begin (set! seen (cons x seen)) 1)))))
                    

(require rackunit)

(check-equal? (count-pairs '(1 2 3)) 3)

(define x (cons 1 '()))
(define y (cons x x))
(define z (cons y '()))
(check-equal? (count-pairs z) 3)

(set! seen '())

(define a (cons 1 '()))
(define b (cons a a))
(define c (cons b b))
(check-equal? (count-pairs c) 3)

;; TODO decide if we should do 3.18 / 3.19

;; from book (note use #sicp -- aka MIT Scheme -- for set-car!/set-cdr! ; Racket and Scheme don't have it)

(define (front-ptr queue) (car queue))
(define (rear-ptr queue)  (cdr queue))
(define (set-front-ptr! queue item) (set-car! queue item))
(define (set-rear-ptr!  queue item) (set-cdr! queue item))

(define (make-queue) (cons '() '()))

(define (empty-queue? queue) (null? (front-ptr queue)))

(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with an empty queue" queue)
      (car (front-ptr queue))))

(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (else
           (set-cdr! (rear-ptr queue) new-pair)
           (set-rear-ptr! queue new-pair)
           queue))))

(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "DELETE! called with an empty queue" queue))
        (else (set-front-ptr! queue (cdr (front-ptr queue)))
              queue)))

;; Exercise 3.21 (page 359)

(define q1 (make-queue))
(insert-queue! q1 'a) ;((a) a)
(insert-queue! q1 'b) ;((a b) b)
(insert-queue! q1 'c) ;((a b c) c)
(delete-queue! q1)    ;((b c) c)
(delete-queue! q1)    ;((c) c)

;; front-ptr essentially grows as a list to represent the queue, back-ptr is
;; just for inserting. print should just print front-ptr

(define (print-queue queue) (front-ptr queue))

;; then just update insert and delete

(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           (print-queue queue))
          (else
           (set-cdr! (rear-ptr queue) new-pair)
           (set-rear-ptr! queue new-pair)
           (print-queue queue)))))

(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "DELETE! called with an empty queue" queue))
        (else (set-front-ptr! queue (cdr (front-ptr queue)))
              (print-queue queue))))

;; Exercise 3.23 

;; TODO

;; Exercise 3.25

;; TODO
