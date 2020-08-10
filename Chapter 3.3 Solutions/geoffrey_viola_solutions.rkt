#lang racket

(require rackunit)
(require rnrs/mutable-pairs-6)
(require compatibility/mlist)
(require racket/set)

(define (pair-or-mpair x)
  (or (pair? x) (mpair? x)))

(define (car-or-mcar x)
  (cond ((pair? x) (car x))
        ((mpair? x) (mcar x))
        (error "neither pair nor mpair")))

(define (cdr-or-mcdr x)
  (cond ((pair? x) (cdr x))
        ((mpair? x) (mcdr x))
        (error "neither pair nor mpair")))

;; Exercise 3.16.
(define (count-pairs x)
  (if (not (pair-or-mpair x))
      0
      (+ (count-pairs (car-or-mcar x))
         (count-pairs (cdr-or-mcdr x))
         1)))

(define (recursive-pair)
  (define my-list (mcons 'a 'b))
  (set-mcdr! my-list my-list)
  (cons 'a (cons 'a my-list)))

(module+ test
  (begin
    (check-equal? (count-pairs (cons (cons (cons 'a 'b) 'b) 'b)) 3)
    (check-equal? (count-pairs (list (cons (cons 'a 'b) 'b) 'b)) 4)
    (check-equal? (count-pairs (list (list 'a (list 'a (cons 'a 'b))) 'b)) 7)
    ;; infinite
    ;(check-equal? (count-pairs (recursive-pair)) 3)
    ))

;; Exercise 3.17.

(define (count-pairs-id-aware x)
  (define seen (mutable-set))
  (define (iter x)
    (if (or (not (pair-or-mpair x))
            (set-member? seen (eq-hash-code x))
            (null? (cdr-or-mcdr x)))
        0
        (begin
          (set-add! seen (eq-hash-code x))
          (+ (iter (car-or-mcar x))
             (iter (cdr-or-mcdr x))
             1))))
  (iter x))

(module+ test
  (begin
    (check-equal? (count-pairs-id-aware (cons (cons (cons 'a 'b) 'b) 'b)) 3)
    (check-equal? (count-pairs-id-aware (list (cons (cons 'a 'b) 'b) 'b)) 3)
    (check-equal? (count-pairs-id-aware (list (list 'a (list 'a) 'b) 'b)) 3)
    (check-equal? (count-pairs-id-aware (recursive-pair)) 3)
    ))

;; Exercise 3.21.

;; From book
(define (front-ptr queue) (mcar queue))
(define (rear-ptr queue) (mcdr queue))
(define (set-front-ptr! queue item) (set-car! queue item))
(define (set-rear-ptr! queue item) (set-cdr! queue item))
(define (empty-queue? queue) (null? (front-ptr queue)))
(define (make-queue) (mcons '() '()))
(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with an empty queue" queue)
      (mcar (front-ptr queue))))
(define (insert-queue! queue item)
  (let ((new-pair (mcons item '())))
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
        (else
         (set-front-ptr! queue (mcdr (front-ptr queue)))
         queue)))

;; My code
(define (print-queue q)
  (~a (front-ptr q)))

(module+ test
  (begin
    (define (get-empty-queue)
      (define q (make-queue))
      (insert-queue! q 'a)
      (delete-queue! q)
      q)
    (define (get-filled-queue)
      (define q (make-queue))
      (insert-queue! q 'a)
      (insert-queue! q 'b)
      (insert-queue! q 'c)
      q)
    (check-equal? (print-queue (make-queue)) "()")
    (check-equal? (print-queue (get-empty-queue)) "()")
    (check-equal? (print-queue (get-filled-queue)) "{a b c}")
    ))

;; Exercise 3.23.
(define (front-dequeue-ptr dequeue) (mcar dequeue))
(define (rear-dequeue-ptr dequeue) (mcdr dequeue))
(define (set-dequeue-front-ptr! dequeue item) (set-car! dequeue item))
(define (set-dequeue-rear-ptr! dequeue item) (set-cdr! dequeue item))
(define (empty-dequeue? dequeue) (null? (front-dequeue-ptr dequeue)))
(define (make-dequeue) (mcons '() '()))
(define (front-dequeue dequeue)
  (if (empty-dequeue? dequeue)
      (error "FRONT called with an empty queue" dequeue)
      (mcar (front-dequeue-ptr dequeue))))
(define (rear-dequeue dequeue)
  (if (empty-dequeue? dequeue)
      (error "FRONT called with an empty queue" dequeue)
      (mcar (rear-dequeue-ptr dequeue))))
(define (front-insert-dequeue! dequeue item)
  ;; doubly linked list {item, next, prev}
  (let ((new-pair (mlist item (front-dequeue-ptr dequeue) '())))
    (cond ((empty-dequeue? dequeue)
           (set-dequeue-front-ptr! dequeue new-pair)
           (set-dequeue-rear-ptr! dequeue new-pair)
           dequeue)
          (else
           (set-dequeue-front-ptr! dequeue new-pair)
           dequeue))))
(define (rear-insert-dequeue! dequeue item)
  (let ((new-pair (mlist item '() (rear-dequeue-ptr dequeue))))
    (cond ((empty-dequeue? dequeue)
           (set-dequeue-front-ptr! dequeue new-pair)
           (set-dequeue-rear-ptr! dequeue new-pair)
           dequeue)
          (else
           (set-cdr! (rear-dequeue-ptr dequeue) new-pair)
           (set-dequeue-rear-ptr! dequeue new-pair)
           dequeue))))
(define (front-delete-dequeue! dequeue)
  (cond ((empty-dequeue? dequeue)
         (error "DELETE! called with an empty queue" dequeue))
        (else
         (set-dequeue-front-ptr! dequeue (mcar (mcdr (front-dequeue-ptr dequeue))))
         dequeue)))
(define (rear-delete-dequeue! dequeue)
  (cond ((empty-dequeue? dequeue)
         (error "DELETE! called with an empty queue" dequeue))
        (else
         (set-dequeue-rear-ptr! dequeue (mcar (mcdr (mcdr (rear-dequeue-ptr dequeue)))))
         dequeue)))

(module+ test
  (begin
    (define (get-dequeue-abc)
      (define dequeue (make-dequeue))
      (front-insert-dequeue! dequeue 'b)
      (front-insert-dequeue! dequeue 'a)
      (rear-insert-dequeue! dequeue 'c)
      dequeue)
    (check-equal? (empty-dequeue? (make-dequeue)) #t)
    (check-equal? (front-dequeue (get-dequeue-abc)) 'a)
    (check-equal? (rear-dequeue (get-dequeue-abc)) 'c)
    (check-equal? (front-dequeue (front-delete-dequeue! (get-dequeue-abc))) 'b)
    (check-equal? (rear-dequeue (rear-delete-dequeue! (get-dequeue-abc))) 'b)
    ))

;;  Exercise 3.25.
pass
