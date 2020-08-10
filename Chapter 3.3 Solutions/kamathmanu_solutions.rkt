#lang sicp

; Exercise 3.16 - See other pdf for box-ptr diagrams

; returns 3 
(count-pairs '(a b c))
; returns 4
(define 3rd (cons 'a '()))
(define 2nd (cons 3rd 3rd))
(define lst (cons 2nd '()))
(count-pairs lst)
; returns 7
(define x (cons 2nd 2nd))
(count-pairs x)
; returns never - idea is an infinitely looping list
; I'm not sure if that's what "never returns" means - technically it is SO
(define a (cons '() '()))
(define b (cons '() '()))
(set-car! a b)
(set-car! b a)
(count-pairs a)

; Exercise 3.17

; Hash to keep track of visited nodes

(define visited-nodes '())

(define (visited? x)
  (if (not (memq x visited-nodes))
      (begin (set! visited-nodes (cons x visited-nodes)) 1)
      0))

(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         (visited? x))))

; Tests
(set! visited-nodes '()) ; need to reset each time?
(count-pairs '(a b c)) ; 3
;
(set! visited-nodes '())
(define 3rd (cons 'a '()))
(define 2nd (cons 3rd 3rd))
(define lst (cons 2nd '()))
(count-pairs lst) ; 3
;
(set! visited-nodes '())
(define x (cons 2nd 2nd))
;(count-pairs x) ; S/O

; Exercise 3.21

(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))
(define (set-front-ptr! queue item)
  (set-car! queue item))
(define (set-rear-ptr! queue item)
  (set-cdr! queue item))

(define (empty-queue? queue)
  (null? (front-ptr queue)))

(define (make-queue) (cons '() '()))

; The Lisp printer prints it this way because the queue is
; a pair of front-ptr and read-ptr, and front-ptr is itself a
; ptr to a list of the queue elements. So we see the last element twice
; after insert-queue! and delete-queue!.
; To fix this, we can simply print front-ptr instead

;(define (print-queue queue)
;  (map display (front-ptr queue))) ; this works in theory but doesn't print out nicely
(define (print-queue queue)
  (front-ptr queue))

; to print correctly, instead of returning the queue in insert-queue! and delete-queue!,
; return print-queue.

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

; Tests
(define q1 (make-queue))
(insert-queue! q1 'a)
(insert-queue! q1 'b)
(delete-queue! q1)
(delete-queue! q1)

; Exercise 3.23

; doubly linked list.
; the deque is a pair of two pointers (front rear)
; each is a pointer to first node of each side of the deque
; a node is pair(item, pair(next, prev))

; constructors and selectors

(define (make-deque) (cons '() '()))
(define (front deque) (car deque))
(define (rear deque) (cdr deque))
(define (empty-deque? deque)
  (or (null? (front deque)) (null? (rear deque))))

; front/back via helper procedure

(define (get-first-elem side deque)
  (if (empty-queue? deque)
      (error "Cannot retrieve from an empty deque")
      (car (side deque))))

(define (front-deque deque)(get-first-elem front deque))
(define (rear-deque deque)(get-first-elem rear deque))

; insertion via helper procedure(s)
(define (set-front! deque node) (set-car! deque node)) 
(define (set-rear! deque node) (set-cdr! deque node))

; this isn't working based on the tests
(define (insert-deque! side deque item)
  (let ((new-pair
         (cons item (cons '() '()))))
    (cond (empty-deque? deque)
          (set-front! deque new-pair)
          (set-rear! deque new-pair)
          (print-deque deque)
          ((eq? side 'front)
           (set-car! (cdr new-pair) (front deque))
           (set-cdr! (cdr (front deque)) new-pair)
           (set-front! deque new-pair)
           (print-deque deque))
          (else
           (set-cdr! (cdr new-pair) (rear deque))
           (set-car! (cdr (rear deque)) new-pair)
           (set-rear! deque new-pair)
           (print-deque deque)))))

(define (front-insert-deque! deque item) (insert-deque! 'front deque item))
(define (rear-insert-deque! deque item) (insert-deque! 'rear deque item))

; deletion

(define (front-deque-delete! deque)
  (cond ((empty-deque? deque) (error "Deleting from empty deque" deque))
        (else
         (set-front! deque (cadr (front deque)))
         (if (empty-deque? deque)
             (set-rear! deque '())
             (set-cdr! (cdr (front deque)) '()))
         (print-deque deque))))

(define (rear-deque-delete! deque)
  (cond ((empty-deque? deque) (error "Deleting from empty deque" deque))
        (else
         (set-rear! deque (cddr (rear deque)))
         (if (empty-deque? deque)
             (set-front! deque '())
             (set-car! (cdr (rear deque)) '()))
         (print-deque deque))))

; print by traversing the next ptrs.

(define (print-deque deque)
  (define (iter node)
    (if (null? node)
        (display "\n")
        (begin
          (display (car node))
          (display " ")
          (iter (cadr node)))))
  (iter (front deque)))

; Tests - my solution doesn't actually work, not sure why

;(define dq (make-deque))
;(front-insert-deque! dq 3) ; --> (()) ????
;(print-deque dq) ; 3
;(front-insert-deque! dq 2) 
;(print-deque dq) ; 2 3

;(rear-insert-deque! dq 4)
;(print-deque dq) ; 2 3 4

;(rear-deque-delete! dq)
;(print-deque dq) ; 2 3

;(front-deque-delete! dq)
;(print-deque dq) ; 3

;(rear-deque-delete! dq)
;(empty-deque? dq) ; #t

; Exercise 3.25

; just replace a single key with a list of keys,
; since equal? in assoc checks for list equality

(define (assoc key records)
  (cond ((null? records) false)
        ((equal? key (caar records))
         (car records))
        (else (assoc key (cdr records)))))

(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup keys)
      (let ((record
             (assoc keys (cdr local-table))))
        (if record
            (cdr record)
            #f)))
    (define (insert! keys value)
      (let ((record
             (assoc keys (cdr local-table))))
        (if record
            (set-cdr! record value)
            (set-cdr! local-table (cons (cons keys value)
                                        (cdr local-table))))))
    (define (dispatch m)
      (cond ((eq? m 'get) lookup)
            ((eq? m 'put!) insert!)
            (else (error "Unknown operation: TABLE" m))))
    dispatch))

; Tests

(define t (make-table))


((t 'put!) (list 'colours 'red) 'R)
((t 'put!) (list 'colours 'blue) 'B)
((t 'get) (list 'colours 'blue)) ;; B
((t 'put!) (list 'colours 'blue) 'BLUE)
((t 'get) (list 'colours 'blue)) ;; BLUE

; variadic

((t 'put!) (list 'colours 'secondary 'green) 'G)
((t 'put!) (list 'food 'italian 'carb 'pasta) 'Carbonara)

((t 'get) (list 'colours 'secondary 'green)) ; G
((t 'get) (list 'colours 'secondary 'blue)) ; #f

((t 'get) (list 'food 'italian 'carb 'pasta)) ; Carbonara
