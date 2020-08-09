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

;; Exercise 3.23 (this is an incorrect solution as I didn't use a doubly-linked list 
;;                meaning that the big-O of pop-back-deque is O(n)

(define (front-ptr deque) (car deque))
(define (rear-ptr deque)  (cdr deque))
(define (set-front-ptr! deque item) (set-car! deque item))
(define (set-rear-ptr!  deque item) (set-cdr! deque item))

(define (make-deque) (cons '() '()))

(define (empty-deque? deque) (null? (front-ptr deque)))

(define (front-deque deque)
  (if (empty-deque? deque)
      (error "FRONT called with an empty deque" deque)
      (car (front-ptr deque))))

(define (back-deque deque)
  (if (empty-deque? deque)
      (error "BACK called with an empty deque" deque)
      (car (rear-ptr deque))))

(define (print-deque deque)
  (if (empty-deque? deque)
      "EMPTY"
      (front-ptr deque)))

(define (push-back-deque! deque item)
  (let ((new-pair (cons item '())))
    (cond ((empty-deque? deque)
           (set-front-ptr! deque new-pair)
           (set-rear-ptr! deque new-pair)
           (print-deque deque))
          (else
           (set-cdr! (rear-ptr deque) new-pair)
           (set-rear-ptr! deque new-pair)
           (print-deque deque)))))

(define (push-front-deque! deque item)
  (let ((new-pair (cons item '())))
    (cond ((empty-deque? deque)
           (set-front-ptr! deque new-pair)
           (set-rear-ptr! deque new-pair)
           (print-deque deque))
          (else
           (set-cdr! new-pair (front-ptr deque))
           (set-front-ptr! deque new-pair)
           (print-deque deque)))))

(define (pop-front-deque! deque)
  (cond ((empty-deque? deque)
         (error "POP-FRONT! called with an empty deque" deque))
        (else (set-front-ptr! deque (cdr (front-ptr deque)))
              (print-deque deque))))

(define (get-second-last-pair lst)
  (if (null? (cdr (cdr lst)))
      lst
      (get-second-last-pair (cdr lst))))

(define (pop-back-deque! deque)
  (cond ((empty-deque? deque)
         (error "POP-BACK! called with an empty deque" deque))
        ((= (length (front-ptr deque)) 1) (set! deque (cons '() '()))
                                          (print-deque deque))
        (else (set-rear-ptr! deque (get-second-last-pair (front-ptr deque)))
              (set-cdr! (get-second-last-pair (front-ptr deque)) '())
              (print-deque deque))))

(define q (make-deque))
(push-back-deque! q 'a)  ; (a)
(push-back-deque! q 'b)  ; (a b)
(push-back-deque! q 'c)  ; (a b c)
(front-deque q)          ; 'a
(back-deque q)           ; 'c
(pop-front-deque! q)     ; (b c)
(pop-front-deque! q)     ; (c)
(push-front-deque! q 'a) ; (a c)
(push-front-deque! q 'b) ; (b a c)
(pop-back-deque! q)      ; (b a)
(pop-back-deque! q)      ; (b)
(pop-back-deque! q)      ; ()
(push-back-deque! q 'a)  ; ((a) a) <- fails

;; Exercise 3.25

;; TODO
