;; - 3.16

(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))

(count-pairs
 (cons 1 (cons (cons 1 1) '()))) ;=> 3

(count-pairs
 (let ((x (cons 1 1)))
   (cons x (cons x '())))) ;=> 4

(count-pairs
 (let* ((x (cons 1 1))
        (y (cons x x)))
   (cons y y))) ;=> 7

(count-pairs
 (let* ((tail (cons 3 '()))
        (x (cons 1 (cons 2 tail))))
   (set-cdr! tail x)
   x)) ;=> inf loop



;; - 3.17

(define (count-pairs x)
  (let ((seen '()))
    (define (count x)
      (if (or (not (pair? x))
              (memq x seen))
          0
          (begin
            (set! seen (cons x seen))
            (+ (count (car x))
               (count (cdr x))
               1))))
    (count x)))

;; - 3.21

(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))

(define (set-front-ptr! queue item) (set-car! queue item))
(define (set-rear-ptr! queue item) (set-cdr! queue item))

(define (empty-queue? queue) (null? (front-ptr queue)))
(define (make-queue) (cons '() '()))

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
        (else
         (set-front-ptr! queue (cdr (front-ptr queue)))
         queue)))

(define (print-queue queue)
  (display (front-ptr queue))
  (display "\n"))

;; - 3.23

(define (make-deque)
  (cons '() '()))
(define (empty-deque? deq)
  (null? (car deq)))

;; (define (make-dnode value prev next)
;;   (cons value (cons prev next)))
;; (define (dnode-))

(define (front-insert-deque! dq item)
  (cond ((empty-deque? dq)
         (let ((new-triple (cons item (cons '() '()))))
           (set-car! dq new-triple)
           (set-cdr! dq new-triple)))
        (else
         (let* ((front (car dq))
                (new-front (cons item (cons '() front))))
           (set-car! dq new-front)
           (set-car! (cdr front) new-front))))
  (void))
(define (rear-insert-deque! dq item)
  (cond ((empty-deque? dq)
         (let ((new-triple (cons item (cons '() '()))))
           (set-car! dq new-triple)
           (set-cdr! dq new-triple)))
        (else
         (let* ((rear (cdr dq))
                (new-rear (cons item (cons rear '()))))
           (set-cdr! dq new-rear)
           (set-cdr! (cdr rear) new-rear))))
  (void))
(define (front-deque dq)
  (if (empty-deque? dq)
      (error "empty deque" dq)
      (caar dq)))
(define (rear-deque dq)
  (if (empty-deque? dq)
      (error "empty deque" dq)
      (cadr dq)))

(define (front-delete-deque! dq)
  (cond ((empty-deque? dq)
         (error "empty deque"))
        (else
         (let* ((old-front (car dq))
                (new-front (cddr old-front)))
           (set-car! dq new-front)
           (if (null? new-front)
               (set-cdr! dq '())
               (set-car! (cdr new-front) '())))))
  (void))

(define (rear-delete-deque! dq)
  (cond ((empty-deque? dq)
         (error "empty deque"))
        (else
         (let* ((old-rear (cdr dq))
                (new-rear (cadr old-rear)))
           (set-cdr! dq new-rear)
           (if (null? new-rear)
               (set-car! dq '())
               (set-cdr! (cdr new-rear) '())))))
  (void))

(define (deque-print dq)
  (define (iter node)
   (if (null? node)
       (display "\n")
       (begin
         (display (car node))
         (display " ")
         (iter (cddr node)))))
  (iter (car dq)))
(define (deque-rev-print dq)
  (define (iter node)
   (if (null? node)
       (display "\n")
       (begin
         (display (car node))
         (display " ")
         (iter (cadr node)))))
  (iter (cdr dq)))

(define dq1 (make-deque))
(front-insert-deque! dq1 1)
(deque-print dq1)                       ; 1
(deque-rev-print dq1)                   ; 1
(front-insert-deque! dq1 2)
(deque-print dq1)                       ; 2 1
(deque-rev-print dq1)                   ; 1 2
(rear-insert-deque! dq1 0)
(deque-print dq1)                       ; 2 1 0
(deque-rev-print dq1)                   ; 0 1 2
(front-delete-deque! dq1)
(deque-print dq1)                       ; 1 0
(deque-rev-print dq1)                   ; 0 1
(rear-delete-deque! dq1)
(deque-print dq1)                       ; 1
(deque-rev-print dq1)                   ; 1
(rear-delete-deque! dq1)
(empty-deque? dq1) ;=> #t

;; - 3.25

(define (lookup . keys-and-table)
  (let* ((keys (butlast keys-and-table))
         (table (list-ref keys-and-table (length keys)))
         (record (assoc keys (cdr table))))
    (if record
        (cdr record)
        #f)))

(define (insert! . keys-value-table)
  (let* ((kv (butlast keys-value-table))
         (keys (butlast kv))
         (value (list-ref kv (length keys)))
         (table (list-ref keys-value-table (length kv)))
         (record (assoc keys (cdr table))))
    (if record
        (set-cdr! record value)
        (set-cdr! table
                  (cons (cons keys value) (cdr table)))))
  'ok)

(define (make-table)
  (list '*table*))
