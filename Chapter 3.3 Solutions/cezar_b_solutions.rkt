#lang racket
(require rackunit)
(require racket/list)
(require rnrs/mutable-pairs-6)
(require compatibility/mlist)

; Ex 3.16
;     +-------+-------+
;     | C     |       |
;     |   o   |   o---+---+
;     |   |   |       |   |
;     +---+---+-------+   |
;         |               |
;         |   +-------+---x---+
;         |   | B     |       |
;         +-->|   o   |   o---+----+
;             |   |   |       |    |
;             +---+---+-------+    |
;                 |                |
;                 |   +-------+----x--+
;                 |   | A     |       |
;                 +--->   1   |   2   |
;                     |       |       |
;                     +-------+-------+

(define (count-pairs-bad x)
  (if (not (pair? x))
      0
      (+ (count-pairs-bad (car x))
         (count-pairs-bad (cdr x))
         1)))

(module+ test
  (begin
    (let* ((a (cons 1 2))
           (b (cons a a))
           (c (cons b b)))
      (check-equal? (count-pairs-bad c) 7))))


; Ex 3.17
(define (count-pairs-good x)
  (define acc '())
  (define (iter els)
    (cond ((not (pair? els)) 0)
          ((not (null? (filter (lambda (z) (eq? els z)) acc))) 0)
          (else (begin
                  (set! acc (cons els acc))
                  (+ (iter (car els))
                     (iter (cdr els))
                     1)))))

  (iter x))

(module+ test
  (begin
    (let* ((a (cons 1 2))
           (b (cons a a))
           (c (cons b b)))
      (check-equal? (count-pairs-good c) 3))))


; Ex. 3.21

;-------------------------------------------------
(define (front-ptr queue) (mcar queue))
(define (rear-ptr queue) (mcdr queue))
(define (set-front-ptr! queue item)
  (set-car! queue item))
(define (set-rear-ptr! queue item)
  (set-cdr! queue item))

(define (empty-queue? queue)
  (null? (front-ptr queue)))

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
        (else (set-front-ptr! queue (mcdr (front-ptr queue)))
              queue)))
;-------------------------------------------------

(define (print-queue q)
  ; returns a list
  (define (helper ptr)
    (if (eq? ptr (rear-ptr q))
        (list (mcar ptr))
        (cons (mcar ptr)
              (helper (mcdr ptr)))))
  (if (empty-queue? q)
      '()
      (helper (front-ptr q))))

(module+ test
  (begin
    (let* ((q1 (make-queue)))
      (begin
        (insert-queue! q1 'a)
        (insert-queue! q1 'b)
        (check-equal? (print-queue q1) '(a b))
        (delete-queue! q1)
        (delete-queue! q1)
        (check-equal? (print-queue q1) '())
        ))))
  
; Ex 3.23

; a cell is (item [prev next]) where item is the content and prev/next are pointers

(define (cell-item cell) (car cell))
(define (cell-next cell) (mcdr (cdr cell)))
(define (cell-prev cell) (mcar (cdr cell)))
(define (cell-ptr cell) (cdr cell))
(define (make-cell item) (cons item (mcons '() '())))
(define (cell-set-next! cell ptr) (set-cdr! (cell-ptr cell) ptr))
(define (cell-set-prev! cell ptr) (set-car! (cell-ptr cell) ptr))

(define (make-dequeue)
  ; create an empty cell, front and rear point to this cell
  (let ((head (make-cell '())))
    (begin
      (cell-set-next! head head)
      (cell-set-prev! head head)
      head)))
    

(define front-dequeue-ptr cell-next)
(define rear-dequeue-ptr cell-prev)

(define (front-dequeue q)
  (deref-ptr (front-dequeue-ptr q) q))

(define (rear-dequeue q)
  (deref-ptr (rear-dequeue-ptr q) q))

(define (deref-ptr ptr q)
  (if (eq? ptr q)
      (error "Access violation")
      (cell-item ptr)))

(define (empty-dequeue? q)
  (eq? (front-dequeue-ptr q) q))


(define (front-insert-deque! q item)
  (let ((new-cell (make-cell item))
        (f (front-dequeue-ptr q)))
        (cell-insert-dequeue! (cell-prev f) f new-cell)))

(define (rear-insert-deque! q item)
  (let ((new-cell (make-cell item))
        (r (rear-dequeue-ptr q)))
        (cell-insert-dequeue! r (cell-next r) new-cell)))


(define (cell-insert-dequeue! a b new-cell)
  (cell-set-next! new-cell b)
  (cell-set-prev! new-cell a)
  (cell-set-next! a new-cell)
  (cell-set-prev! b new-cell))

(define (print-dequeue q)
  (define (iter ptr)
    (if (eq? ptr q)
        '()
        (cons (deref-ptr ptr q)
              (iter (cell-next ptr)))))
  (iter (front-dequeue-ptr q)))

(define (front-delete-dequeue! q)  
  (if (empty-dequeue? q)
      (error "Empty dequeue")
      (let ((new-cell (cell-next (front-dequeue-ptr q))))
        (cell-set-next! q new-cell)
        (cell-set-prev! new-cell q))))

(define (rear-delete-dequeue! q)  
  (if (empty-dequeue? q)
      (error "Empty dequeue")
      (let ((new-cell (cell-prev (rear-dequeue-ptr q))))
        (cell-set-prev! q new-cell)
        (cell-set-next! new-cell q))))

(module+ test
  (begin
    (define q2 (make-dequeue))
    (check-equal? (empty-dequeue? q2) #t)
    (rear-insert-deque! q2 4)
    (front-insert-deque! q2 3)
    (front-insert-deque! q2 2)
    (front-insert-deque! q2 1)
    (rear-insert-deque! q2 5)
    (check-equal? (empty-dequeue? q2) #f)
    (rear-insert-deque! q2 6)
    (rear-insert-deque! q2 7)
    (check-equal? (print-dequeue q2) '(1 2 3 4 5 6 7))
    (front-delete-dequeue! q2)
    (rear-delete-dequeue! q2)
    (check-equal? (print-dequeue q2) '(2 3 4 5 6))
    (front-delete-dequeue! q2)
    (rear-delete-dequeue! q2)
    (front-delete-dequeue! q2)
    (rear-delete-dequeue! q2)
    (rear-delete-dequeue! q2)
    (check-equal? (empty-dequeue? q2) #t)
    ))
  
; Ex. 3.25 
; ( (nil . nil)
;   (key1 . ( value? . subtable )
;

(define (mcaar e) (mcar (mcar e)))
(define (mcadr e) (mcar (mcdr e)))
(define (mcddr e) (mcdr (mcdr e)))

(define (make-table) (mlist (make-table-row '(*table) null)))                            
                      
(define (make-table-row key value)
  (if (null? key)
      (mcons '() '())
      (mcons (car key)
             (mcons (if (null? (cdr key)) value #f) (mlist (make-table-row (cdr key) value))))))

(define (mlist-insert! alist e)
  (set-cdr! alist (mcons e (mcdr alist))))

(define (insert! key value table)
  (let ((kv (massoc (car key) table)))    
    (if kv
        (let* ((value-subtable-pair (mcdr kv))
              (subtable (mcdr value-subtable-pair)))
          (if (null? (cdr key))
              (set-car! value-subtable-pair value)
              (if (null? subtable)
                  (set-cdr! value-subtable-pair (mlist (make-table-row (cdr key) value)))
                  (insert! (cdr key) value subtable))))
        (mlist-insert! table (make-table-row key value)))))

(define (lookup key table)  
  (let ((kv (massoc (car key) table)))
    (if kv
        (let* ((value-subtable-pair (mcdr kv))
               (subtable (mcdr value-subtable-pair)))
          (if (null? (cdr key))
              (mcar value-subtable-pair)
              (unless (null? subtable)
                (lookup (cdr key) subtable))))
        #f)))


(module+ test
  (begin
    (define t1 (make-table))
    (insert! '(1 2) 'a t1)
    (insert! '(3 4) 'b t1)
    (insert! '(1 2 3 4) 'c t1)
    (check-equal? (lookup '(3 4) t1) 'b)
    (check-equal? (lookup '(1 2 3) t1) #f)
    (check-equal? (lookup '(1 2 3 4) t1) 'c)))
