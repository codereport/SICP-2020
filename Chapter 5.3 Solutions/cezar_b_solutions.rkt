#lang racket
(require rackunit)
(require rnrs/mutable-pairs-6)
(require compatibility/mlist)
(require racket/stream)

;---------------------------------------------------------

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (make-machine register-names ops controller-text)
  (let ((machine (make-new-machine)))
    (for-each (lambda (register-name)
                ((machine 'allocate-register) register-name))
              register-names)
    ((machine 'install-operations) ops)    
    ((machine 'install-instruction-sequence)
     (assemble controller-text machine))
    machine))

(define (make-register name)
  (let ((contents '*unassigned*))
    (define (dispatch message)
      (cond ((eq? message 'get) contents)
            ((eq? message 'set)
             (lambda (value) (set! contents value)))
            (else
             (error "Unknown request -- REGISTER" message))))
    dispatch))

(define (get-contents register)
  (register 'get))

(define (set-contents! register value)
  ((register 'set) value))

(define (make-stack)
  (let ((s '()))
    (define (push x)
      (set! s (cons x s)))
    (define (pop)
      (if (null? s)
          (error "Empty stack -- POP")
          (let ((top (car s)))
            (set! s (cdr s))
            top)))
    (define (initialize)
      (set! s '())
      'done)
    (define (dispatch message)
      (cond ((eq? message 'push) push)
            ((eq? message 'pop) (pop))
            ((eq? message 'initialize) (initialize))
            (else (error "Unknown request -- STACK"
                         message))))
    dispatch))

(define (pop stack)
  (stack 'pop))

(define (push stack value)
  ((stack 'push) value))

(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
        (the-instruction-sequence '())
        (trace #f))
        
    (let ((the-ops
           (list (list 'initialize-stack
                       (lambda () (stack 'initialize)))))
          (register-table
           (list (list 'pc pc) (list 'flag flag))))
      (define (allocate-register name)
        (if (assoc name register-table)
            (error "Multiply defined register: " name)
            (set! register-table
                  (cons (list name (make-register name))
                        register-table)))
        'register-allocated)
      (define (lookup-register name)
        (let ((val (assoc name register-table)))
          (if val
              (cadr val)
              (error "Unknown register:" name))))
      (define (has-register? name)
        (let ((val (assoc name register-table)))
          (if val
              #t
              #f)))
      (define (trace-on) (set! trace #t))
      (define (trace-off) (set! trace #f))
      (define (execute)
        (let ((insts (get-contents pc)))
          (if (null? insts)
              'done
              (begin
                (when trace
                  (displayln (instruction-text (car insts))))
                ((instruction-execution-proc (car insts)))
                (execute)))))
      (define (dispatch message)
        (cond ((eq? message 'start)
               (set-contents! pc the-instruction-sequence)
               (execute))
              ((eq? message 'install-instruction-sequence)
               (lambda (seq) (set! the-instruction-sequence seq)))
              ((eq? message 'allocate-register) allocate-register)
              ((eq? message 'get-register) lookup-register)
              ((eq? message 'has-register?) has-register?)
              ((eq? message 'install-operations)
               (lambda (ops) (set! the-ops (append the-ops ops))))
              ((eq? message 'stack) stack)
              ((eq? message 'operations) the-ops)
              ((eq? message 'trace-on) trace-on)
              ((eq? message 'trace-off) trace-off)
              (else (error "Unknown request -- MACHINE" message))))
      dispatch)))

(define (start machine)
  (machine 'start))
(define (get-register-contents machine register-name)
  (get-contents (get-register machine register-name)))
(define (set-register-contents! machine register-name value)
  (set-contents! (get-register machine register-name) value)
  'done)

(define (get-register machine reg-name)
  (unless (has-register? machine reg-name)
    (allocate-register machine reg-name))
  ((machine 'get-register) reg-name))

(define (has-register? machine reg-name)
  ((machine 'has-register?) reg-name))
(define (allocate-register machine reg-name)
  ((machine 'allocate-register) reg-name))

(define (extract-labels text receive)
  (if (null? text)
      (receive '() '())
      (extract-labels (cdr text)
                      (lambda (insts labels)
                        (let ((next-inst (car text)))
                          (if (symbol? next-inst)
                              (if (member next-inst (map car labels))
                                  (error "DUPLICATE LABELS")
                                  (receive insts
                                      (cons (make-label-entry next-inst
                                                              insts)
                                            labels)))
                              (receive (cons (make-instruction next-inst)
                                             insts)
                                  labels)))))))

(define (assemble controller-text machine)
  (extract-labels controller-text
                  (lambda (insts labels)
                    (update-insts! insts labels machine)
                    insts)))



(define (update-insts! insts labels machine)
  (let ((pc (get-register machine 'pc))
        (flag (get-register machine 'flag))
        (stack (machine 'stack))
        (ops (machine 'operations)))
    (for-each
     (lambda (inst)
       (set-instruction-execution-proc! 
        inst
        (make-execution-procedure
         (instruction-text inst) labels machine
         pc flag stack ops)))
     insts)))

(define (make-instruction text)
  (mcons text '()))
(define (instruction-text inst)
  (mcar inst))
(define (instruction-execution-proc inst)
  (mcdr inst))
(define (set-instruction-execution-proc! inst proc)
  (set-cdr! inst proc))

(define (make-label-entry label-name insts)
  (cons label-name insts))

(define (lookup-label labels label-name)
  (let ((val (assoc label-name labels)))
    (if val
        (cdr val)
        (error "Undefined label -- ASSEMBLE" label-name))))

(define (make-execution-procedure inst labels machine
                                  pc flag stack ops)
  (cond ((eq? (car inst) 'assign)
         (make-assign inst machine labels ops pc))
        ((eq? (car inst) 'test)
         (make-test inst machine labels ops flag pc))
        ((eq? (car inst) 'branch)
         (make-branch inst machine labels flag pc))
        ((eq? (car inst) 'goto)
         (make-goto inst machine labels pc))
        ((eq? (car inst) 'save)
         (make-save inst machine stack pc))
        ((eq? (car inst) 'restore)
         (make-restore inst machine stack pc))
        ((eq? (car inst) 'perform)
         (make-perform inst machine labels ops pc))
        (else (error "Unknown instruction type -- ASSEMBLE"
                     inst))))

(define (make-assign inst machine labels operations pc)
  (let ((target
         (get-register machine (assign-reg-name inst)))
        (value-exp (assign-value-exp inst)))
    (let ((value-proc
           (if (operation-exp? value-exp)
               (make-operation-exp
                value-exp machine labels operations)
               (make-primitive-exp
                (car value-exp) machine labels))))
      (lambda ()                ; execution procedure for assign
        (set-contents! target (value-proc))
        (advance-pc pc)))))

(define (assign-reg-name assign-instruction)
  (cadr assign-instruction))
(define (assign-value-exp assign-instruction)
  (cddr assign-instruction))

(define (advance-pc pc)
  (set-contents! pc (cdr (get-contents pc))))

(define (make-test inst machine labels operations flag pc)
  (let ((condition (test-condition inst)))
    (if (operation-exp? condition)
        (let ((condition-proc
               (make-operation-exp
                condition machine labels operations)))
          (lambda ()
            (set-contents! flag (condition-proc))
            (advance-pc pc)))
        (error "Bad TEST instruction -- ASSEMBLE" inst))))
(define (test-condition test-instruction)
  (cdr test-instruction))

(define (make-branch inst machine labels flag pc)
  (let ((dest (branch-dest inst)))
    (if (label-exp? dest)
        (let ((insts
               (lookup-label labels (label-exp-label dest))))
          (lambda ()
            (if (get-contents flag)
                (set-contents! pc insts)
                (advance-pc pc))))
        (error "Bad BRANCH instruction -- ASSEMBLE" inst))))
(define (branch-dest branch-instruction)
  (cadr branch-instruction))

(define (make-goto inst machine labels pc)
  (let ((dest (goto-dest inst)))
    (cond ((label-exp? dest)
           (let ((insts
                  (lookup-label labels
                                (label-exp-label dest))))
             (lambda () (set-contents! pc insts))))
          ((register-exp? dest)
           (let ((reg
                  (get-register machine
                                (register-exp-reg dest))))
             (lambda ()
               (set-contents! pc (get-contents reg)))))
          (else (error "Bad GOTO instruction -- ASSEMBLE"
                       inst)))))
(define (goto-dest goto-instruction)
  (cadr goto-instruction))

(define (make-save inst machine stack pc)
  (let ((reg (get-register machine
                           (stack-inst-reg-name inst))))
    (lambda ()
      (push stack (get-contents reg))
      (advance-pc pc))))
(define (make-restore inst machine stack pc)
  (let ((reg (get-register machine
                           (stack-inst-reg-name inst))))
    (lambda ()
      (set-contents! reg (pop stack))    
      (advance-pc pc))))
(define (stack-inst-reg-name stack-instruction)
  (cadr stack-instruction))

(define (make-perform inst machine labels operations pc)
  (let ((action (perform-action inst)))
    (if (operation-exp? action)
        (let ((action-proc
               (make-operation-exp
                action machine labels operations)))
          (lambda ()
            (action-proc)
            (advance-pc pc)))
        (error "Bad PERFORM instruction -- ASSEMBLE" inst))))
(define (perform-action inst) (cdr inst))

(define (make-primitive-exp exp machine labels)
  (cond ((constant-exp? exp)
         (let ((c (constant-exp-value exp)))
           (lambda () c)))
        ((label-exp? exp)
         (let ((insts
                (lookup-label labels
                              (label-exp-label exp))))
           (lambda () insts)))
        ((register-exp? exp)
         (let ((r (get-register machine
                                (register-exp-reg exp))))
           (lambda () (get-contents r))))
        (else
         (error "Unknown expression type -- ASSEMBLE" exp))))

(define (register-exp? exp) (tagged-list? exp 'reg))
(define (register-exp-reg exp) (cadr exp))
(define (constant-exp? exp) (tagged-list? exp 'const))
(define (constant-exp-value exp) (cadr exp))
(define (label-exp? exp) (tagged-list? exp 'label))
(define (label-exp-label exp) (cadr exp))

(define (make-operation-exp exp machine labels operations)
  (let ((op (lookup-prim (operation-exp-op exp) operations))
        (aprocs
         (map (lambda (e)
                (make-primitive-exp e machine labels))
              (operation-exp-operands exp))))
    (lambda ()
      (apply op (map (lambda (p) (p)) aprocs)))))

(define (operation-exp? exp)
  (and (pair? exp) (tagged-list? (car exp) 'op)))
(define (operation-exp-op operation-exp)
  (cadr (car operation-exp)))
(define (operation-exp-operands operation-exp)
  (cdr operation-exp))

(define (lookup-prim symbol operations)
  (let ((val (assoc symbol operations)))
    (if val
        (cadr val)
        (error "Unknown operation -- ASSEMBLE" symbol))))

(define (trace-on machine)
  ((machine 'trace-on)))
(define (trace-off machine)
  ((machine 'trace-off)))


;----------------------

; Ex 5.20

;         +---+---+    +---+---+
;         | . | . +--->+   |   |
;       1 +-+-+---+    +-+-+---+ 4
;           |            |
;           |            |
;       2 +-v-+---+    +-v-+---+ 3
;         | 1 | 2 |    | 1 | 2 |
;         +---+---+    +---+---+
;
;
;            1    2    3    4
;          +----+----+----+----+
;          | p2 | n1 | n1 | p3 |
;          +----+----+----+----+
;          | p4 | n2 | n2 | e0 |
;          +----+----+----+----+
;


; Ex. 5.21

(define m0
  (make-machine
   ; use idx for current cell, lr for return register
   '(the-cars the-cdrs a b c d idx lr) 
   (list (list 'vector-ref vector-ref)
         (list '= =)
         (list '+ +)
         (list 'eq? eq?)
         (list 'cel-type car)    ; Ex p, n, e
         (list 'cel-off cadr))
   
   '(start
     (assign lr (label exit)) ; call count-leaves
     (goto (label cl))
     

     ; function: count-leaves
     ; input: idx is the typed pointer (ex: '(p 1)
     ; output: resul in (reg a)
     cl
     (save lr)
     (assign lr (label l0))
     (goto (label isnull))  ; check if null
     l0
     (test (op =) (reg a) (const 1))
     (branch (label cl-done0))
     (assign lr (label l1))
     (goto (label isnotpair))
     l1
     (test (op =) (reg a) (const 1))
     (branch (label cl-done1))
     ; at this point we need to do recursion

     (save c)
     (save d)
     (assign c (reg idx)) ; save idx
     (assign c (op cel-off) (reg c))

     ; call count leaves for car
     (assign idx (op vector-ref) (reg the-cars) (reg c))     
     (assign lr (label l2))
     (goto (label cl))     
     l2
     (assign d (reg a)) ; store first count in d
     ; call count leaves for cdr
     (assign idx (op vector-ref) (reg the-cdrs) (reg c))
     (assign lr (label l3))
     (goto (label cl))     
     l3
     ; need to add 
     (assign a (op +) (reg a) (reg d))
     (restore d)
     (restore c)
     (restore lr)
     (goto (reg lr))
     
     cl-done0
     (assign a (const 0))
     (restore lr)
     (goto (reg lr))
     cl-done1
     (assign a (const 1))
     (restore lr)
     (goto (reg lr))


     ; function: isnull
     ; inputs: idx - current cell
     ; output: (reg a) 0 or 1
     isnull
     (assign a (op cel-type) (reg idx)) ; store cell type in a
     (test (op eq?) (reg a) (const e))  ; check if pointer type is e
     (branch (label isnull-yes))        
     (assign a (const 0))
     (goto (reg lr))
     isnull-yes
     (assign a (const 1))
     (goto (reg lr))

     ; function: isnotpair
     ; inputs: idx - current cell
     ; output: (reg a) 0 or 1
     isnotpair
     (assign a (op cel-type) (reg idx)) ; store cell type in a
     (test (op eq?) (reg a) (const p))  ; check if pointer type is e
     (branch (label isnotpair-f))        
     (assign a (const 1))
     (goto (reg lr))
     isnotpair-f
     (assign a (const 0))
     (goto (reg lr))
         
     exit
     )))

; Ex 5.14 ((1 2) 3 4)

(set-register-contents! m0 'the-cars (vector '(e 0) '(p 5) '(n 3) '(e 0) '(n 4) '(n 1) '(e 0) '(n 2)))
(set-register-contents! m0 'the-cdrs (vector '(e 0) '(p 2) '(p 4) '(e 0) '(e 0) '(p 7) '(e 0) '(e 0)))
(set-register-contents! m0 'idx '(p 1))

(trace-on m0)
(start m0)
(get-register-contents m0 'a) ; this returns 4

