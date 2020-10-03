#lang racket
(require rackunit)
(require rnrs/mutable-pairs-6)
(require compatibility/mlist)
(require racket/promise)

;---------------------------------------------------------------------
; Lots of code from the book
;---------------------------------------------------------------------
; 4.1.1 The Core of the Evaluator
(define (sicp-eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp env))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp) (make-procedure (lambda-parameters exp)
                                       (lambda-body exp)
                                       env))
        ((begin? exp)
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (sicp-eval (cond->if exp) env))
        ((application? exp)
         (sicp-apply (actual-value (operator exp) env)
                (operands exp)
                env))        
        
        (else
         (error "Unknown expression type: EVAL" exp))))

(define (delay-it exp env)
  (list 'thunk exp env))

(define (thunk? obj)
  (tagged-list? obj 'thunk))
(define (thunk-exp thunk) (cadr thunk))
(define (thunk-env thunk) (caddr thunk))
(define (force-it obj)
  (if (thunk? obj)
      (actual-value (thunk-exp obj) (thunk-env obj))
      obj))


(define (actual-value exp env)
  (force-it (sicp-eval exp env)))

(define (list-of-arg-values exps env)
  (if (no-operands? exps)
      '()
      (cons (actual-value (first-operand exps)
                          env)
            (list-of-arg-values (rest-operands exps)
                                env))))

(define (eval-if exp env)
  (if (true? (actual-value (if-predicate exp) env))
      (sicp-eval (if-consequent exp) env)
      (sicp-eval (if-alternative exp) env)))

(define (eval-sequence exps env)
(cond ((last-exp? exps)
       (sicp-eval (first-exp exps) env))
      (else
       (sicp-eval (first-exp exps) env)
       (eval-sequence (rest-exps exps) env))))

(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (sicp-eval (assignment-value exp) env)
                       env)
  'ok)

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
    (sicp-eval (definition-value exp) env)
    env)
  'ok)

; 4.1.2 Representing Expressions
(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))        
(define (variable? exp) (symbol? exp))
(define (quoted? exp) (tagged-list? exp 'quote))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))
(define (assignment? exp) (tagged-list? exp 'set!))
(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))
(define (definition? exp) (tagged-list? exp 'define))
(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))
(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp) ; formla
                   (cddr exp)))) ; body
(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-body exp) (cddr exp))


(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))
(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))
(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))
(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))
(define (make-begin seq) (cons 'begin seq))
(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

(define (cond? exp) (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))
(define (cond->if exp) (expand-clauses (cond-clauses exp)))
(define (expand-clauses clauses)
  (if (null? clauses)
      'false ; no else clause
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last: COND->IF"
                       clauses))
            (make-if (cond-predicate first)
                     (sequence->exp (cond-actions first))
                     (expand-clauses rest))))))


; 4.1.3 Evaluator Data Structures
(define (true? x) (not (eq? x false)))
(define (false? x) (eq? x false))

(define (make-procedure parameters body env)
  (list 'procedure parameters body env))
(define (compound-procedure? p)
  (tagged-list? p 'procedure))
(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))

(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())

(define (make-frame variables values)
  (mcons (list->mlist variables) (list->mlist values)))
(define (frame-variables frame) (mcar frame))
(define (frame-values frame) (mcdr frame))
(define (add-binding-to-frame! var val frame)
  (set-car! frame (mcons var (mcar frame)))
  (set-cdr! frame (mcons val (mcdr frame))))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (mcar vars)) (mcar vals))
            (else (scan (mcdr vars) (mcdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))
(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (mcar vars)) (set-car! vals val))
            (else (scan (mcdr vars) (mcdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable: SET!" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (add-binding-to-frame! var val frame))
            ((eq? var (mcar vars)) (set-car! vals val))
            (else (scan (mcdr vars) (mcdr vals)))))
    (scan (frame-variables frame) (frame-values frame))))


; 4.1.4 Running the Evaluator as a Program

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc) (cadr proc))

(define (xcons x y) (lambda (m) (m x y)))
(define (xcar z) (z (lambda (p q) p)))
(define (xcdr z) (z (lambda (p q) q)))

(define primitive-procedures
  (list
   (list 'car car)
   (list 'cdr cdr)
   (list 'cons cons)
   (list 'null? null?)
   (list 'display display)
   (list 'format format)
   (list 'add2 +)
   (list 'lt <)
   (list '= =)
   (list 'div2 /)
   (list 'xcar xcar)
   (list 'xcons xcons)
   (list 'xcdr xcdr)
   ))

(define (primitive-procedure-names)
  (map car primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

(define (apply-primitive-procedure proc args)
  (apply
   (primitive-implementation proc) args))

(define (setup-environment)
  (let ((initial-env
         (extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))
(define the-global-environment (setup-environment))

(define input-prompt ";;; L-Eval input:")
(define output-prompt ";;; L-Eval value:")
(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output
           (actual-value
            input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))

(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))
(define (announce-output string)
  (newline) (display string) (newline))

(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
                     (procedure-parameters object)
                     (procedure-body object)
                     '<procedure-env>))
      (display object)))
;---------------------------------------------------------------------

; Ex. 4.25 -  Infinite run

; Ex. 4.27
;;; L-Eval input:
; count
;;; L-Eval value:
; A: 1 <- The rest if id (...) is a thunk
;;; L-Eval input:
;w
;;; L-Eval value:
; A: 10 <- w is evaluated
;;; L-Eval input:
; count
;;; L-Eval value:
; A: 2 <- w is forced


; Ex. 4.28
; The argument is a procedure thunk  that produces a primitime procedure

; Ex. 4.31 (force and lazy)

; Change make-lambda to normalize the parameter list
; A lambda expression would look like this
; (lambda ((a force) (b lazy)) (add2 a b))

(define (normalize-arg arg)
  (cond ((not (pair? arg)) (list arg 'force))
        ((eq? (cadr arg) 'force) (list (car arg) 'force))
        ((eq? (cadr arg) 'lazy) (list  (car arg) 'lazy))
        (else (error "Unknown type"))))

(define (make-lambda parameters body)
  (cons 'lambda (cons (map normalize-arg parameters) body)))

(define (lambda-parameters exp)
  (map normalize-arg (cadr exp)))

; Change apply -> extend environment for compound procedures

(define (sicp-apply procedure arguments env)
  (cond ((primitive-procedure? procedure)         
         (apply-primitive-procedure
          procedure
          (list-of-arg-values arguments env))) ; changed
        ((compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (extend-environment
           (map car (procedure-parameters procedure))
           (list-of-delayed-args arguments (map cadr (procedure-parameters procedure)) env) ; changed
           (procedure-environment procedure))))
        (else (error "Unknown procedure type: APPLY"
                     procedure))))

(define (list-of-delayed-args exps eval-kinds env)
  (if (no-operands? exps)
      '()
      (let ((eval-kind (car eval-kinds)))
        
        (cons (cond ((eq? 'force eval-kind) (sicp-eval
                                             (first-operand exps) env))
                    (else (delay-it (first-operand exps) env)))
              (list-of-delayed-args (rest-operands exps)
                                    (cdr eval-kinds)
                                    env)))))
                  

(module+ test
   (begin
     (define testenv (setup-environment))
     (sicp-eval '(define (f (a force) (b lazy)) a) testenv)
     (check-equal? (sicp-eval '(f 1 (div2 2 0)) testenv) 1)))

;4.33

; transform a list like '(1 2 3) -> '(xcons 1 (xcons 2 (xcons 3 0)))
; xcons/xcar/xcdr are primitive procedures (see above)
(define (transform-list exp)
  (if (null? exp)
      0
      (list 'xcons (car exp) (transform-list (cdr exp)))))

(define (text-of-quotation exp env)
  (if (pair? exp)
      (sicp-eval (transform-list (cadr exp)) env)
      (cadr exp)))

(module+ test
   (begin
     (define testenv2 (setup-environment))     
     (check-equal? (sicp-eval '(xcar '(1 2 3)) testenv2) 1)))


