;; Exercise 4.25 (543-4)

;; Applicative order will run forever - because base case will never be hit "alone" 
;;     - both arguments will always be evaluated.
;; Normal order will not have this problem and will work fine.

;; CODE FROM BOOK - changes to make to 4.1 Scheme Interpreter

;; appliction - Before

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)                                ; #1 Literal
        ((variable?        exp) (lookup-variable-value exp env))    ; #2 Variable Reference
        ((quoted?          exp) (text-of-quotation exp))            ; #1 Special Form
        ((assignment?      exp) (eval-assignment exp env))          ; #3 Special Form
        ((definition?      exp) (eval-definition exp env))          ; #3 Special Form
        ((if?              exp) (eval-if exp env))                  ; #3 Special Form
        ((lambda?          exp) (make-procedure                     ; #3 Special Form
                                 (lambda-parameters exp)
                                 (lambda-body exp)
                                 env))
        ((begin?           exp) (eval-sequence                      ; #3 Special Form
                                 (begin-actions exp) env))
        ((cond?            exp) (eval (cond->if exp) env))          ; #3 Special Form
        ((and?             exp) (eval-and exp env))                 ; #3 Special Form
        ((or?              exp) (eval-or exp env))                  ; #3 Special Form
        ((let?             exp) (eval (let->combination exp) env))  ; #3 Special Form
        ((application?     exp) (my-apply (eval (operator exp) env) ; #4 Procedure Call
                                       (list-of-values
                                        (operands exp) env)))
        (else
         (error "Unknown expression type: FAIL" exp))))
         
;; application - After

        ((application?     exp) (my-apply (actual-value             ; #4 Procedure Call
                                           (operator exp) env)
                                       (operands exp)
                                       env))
                                       
;; actual-value - Add

(define (actual-value exp env)
  (force-it (eval exp env)))
  
;; apply - Before

(define (my-apply procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (extend-environment
           (procedure-parameters procedure)
           arguments
           (procedure-environment procedure))))
        (else
         (error
          "Unknown procedure type: APPLY" procedure))))
          
;; apply - After

(define (apply procedure arguments env)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure
          procedure
          (list-of-arg-values arguments env)))  ; changed
        ((compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (extend-environment
           (procedure-parameters procedure)
           (list-of-delayed-args arguments env) ; changed
           (procedure-environment procedure))))
        (else (error "Unknown procedure type: APPLY"
                     procedure))))
                     
;; list-of-values - Removed

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))
            
;; list-of-* - Added

(define (list-of-arg-values exps env)
  (if (no-operands? exps)
      '()
      (cons (actual-value (first-operand exps)
                          env)
            (list-of-arg-values (rest-operands exps)
                                env))))

(define (list-of-delayed-args exps env)
  (if (no-operands? exps)
      '()
      (cons (delay-it (first-operand exps)
                      env)
            (list-of-delayed-args (rest-operands exps)
                                  env))))
                                  
;; eval-if - Before

(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))
      
;; eval-if - After

(define (eval-if exp env)
  (if (true? (actual-value (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))
      
;; driver-loop - Before

(define input-prompt  ";;; M-Eval input:")
(define output-prompt ";;; M-Eval value:")

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (eval input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))
  
;; driver-loop - After

(define input-prompt ";;; L-Eval input:")
(define output-prompt ";;; L-Eval value:")

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (actual-value input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))

;; additions (from Representing thunks)

;(define (force-it obj)
;  (if (thunk? obj)
;      (actual-value (thunk-exp obj) (thunk-env obj))
;      obj))

(define (delay-it exp env) (list 'thunk exp env))
(define (thunk? obj)       (tagged-list? obj 'thunk))
(define (thunk-exp thunk)  (cadr thunk))
(define (thunk-env thunk)  (caddr thunk))

(define (evaluated-thunk? obj)
  (tagged-list? obj 'evaluated-thunk))
(define (thunk-value evaluated-thunk)
  (cadr evaluated-thunk))

(define (force-it obj)
  (cond ((thunk? obj)
         (let ((result (actual-value (thunk-exp obj)
                                     (thunk-env obj))))
           (set-car! obj 'evaluated-thunk)
           (set-car! (cdr obj) result) ; replace exp with its value
           (set-cdr! (cdr obj) '())    ; forget unneeded env
           result))
        ((evaluated-thunk? obj) (thunk-value obj))
        (else obj)))

;; Exercise 4.27 (page 551)

;;; L-Eval input:
(define count 0)
;;; L-Eval value: 
ok

;;; L-Eval input:
(define (id x) (set! count (+ count 1)) x)
;;; L-Eval value: 
ok

;;; L-Eval input:
(define w (id (id 10)))
;;; L-Eval value: 
ok

;;; L-Eval input:
count
;;; L-Eval value: 
1

;;; L-Eval input:
w
;;; L-Eval value: 
10

;;; L-Eval input:
count
;;; L-Eval value: 
2

;; w becomes a thunk and only one of the calls to id occurs, setting it to 1.
;; Only after evaluating w does it set the counter to 2.

;; Exercise 4.28 (page 551)

;; Higher order procedures that take other procedures as arguments would fail
;; if eval didn't use actual-value

;; Exercise 4.31 (if you have time)

;; TODO

;; Exercise 4.33

;; TODO
