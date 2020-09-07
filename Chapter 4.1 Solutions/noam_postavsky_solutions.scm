(module eval-4.1 (driver-loop)
  (import (only scheme quote define cond lambda if let let* quasiquote unquote)
          (only chicken.base error gensym)
          (prefix scheme s:))
  (define (tagged-list? exp tag)
    (if (s:pair? exp)
        (s:eq? (s:car exp) tag)
        #f))

  ;; Application.
  (define (application? exp) (s:pair? exp))
  (define (operator exp) (s:car exp))
  (define (operands exp) (s:cdr exp))
  (define (no-operands? ops) (s:null? ops))
  (define (first-operand ops) (s:car ops))
  (define (rest-operands ops) (s:cdr ops))
  (define (make-application operator operands)
    (s:cons operator operands))

  
  (define (list-of-values exps env)
    (if (no-operands? exps)
        '()
        (s:cons (eval (first-operand exps) env)
                (list-of-values (rest-operands exps) env))))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; - 4.1
  (define (list-of-values-left-to-right exps env)
    (if (no-operands? exps)
        '()
        (let ((1st (eval (first-operand exps) env)))
          (s:cons 1st (list-of-values (rest-operands exps) env)))))
  (define (list-of-values-right-to-left exps env)
    (if (no-operands? exps)
        '()
        (let ((rest (list-of-values (rest-operands exps) env)))
          (s:cons (eval (first-operand exps) env)
                  rest))))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define (apply procedure arguments)
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
            "Unknown procedure type -- APPLY" procedure))))


  ;; Begin sequence.
  (define (begin? exp) (tagged-list? exp 'begin))
  (define (begin-actions exp) (s:cdr exp))
  (define (last-exp? seq) (s:null? (s:cdr seq)))
  (define (first-exp seq) (s:car seq))
  (define (rest-exps seq) (s:cdr seq))
  (define (sequence->exp seq)
    (cond ((s:null? seq) seq)
          ((last-exp? seq) (first-exp seq))
          (else (make-begin seq))))
  (define (make-begin seq) (s:cons 'begin seq))
  (define (eval-sequence exps env)
    (cond ((last-exp? exps) (eval (first-exp exps) env))
          (else (eval (first-exp exps) env)
                (eval-sequence (rest-exps exps) env))))


  ;; Lambda.
  (define (lambda? exp) (tagged-list? exp 'lambda))
  (define (lambda-parameters exp) (s:cadr exp))
  (define (lambda-body exp) (s:cddr exp))
  (define (make-lambda parameters body)
    (s:cons 'lambda (s:cons parameters body)))

  ;; Define.
  (define (definition? exp)
    (tagged-list? exp 'define))
  (define (definition-variable exp)
    (if (s:symbol? (s:cadr exp))
        (s:cadr exp)
        (s:caadr exp)))
  (define (definition-value exp)
    (if (s:symbol? (s:cadr exp))
        (s:caddr exp)
        (make-lambda (s:cdadr exp)   ; formal parameters
                     (s:cddr exp)))) ; body
  (define (make-definition var value)
    (s:list 'define var value))
  (define (eval-definition exp env)
    (define-variable! (definition-variable exp)
      (eval (definition-value exp) env)
      env)
    'ok)

  ;; If.
  (define (if? exp) (tagged-list? exp 'if))
  (define (if-predicate exp) (s:cadr exp))
  (define (if-consequent exp) (s:caddr exp))
  (define (if-alternative exp)
    (if (s:not (s:null? (s:cdddr exp)))
        (s:cadddr exp)
        'false))
  (define (make-if predicate consequent alternative)
    (s:list 'if predicate consequent alternative))
  (define (false? x) (s:eq? x #f))
  (define (true? x) (s:not (false? x)))
  (define (eval-if exp env)
    (if (true? (eval (if-predicate exp) env))
        (eval (if-consequent exp) env)
        (eval (if-alternative exp) env)))

  ;; Atoms.
  (define (self-evaluating? exp)
    (cond ((s:number? exp) #t)
          ((s:string? exp) #t)
          (else #f)))
  (define (variable? exp) (s:symbol? exp))
  ;; Quote.
  (define (quoted? exp)
    (tagged-list? exp 'quote))
  (define (text-of-quotation exp) (s:cadr exp))

  ;; Assignment.
  (define (assignment? exp)
    (tagged-list? exp 'set!))
  (define (assignment-variable exp) (s:cadr exp))
  (define (assignment-value exp) (s:caddr exp))
  (define (eval-assignment exp env)
    (set-variable-value! (assignment-variable exp)
                         (eval (assignment-value exp) env)
                         env)
    'ok)

  ;; Cond.
  (define (cond? exp) (tagged-list? exp 'cond))
  (define (cond-clauses exp) (s:cdr exp))
  (define (cond-else-clause? clause)
    (s:eq? (cond-predicate clause) 'else))
  (define (cond-predicate clause) (s:car clause))
  (define (cond-actions clause) (s:cdr clause))
  (define (cond->if exp)
    (define (expand-clauses clauses)
      (if (s:null? clauses)
          'false                          ; no `else' clause
          (let ((first (s:car clauses))
                (rest (s:cdr clauses)))
            (if (cond-else-clause? first)
                (if (s:null? rest)
                    (sequence->exp (cond-actions first))
                    (error "ELSE clause isn't last -- COND->IF"
                           clauses))
                (make-if (cond-predicate first)
                         (sequence->exp (cond-actions first))
                         (expand-clauses rest))))))
    (expand-clauses (cond-clauses exp)))

  ;; Eval.
  (define (eval exp env)
    ;; (s:map s:display `("eval: " ,exp "\n"))
    (cond ((self-evaluating? exp) exp)
          ((variable? exp) (lookup-variable-value exp env))
          ((quoted? exp) (text-of-quotation exp))
          ((assignment? exp) (eval-assignment exp env))
          ((definition? exp) (eval-definition exp env))
          ((if? exp) (eval-if exp env))
          ((lambda? exp)
           (make-procedure (lambda-parameters exp)
                           (lambda-body exp)
                           env))
          ;; ((let? exp) (let->combination exp))
          ((let? exp) (eval (let->combination exp) env))
          ;; ((while? exp) (while->recursion exp))
          ((while? exp) (eval (while->recursion exp) env))
          ((begin? exp)
           (eval-sequence (begin-actions exp) env))
          ((cond? exp) (eval (cond->if exp) env))
          ((application? exp)
           (apply (eval (operator exp) env)
                  (list-of-values (operands exp) env)))
          (else
           (error "Unknown expression type -- EVAL" exp))))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; - 4.2 a)
  ;;; Assignment statements will satisfy both the application? and
  ;;; assignment? procedures.  So putting the application? clause
  ;;; first will mean that the assignment? clause is never reached.
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; - 4.4
  ;; And.
  (define (and? exp) (tagged-list? exp 'and))
  (define (and-operands exp) (s:cdr exp))
  (define (eval-and exp env)
    (define (eval-and-subsequence operands)
      (if (s:null? operands)
          'true
          (let ((1st (eval (s:car operands) env)))
            (if (false? 1st) 'false
                (eval-and-subsequence (s:cdr operands))))))
    (eval-and-subsequence (and-operands exp)))
  ;; Or.
  (define (or? exp) (tagged-list? exp 'or))
  (define (or-operands exp) (s:cdr exp))
  (define (eval-or exp env)
    (define (eval-or-subsequence operands)
      (if (s:null? operands)
          'false
          (let ((1st (eval (s:car operands) env)))
            (if (true? 1st) 1st
                (eval-or-subsequence (s:cdr operands))))))
    (eval-or-subsequence (or-operands exp)))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; - 4.6
  ;; Let.
  (define (let? exp) (tagged-list? exp 'let))
  (define (let-bindings exp) (s:cadr exp))
  (define (let-body exp) (s:cddr exp))
  (define (let->combination exp)
    (let* ((bindings (let-bindings exp))
           (body (let-body exp))
           (vars (s:map s:car bindings))
           (exps (s:map s:cadr bindings)))
      (make-application (make-lambda vars body)
                        exps)))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; - 4.9 (while <PRED> <BODY...>)
  (define (while? exp) (tagged-list? exp 'while))
  (define (while-predicate exp) (s:cadr exp))
  (define (while-body exp) (s:cddr exp))
  (define (while->recursion exp)
    (let ((predicate (while-predicate exp))
          (body (while-body exp))
          ;; XXX: possible without gensym?
          (recursive-name (gensym "while")))
      (make-begin
       (s:list (make-definition
                recursive-name
                (make-lambda
                 '()
                 (s:list
                  (make-if predicate
                           (make-begin
                            (s:append body
                                      (s:list (make-application recursive-name '()))))
                           'false))))
               (make-application recursive-name '())))))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; Procedures.
  (define (make-procedure parameters body env)
    (s:list 'procedure parameters body env))
  (define (compound-procedure? p)
    (tagged-list? p 'procedure))
  (define (procedure-parameters p) (s:cadr p))
  (define (procedure-body p) (s:caddr p))
  (define (procedure-environment p) (s:cadddr p))

  ;; Primitive Procedures.
  (define (primitive-procedure? proc)
    (tagged-list? proc 'primitive))
  (define (primitive-implementation proc) (s:cadr proc))
  (define primitive-procedures
    (s:list (s:list 'car s:car)
            (s:list 'cdr s:cdr)
            (s:list 'cons s:cons)
            (s:list 'null? s:null?)
            ;; <MORE PRIMITIVES>
            (s:list '+ s:+)
            (s:list '< s:<)
            ))
  (define (primitive-procedure-names)
    (s:map s:car primitive-procedures))
  (define (primitive-procedure-objects)
    (s:map (lambda (proc) (s:list 'primitive (s:cadr proc)))
           primitive-procedures))
  (define (apply-primitive-procedure proc args)
    (s:apply (primitive-implementation proc) args))


  ;; Environment.
  (define (enclosing-environment env) (s:cdr env))
  (define (first-frame env) (s:car env))
  (define the-empty-environment '())
  (define (make-frame variables values)
    (s:cons variables values))
  (define (frame-variables frame) (s:car frame))
  (define (frame-values frame) (s:cdr frame))
  (define (add-binding-to-frame! var val frame)
    (s:set-car! frame (s:cons var (s:car frame)))
    (s:set-cdr! frame (s:cons val (s:cdr frame))))

  (define (extend-environment vars vals base-env)
    (if (s:= (s:length vars) (s:length vals))
        (s:cons (make-frame vars vals) base-env)
        (if (s:< (s:length vars) (s:length vals))
            (error "Too many arguments supplied" vars vals)
            (error "Too few arguments supplied" vars vals))))

  (define (lookup-variable-value var env)
    (define (env-loop env)
      (define (scan vars vals)
        (cond ((s:null? vars)
               (env-loop (enclosing-environment env)))
              ((s:eq? var (s:car vars))
               (s:car vals))
              (else (scan (s:cdr vars) (s:cdr vals)))))
      (if (s:eq? env the-empty-environment)
          (error "Unbound variable" var)
          (let ((frame (first-frame env)))
            (scan (frame-variables frame)
                  (frame-values frame)))))
    (env-loop env))

  (define (set-variable-value! var val env)
    (define (env-loop env)
      (define (scan vars vals)
        (cond ((s:null? vars)
               (env-loop (enclosing-environment env)))
              ((s:eq? var (s:car vars))
               (s:set-car! vals val))
              (else (scan (s:cdr vars) (s:cdr vals)))))
      (if (s:eq? env the-empty-environment)
          (error "Unbound variable -- SET!" var)
          (let ((frame (first-frame env)))
            (scan (frame-variables frame)
                  (frame-values frame)))))
    (env-loop env))
  (define (define-variable! var val env)
    (let ((frame (first-frame env)))
      (define (scan vars vals)
        (cond ((s:null? vars)
               (add-binding-to-frame! var val frame))
              ((s:eq? var (s:car vars))
               (s:set-car! vals val))
              (else (scan (s:cdr vars) (s:cdr vals)))))
      (scan (frame-variables frame)
            (frame-values frame))))

  (define (setup-environment)
    (let ((initial-env
           (extend-environment (primitive-procedure-names)
                               (primitive-procedure-objects)
                               the-empty-environment)))
      (define-variable! 'true #t initial-env)
      (define-variable! 'false #f initial-env)
      initial-env))
  (define the-global-environment (setup-environment))

  ;; ;; REPL.
  (define input-prompt ";;; M-Eval input:")
  (define output-prompt ";;; M-Eval value:")
  (define (driver-loop)
    (prompt-for-input input-prompt)
    (let ((input (s:read)))
      (if (s:eq? input #!eof)
          (announce-output ";;; bye!")
          (let ((output (eval input the-global-environment)))
            (announce-output output-prompt)
            (user-print output)
            (s:newline)
            (driver-loop)))))
  (define (prompt-for-input string) (s:display string) (s:newline))
  (define (announce-output string) (s:display string) (s:newline))
  (define (user-print object)
    (if (compound-procedure? object)
        (s:display (s:list 'compound-procedure
                           (procedure-parameters object)
                           (procedure-body object)
                           '<procedure-env>))
        (s:display object)))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; - 4.14
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  )
