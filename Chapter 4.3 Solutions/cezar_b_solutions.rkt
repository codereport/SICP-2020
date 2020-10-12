#lang racket
(require rackunit)
(require rnrs/mutable-pairs-6)
(require compatibility/mlist)
(require racket/stream)
(require algorithms)

;------------------------------------------------------------------
;                           CORE EVALUATOR
;; syntax
(define (true? x) (not (eq? x false)))
(define (false? x) (eq? x false))
(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))
(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))
(define (quoted? exp) (tagged-list? exp 'quote))
(define (text-of-quotation exp) (cadr exp))
(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (make-procedure parameters body env)
  (list 'procedure parameters body env))
(define (compound-procedure? p)
  (tagged-list? p 'procedure))
(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))
(define (variable? exp) (symbol? exp))
(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))
(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))
(define (let? exp) (tagged-list? exp 'let))
(define (let-bindings exp) (cadr exp))
(define (let-body exp) (cddr exp))
(define (definition? exp) (tagged-list? exp 'define))
(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))
(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)
                   (cddr exp)))) 
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
(define (assignment? exp) (tagged-list? exp 'set!))
(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))
; primitives
(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))
(define (primitive-implementation proc) (cadr proc))
(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list 'display display)
        (list 'displayln displayln)
        (list 'format format)
        (list 'format format)
        (list '+ +)
        (list '- -)
        (list 'not not)
        (list '< <)
        (list '<= <=)        
        (list '> >)
        (list '>= >=)
        (list '= =)
        (list 'member member)
        (list 'list list)
        (list 'abs abs)
        (list 'memq memq)
        ))
(define (primitive-procedure-names)
  (map car primitive-procedures))
(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))
(define (apply-primitive-procedure proc args)
  (apply
   (primitive-implementation proc) args))
; environment
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
(define (setup-environment)
  (let ((initial-env
         (extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))
(define the-global-environment (setup-environment))
;------------------------------------------------------------------


(define (analyze-self-evaluating exp)
  (lambda (env succeed fail)
    (succeed exp fail)))

(define (analyze-quoted exp)
  (let ((qval (text-of-quotation exp)))
    (lambda (env succeed fail)
      (succeed qval fail))))

(define (analyze-variable exp)
  (lambda (env succeed fail)
    (succeed (lookup-variable-value exp env) fail)))

(define (analyze-lambda exp)
  (let ((vars (lambda-parameters exp))
        (bproc (analyze-sequence (lambda-body exp))))
    (lambda (env succeed fail)
      (succeed (make-procedure vars bproc env) fail))))

(define (analyze-let exp)
  (let* ((body (analyze-sequence (let-body exp)))
         (arg-names (map car (let-bindings exp)))
         (arg-procs (map analyze (map cadr (let-bindings exp)))))
    (lambda (env succeed fail)
      (get-args arg-procs env
                ; if I can get some arguments then create a new
                ; environment and evaluate the body of let in
                ; in this  new  environment
                (lambda (val fail1)
                  (body
                   (extend-environment arg-names
                                       val
                                       env)
                   (lambda (val2 fail2)
                     ; If the body of lambda succeeds then call the continuation
                     ; But pay atention - the success alternative may not be happy
                     ; with this new set of arguments: In this case try a new set of arguments (goto fail1)
                     (succeed val2 fail1))
                   ; the body of lambda was not happy with these arguments
                   ; so what can we do? try other arguments (fail1)
                   fail1))
                fail))))

(define (analyze-if exp)
  (let ((pproc (analyze (if-predicate exp)))
        (cproc (analyze (if-consequent exp)))
        (aproc (analyze (if-alternative exp))))
    (lambda (env succeed fail)
      (pproc env
             ;; success continuation for evaluating the predicate
             ;; to obtain pred-value
             (lambda (pred-value fail2)
               (if (true? pred-value)
                   (cproc env succeed fail2)
                   (aproc env succeed fail2)))
             ;; failure continuation for evaluating the predicate
             fail))))

(define (analyze-definition exp)
  (let ((var (definition-variable exp))
        (vproc (analyze (definition-value exp))))
    (lambda (env succeed fail)
      (vproc env
             (lambda (val fail2)
               (define-variable! var val env)
               (succeed 'ok fail2))
             fail))))

(define (analyze-assignment exp)
  (let ((var (assignment-variable exp))
        (vproc (analyze (assignment-value exp))))
    (lambda (env succeed fail)
      (vproc env
             (lambda (val fail2) ; *1*
               (let ((old-value
                      (lookup-variable-value var env)))
                 (set-variable-value! var val env)
                 (succeed 'ok
                          (lambda () ; *2*
                            (set-variable-value!
                             var old-value env)
                            (fail2)))))
             fail))))

(define (analyze-application exp)
  (let ((fproc (analyze (operator exp)))
        (aprocs (map analyze (operands exp))))
    (lambda (env succeed fail)
      (fproc env
             (lambda (proc fail2)
               (get-args aprocs
                         env
                         (lambda (args fail3)
                           (execute-application
                            proc args succeed fail3))
                         fail2))
             fail))))
(define (get-args aprocs env succeed fail)
  (if (null? aprocs)
      (succeed '() fail)
      ((car aprocs)
       env
       ;; success continuation for this aproc
       (lambda (arg fail2)
         (get-args
          (cdr aprocs)
          env
          ;; success continuation for
          ;; recursive call to get-args
          (lambda (args fail3)
            (succeed (cons arg args) fail3))
          fail2))
       fail)))

(define (execute-application proc args succeed fail)
  (cond ((primitive-procedure? proc)
         (succeed (apply-primitive-procedure proc args)
                  fail))
        ((compound-procedure? proc)
         ((procedure-body proc)
          (extend-environment
           (procedure-parameters proc)
           args
           (procedure-environment proc))
          succeed
          fail))
        (else
         (error "Unknown procedure type: EXECUTE-APPLICATION"
                proc))))

(define (amb? exp) (tagged-list? exp 'amb))
(define (amb-choices exp) (cdr exp))

(define (analyze-amb exp)
  (let ((cprocs (map analyze (amb-choices exp))))
    (lambda (env succeed fail)
      (define (try-next choices)
        (if (null? choices)
            (fail)
            ((car choices)
             env
             succeed
             (lambda () (try-next (cdr choices))))))
      (try-next cprocs))))

(define (analyze-sequence exps)
  (define (sequentially a b)
    (lambda (env succeed fail)
      (a env
         ;; success continuation for calling a
         (lambda (a-value fail2)
           (b env succeed fail2))
         ;; failure continuation for calling a
         fail)))
  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
        first-proc
        (loop (sequentially first-proc
                            (car rest-procs))
              (cdr rest-procs))))
  (let ((procs (map analyze exps)))
    (when (null? procs)
        (error "Empty sequence: ANALYZE"))
    (loop (car procs) (cdr procs))))



(define (analyze exp)
  (cond ((self-evaluating? exp) (analyze-self-evaluating exp))
        ((quoted? exp) (analyze-quoted exp))
        ((variable? exp) (analyze-variable exp))
        ((assignment? exp) (analyze-assignment exp))
        ((definition? exp) (analyze-definition exp))
        ((if? exp) (analyze-if exp))
        ((lambda? exp) (analyze-lambda exp))
        ((begin? exp) (analyze-sequence (begin-actions exp)))
        ((cond? exp) (analyze (cond->if exp)))
        ((amb? exp) (analyze-amb  exp))
        ((ramb? exp) (analyze-ramb  exp))        
        ((let? exp) (analyze-let  exp))
        ((application? exp) (analyze-application exp))
        (else (error "Unknown expression type: ANALYZE" exp))))

(define (ambeval exp env succeed fail)
  ((analyze exp) env succeed fail))

(define input-prompt ";;; Amb-Eval input:")
(define output-prompt ";;; Amb-Eval value:")

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



(define (driver-loop)
  (define (internal-loop try-again)
    (prompt-for-input input-prompt)
    (let ((input (read)))
      (if (eq? input 'try-again)
          (try-again)
          (begin
            (newline) (display ";;; Starting a new problem ")
            (ambeval
             input
             the-global-environment
             ;; ambeval success
             (lambda (val next-alternative)
               (announce-output output-prompt)
               (user-print val)
               (internal-loop next-alternative))
             ;; ambeval failure
             (lambda ()
               (announce-output
                ";;; There are no more values of")
               (user-print input)
               (driver-loop)))))))
  (internal-loop
   (lambda ()
     (newline) (display ";;; There is no current problem")
     (driver-loop))))

(define (distinct? items)
  (cond ((null? items) true)
        ((null? (cdr items)) true)
        ((member (car items) (cdr items)) false)
        (else (distinct? (cdr items)))))

;-----------------------------------------------


(define (stream-cartesian-product all-axis )
  (define (list->stream els)
    (if (null? els)
        empty-stream
        (stream-cons (car els) (list->stream (stream-rest els)))))
  
  (define (make-axis-stream axis rest-of-product)
    (if (null? axis) 
        empty-stream
        (stream-cons (cons (car axis) rest-of-product)
                     (make-axis-stream (cdr axis) rest-of-product))))
  
  (define (axis-times-streams axis rest-of-products-stream)
    (cond ((null? axis) empty-stream)
          ((stream-empty? rest-of-products-stream)
           (list->stream (map list axis)))        
          (else (stream-append
                 (make-axis-stream axis (stream-first rest-of-products-stream))
                 (if (not (stream-empty? (stream-rest rest-of-products-stream)))
                     (axis-times-streams axis (stream-rest rest-of-products-stream))
                     empty-stream)))))  
  (if (null? all-axis)
      empty-stream
      (let ((rest-of-products-stream (stream-cartesian-product (cdr all-axis)))
            (axis (car all-axis)))
        (axis-times-streams axis rest-of-products-stream))))


; Ex 4.35
(module+ test
  (check-equal? 2
                (ambeval '(begin
                            (define (require p) (if (not p) (amb)))
                            
                            (define (an-integer-between a b)
                              (require (<= a b))
                              (amb a (an-integer-between (+ a 1) b)))
                            (an-integer-between 2 10)                            
                            )
                         
                         (setup-environment)
                         (lambda (val next) val)
                         (lambda () (display "fail")))))




;; ; Ex 4.38
; The implementation of analyze-let is at line 209

; - solution 1 use the new evaluator
(define (solve438-1)
  (define result '())
  (ambeval '(begin
              (define (require p) (if (not p) (amb)))
              (define (distinct? items)
                (cond ((null? items) true)
                      ((null? (cdr items)) true)
                      ((member (car items) (cdr items)) false)
                      (else (distinct? (cdr items)))))            
              (define (multiple-dwelling)
              (let ((baker (amb 1 2 3 4 5))
                    (cooper (amb 1 2 3 4 5))
                    (fletcher (amb 1 2 3 4 5))
                    (miller (amb 1 2 3 4 5))
                    (smith (amb 1 2 3 4 5)))
                (require
                 (distinct? (list baker cooper fletcher miller smith)))
                (require (not (= cooper 1)))
                (require (not (= fletcher 5)))
                (require (not (= fletcher 1)))
                (require (> miller cooper))
                (require (not (= (abs (- fletcher cooper)) 1)))
                (list (list 'baker baker) (list 'cooper cooper)
                      (list 'fletcher fletcher) (list 'miller miller)
                      (list 'smith smith))))
            (multiple-dwelling))
           (setup-environment)
           (lambda (val next) (set! result (cons val result)) (next))
           (lambda () 'fail))
  result)
  
; - solution 2 brute force using lazy streams for cartesian product
(define (solve438-2)
  (define (test baker cooper fletcher miller smith)
    (and
     (distinct? (list baker cooper fletcher miller smith))
     (not (= cooper 1))
     (not (= fletcher 5))
     (not (= fletcher 1))
     (> miller cooper)
     (not (= (abs (- fletcher cooper)) 1))))
  (define axis '(1 2 3 4 5))
  (map (lambda (sol) (zip '(baker cooper fletcher miller smith) sol))
   (stream->list (stream-filter (lambda (el) (apply test el))
                                (stream-cartesian-product (list axis axis axis axis axis))))))


(module+ test
  (check-equal? (solve438-1)
                '(((baker 5) (cooper 2) (fletcher 4) (miller 3) (smith 1))
                  ((baker 3) (cooper 4) (fletcher 2) (miller 5) (smith 1))
                  ((baker 3) (cooper 2) (fletcher 4) (miller 5) (smith 1))
                  ((baker 1) (cooper 4) (fletcher 2) (miller 5) (smith 3))
                  ((baker 1) (cooper 2) (fletcher 4) (miller 5) (smith 3))
                  ((baker 1) (cooper 2) (fletcher 4) (miller 3) (smith 5))))
  (check-equal? (solve438-2)
                '(((baker 5) (cooper 2) (fletcher 4) (miller 3) (smith 1))
                  ((baker 3) (cooper 4) (fletcher 2) (miller 5) (smith 1))
                  ((baker 3) (cooper 2) (fletcher 4) (miller 5) (smith 1))
                  ((baker 1) (cooper 4) (fletcher 2) (miller 5) (smith 3))
                  ((baker 1) (cooper 2) (fletcher 4) (miller 5) (smith 3))
                  ((baker 1) (cooper 2) (fletcher 4) (miller 3) (smith 5))))
  )

; Ex 4.41

(define (solve441)
  (define (test baker cooper fletcher miller smith)
    (and
     (distinct? (list baker cooper fletcher miller smith))
     (not (= cooper 1))
     (not (= fletcher 5))
     (not (= fletcher 1))
     (> miller cooper)
     (not (= (abs (- smith fletcher)) 1))
     (not (= (abs (- fletcher cooper)) 1))))
  (define axis '(1 2 3 4 5))
  (stream->list (stream-filter (lambda (el) (apply test el))
                               (stream-cartesian-product (list axis axis axis axis axis)))))

; Ex 4.42 - This solution uses streams
; 1=Kitty,2=Joan,3=Betty,4=Mary,5=Ethel
(define (solve442)
  (define (test b e j k m)
    (and
     (distinct? (list b e j k m))
     (xor (= k 2) (= b 3))
     (xor (= e 1) (= j 2))
     (xor (= j 3) (= e 5))
     (xor (= k 2) (= m 4))
     (xor (= m 4) (= b 1))
     ))
  (define axis '(1 2 3 4 5))
  (stream->list (stream-filter (lambda (el) (apply test el))
                               (stream-cartesian-product (list axis axis axis axis axis)))))


; Ex 4.49

(define (solve449)
  (define result '())
  (ambeval '(begin
              (define (require p) (if (not p) (amb)))
              (define nouns '(student professor cat class))
              (define verbs '(studies lectures eats sleeps))
              (define articles '(the a))
              (define prepositions '(for to in by with))
              (define (amb-word word-list)
                (if (null? word-list)
                    (amb)
                    (amb (car word-list) (amb-word (cdr word-list))))) 
              (define (generate)
                (let ((article (amb-word articles))
                      (noun (amb-word nouns))
                      (verb (amb-word verbs)))
                  (list article noun verb)))
              (generate))           
           (setup-environment)
           (lambda (val next) (set! result (cons val result)) (next))
           (lambda () 'fail))
  result)

;(a class sleeps)
;(a class eats)
;(a class lectures)
;(a class studies)
;(a cat sleeps)
;(a cat eats)
;(a cat lectures)
;(a cat studies)
;(a professor sleeps)
;(a professor eats)


; Ex 4.50

; Usage: (ramb articles nouns verbs)
; makes a random phrase with 3 words chosen at random from each list

(define (ramb? exp) (tagged-list? exp 'ramb))
(define (ramb-choices exp) (cdr exp))

(define (analyze-ramb exp)
  (let ((cprocs (map analyze (ramb-choices exp))))
    (lambda (env succeed fail)
      (define (try-next)
        (define result '())
        (map (lambda (proc)
               (proc env
                     (lambda (val fail)
                       (set! result (cons (car (shuffle val)) result)))

                     fail))
             cprocs)
        (succeed (reverse result) (lambda () (try-next))))
      (try-next))))

(define (solve450)
  (define result '())
  (ambeval '(begin
              (define nouns '(student professor cat class))
              (define verbs '(studies lectures eats sleeps))
              (define articles '(the a))
              (define prepositions '(for to in by with))
              (define (generate)
                (displayln (ramb articles nouns verbs)))
              (generate))           
           (setup-environment)
           (lambda (val next) (set! result (cons val result)) (next))
           (lambda () 'fail))
  result)
;(solve450)
