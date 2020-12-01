lang racket
(require rackunit)
(require rnrs/mutable-pairs-6)
(require compatibility/mlist)
(require racket/stream)

;------------------------------------------------------
(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))


(define ops (make-hash))
;(define (opkey op type) (format "~s|~s" op type))
(define (opkey op type) (cons op type))
(define (put op type item) (hash-set! ops (opkey op type) item))    
(define (get op type) (hash-ref ops (opkey op type) #f))


; 4.4.4.1 The Driver Loop and Instantiation

(define input-prompt ";;; Query input:")
(define output-prompt ";;; Query results:")

(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))
(define (query-driver-loop)
  (prompt-for-input input-prompt)
  (let ((q (query-syntax-process (read))))
    (cond ((assertion-to-be-added? q)
           (add-rule-or-assertion! (add-assertion-body q))
           (newline)
           (display "Assertion added to data base.")
           (query-driver-loop))
          (else
           (newline)
           (displayln output-prompt)
           (display-stream
            (stream-map
             (lambda (frame)
               (instantiate
                   q
                   frame
                 (lambda (v f)
                   (contract-question-mark v))))
             (qeval q (singleton-stream '()))))
           (query-driver-loop)))))  
(define (instantiate exp frame unbound-var-handler)
  (define (copy exp)
    (cond ((var? exp)
           (let ((binding (binding-in-frame exp frame)))
             (if binding
                 (copy (binding-value binding))
                 (unbound-var-handler exp frame))))
          ((pair? exp)
           (cons (copy (car exp)) (copy (cdr exp))))
          (else exp)))
  (copy exp)) 

; 4.4.4.2 The Evaluator
(define (qeval query frame-stream)
  (let ((qproc (get (type query) 'qeval)))
    (if qproc
        (qproc (contents query) frame-stream)
        (simple-query query frame-stream))))

(define (simple-query query-pattern frame-stream)
  (stream-flatmap
   (lambda (frame)
     (stream-append-delayed
      (find-assertions query-pattern frame)
      (delay (apply-rules query-pattern frame))))
   frame-stream))

(define (conjoin conjuncts frame-stream)
  (if (empty-conjunction? conjuncts)
      frame-stream
      (conjoin (rest-conjuncts conjuncts)
               (qeval (first-conjunct conjuncts) frame-stream))))

(put 'and 'qeval conjoin)

(define (disjoin disjuncts frame-stream)
  (if (empty-disjunction? disjuncts)
      empty-stream
      (interleave-delayed
       (qeval (first-disjunct disjuncts) frame-stream)
       (delay (disjoin (rest-disjuncts disjuncts) frame-stream)))))
(put 'or 'qeval disjoin)

(define (negate operands frame-stream)
  (stream-flatmap
   (lambda (frame)
     (if (stream-empty?
          (qeval (negated-query operands)
                 (singleton-stream frame)))
         (singleton-stream frame)
         empty-stream))
   frame-stream))
(put 'not 'qeval negate)

(define (lisp-value call frame-stream)
  (stream-flatmap
   (lambda (frame)
     (if (execute
          (instantiate
              call
              frame
            (lambda (v f)
              (error "Unknown pat var: LISP-VALUE" v))))
         (singleton-stream frame)
         empty-stream))
   frame-stream))
(put 'lisp-value 'qeval lisp-value)

(define (execute exp)
  (apply (eval (predicate exp) )
         (args exp)))
(define (always-true ignore frame-stream) frame-stream)
(put 'always-true 'qeval always-true)

; 4.4.4.3 Finding Assertions by Pattern Matching

(define (find-assertions pattern frame)
  (stream-flatmap
   (lambda (datum) (check-an-assertion datum pattern frame))
   (fetch-assertions pattern frame)))

(define (check-an-assertion assertion query-pat query-frame)
  (let ((match-result
         (pattern-match query-pat assertion query-frame)))
    (if (eq? match-result 'failed)
        empty-stream
        (singleton-stream match-result))))

(define (pattern-match pat dat frame)
  (cond ((eq? frame 'failed) 'failed)
        ((equal? pat dat) frame)
        ((var? pat) (extend-if-consistent pat dat frame))
        ((and (pair? pat) (pair? dat))
         (pattern-match
          (cdr pat)
          (cdr dat)
          (pattern-match (car pat) (car dat) frame)))
        (else 'failed)))

(define (extend-if-consistent var dat frame)
  (let ((binding (binding-in-frame var frame)))
    (if binding
        (pattern-match (binding-value binding) dat frame)
        (extend var dat frame))))

; 4.4.4.4 Rules and Unification

(define (apply-rules pattern frame)
  (stream-flatmap (lambda (rule)
                    (apply-a-rule rule pattern frame))
                  (fetch-rules pattern frame)))
(define (apply-a-rule rule query-pattern query-frame)
  (let ((clean-rule (rename-variables-in rule)))
    (let ((unify-result (unify-match query-pattern
                                     (conclusion clean-rule)
                                     query-frame)))
      (if (eq? unify-result 'failed)
          empty-stream
          (qeval (rule-body clean-rule)
                 (singleton-stream unify-result))))))
(define (rename-variables-in rule)
  (let ((rule-application-id (new-rule-application-id)))
    (define (tree-walk exp)
      (cond ((var? exp)
             (make-new-variable exp rule-application-id))
            ((pair? exp)
             (cons (tree-walk (car exp))
                   (tree-walk (cdr exp))))
            (else exp)))
    (tree-walk rule)))
(define (unify-match p1 p2 frame)
  (cond ((eq? frame 'failed) 'failed)
        ((equal? p1 p2) frame)
        ((var? p1) (extend-if-possible p1 p2 frame))
        ((var? p2) (extend-if-possible p2 p1 frame)) ; ***
        ((and (pair? p1) (pair? p2))
         (unify-match (cdr p1)
                      (cdr p2)
                      (unify-match (car p1)
                                   (car p2)
                                   frame)))
        (else 'failed)))        

(define (extend-if-possible var val frame)
  (let ((binding (binding-in-frame var frame)))
    (cond (binding
           (unify-match (binding-value binding) val frame))
          ((var? val) ; ***
           (let ((binding (binding-in-frame val frame)))
             (if binding
                 (unify-match
                  var (binding-value binding) frame)
                 (extend var val frame))))
          ((depends-on? val var frame) ; ***
           'failed)
          (else (extend var val frame)))))         
(define (depends-on? exp var frame)
  (define (tree-walk e)
    (cond ((var? e)
           (if (equal? var e)
               true
               (let ((b (binding-in-frame e frame)))
                 (if b
                     (tree-walk (binding-value b))
                     false))))
          ((pair? e)
           (or (tree-walk (car e))
               (tree-walk (cdr e))))
          (else false)))
  (tree-walk exp))

; 4.4.4.5 Maintaining the Data Base

(define THE-ASSERTIONS empty-stream)
(define (fetch-assertions pattern frame)
  (if (use-index? pattern)
      (get-indexed-assertions pattern)
      (get-all-assertions)))
(define (get-all-assertions) THE-ASSERTIONS)
(define (get-indexed-assertions pattern)
  (get-stream (index-key-of pattern) 'assertion-stream))

(define (get-stream key1 key2)
  (let ((s (get key1 key2)))
    (if s s empty-stream)))
(define THE-RULES empty-stream)
(define (fetch-rules pattern frame)
  (if (use-index? pattern)
      (get-indexed-rules pattern)
      (get-all-rules)))
(define (get-all-rules) THE-RULES)
(define (get-indexed-rules pattern)
  (stream-append
   (get-stream (index-key-of pattern) 'rule-stream)
   (get-stream '? 'rule-stream)))
(define (add-rule-or-assertion! assertion)

  (if (rule? assertion)
      (add-rule! assertion)
      (add-assertion! assertion)))
(define (add-assertion! assertion)
  (store-assertion-in-index assertion)
  (let ((old-assertions THE-ASSERTIONS))
    (set! THE-ASSERTIONS
          (stream-cons assertion old-assertions))
    'ok))
(define (add-rule! rule)
  (store-rule-in-index rule)
  (let ((old-rules THE-RULES))
    (set! THE-RULES (stream-cons rule old-rules))
    'ok))

(define (store-assertion-in-index assertion)
  (when (indexable? assertion)
      (let ((key (index-key-of assertion)))
        (let ((current-assertion-stream
               (get-stream key 'assertion-stream)))
          (put key
               'assertion-stream
               (stream-cons
                assertion
                current-assertion-stream))))))
(define (store-rule-in-index rule)
  (let ((pattern (conclusion rule)))
    (when (indexable? pattern)
        (let ((key (index-key-of pattern)))
          (let ((current-rule-stream
                 (get-stream key 'rule-stream)))
            (put key
                 'rule-stream
                 (stream-cons rule
                              current-rule-stream)))))))
(define (indexable? pat)
  (or (constant-symbol? (car pat))
      (var? (car pat))))
(define (index-key-of pat)
  (let ((key (car pat)))
    (if (var? key) '? key)))
(define (use-index? pat) (constant-symbol? (car pat)))

; 4.4.4.6 Stream Operations
(define (stream-append-delayed s1 delayed-s2)
  (if (stream-empty? s1)
      (force delayed-s2)
      (stream-cons
       (stream-first s1)
       (stream-append-delayed
        (stream-rest s1)
        delayed-s2))))
(define (interleave-delayed s1 delayed-s2)
  (if (stream-empty? s1)
      (force delayed-s2)
      (stream-cons
       (stream-first s1)
       (interleave-delayed
        (force delayed-s2)
        (delay (stream-rest s1))))))

(define (stream-flatmap proc s)
  
  (flatten-stream (stream-map proc s)))
(define (flatten-stream stream)
  (if (stream-empty? stream)
      empty-stream
      (interleave-delayed
       (stream-first stream)
       (delay (flatten-stream (stream-rest stream))))))      

(define (display-stream s)
  (stream-for-each display-line s))
(define (display-line x) (newline) (display x))

(define (singleton-stream x)
  (stream-cons x empty-stream))

; 4.4.4.7 Query Syntax Procedures
(define (type exp)
  (if (pair? exp)
      (car exp)
      (error "Unknown expression TYPE" exp)))
(define (contents exp)
  (if (pair? exp)
      (cdr exp)
      (error "Unknown expression CONTENTS" exp)))
(define (assertion-to-be-added? exp)
  (eq? (type exp) 'assert!))
(define (add-assertion-body exp) (car (contents exp)))
(define (empty-conjunction? exps) (null? exps))
(define (first-conjunct exps) (car exps))
(define (rest-conjuncts exps) (cdr exps))
(define (empty-disjunction? exps) (null? exps))
(define (first-disjunct exps) (car exps))
(define (rest-disjuncts exps) (cdr exps))
(define (negated-query exps) (car exps))
(define (predicate exps) (car exps))
(define (args exps) (cdr exps))
(define (rule? statement)
  (tagged-list? statement 'rule))
(define (conclusion rule) (cadr rule))
(define (rule-body rule)
  (if (null? (cddr rule)) '(always-true) (caddr rule)))
(define (query-syntax-process exp)
  (map-over-symbols expand-question-mark exp))
(define (map-over-symbols proc exp)
  (cond ((pair? exp)
         (cons (map-over-symbols proc (car exp))
               (map-over-symbols proc (cdr exp))))
        ((symbol? exp) (proc exp))
        (else exp)))
(define (expand-question-mark symbol)
  (let ((chars (symbol->string symbol)))
    (if (string=? (substring chars 0 1) "?")
        (list '?
              (string->symbol
               (substring chars 1 (string-length chars))))
        symbol)))
(define (var? exp) (tagged-list? exp '?))
(define (constant-symbol? exp) (symbol? exp))
(define rule-counter 0)
(define (new-rule-application-id)
  (set! rule-counter (+ 1 rule-counter))
  rule-counter)
(define (make-new-variable var rule-application-id)
  (cons '? (cons rule-application-id (cdr var))))
(define (contract-question-mark variable)
  (string->symbol
   (string-append "?"
                  (if (number? (cadr variable))
                      (string-append (symbol->string (caddr variable))
                                     "-"
                                     (number->string (cadr variable)))
                      (symbol->string (cadr variable))))))

; 4.4.4.8 Frames and Bindings
(define (make-binding variable value)
  (cons variable value))
(define (binding-variable binding) (car binding))
(define (binding-value binding) (cdr binding))
(define (binding-in-frame variable frame)
  (assoc variable frame))
(define (extend variable value frame)
  (cons (make-binding variable value) frame))

;-------------------------------------------


(define simple-db-assertions
  '(
    (address (Bitdiddle Ben) (Slumerville (Ridge Road) 10))
    (job (Bitdiddle Ben) (computer wizard))
    (salary (Bitdiddle Ben) 60000)
    (address (Hacker Alyssa P) (Cambridge (Mass Ave) 78))
    (job (Hacker Alyssa P) (computer programmer))
    (salary (Hacker Alyssa P) 40000)
    (supervisor (Hacker Alyssa P) (Bitdiddle Ben))
    (address (Fect Cy D) (Cambridge (Ames Street) 3))
    (job (Fect Cy D) (computer programmer))
    (salary (Fect Cy D) 35000)
    (supervisor (Fect Cy D) (Bitdiddle Ben))
    (address (Tweakit Lem E) (Boston (Bay State Road) 22))
    (job (Tweakit Lem E) (computer technician))
    (salary (Tweakit Lem E) 25000)
    (supervisor (Tweakit Lem E) (Bitdiddle Ben))
    (address (Reasoner Louis) (Slumerville (Pine Tree Road) 80))
    (job (Reasoner Louis) (computer programmer trainee))
    (salary (Reasoner Louis) 30000)
    (supervisor (Reasoner Louis) (Hacker Alyssa P))
    (supervisor (Bitdiddle Ben) (Warbucks Oliver))
    (address (Warbucks Oliver) (Swellesley (Top Heap Road)))
    (job (Warbucks Oliver) (administration big wheel))
    (salary (Warbucks Oliver) 150000)
    (address (Scrooge Eben) (Weston (Shady Lane) 10))
    (job (Scrooge Eben) (accounting chief accountant))
    (salary (Scrooge Eben) 75000)
    (supervisor (Scrooge Eben) (Warbucks Oliver))
    (address (Cratchet Robert) (Allston (N Harvard Street) 16))
    (job (Cratchet Robert) (accounting scrivener))
    (salary (Cratchet Robert) 18000)
    (supervisor (Cratchet Robert) (Scrooge Eben))
    (address (Aull DeWitt) (Slumerville (Onion Square) 5))
    (job (Aull DeWitt) (administration secretary))
    (salary (Aull DeWitt) 25000)
    (supervisor (Aull DeWitt) (Warbucks Oliver))
    (can-do-job (computer wizard) (computer programmer))
    (can-do-job (computer wizard) (computer technician))
    (can-do-job (computer programmer)
                (computer programmer trainee))
    (can-do-job (administration secretary)
                (administration big wheel))

    (rule (same ?x ?x))
    
    (rule (lives-near ?person-1 ?person-2)
          (and (address ?person-1 (?town . ?rest-1))
               (address ?person-2 (?town . ?rest-2))
               (not (same ?person-1 ?person-2))))

    (rule (can-replace ?person-1 ?person-2)
          (and (job ?person-1 (?job . ?rest-1))
               (job ?person-2 (?job . ?rest-2))
               (not (same ?person-1 ?person-2))))

    (rule (append-to-form () ?y ?y))
    (rule (append-to-form (?u . ?v) ?y (?u . ?z))
          (append-to-form ?v ?y ?z))


    )
  )


(define (build-db)
  (map (lambda (e)
         (add-rule-or-assertion! (query-syntax-process e))
         )
       simple-db-assertions)
  'ok)
(build-db)

; Ex 4.55
; 1. (supervisor ?x (Bitdiddle Ben))
; 2. (job ?x (accounting . ?y))
; 3. (address ?x (Slumerville . ?y))

; Ex. 4.58
;; (assert! (rule (big-shot  ?boss)
;;                (and (supervisor ?person ?boss)
;;                     (job ?person (?person-dept . ?x))
;;                     (job ?boss (?boss-dept . ?y))
;;                     (not (same ?boss-dept ?person-dept)))))


; Ex . 4.61
;Q: (?x next-to ?y in (1 (2 3) 4))
; A:
;((2 3) next-to 4 in (1 (2 3) 4))
;(1 next-to (2 3) in (1 (2 3) 4))

; Q: (?x next-to 1 in (2 1 3 1))
; A:
; (3 next-to 1 in (2 1 3 1))
; (2 next-to 1 in (2 1 3 1))



; Ex 4.75

(define (unique-asserted query frame-stream)
  (stream-flatmap
   (lambda (frame)
     (let ((result (qeval (car query) (singleton-stream frame))))
       (if (stream-empty? result)
           empty-stream
           (let ((el (stream-first result)))
             (if (stream-empty? (stream-rest result))
                 (stream-cons el empty-stream)
                 empty-stream)))))
   frame-stream))

(put 'unique 'qeval unique-asserted)


;(define q1 '(and (job ?x ?j) (unique (job ?anyone ?j))))
;(display-stream (qeval (query-syntax-process q1) (singleton-stream '())))

;(define q2 '(and (supervisor ?x ?y) (unique (supervisor ?z ?y))))
;(display-stream (qeval (query-syntax-process q2) (singleton-stream '())))
