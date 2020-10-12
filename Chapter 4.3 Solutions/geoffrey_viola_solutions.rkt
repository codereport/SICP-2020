#lang racket

(require rackunit)

;; https://www.rosettacode.org/wiki/Amb#Racket
;; A quick `amb' implementation (same as in the Twelve Statements task)
(define failures null)
 
(define (fail)
  (if (pair? failures) ((first failures)) (error "no more choices!")))
 
(define (amb/thunks choices)
  (let/cc k (set! failures (cons k failures)))
  (if (pair? choices)
      (let ([choice (first choices)]) (set! choices (rest choices)) (choice))
      (begin (set! failures (rest failures)) (fail))))
 
(define-syntax-rule (amb E ...) (amb/thunks (list (lambda () E) ...)))
 
(define (assert condition) (unless condition (fail)))
;; end snippet

;; (mostly) from the book
(define (sicp-require p)
  (if (not p) (amb) '()))
;; end from the book

;; Exercise 4.35.
(define (an-integer-between low high)
  (sicp-require (<= low high))
  (amb low (an-integer-between (+ low 1) high)))

(define (a-pythagorean-triple-between low high)
  (let ((i (an-integer-between low high)))
    (let ((j (an-integer-between i high)))
      (let ((k (an-integer-between j high)))
        (sicp-require (= (+ (* i i) (* j j)) (* k k)))
        (list i j k)))))

(module+ test
  (begin
    (check-equal? (a-pythagorean-triple-between 3 5) '(3 4 5))
    ))

;; Exercise 4.38.
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
    (sicp-require
     (distinct? (list baker cooper fletcher miller smith)))
    (sicp-require (not (= baker 5)))
    (sicp-require (not (= cooper 1)))
    (sicp-require (not (= fletcher 5)))
    (sicp-require (not (= fletcher 1)))
    (sicp-require (> miller cooper))
    ;; the questions asks that the following condition be removed
    ;(sicp-require (not (= (abs (- smith fletcher)) 1)))
    (sicp-require (not (= (abs (- fletcher cooper)) 1)))
    (list (list 'baker baker)
          (list 'cooper cooper)
          (list 'fletcher fletcher)
          (list 'miller miller)
          (list 'smith smith))))

(module+ test
  (begin
    (check-equal? (multiple-dwelling) '((baker 1) (cooper 2) (fletcher 4) (miller 3) (smith 5)))
    ))

;; try-again doesn't work here, answers from http://community.schemewiki.org/?sicp-ex-4.38
;; ((baker 1) (cooper 2) (fletcher 4) (miller 3) (smith 5))
;; ((baker 1) (cooper 2) (fletcher 4) (miller 5) (smith 3))
;; ((baker 1) (cooper 4) (fletcher 2) (miller 5) (smith 3))
;; ((baker 3) (cooper 2) (fletcher 4) (miller 5) (smith 1))
;; ((baker 3) (cooper 4) (fletcher 2) (miller 5) (smith 1))

;; Exercise 4.41
(define (ordinary-multiple-dwelling-constrained? floor-choices)
  (and
   (distinct? floor-choices)
   (let ((baker (list-ref floor-choices 0))
         (cooper (list-ref floor-choices 1))
         (fletcher (list-ref floor-choices 2))
         (miller (list-ref floor-choices 3))
         (smith (list-ref floor-choices 4)))
     (and (not (= baker 5))
          (not (= cooper 1))
          (not (= fletcher 5))
          (not (= fletcher 1))
          (> miller cooper)
          (not (= (abs (- smith fletcher)) 1))
          (not (= (abs (- fletcher cooper)) 1))))))

(define (ordinary-multiple-dwelling-helper constrained?)
  (define min-floor 1)
  (define max-floor 5)
  (define init-floor min-floor)
  (define (recurse floor-options floor-choices)
    (if (null? floor-options) (if (constrained? floor-choices) floor-choices '())
        (let* ((cur (car floor-options))
               (first-try (recurse (cdr floor-options) (append floor-choices (list cur)))))
          (if (not (null? first-try)) first-try
              (if (>= cur max-floor) '()
                  (recurse (cons (+ cur 1) (cdr floor-options)) floor-choices)
                  )))))
  (recurse (build-list 5 (const init-floor)) '()))

(define (ordinary-multiple-dwelling)
  (ordinary-multiple-dwelling-helper ordinary-multiple-dwelling-constrained?))

(module+ test
  (begin
    (check-equal? (ordinary-multiple-dwelling) '(3 2 4 5 1))
    (check-equal? (ordinary-multiple-dwelling-helper (λ (x) #f)) '())
    ))

;; Exercise 4.42
;; check that truth pairs are bijective to positions and
;; lies don't contradict truths
(define (check-for-lies truths lies)
  (define truths-kv (make-hash))
  (define truths-vk (make-hash))
  (define lies-ht (make-hash lies))
  (define (check-truths remaining-truths)
    (if (null? remaining-truths) #t
        (let* ((ns -1) ; numeric sentinel
               (qs 'Nothing) ; quoted sentinel
               (truth-key (caar remaining-truths))
               (truth-value (cdar remaining-truths))
               (lookup-value (hash-ref truths-kv truth-key ns))
               (lookup-key (hash-ref truths-vk truth-value qs)))
          ;; core conditional
          (if (or (and (> lookup-value ns) (not (= lookup-value truth-value)))
                  (and (not (eq? lookup-key qs)) (not (eq? lookup-key truth-key)))
                  (= (hash-ref lies-ht truth-key ns) truth-value))
              #f
              (begin
                (hash-set! truths-vk truth-value truth-key)
                (hash-set! truths-kv truth-key truth-value)
                (check-truths (cdr remaining-truths)))
              ))))
  (check-truths truths))

(define (solve-liars-puzzle-ordinary)
  (define options '(((Kitty . 2) (Betty . 3))
                    ((Ethel . 1) (Joan . 2))
                    ((Joan . 3) (Ethel . 5))
                    ((Kitty . 2) (Mary . 4))
                    ((Mary . 4) (Betty . 1))))
  (define (recurse options truths lies)
    (if (null? options)
        (if (check-for-lies truths lies)
            truths
            '())
        (let ((first-try (recurse (cdr options) (append truths (list (caar options)))
                                  (append lies (cdar options)))))
          (if (null? first-try)
              (recurse (cdr options) (append truths (cdar options))
                       (append lies (list (caar options))))
              first-try))))
  (recurse options '() '()))

(module+ test
  (begin
    ;; mapping is a function
    (check-equal? (check-for-lies '((a . 1) (a . 2)) '()) #f)
    ;; multiple statements are OK
    (check-equal? (check-for-lies '((a . 1) (b . 2)) '()) #t)
    ;; contradiction
    (check-equal? (check-for-lies '((a . 1)) '((a . 1))) #f)
    ;; irrelevant lie is OK
    (check-equal? (check-for-lies '((a . 1)) '((a . 2))) #t)
    ;; injective check
    (check-equal? (check-for-lies '((a . 1) (b . 1)) '()) #f)
    ;; redundant data is OK
    (check-equal? (check-for-lies '((a . 1) (a . 1)) '()) #t)
    (check-equal? (solve-liars-puzzle-ordinary) '((Betty . 3) (Joan . 2) (Ethel . 5) (Mary . 4) (Mary . 4))
                  )
    ))



;; Exercise 4.49
;; Book code start
(define nouns '(noun student professor cat class))
(define verbs '(verb studies lectures eats sleeps))
(define articles '(article the a))
(define (parse-word word-list)
  (sicp-require (not (null? *unparsed*)))
  (sicp-require (memq (car *unparsed*) (cdr word-list)))
  (let ((found-word (car *unparsed*)))
    (set! *unparsed* (cdr *unparsed*))
    (list (car word-list) found-word)))
;; End Book code

;; http://community.schemewiki.org/?sicp-ex-4.49
;(define (parse-word2 word-list) 
;  (sicp-require (not (null? *unparsed*))) 
;  (set! *unparsed* (cdr *unparsed*)) 
;  (list (car word-list) (amb (cdr word-list))))
(define (list-amb li) 
  (if (null? li) 
      (amb) 
      (amb (car li) (list-amb (cdr li)))))
(define (parse-word3 word-list) 
  (sicp-require (not (null? *unparsed*))) 
  (sicp-require (memq (car *unparsed*) (cdr word-list))) 
  (let ((found-word (car *unparsed*))) 
    (set! *unparsed* (cdr *unparsed*)) 
    (list-amb (cdr word-list))))

;; Modified book code start
(define *unparsed* '())
(define (parse input)
  (set! *unparsed* input)
  (let ((sent (parse-sentence)))
    (sicp-require (null? *unparsed*))
    sent))
(define prepositions '(prep for to in by with))
(define (parse-prepositional-phrase)
  (list 'prep-phrase
        (parse-word3 prepositions)
        (parse-noun-phrase)))
(define (parse-sentence)
  (list 'sentence
        (parse-noun-phrase)
        (parse-verb-phrase)))
(define (parse-verb-phrase)
  (define (maybe-extend verb-phrase)
    (amb verb-phrase
         (maybe-extend (list 'verb-phrase
                             verb-phrase
                             (parse-prepositional-phrase)))))
  (maybe-extend (parse-word3 verbs)))
(define (parse-simple-noun-phrase)
  (list 'simple-noun-phrase
        (parse-word3 articles)
        (parse-word3 nouns)))
(define (parse-noun-phrase)
  (define (maybe-extend noun-phrase)
    (amb noun-phrase
         (maybe-extend (list 'noun-phrase
                             noun-phrase
                             (parse-prepositional-phrase)))))
  (maybe-extend (parse-simple-noun-phrase)))
;; End book code

(module+ test
  (begin
    (check-equal?
     (parse '(the student with the cat sleeps in the class))
     '(sentence
       (noun-phrase (simple-noun-phrase the student) (prep-phrase for (simple-noun-phrase the student)))
       (verb-phrase studies (prep-phrase for (simple-noun-phrase the student)))))))

;;- 4.50
;; pass
