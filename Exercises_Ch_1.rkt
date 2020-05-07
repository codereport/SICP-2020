; Exercise 1.3 (page 27)

(require threading)

(define (sum-square-two-largest lst)
  (~> lst
      (sort >)
      (take 2)
      (map (λ (x) (* x x)) _)
      (foldl + 0 _)))

; Exercise 1.6 (page 32)

; http://community.schemewiki.org/?sicp-ex-1.6
; The default if statement is a special form which means that even when an interpreter follows 
; applicative substitution, it only evaluates one of its parameters- not both. However, the newly 
; created new-if doesn't have this property and hence, it never stops calling itself due to the third 
; parameter passed to it in sqrt-iter.
