;; Exercise 3.38 (page 409-10)

a) 

10+÷∘2⊣¯20+100 ⍝ 50
÷∘2⊣¯20+10+100 ⍝ 45
¯20+10+÷∘2⊣100 ⍝ 40
¯20+÷∘2⊣10+100 ⍝ 35

b) worst case would be:

¯20+÷∘2⊣100 ⍝ 30

;; Exercise 3.39 (page 414)

;; TODO come back to this one

;; x 101: P 1 sets x to 100 and then P 2 increments x to 101.
;; x 121: P 2 increments x to 11 and then P 1 sets x to x * x .
;;   110: P 2 changes x from 10 to 11 between the two times that
;;     P 1 accesses the value of x during the evaluation of (* x x) .
;; x 11:  P 2 accesses x , then P 1 sets x to 100, then P 2 sets x .
;;   100: P 1 accesses x (twice), then P 2 sets x to 11, then P 1 sets x .

;; Exercise 3.40 (page 414)

;; a) 100 1K 10K 100K 1M
;; b) 1M

;; Exercise 3.41 (page 414-5)

;; Not a mutator so no need to serialize

;; Exercise 3.42 (page 415-6)

;; They extra let expression will create an additional frame in the environment model 
;; - but other than that they are the same

;; Exercise 3.47 (page 425)

;; TODO

;; Exercise 3.48 (page 426)

;; TODO

;; Exercise 3.49 (page 426)

;; TODO
