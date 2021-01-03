;; Exercise 5.21 a)

;; modified from:
;; http://community.schemewiki.org/?sicp-ex-5.21 meteorgan solution

(define count-leaves-machine 
  (make-machine
   '(continue val tree var) ; added explicit registers
   (list (list '+ +) (list 'null? null?) 
         (list 'pair? pair?) (list 'car car) (list 'cdr cdr)) 
   '( 
     (assign continue (label count-leaves-done)) 
     (assign val (const 0)) 
     tree-loop 
     (test (op null?) (reg tree)) 
     (branch (label null-tree)) 
     (test (op pair?) (reg tree)) 
     (branch (label left-tree)) 
     (assign val (const 1)) 
     (goto (reg continue)) 
     left-tree 
     (save tree) 
     (save continue) 
     (assign continue (label right-tree)) 
     (assign tree (op car) (reg tree)) 
     (goto (label tree-loop)) 
     right-tree 
     (restore continue) 
     (restore tree) 
     (save continue) 
     (save val) 
     (assign continue (label after-tree)) 
     (assign tree (op cdr) (reg tree)) 
     (goto (label tree-loop)) 
     after-tree 
     (assign var (reg val)) 
     (restore val) 
     (restore continue) 
     (assign val (op +) (reg var) (reg val)) 
     (goto (reg continue)) 
     null-tree 
     (assign val (const 0)) 
     (goto (reg continue)) 
     count-leaves-done))) 
  
(set-register-contents! count-leaves-machine 'tree '(a (b c (d)) (e f) g)) 
(start count-leaves-machine) 
(get-register-contents count-leaves-machine 'val) ; 7
