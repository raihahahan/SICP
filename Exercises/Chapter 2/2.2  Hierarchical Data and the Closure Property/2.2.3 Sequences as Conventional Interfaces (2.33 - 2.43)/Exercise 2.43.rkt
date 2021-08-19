#lang racket/base
;; Exercise 2.43
(define (queens board-size)
  (define (queen-cols k)  
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

;; 1. map through (queen-cols (- k 1)) once: (n-1)!
;; 2. for each iteration above, map through (enum 1 n): n
;; simultaneous: total = n*(n-1)! = n!

;; vs

(define (queens board-size)
  (define (queen-cols k)  
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (new-row)
            (map (lambda (rest-of-queens)
                   (adjoin-position new-row k rest-of-queens))
                 (queen-cols (- k 1))))
          (enumerate-interval 1 board-size)))))
  (queen-cols board-size))

;; 1. map through (enum 1 n): n
;; 2. for each iteration above, map through (queen-cols (- n 1)). evaluate (queen-cols (- n 1)) for each of the n iterations.
;; total = (n-1)*(n-1)*...*(n-1) (n times) == (n-1)^n ::= n^n

;; difference:
; 1. O(n!)
; 2. O(n^n)

;; Above answer is wrong. (queen-cols) changes for each iteration.

;; Below is the explanation by Bill The Lizard. https://billthelizard.blogspot.com/2011/06/sicp-242-243-n-queens-problem.html

;; In the original solution, queen-cols is called once for each column in the board. This is an expensive procedure to call, since it generates the sequence of all possible ways to place k queens in k columns.

;; By moving queen-cols so it gets called by flatmap, we're transforming a linear recursive process to a tree-recursive process. The flatmap procedure is called for each row of the kth column, so the new procedure is generating all the possible solutions for the first k - 1 columns for each one of these rows.

;; We learned back in section 1.2.2 that a tree-recursive process grows exponentially. If it takes time T to execute the original version of queens for a given board size, we can expect the new version to take roughly T^board-size time to execute.








           

       

       
             
             
         
   