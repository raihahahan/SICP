#lang racket/base
;; Exercise 2.42
(define nil '())

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (filter predicate sequence)
  (cond ((null? sequence) '())
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ low 1) high))))

(define (enumerate-tree tree)
  (cond ((null? tree) '())
        ((not (pair? tree)) (list tree))
        (else
         (append (enumerate-tree (car tree))
                 (enumerate-tree (cdr tree))))))

(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) '() sequence))

;; START
(define (queens board-size)
  (define (queen-cols k) ;; internal procedure that returns the sequence of all ways to place queens in the first k columns
    (if (= k 0) ;; if no columns
        (list empty-board) ;; then the only posssible sequence is an empty board
        (filter ;; else, filter out the positions that are not safe
         (lambda (positions) (safe? k positions))
         (flatmap ;; for each possible sequence of k-1 queens, adjoin the kth queen into each row of the kth column
          ;; so this flatmap returns a list of all possible sequences of k queens, where the k-1 queens in the first k-1 columns are arranged correctly while the kth queen may or may not be arranged correctly.
          ;; filter is thus used to filter out those positions that are not safe
          (lambda (rest-of-queens) ;; for each arrangement of k-1 queens, map through it 
            (map (lambda (new-row) ;; rest-of-queens is a way to place k-1 queens in the first k-1 columns, new-row os a proposed row in which to place the kth column
                   (adjoin-position new-row k rest-of-queens)) ;; adjoin kth queen into the new kth row
                 (enumerate-interval 1 board-size))) ;; this list represents the rows of the kth column, where we map through this column to place the kth queen into the kth column at each iteration
          (queen-cols (- k 1)))))) ;; this is the recursive process which will eventually return the list of sequences of k-1 queens arranged properly, starting from the empty board.
  (queen-cols board-size)) ;; start of iterative process.

;; TODO
; 1. Implement representation for the sets of board positions
; 2. (adjoin-position new-row k rest-of-queens): adjoins a new row-column position to a set of positions
; 3. empty-board: represents an empty set of positions
; 4. (safe? k positions): determines for a set of positions, whether the queen in the kth column is safe wrt to others

;; Workings
; Representation of board positions: sequence of sequences-1 of sequences-2. sequences-1 represents the sequence of all possible positions in a k*k board. Each sequences-2 represents the position of a chess piece, which is (row, column).
; eg. {[(),(),(),()], [(),(),(),()]} --- (*)

; (adjoin-position new-row k rest-of-queens):
;    Taking the above example (*) as reference, flatmap maps through each [], where rest-of-queens in (lambda ...) represents each [].
;    Next, map procedure maps through each ().
;    Now, adjoin-position will add a new (row,column) to rest-of-queens []. column must be k. row will be the row of new-row.
;    For each new-row (), say, (a,1), adjoin (a,k) to rest-of queens []. Repeat this for the next rows.

(define (adjoin-position new-row k rest-of-queens)
  (let ((row-pos (car new-row)))
    (append rest-of-queens (list (list row-pos k)))))

; empty-board: []
(define empty-board '())

; (safe? k positions)
; What is considered safe: piece is not in the same
;  1. row
;  2. column
;  3. diagonal
; of any other piece in the chess board
; positions will be [] in the representation (*) above. returns boolean
; eg. if first position is (1,1) and size 3, then (2,1), (1,2), (2,2), (3,3) are all not safe (amongst many others)
; generally, if position is (r,c) and size k, then (r+a,c), (r,c+a), (r+d,c+d) are all not safe, for all integers a,d, s.t 1 <= c,d < n

(define (pair-compare pair1 pair2)
  ;; takes 2 lists as args
  (and (= (car pair1) (car pair2)) (= (cadr pair1) (cadr pair2))))

(define (includes seq1 seq2)
  ;; takes as args 2 sequences
  ;; returns boolean value to indicate if any elem in seq1 is inside seq2, where the elements are pairs represented as lists
  (define truth-seq
    (flatmap
     (lambda (elem1)
       (map
        (lambda (elem2)
          (if (pair-compare elem1 elem2)
              1
              0
              ))
        seq2)
       )
     seq1)
    )
  (if (> (accumulate + 0 truth-seq) 0)
      #t
      #f)
  )              

(define (safe? k positions)
  (define truth-val (accumulate
   +
   0
   (map
   (lambda (pos)
     (let ((row-pos (car pos))
           (col-pos (cadr pos)))
       
       (define (generate-rows k)
         (map
          (lambda (new-row)
            (list new-row col-pos))
          (enumerate-interval (+ row-pos 1) k)))
       
       (define (generate-columns k)
         (map
          (lambda (new-col)
            (list row-pos new-col))
          (enumerate-interval (+ col-pos 1) k)))       
       (define (generate-diagonals k)
         (map
          (lambda (new-pos)
            (list (+ row-pos new-pos) (+ col-pos new-pos)))
             (enumerate-interval 1 (min (- k row-pos) (- k col-pos)))))
       
       (define (generate-unsafe-seq rows cols diags)
         (append rows cols diags))
       (define unsafe-seq (generate-unsafe-seq
                           (generate-rows k)
                           (generate-columns k)
                           (generate-diagonals k)
                           ))
       (if (includes unsafe-seq positions)
           1
           0
           )))
   positions)))
  (= truth-val 0))



           

       

       
             
             
         
   