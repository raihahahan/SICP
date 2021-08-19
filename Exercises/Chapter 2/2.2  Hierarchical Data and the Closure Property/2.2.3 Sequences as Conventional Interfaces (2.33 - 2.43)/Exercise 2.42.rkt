#lang racket/base
;; Exercise 2.42
(define nil '())

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
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
      nil
      (cons low (enumerate-interval (+ low 1) high))))

(define (enumerate-tree tree)
  (cond ((null? tree) nil)
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
            (map (lambda (new-row) ;; rest-of-queens is a way to place k-1 queens in the first k-1 columns, new-row os a proposed row in which to place the kth column -- (1)
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
;   map in (1) maps through (enum 1 board-size).
;   new-row represents each row: datatype of number
;   rest-of-queens represents [] in (*)
;   for each iteration, append rest-of queens with (list (list new-row k))

(define (adjoin-position new-row k rest-of-queens)
  (append rest-of-queens (list (list new-row k))))

; empty-board: []
(define empty-board nil)

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
  ;; returns boolean to check if pair1 == pair2
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
        seq2))
     seq1))
  (if (> (accumulate + 0 truth-seq) 0)
      #t
      #f))              

(define (get-all-unsafe-positions k positions)
 (flatmap
    (lambda (pos)
     (let ((row-pos (car pos))
           (col-pos (cadr pos)))
       
       ;; generate unsafe row, col, diagonal to the POSITIVE direction of chess piece
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

       ;; generate unsafe row, col, diagonal to the NEGATIVE direction of chess piece
       (define (generate-rows_neg k)
         (map
          (lambda (new-row)
            (list new-row col-pos))
          (enumerate-interval 1 (- row-pos 1))))
       
       (define (generate-columns_neg k)
         (map
          (lambda (new-col)
            (list row-pos new-col))
          (enumerate-interval 1 (- col-pos 1))))
       
       (define (generate-diagonals_neg k)
         (map
          (lambda (new-pos)
            (list (- row-pos new-pos) (- col-pos new-pos)))
             (enumerate-interval 1 (min (- row-pos 1) (- col-pos 1)))))

       ;; generate unsafe diagonal to north-west direction of chess piece
       (define (generate-diag-NW k)
         (map
          (lambda (new-pos)
            (list (- row-pos new-pos) (+ col-pos new-pos)))
            (enumerate-interval 1 (min (- row-pos 1) (- k col-pos)))))

       (define (generate-diag-NW_neg k)
         (map
          (lambda (new-pos)
            (list (+ row-pos new-pos) (- col-pos new-pos)))
            (enumerate-interval 1 (min (- k row-pos) (- col-pos 1)))))

       ;; Generate all the unsafe positions into a list
       (define (generate-unsafe-seq rows cols diags rows_neg cols_neg diags_neg diags-NW diags-NW_neg)
         (append rows cols diags rows_neg cols_neg diags_neg diags-NW diags-NW_neg))
       
       (define unsafe-seq (generate-unsafe-seq
                           (generate-rows k)
                           (generate-columns k)
                           (generate-diagonals k)
                           (generate-rows_neg k)
                           (generate-columns_neg k)
                           (generate-diagonals_neg k)
                           (generate-diag-NW k)
                           (generate-diag-NW_neg k)
                           ))
       unsafe-seq))
   positions))

(define (safe? k positions)
  (not (includes (get-all-unsafe-positions k positions) positions)))

;; TEST

(queens 9)

;; OUTPUT
;; n=1
; '(((1 1)))

;; n=2
; '()

;; n=3
; '()

;; n=4
; '(((2 1) (4 2) (1 3) (3 4)) ((3 1) (1 2) (4 3) (2 4)))

;; n=5
; '(((1 1) (3 2) (5 3) (2 4) (4 5))
;  ((1 1) (4 2) (2 3) (5 4) (3 5))
;  ((2 1) (4 2) (1 3) (3 4) (5 5))
;  ((2 1) (5 2) (3 3) (1 4) (4 5))
;  ((3 1) (1 2) (4 3) (2 4) (5 5))
;  ((3 1) (5 2) (2 3) (4 4) (1 5))
;  ((4 1) (1 2) (3 3) (5 4) (2 5))
;  ((4 1) (2 2) (5 3) (3 4) (1 5))
;  ((5 1) (2 2) (4 3) (1 4) (3 5))
;  ((5 1) (3 2) (1 3) (4 4) (2 5)))

;; n=6
; '(((2 1) (4 2) (6 3) (1 4) (3 5) (5 6))
;  ((3 1) (6 2) (2 3) (5 4) (1 5) (4 6))
;  ((4 1) (1 2) (5 3) (2 4) (6 5) (3 6))
;  ((5 1) (3 2) (1 3) (6 4) (4 5) (2 6)))

;; n=8
; '(((1 1) (5 2) (8 3) (6 4) (3 5) (7 6) (2 7) (4 8))
;  ((1 1) (6 2) (8 3) (3 4) (7 5) (4 6) (2 7) (5 8))
;  ((1 1) (7 2) (4 3) (6 4) (8 5) (2 6) (5 7) (3 8))
;  ((1 1) (7 2) (5 3) (8 4) (2 5) (4 6) (6 7) (3 8))
;  ((2 1) (4 2) (6 3) (8 4) (3 5) (1 6) (7 7) (5 8))
;  ((2 1) (5 2) (7 3) (1 4) (3 5) (8 6) (6 7) (4 8))
;  ((2 1) (5 2) (7 3) (4 4) (1 5) (8 6) (6 7) (3 8))
;  ((2 1) (6 2) (1 3) (7 4) (4 5) (8 6) (3 7) (5 8))
;  ((2 1) (6 2) (8 3) (3 4) (1 5) (4 6) (7 7) (5 8))
;  ((2 1) (7 2) (3 3) (6 4) (8 5) (5 6) (1 7) (4 8))
;  ((2 1) (7 2) (5 3) (8 4) (1 5) (4 6) (6 7) (3 8))
;  ((2 1) (8 2) (6 3) (1 4) (3 5) (5 6) (7 7) (4 8))
;  ((3 1) (1 2) (7 3) (5 4) (8 5) (2 6) (4 7) (6 8))
;  ((3 1) (5 2) (2 3) (8 4) (1 5) (7 6) (4 7) (6 8))
;  ((3 1) (5 2) (2 3) (8 4) (6 5) (4 6) (7 7) (1 8))
;  ((3 1) (5 2) (7 3) (1 4) (4 5) (2 6) (8 7) (6 8))
;  ((3 1) (5 2) (8 3) (4 4) (1 5) (7 6) (2 7) (6 8))
;  ((3 1) (6 2) (2 3) (5 4) (8 5) (1 6) (7 7) (4 8))
;  ((3 1) (6 2) (2 3) (7 4) (1 5) (4 6) (8 7) (5 8))
;  ((3 1) (6 2) (2 3) (7 4) (5 5) (1 6) (8 7) (4 8))
;  ((3 1) (6 2) (4 3) (1 4) (8 5) (5 6) (7 7) (2 8))
;  ((3 1) (6 2) (4 3) (2 4) (8 5) (5 6) (7 7) (1 8))
;  ((3 1) (6 2) (8 3) (1 4) (4 5) (7 6) (5 7) (2 8))
;  ((3 1) (6 2) (8 3) (1 4) (5 5) (7 6) (2 7) (4 8))
;  ((3 1) (6 2) (8 3) (2 4) (4 5) (1 6) (7 7) (5 8))
;  ((3 1) (7 2) (2 3) (8 4) (5 5) (1 6) (4 7) (6 8))
;  ((3 1) (7 2) (2 3) (8 4) (6 5) (4 6) (1 7) (5 8))
;  ((3 1) (8 2) (4 3) (7 4) (1 5) (6 6) (2 7) (5 8))
;  ((4 1) (1 2) (5 3) (8 4) (2 5) (7 6) (3 7) (6 8))
;  ((4 1) (1 2) (5 3) (8 4) (6 5) (3 6) (7 7) (2 8))
;  ((4 1) (2 2) (5 3) (8 4) (6 5) (1 6) (3 7) (7 8))
;  ((4 1) (2 2) (7 3) (3 4) (6 5) (8 6) (1 7) (5 8))
;  ((4 1) (2 2) (7 3) (3 4) (6 5) (8 6) (5 7) (1 8))
;  ((4 1) (2 2) (7 3) (5 4) (1 5) (8 6) (6 7) (3 8))
;  ((4 1) (2 2) (8 3) (5 4) (7 5) (1 6) (3 7) (6 8))
;  ((4 1) (2 2) (8 3) (6 4) (1 5) (3 6) (5 7) (7 8))
;  ((4 1) (6 2) (1 3) (5 4) (2 5) (8 6) (3 7) (7 8))
;  ((4 1) (6 2) (8 3) (2 4) (7 5) (1 6) (3 7) (5 8))
;  ((4 1) (6 2) (8 3) (3 4) (1 5) (7 6) (5 7) (2 8))
;  ((4 1) (7 2) (1 3) (8 4) (5 5) (2 6) (6 7) (3 8))
;  ((4 1) (7 2) (3 3) (8 4) (2 5) (5 6) (1 7) (6 8))
;  ((4 1) (7 2) (5 3) (2 4) (6 5) (1 6) (3 7) (8 8))
;  ((4 1) (7 2) (5 3) (3 4) (1 5) (6 6) (8 7) (2 8))
;  ((4 1) (8 2) (1 3) (3 4) (6 5) (2 6) (7 7) (5 8))
;  ((4 1) (8 2) (1 3) (5 4) (7 5) (2 6) (6 7) (3 8))
;  ((4 1) (8 2) (5 3) (3 4) (1 5) (7 6) (2 7) (6 8))
;  ((5 1) (1 2) (4 3) (6 4) (8 5) (2 6) (7 7) (3 8))
;  ((5 1) (1 2) (8 3) (4 4) (2 5) (7 6) (3 7) (6 8))
;  ((5 1) (1 2) (8 3) (6 4) (3 5) (7 6) (2 7) (4 8))
;  ((5 1) (2 2) (4 3) (6 4) (8 5) (3 6) (1 7) (7 8))
;  ((5 1) (2 2) (4 3) (7 4) (3 5) (8 6) (6 7) (1 8))
;  ((5 1) (2 2) (6 3) (1 4) (7 5) (4 6) (8 7) (3 8))
;  ((5 1) (2 2) (8 3) (1 4) (4 5) (7 6) (3 7) (6 8))
;  ((5 1) (3 2) (1 3) (6 4) (8 5) (2 6) (4 7) (7 8))
;  ((5 1) (3 2) (1 3) (7 4) (2 5) (8 6) (6 7) (4 8))
;  ((5 1) (3 2) (8 3) (4 4) (7 5) (1 6) (6 7) (2 8))
;  ((5 1) (7 2) (1 3) (3 4) (8 5) (6 6) (4 7) (2 8))
;  ((5 1) (7 2) (1 3) (4 4) (2 5) (8 6) (6 7) (3 8))
;  ((5 1) (7 2) (2 3) (4 4) (8 5) (1 6) (3 7) (6 8))
;  ((5 1) (7 2) (2 3) (6 4) (3 5) (1 6) (4 7) (8 8))
;  ((5 1) (7 2) (2 3) (6 4) (3 5) (1 6) (8 7) (4 8))
;  ((5 1) (7 2) (4 3) (1 4) (3 5) (8 6) (6 7) (2 8))
;  ((5 1) (8 2) (4 3) (1 4) (3 5) (6 6) (2 7) (7 8))
;  ((5 1) (8 2) (4 3) (1 4) (7 5) (2 6) (6 7) (3 8))
;  ((6 1) (1 2) (5 3) (2 4) (8 5) (3 6) (7 7) (4 8))
;  ((6 1) (2 2) (7 3) (1 4) (3 5) (5 6) (8 7) (4 8))
;  ((6 1) (2 2) (7 3) (1 4) (4 5) (8 6) (5 7) (3 8))
;  ((6 1) (3 2) (1 3) (7 4) (5 5) (8 6) (2 7) (4 8))
;  ((6 1) (3 2) (1 3) (8 4) (4 5) (2 6) (7 7) (5 8))
;  ((6 1) (3 2) (1 3) (8 4) (5 5) (2 6) (4 7) (7 8))
;  ((6 1) (3 2) (5 3) (7 4) (1 5) (4 6) (2 7) (8 8))
;  ((6 1) (3 2) (5 3) (8 4) (1 5) (4 6) (2 7) (7 8))
;  ((6 1) (3 2) (7 3) (2 4) (4 5) (8 6) (1 7) (5 8))
;  ((6 1) (3 2) (7 3) (2 4) (8 5) (5 6) (1 7) (4 8))
;  ((6 1) (3 2) (7 3) (4 4) (1 5) (8 6) (2 7) (5 8))
;  ((6 1) (4 2) (1 3) (5 4) (8 5) (2 6) (7 7) (3 8))
;  ((6 1) (4 2) (2 3) (8 4) (5 5) (7 6) (1 7) (3 8))
;  ((6 1) (4 2) (7 3) (1 4) (3 5) (5 6) (2 7) (8 8))
;  ((6 1) (4 2) (7 3) (1 4) (8 5) (2 6) (5 7) (3 8))
;  ((6 1) (8 2) (2 3) (4 4) (1 5) (7 6) (5 7) (3 8))
;  ((7 1) (1 2) (3 3) (8 4) (6 5) (4 6) (2 7) (5 8))
;  ((7 1) (2 2) (4 3) (1 4) (8 5) (5 6) (3 7) (6 8))
;  ((7 1) (2 2) (6 3) (3 4) (1 5) (4 6) (8 7) (5 8))
;  ((7 1) (3 2) (1 3) (6 4) (8 5) (5 6) (2 7) (4 8))
;  ((7 1) (3 2) (8 3) (2 4) (5 5) (1 6) (6 7) (4 8))
;  ((7 1) (4 2) (2 3) (5 4) (8 5) (1 6) (3 7) (6 8))
;  ((7 1) (4 2) (2 3) (8 4) (6 5) (1 6) (3 7) (5 8))
;  ((7 1) (5 2) (3 3) (1 4) (6 5) (8 6) (2 7) (4 8))
;  ((8 1) (2 2) (4 3) (1 4) (7 5) (5 6) (3 7) (6 8))
;  ((8 1) (2 2) (5 3) (3 4) (1 5) (7 6) (4 7) (6 8))
;  ((8 1) (3 2) (1 3) (6 4) (2 5) (5 6) (7 7) (4 8))
;  ((8 1) (4 2) (1 3) (3 4) (6 5) (2 6) (7 7) (5 8)))
;> 





           

       

       
             
             
         
   