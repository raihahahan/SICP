#lang racket/base

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

(define (adjoin-position new-row k rest-of-queens)
  (let ((row-pos (car new-row)))
    (append rest-of-queens (list (list row-pos k)))))

(define test (list '(1 2) '(3 4) '(5 7)))
(adjoin-position (list 6 7) 10 test)
(max 2 3)

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



;(pair-compare (list 1 2) (list 1 2))
  

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
       ;;unsafe-seq))
       (if (includes unsafe-seq positions)
           1
           0
           )))
   positions)))
  (= truth-val 0))
  
      
      

;;{[(),(),(),()], [(),(),(),()]}

(define my-pos (list '(4 1) '(2 2) '(8 3) '(5 4) '(7 5) '(1 6) '(3 7) '(6 8)))
(safe? 8 my-pos)
 