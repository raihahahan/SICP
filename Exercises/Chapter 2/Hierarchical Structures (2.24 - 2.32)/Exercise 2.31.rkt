#lang racket/base
;; Exercise 2.31
(define (square x)
  (* x x))

(define (tree-map procedure tree)
  (cond ((null? tree) '())
        ((not (pair? tree)) (procedure tree))
        (else (cons (tree-map procedure (car tree))
                    (tree-map procedure (cdr tree))))))

;; Test
(tree-map square (list 1
                       (list 2 (list 3 4) 5)
                       (list 6 7)))
;; Output
;'(1 (4 (9 16) 25) (36 49))