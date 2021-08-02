#lang racket/base
(define (scale-tree tree factor)
  ;; takes as argument a tree (sequence of sequences) and number factor
  ;; returns tree with leaves multiplied by scale factor
  (cond ((null? tree) '())
        ((not (pair? tree) (* tree factor)))
        (else (cons (scale-tree (car tree) factor)
                    (scale-tree (cdr tree) factor)))))

(define (scale-tree-1 tree factor)
  ;; treats tree as a sequence with elemnts that can be a sequence
  ;; maps through elements.
  ;; if element is a sub-tree, then map through elements in sub-tree
  ;; else, apply procedure on element
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (scale-tree-1 sub-tree factor)
             (* sub-tree factor)))))

(define (square x)
  (* x x))

;; Exercise 2.30
(define (square-tree tree)
  ;; base case 1: empty tree => return nil
  ;; base case 2: element is a leaf => return (square tree)
  ;; else: (square-tree sub-tree)
  (cond ((null? tree) '())
        ((not (pair? tree)) (square tree))
        (else (cons (square-tree (car tree))
                    (square-tree (cdr tree))))))

;; Test
(square-tree
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))

;; Output
;'(1 (4 (9 16) 25) (36 49))

(define (square-tree-1 tree)
  ;; Using map
  (map (lambda (subtree)
         (if (pair? subtree)
             (square-tree-1 subtree)
             (square subtree)))
       tree))

;; Test

(define list-1 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))

(square-tree-1 list-1)

;; Output
;'(1 (4 (9 16) 25) (36 49))