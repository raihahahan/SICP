#lang racket/base
;; Exercise 2.28

(define (fringe l)
  ;; takes as argument a tree represented as a list
  ;; returns a list whose elements are all the leaves of the tree arranged in left to right order
  ;; Workings:
  ; Base case(s):
  ; 1. no elements in list is a sequence: return list
  ; 2. empty list: return list
  ; Else: append the fringe of (car l) to fringe of (cdr l)
  (cond ((null? l) l)
        ((not (any_seq? l)) l)
        ((not (pair? (car l))) (append (list (car l)) (fringe (cdr l))))
        (else (append (fringe (car l)) (fringe (cdr l))))))

;; Tests
(define tree-1 (list (list 1 2) (list 3 4)))
(define tree-2 (list (list 1 2 (list 3 4 5)) (list 6 7) 8 9))
(define tree-3 (list 1 2 3 (list 4 5 6 7) (list 8 (list 9 (list 10 11 (list 12)))) 13))
(define tree-4 (list '()))
(define tree-5 (list 1 2))

(display "*********")
(newline)
(fringe tree-1)
(fringe tree-2)
(fringe tree-3)
(fringe tree-4)
(fringe tree-5)

;; Output
;*********
;'(1 2 3 4)
;'(1 2 3 4 5 6 7 8 9)
;'(1 2 3 4 5 6 7 8 9 10 11 12 13)
;'(())
;'(1 2)

;; End of Exercise 2.28





