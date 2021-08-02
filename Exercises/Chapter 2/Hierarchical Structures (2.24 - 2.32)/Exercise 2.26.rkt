#lang racket/base
;; Exercise 2.26
(define x (list 1 2 3))
(define y (list 4 5 6))

(define (append l1 l2)
  (if (null? l1)
      l2
      (cons (car l1) (append (cdr l1) l2))))

; 1. (1 2 3 4 5 6)
; 2. ((1 2 3) 4 5 6))
; 3. ((1 2 3) (4 5 6))

;; Draw box and pointer diagram for clearer illustration

(append x y)
(cons x y)
(list x y)