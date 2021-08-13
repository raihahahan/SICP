#lang racket/base
;; Exercise 2.35
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

;; START OF EXERCISE 2.35
(define (map-tree-to-1 tree)
  ;; returns tree with each element mapped to 1 if not null, else 0
  (cond ((null? tree) 0)
        ((not (pair? tree)) 1)
        (else
         (cons (map-tree-to-1 (car tree))
               (map-tree-to-1 (cdr tree))))))

(define (count-leaves t)
  (accumulate
   +
   0
   (map map-tree-to-1 (enumerate-tree t)))) ;; this line transforms tree t to a flat list of 1's

;; Tests

(define x (cons (list 1 2) (list 3 4)))
x
(map map-tree-to-1 (enumerate-tree x))
(define emp-elm (list 1 '() 2 3 '() 0))
(map map-tree-to-1 (enumerate-tree emp-elm))
(map-tree-to-1 (enumerate-tree emp-elm))
(count-leaves x) ;; 4
(count-leaves emp-elm) ;; 4
(count-leaves '(1 2 () () (3 () ()))) ;; 3
(count-leaves '(())) ;; 0

;; Output
;'((1 2) 3 4)
;4
;4
;3
;0

              
                        
  