#lang racket/base
(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))
(list 1 (list 2 (list 3 4)))

;; Exercise 2.25
(define one (list 1 3 (list 5 7) 9))
(car (cdr (car (cdr (cdr one))))) ; 7

(define two (list (list 7)))
(car (car two)) ; 7

(define three (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))
(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr three))))))))))))