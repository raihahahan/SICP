#lang racket
;; Exercise 2.62

(define (union-set s1 s2)
  (cond
    ((and (null? s1) (null? s2)) '())
    ((and (null? s1) (not (null? s2))) s2)
    ((and (null? s2) (not (null? s1))) s1)
    ((< (car s1) (car s2)) (cons (car s1) (union-set (cdr s1) s2)))
    ((= (car s1) (car s2)) (cons (car s1) (union-set (cdr s1) (cdr s2))))
    (else (union-set (cdr s1) s2))))

(define s1 '(1 2 3 7 9 10))
(define s2 '(3 4 5 7 12 13))

(union-set s1 s2)
;; '(1 2 3 4 5 7 12 13)