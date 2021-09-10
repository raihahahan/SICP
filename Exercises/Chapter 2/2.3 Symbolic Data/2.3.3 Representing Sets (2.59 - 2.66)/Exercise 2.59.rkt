#lang racket
(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((equal? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))

(define (adjoin x set)
  (if (element-of-set? x)
      set
      (cons x set)))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

;; Exercise 2.59
(define (union-set set1 set2)
  (cond ((and (null? set1) (not (null? set2))) set2)
        ((and (null? set2) (not (null? set1))) set1)
        ((not (element-of-set? (car set1) set2))
         (cons (car set1)
               (union-set (cdr set1) set2)))
        (else (union-set (cdr set1) set2))))
        

(define s1 '(1 2 3 6 0 7))
(define s2 '(2 3 4 5))

(union-set s1 s2)
;; '(1 6 0 7 2 3 4 5)
