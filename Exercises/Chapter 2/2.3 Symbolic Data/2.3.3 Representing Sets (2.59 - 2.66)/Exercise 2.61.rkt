#lang racket
(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((= x (car set)) #t)
        ((< x (car set)) #f)
        (else (element-of-set? x (cdr set)))))

(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1
                     (intersection-set (cdr set1)
                                       (cdr set2))))
              ((< x1 x2)
               (intersection-set (cdr set1) set2))
              ((< x2 x1)
               (intersection-set (cdr set2 set1)))))))

;; Exercise 2.61
(define (adjoin-set x set)
  (cond
    ((null? set) x)
    ((< x (car set)) (cons x set)) 
    ((= x (car set)) set)
    (else (cons (car set) (adjoin-set x (cdr set))))))
;; Best case: omega(1). If x is smaller than the smallest element of set, then x is not in set. cons them.
;; Worst case: O(n). If x is larger than the largest element of set, then x is not in set. This takes n steps to iterate through the list.
;; Avergae: theta(n/2) ~~ theta(n)

(adjoin-set 5 '(2 3 4))
;; '(2 3 4 . 5) 
      
               

