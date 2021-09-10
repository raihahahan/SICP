#lang racket
;; Exercise 2.60
(define (transform-set s1)
  ;; transforms a set with duplicates into one without any duplicates
  ;; may not eventually be used in this exercise.
  (define (element-of-set? x set)
    (cond ((null? set) #f)
          ((equal? x (car set)) #t)
          (else (element-of-set? x (cdr set)))))
  (define (iter tmp s1)
    (cond
      ((null? s1) tmp)
      ((null? tmp) (iter (cons (car s1) tmp) (cdr s1)))
      ((element-of-set? (car s1) tmp) (iter tmp (cdr s1)))
      (else (iter (cons (car s1) tmp) (cdr s1)))))
  (iter '() s1))

(define d-set '(2 3 2 1 3 2 2 4 4 21 1 21 2 21))
(transform-set d-set)
;; '(21 4 1 3 2)

;; START
(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((equal? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))

(element-of-set? 4 '(2 3 2 1 3 2 2)) ;; #f
(element-of-set? 2 '(2 3 2 1 3 2 2)) ;; #t

(define (adjoin-set x set)
  (cons x set))

(adjoin-set 2 d-set)
;; '(2 2 3 2 1 3 2 2 4 4 21 1 21 2 21)
(adjoin-set 5 d-set)
;; '(5 2 3 2 1 3 2 2 4 4 21 1 21 2 21)

(define (intersection-set1 s1 s2)
  (append (filter (lambda (x) (element-of-set? x s2)) s1)
          (filter (lambda (y) (element-of-set? y s1)) s2)))

(define d1 '(2 3 1 2 2 3))
(define d2 '(2 3 5 5 5 6 3 2))

(intersection-set1 d1 d2)
;; '(2 3 2 2 3 2 3 3 2)


(define (union-set s1 s2)
  (append s1 s2))

(union-set d1 d2)
;; '(2 3 1 2 2 3 2 3 5 5 5 6 3 2)

;; __Efficiency comparison__

;; 1. element-of-set? still grows with complexity of n steps as we have to iterate through all n elements in the worst case.
;; 2. adjoin-set now has a constant time complexity (i.e cons takes 1 step) as we do not have to check for existence of the element x in the set as we allow duplicates.
;; 3. intersection-set now has a time complexity of n steps as we have to apply the filter procedure twice.
;; 4. union-set now has a time complexity of n steps as we are only applyig append.

;; __Possible applications of duplicates__

;; 1. When a shorter runtime matters more than memory usage
