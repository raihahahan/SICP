#lang racket/base
;; Exercise 2.27
(define (reverse-1 l)
  (if (null? l)
      l
      (append (reverse-1 (cdr l)) (list (car l)))))

(define (deep-reverse l)
  ;; base cases:
  ;; 1. empty list: return l
  ;; 2. all elements in l are not sequences: return (reverse-1 l)
  ;; Recursion: append deep-reverse of (cdr l) with list of deep-reverse of (car l)
  (cond ((null? l) l)
        ((not (any_seq? l)) (reverse-1 l))
        (else (append (deep-reverse (cdr l)) (list (deep-reverse (car l)))))))

(define (list-length l)
  ;; takes as argument a list l
  ;; returns the length of list
  (if (null? l)
      0
      (+ 1 (list-length (cdr l)))))

(define (any_seq? l)
  ;; Helper function to check if any elements in list l is a sequence
  ;; returns boolean value
  (cond
    ((null? l) #f)
    ((pair? (car l)) #t)
    ((not (pair? (car l))) (any_seq? (cdr l)))))

(define (map-1 p l) ; p === procedure passed to each element of list
  (if (null? l)
      '()
      (cons (p (car l))
            (map-1 p (cdr l)))))

(define (deep-reverse-1 l)
  (if (pair? l)
      (reverse (map deep-reverse-1 l))
      l))

(define z (list (list 1 2) (list 3 4)))
(define z1 (list (list (list 1 2) (list 3 4)) (list 5 6)))
z1

(display "**********")
(newline)
z1
(reverse-1 z1)
(deep-reverse z1)
(deep-reverse-1 z1)

;; End of exercise 2.27

;; Output
;'(((1 2) (3 4)) (5 6))
;**********
;'(((1 2) (3 4)) (5 6))
;'((5 6) ((1 2) (3 4)))
;'((6 5) ((4 3) (2 1)))
;'((6 5) ((4 3) (2 1)))
> 