#lang racket/base
;; Exercise 2.32

(define (subsets s)
  (if (null? s) ;; if s is an empty set,
      (list '()) ;; then return the empty set
      (let ((rest (subsets (cdr s)))) ;; rest defines the other subsets of s
        (append rest (map
                      (lambda (element)
                       (cons (car s) element))
                      rest))))) 

;; The subsets of a set s is equal to the set of the first element of s and the subsets of all the other elements of s.
;; In the procedure passed as argument in the map function above, we are cons'ing the first element of s with each of the element in rest.
;; Note that (car s) should go first as s is a primitive element while the element in rest is a list.

;; Test

(subsets (list 1 2 3))
(subsets (list 1 2 3 4 5))

;; Output

;'(() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3))
;'(()
 ; (5)
 ; (4)
 ; (4 5)
 ; (3)
 ; (3 5)
 ; (3 4)
 ; (3 4 5)
 ; (2)
 ; (2 5)
 ; (2 4)
 ; (2 4 5)
 ; (2 3)
 ; (2 3 5)
 ; (2 3 4)
 ; (2 3 4 5)
 ; (1)
 ; (1 5)
 ; (1 4)
 ; (1 4 5)
 ; (1 3)
 ; (1 3 5)
 ; (1 3 4)
 ; (1 3 4 5)
 ; (1 2)
 ; (1 2 5)
 ; (1 2 4)
 ; (1 2 4 5)
 ; (1 2 3)
 ; (1 2 3 5)
 ; (1 2 3 4)
 ; (1 2 3 4 5))