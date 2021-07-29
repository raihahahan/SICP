#lang scheme
;; Exercise 1.3
;; define a procedure which gives the square sum of the largest two numbers from three numbers
(define (square x)
  (* x x))

(define (sum-of-sqr x y)
  (+ (square x) (square y)))

(define (larger x y)
  (if (> x y)
      x
      y))

(define (smaller x y)
  (if (< x y)
      x
      y))

(define (largest-of-three x y z)
  (larger x (larger y z)))

(define (smallest-of-three x y z)
  (smaller x (smaller y z)))

(define (sum-of-three x y z)
  (+ x y z))

(define (middle-number x y z)
  (- (sum-of-three x y z)
     (+
      (largest-of-three x y z)
      (smallest-of-three x y z))
     ))

(define (sqr-sum-of-big-two x y z)
  (sum-of-sqr (largest-of-three x y z) (middle-number x y z)))

;; Tests

