#lang racket/base
;; Exercise 2.4
(define (cons-2 x y)
  (lambda (m) (m x y)))

(define (car-2 z)
  (z (lambda (p q) p)))

;; Verify: Suppose we construct a data object called test using cons-2. this returns (lambda (m) (m x y)). Passing test as an argument to car-2 will return (test (lambda (p q) p)). i.e (lambda (m) (m x y) (lambda (p q) p)). i.e ((lambda (p q) p) x y). Hence, from the arguments x and y, x will be returned.

;; This implies that for any objects x and y, (car (cons x y)) yields x.

(define (cdr-2 z)
  (z (lambda (p q) q)))

