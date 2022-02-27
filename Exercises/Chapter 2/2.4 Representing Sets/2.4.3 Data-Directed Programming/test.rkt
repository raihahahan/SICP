#lang racket
(define square
  (lambda (x) (* x x)))

(define sum-of-squares
  (lambda (x y) (+ (square x) (square y))))

(define f
  (lambda (a) (sum-of-squares (+ a 1) (* a 2))))

(f 5)
(print 'ask)
         
             