#lang racket/base
;; encoding boolean and logical operators
(define true (lambda (x) (lambda (y) x)))
(define false (lambda (x) (lambda (y) y)))
(define not (lambda (b) (b false true)))
(define and (lambda (p) (lambda (q) (p q p))))
(define or (lambda (p) (lambda (q) (p p q))))
(define xor (lambda (p) (lambda (q) (q (not p) p))))

;; exercise 2.6 church numerals
(define zero (lambda (f) (lambda (x) x))) ;; f is composed zero times
(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

; substitution
(add-1 zero)
(lambda (f) (lambda (x) (f (((lambda (f) (lambda (x) x)) f) x))))
(lambda (f) (lambda (x) (f x))) ;; f is composed 1 time

(define one (lambda (f) (lambda (x) (f x))))
(define two (lambda (f) (lambda (x) (f (f x)))))
(define three (lambda (f) (lambda (x) (f (f (f x))))))

(add-1 one)
(lambda (f) (lambda (x) (f (((lambda (f) (lambda (x) (f x))) f) x))))
(lambda (f) (lambda (x) (f ((lambda (x) (f x)) x))))
(lambda (f) (lambda (x) (f (f x)))) ;; === two

(define (plus a b)
  (lambda (f) (lambda (x) ((a f) ((b f) x)))))
;; ((b f) x) is the single argument passed into (a f)
;; (a f) wil be reduced to (lambda (x) (f (f ... (f x))
;; ((b f x) will be reduced to (f (f ... (f x))
;; Hence, (f (f ... (f x)) is passed into (lambda (x) (f (f ... (f x))
;; which reduces to (f (f (f ... (f x)) where f is composed a plus b times.

(plus one two)
(lambda (f) (lambda (x) (((lambda (f) (lambda (x) (f x))) f) (((lambda (f) (lambda (x) (f (f x)))) f) x))))
(lambda (f) (lambda (x) ((lambda (x) (f x)) (f (f x)))))              
(lambda (f) (lambda (x) (f (f (f x)))))



  