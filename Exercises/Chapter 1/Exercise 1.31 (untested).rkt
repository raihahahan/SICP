#lang racket/base

(define (identity x)
  x)
(define (inc x)
  (+ x 1))
(define (square x)
  (* x x))
(define (cube x)
  (* x x x))
(define (even? x)
  (= (remainder x 2) 0))

;; Exercise 1.31a

(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

(define (factorial n)
  (product identity 1 inc n))

(define (incTwo n)
  (+ n 2.0))


(define (wallis-product n)
  (define (numerator a b)
    (* 2.0 (product square a incTwo b)))
  
  (define (denominator c d)
    (product square c incTwo d))

  (cond ((even? n)
        (/
         (numerator 4.0 n)
         (* (/ 1 (+ n 1)) (denominator 3.0 (+ n 1.0)))))
        (else
         (/
          (* (/ 1 (+ n 1.0)) (numerator 4.0 (+ n 1.0)))
          (denominator 3.0 n)))))

(define (fast-wallis-product n)
  (define (term n)
    (*
     (/
      (* 2 n)
      (- (* 2 n) 1))
     (/
      (* 2 n)
      (+ (* 2 n) 1))))
  (product term 1.0 inc n))




(define (product-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
  
  (iter a 1))