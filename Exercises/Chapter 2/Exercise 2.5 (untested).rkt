#lang racket/base
;; Exercise 2.5
(define (my-cons a b)
  (* (expt 2 a)
     (expt 3 b)))

(define (my-car z)
  (define (iter count result)
    (if (not (even? result))
        count
        (iter (+ count 1) (/ result 2))
        ))
  (iter 0 z))

(define (my-cdr z)
  (define (iter count result)
    (if (not (= (remainder result 3) 0))
        count
        (iter (+ count 1) (/ result 3))
        ))
  (iter 0 z))