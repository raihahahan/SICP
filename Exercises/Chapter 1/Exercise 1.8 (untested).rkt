#lang racket/base
(define (square x)
  (* x x))

; exercise 1.8
(define (cbrt x)
  (define (cbrt-iter guess x)
    (if (cube-good-enough? guess (cube-improve guess x))
        guess
        (cbrt-iter (cube-improve guess x) x)))

  (define (cube-improve guess x)
    (/
     (+
      (/ x
         (square guess))
      (* 2 guess))
     3))

  (define (cube-good-enough? previous-guess-cube guess)
    (< (abs (/ (- previous-guess-cube guess) guess)) 0.0000000000001))
  (cbrt-iter 1.0 x))