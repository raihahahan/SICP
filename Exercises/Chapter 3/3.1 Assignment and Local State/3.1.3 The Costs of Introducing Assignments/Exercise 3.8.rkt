#lang racket
(require racket/trace)

(define f
  (let ((var 1))
    (lambda (x)
      (set! var (* var x))
      var)))
        
(+ (f 1) (f 0)) ; 1
(+ (f 0) (f 1)) ; 0

(define g
  (let ((var 1))
    (lambda (lol)
      (set! var (+ var lol))
      var)))

(define square
  (lambda (x) (* x x)))

