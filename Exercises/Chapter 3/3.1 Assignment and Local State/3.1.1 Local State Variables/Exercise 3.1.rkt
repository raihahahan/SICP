#lang racket
(define (make-accumulator i)
  (lambda (add)
    (if (> i 0)
        (begin
          (set! i (+ add i))
          i)
        "Accumulator can only take nonnegative values")))

(define A (make-accumulator 5))

(A 10)
;15

(A 10)
;25



