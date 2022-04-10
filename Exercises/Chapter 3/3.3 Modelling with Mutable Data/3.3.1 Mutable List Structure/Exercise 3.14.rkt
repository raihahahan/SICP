#lang sicp
;; mystery is a procedure which takes a list as an argument, mutates the cdr of the list to null, and returns a new list that is the reverse of the list given as an argument.

(define (mystery x)
  (define (loop x y)
    (if (null? x)
        y
        (let ((temp (cdr x)))
          (set-cdr! x y)
          (loop temp x))))
  (loop x '()))

(define v (list 'a 'b 'c 'd))
(define w (mystery v))
v

