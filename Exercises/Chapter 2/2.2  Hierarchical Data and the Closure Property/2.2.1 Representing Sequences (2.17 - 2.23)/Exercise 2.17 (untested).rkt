#lang racket/base
;; Exercise 2.17
(define (last-pair l)
  ;; takes as argument a list l
  ;; returns last item in list that is not nil
  (let ((length (list-length-iter l)))
    (list-ref l (- length 1))))

(define (last-pair-2 l)
  ;; takes as argument a list l
  ;; returns last item in list that is not nil
  (if (null? (cdr l))
      (car l)
      (last-pair-2 (cdr l))))