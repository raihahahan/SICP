#lang racket/base
(define (new-filtered-accumulate predicate? combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner
       (if (predicate? a) (term a) null-value)
       (new-filtered-accumulate predicate? combiner null-value term (next a) next b))))

(define (new-filtered-accumulate-iter predicate? combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner
                        (if (predicate? a) (term a) null-value)
                        result))))
  (iter a null-value))