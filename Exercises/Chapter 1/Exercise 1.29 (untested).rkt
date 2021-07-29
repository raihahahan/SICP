#lang racket/base
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

;; Exercise 1.29

(define (simpson-int f a b n)
  (define h (/ (- b a) n))
  (define (add-h x) (+ x (* 2 h)))
  (* (/ h 3)
     (+
      (f a)
      (* 4 (sum f (+ a h) add-h b)) 
      (* 2 (sum f (+ a (* 2 h)) add-h b))
      (f (+ a (* n h))))
      ))