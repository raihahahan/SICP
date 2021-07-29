#lang racket/base
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

;; Exercise 1.30
(define (>= a b)
  (or (> a b) (= a b)))

(define (sum-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
 
  (iter a 0))

(define (simpson-int-iter f a b n)
  (define h (/ (- b a) n))
  (define (add-h x) (+ x (* 2 h)))
  (* (/ h 3)
     (+
      (f a)
      (* 4 (sum-iter f (+ a h) add-h b)) 
      (* 2 (sum-iter f (+ a (* 2 h)) add-h b))
      (f (+ a (* n h))))
      ))

 (define (pi-sum a b) 
  (define (pi-term x) 
          (/ 1.0 (* x (+ x 2)))) 
  (define (pi-next x) 
          (+ x 4)) 
  (sum-iter pi-term a pi-next b))