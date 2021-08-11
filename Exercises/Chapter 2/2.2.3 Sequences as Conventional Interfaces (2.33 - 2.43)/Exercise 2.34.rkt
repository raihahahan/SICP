#lang scheme
;; Exercise 2.34
(define (filter predicate sequence)
  (cond ((null? sequence) '())
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ low 1) high))))

(define (enumerate-tree tree)
  (cond ((null? tree) '())
        ((not (pair? tree) tree))
        (else
         (cons (enumerate-tree (car tree))
               (enumerate-tree (cdr tree))))))

(define (map p sequence)
  (accumulate (lambda (x y) (cons x) y) '() sequence))

;; START OF EXERCISE
;; f(x) = a0x0 + a1x0^2 + ... + anx0^n
;;      = a0 + x0(a1 + x0(a2 + ... + x0(a(n-1) + anx0)...)

(define (square x)
  (* x x))

(define (expt base n)
  ;; takes as arguments base and n (types: number)
  ;; returns base^n
  (cond ((= n 0) 1)
        ((even? n) (expt (square base) (/ n 2)))
        (else (* base (expt base (- n 1))))))
      

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms) (+ this-coeff
                                                   (* x higher-terms)))
              0
              coefficient-sequence))

;; Test
(horner-eval 2 (list 1 3 0 5 0 1))
(horner-eval 4 (list 1 0 2 4 0 2))
;;79
;;2337