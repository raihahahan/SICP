#lang racket/base
(define nil '())

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

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
        ((not (pair? tree)) (list tree))
        (else
         (append (enumerate-tree (car tree))
                 (enumerate-tree (cdr tree))))))

(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) '() sequence))

;; Exercise 2.41
(define (unique-triples n)
  (flatmap
   (lambda (i)
     (flatmap (lambda (j)
            (map (lambda (k)
                   (list i j k))
                 (enumerate-interval 1 (- j 1))))
          (enumerate-interval 1 (- i 1))))
   (enumerate-interval 1 n)))

;; filter out triples which sums does not equal to a given int s
;; map through each pair

(define (sum-check triple s)
  (= (+ (car triple)
        (car (cdr triple))
        (car (cdr (cdr triple))))
     s))

(define (filtered-triples n s)
  (filter
   (lambda (triple)
     (sum-check triple s))
   (unique-triples n)))

;; Test
(filtered-triples 10 12)
(filtered-triples 12 9)
(filtered-triples 4 10)
(filtered-triples 7 20)

;; Output
;'((5 4 3) (6 4 2) (6 5 1) (7 3 2) (7 4 1) (8 3 1) (9 2 1))
;'((4 3 2) (5 3 1) (6 2 1))
;'()
;'()
;>

;; Bonus question: make tuple of arbitrary size
;; TODO
