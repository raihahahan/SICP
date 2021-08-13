#lang scheme
;; Exercise 2.33
(define (even? x)
  (= (remainder x 2) 0))

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

;; START OF EXERCISE

(define (map-1 p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) '() sequence))

;; Test
(define (square x)
  (* x x))

(map-1 square (list 1 2 3 4))
;; (1 4 9 16)

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

;; Test
(append (list 1 2 3 4) (list 4 2 5 4))
;; (1 2 3 4 4 2 5 4)

(define (length sequence)
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))

;; Test
(length (list 1 2 3 4 5))
(length (list 6 7 3))
;; 5
;; 3


