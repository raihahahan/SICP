#lang racket/base
(require racket/trace)
(define nil '())

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

;; Exercise 2.38

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(define fold-right accumulate)

(fold-right / 1 (list 1 2 3)) ;; 3/2
(fold-left / 1 (list 1 2 3)) ;; 1/6
(fold-right list nil (list 1 2 3)) ;; '(1 (2 (3 ())))
(fold-left list nil (list 1 2 3)) ;; '(((() 1) 2) 3)

;; Property of op: (received help from community scheme and Bill The Lizard

;; op must be commutative.
;; fold-l (lambda (x y) (p x) (p y)) === fold-r (lambda (x y) (p y) (p x))


