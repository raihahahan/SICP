#lang racket/base
;; Exercise 2.39
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


;; START
(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(define fold-right accumulate)

(define (reverse-1 sequence)
  (fold-right
   (lambda (x y) (append y (list x)))
   nil
   sequence))

(define (reverse-2 sequence)
  (fold-left (lambda (x y) (append (list y) x)) nil sequence))

(define (reverse-using-left items) 
  (fold-left (lambda (x y) (cons y x)) 
             nil 
             items)) 

(reverse-1 (list 1 2 3 4 5))
(reverse-2 (list 1 2 3 4 5))
(reverse-using-left (list 1 2 3 4 5))
  