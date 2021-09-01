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


(define (includes predicate sequence)
  ;; returns true if any (predicate element) returns true
  (cond ((null? sequence) #f)
        ((predicate (car sequence)) #t)
        (else (includes predicate (cdr sequence)))))

(define (flatten sequence)
  ;; if element of sequence is a pair, flatten it
  (cond
    ((null? sequence) sequence)
    ((pair? (car sequence)) (append (car sequence) (flatten (cdr sequence))))
    (else (cons (car sequence) (flatten (cdr sequence))))))

(define (max sequence)
  (define (iter result sequence)
    (cond ((null? sequence) result)
          ((> (car sequence) result) (iter (car sequence) (cdr sequence)))
          (else (iter result (cdr sequence)))))
  (iter (car sequence) sequence))

(define (first-n sequence n)
  (define (iter result n)
    (cond ((null? result) '())
          ((= n 0) '())
          (else
           (append (list (car result)) (iter (cdr result) (- n 1))))))
  (iter sequence n))

(define (reverse-1 l)
  (if (null? l)
      l
      (append (reverse-1 (cdr l)) (list (car l)))))

(define (last-n sequence n)
  (let ((r1 (reverse sequence)))
    (let ((a1 (first-n r1 n)))
      (reverse a1))))
