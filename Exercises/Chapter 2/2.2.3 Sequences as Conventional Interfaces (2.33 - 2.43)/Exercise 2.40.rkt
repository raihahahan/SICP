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


;; Exercise 2.40

;Define a procedure unique-pairs that, given an integer n, generates the sequence of pairs (i,j) with 1<= j< i<= n. Use unique-pairs to simplify the definition of prime-sum-pairs given above.

; Workings:
; 1. Enumerate interval 1 to n inclusive
; 2. For each i in n, map
; 3. For each j in i, j<i, map (i, j)

(define (unique-pairs n)
  (flatmap
   (lambda (i)
     (map (lambda (j)
            (list i j))
            (enumerate-interval 1 (- i 1))))
   (enumerate-interval 2 n)))

;; for this list of pairs, we want to filter out pairs where the sum is not prime

(define (square x)
  (* x x))

(define (prime? n)
  ;; takes as argument a number n
  ;; returns #t if n is prime, else #f
  (define (smallest-divisor n)
    (find-divisor n 2))
  (define (find-divisor n test-divisor)
    (cond ((> (square test-divisor) n) n)
          ((= (remainder n test-divisor) 0) test-divisor)
          (else (find-divisor n (+ test-divisor 1)))))
  (cond ((< n 2) #f)
        ((= (smallest-divisor n) n) #t)
        (else #f)))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

;; FINAL

(define (prime-sum n)
  (map
   make-pair-sum
   (filter
   prime-sum?
   (unique-pairs n))))

;; Test
(prime-sum 10)
(display "***")
(newline)
(prime-sum 3)
(display "***")
(newline)
(prime-sum 12)

;; Output
;'((2 1 3)
;  (3 2 5)
;  (4 1 5)
;  (4 3 7)
;  (5 2 7)
;  (6 1 7)
;  (6 5 11)
;  (7 4 11)
;  (7 6 13)
;  (8 3 11)
;  (8 5 13)
;  (9 2 11)
;  (9 4 13)
;  (9 8 17)
;  (10 1 11)
;  (10 3 13)
;  (10 7 17)
;  (10 9 19))
;***
;'((2 1 3) (3 2 5))
;***
;'((2 1 3)
;  (3 2 5)
;  (4 1 5)
;  (4 3 7)
;  (5 2 7)
;  (6 1 7)
;  (6 5 11)
;  (7 4 11)
;  (7 6 13)
;  (8 3 11)
;  (8 5 13)
;  (9 2 11)
;  (9 4 13)
;  (9 8 17)
;  (10 1 11)
;  (10 3 13)
;  (10 7 17)
;  (10 9 19)
;  (11 2 13)
;  (11 6 17)
;  (11 8 19)
;  (12 1 13)
;  (12 5 17)
;  (12 7 19)
;  (12 11 23))
;> 
