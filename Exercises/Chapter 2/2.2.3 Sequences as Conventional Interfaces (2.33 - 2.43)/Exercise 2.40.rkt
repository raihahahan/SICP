#lang racket/base
;; Nested Mappings
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

;(accumulate append
 ;           nil
  ;          (map (lambda (i) ;; for each element i in 1 to n,
   ;               (map (lambda (j) (list i j)) ;; for each element j in 1 to i - 1, 
    ;                   (enumerate-interval 1 (- i 1)))) ;; list i to j
     ;       (enumerate-interval 1 n))) ;; make list of interval from 1 to n

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

; 1. Generate the sequence of all ordered pairs of positive integers less than or equal to n (i, j)
; 2. Filter to select those pairs whose sum is prime
; 3. For each pair (i, j) that passes through the filter, produce the triple (i, j, i+j)

;;; 1. GENERATE SEQUENCE OF ORDERED PAIRS
; a. For each i <= n, enumerate integers 1 <= i <= n
; b. For each j < i, enumerate integers j < i
; c. For each i, map i to for each j, map j to list (i, j)
; d. Now, we have a list of list of pairs for each j
; e. To combine the list of list of pairs into a list of pairs, accumulate with append

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))


;;; 2. FILTER SEQUENCE OF PAIRS TO FIND WHICH SUM IS PRIME
(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

;;; 3. GENERATE SEQUENCE OF FILTERS
(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

;;; FINAL
(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (flatmap
                (lambda (i)
                  (map (lambda (j) (list i j))
                       (enumerate-interval 1 (- i 1))))
                (enumerate-interval 1 n)))))
                