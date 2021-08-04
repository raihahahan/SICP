#lang racket/base
(define (square x)
  (* x x))

;; General strategy of cdr-ing down a list

(define (list-ref l n)
  ;; takes as arguments a list l and a number n
  ;; returns the nth item of the list
  ;; index starts from 0
  (if (= n 0)
      (car l)
      (list-ref (cdr l) (- n 1))))

(define (list-length l)
  ;; takes as argument a list l
  ;; returns the length of list
  (if (null? l)
      0
      (+ 1 (list-length (cdr l)))))

(define (list-length-iter l)
  (define (iter count items)
    (if (null? items)
        count
        (iter (+ count 1) (cdr items))))
  (iter 0 l))
                          
(define (append l1 l2)
  (if (null? l1)
      l2
      (cons (car l1) (append (cdr l1) l2))))

;; Exercise 2.17
(define (last-pair l)
  ;; takes as argument a list l
  ;; returns last item in list that is not nil
  (let ((length (list-length-iter l)))
    (list-ref l (- length 1))))

(define (last-pair-2 l)
  ;; takes as argument a list l
  ;; returns last item in list that is not nil
  (if (null? (cdr l))
      (car l)
      (last-pair-2 (cdr l))))

;; Exercise 2.18
(define (remove-last l)
  ;; takes as argument a list l
  ;; returns a list with last element l removed
  (if (null? (cdr l))
      '()
      (cons (car l) (remove-last (cdr l)))))

(define (remove-first l)
  (cdr l))

(remove-first (list 1 2 3 4))

(define (reverse l)
  (define (rev-recur a length)
    (if (= length 0)
        '()
        (cons (last-pair a) (rev-recur (remove-last a) (- length 1)))))
  (let ((len (list-length l)))
    (rev-recur l len)))


(define (reverse-1 l)
  (if (null? l)
      l
      (append (reverse-1 (cdr l)) (list (car l)))))

(define (reverse-2 items) 
  (define (iter items result) 
    (if (null? items) 
        result 
        (iter (cdr items) (cons (car items) result)))) 
  (iter items '())) 

(define test-list (list 1 4 9 16 25))
(reverse test-list)
(reverse-1 test-list)
(reverse-2 test-list)

(define (map-1 p l) ; p === procedure passed to each element of list
  (if (null? l)
      '()
      (cons (p (car l))
            (map-1 p (cdr l)))))

(define (scale-list s l)
  (map-1 (lambda (item) (* item s)) l))

;; for-each
(define (for-each proc list)
  (cond ((null? list) "done")
        (else (display (proc (car list)))
              (newline)
              (for-each proc
                        (cdr list)))))

;; Exercise 2.19
(define us-coins (list 1 5 10 25 50))
;(define uk-coins (list 0.5 5 10 20 50 2 1 100))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(define (count-change amount denomination)
  ;; takes as arguments the amount of money to exchange in coins, and the denomination of the currency
  ;; amount is type number, denomination is type list

  ;; local procedure definitions
  (define (first-denomination coin-values)
    ;; takes as argument a list of the coin types in a particular currency
    ;; returns the first coin in the list
    (list-ref coin-values 0))
  
  (define (except-first-denomination coin-values)
    ;; takes as argument a list of the coin types in a particular currency
    ;; returns the list without the first element
    (remove-first coin-values))
  
  (define no-more? null?)
  
  (define (cc amount coin-values)
    ;; takes as argument a number (amount) and list (coin-values)
    ;; returns the number of coins we can exchange the amount with in a particular currency
    (cond ((= amount 0) 1)
          ((or (< amount 0) (no-more? coin-values)) 0)
          (else (+ (cc amount
                       (except-first-denomination coin-values)) ;; if first kind of coin is not used
                   (cc (- amount
                          (first-denomination coin-values)) ;; if first kind of coin is used
                       coin-values)))))
  (cc amount denomination)
  )
  
;(count-change 100 uk-coins)

;; Order of the list does not affect the value returned by count-change

;; We can see that the order doesn't matter. This is because the procedure recursively evaluates every sub-list after subtracting the value of the first coin from the target amount. It doesn't matter if that value is the smallest, largest, or even if the values are shuffled.

;; Exercise 2.20

(define (filter p l)
  ;; takes as argument procedure p and list l
  ;; returns a list with elements that fulfil a true condition when passed into p as an argument
  (cond ((null? l) '())
        ((p (car l)) (cons (car l) (filter p (cdr l))))
        (else (filter p (cdr l)))))

(define (same-parity? x y)
  ;; takes as arguments two numbers x and y
  ;; returns boolean value to indicate if x and y are same parity
  (if (< x y)
      (= (remainder y x) 0)
      (= (remainder x y) 0)))

(define (parity? x)
  ;; takes as argument a number x
  ;; returns either an even? or odd? procedure
  (if (even? x)
      even?
      odd?))

(define (same-parity x . y)
   ;;takes one or more integers and returns a list of all the arguments that have the same even-odd parity as the first argument
  (cond ((null? y) x)
        (else (cons x (filter (parity? x) y)))))

;; map takes a procedure of n arg and m lists
;; applies operator to each element in each list
;; returns final list
;(map +
 ;    (list 1 2 3)
  ;   (list 40 50 60))

;; Exercise 2.21
(define (square-list-1 l)
  (if (null? l)
      '()
      (cons (square (car l)) (square-list-1 (cdr l)))))

(define (square-list-2 l)
  (map square l))

(square-list-1 (list 1 2 3 4))
(square-list-2 (list 1 2 3 4))

;; Exercise 2.22
; It is in reverse order because we are constructing the list by pushing the elements from the start of the list in chronological order.
; The first element is added first, then the second element is added, and so on.
; As a result, our list is in reversed order

;; For each iteration, we are substituting the answer argument with a data object or a list
;; For the next iteration, we pair this data object with the square of (car things).
;; This results in a final nested list as we are constructing a nested object with (car things) for each iteration.

(define 1-to-3 (list 1 2 3))

;; Exercise 2.23

(define (for-each-1 p l)
  ;; takes as arguments procedure p and list l
  (cond ((null? l) #t)
        (else
         (p (car l))
         (newline)
         (for-each-1 p (cdr l)))))

(for-each-1 (lambda (x) (display (square x))) (list 1 2 3 4))



