#lang racket
(equal? "pop" "pop")
(memq 'c (reverse (list 'a 'b 'c 'd 'e)))

;; 2 numbers of different types in a list
;; return the number with lower type
;; Helper list functions
(define (find item list)  ;; returns true if item is in list, else false.
  (cond ((null? list) #f)
        ((equal? item (car list)) #t)
        (else
         (find item (cdr list)))))

(define (index elem list)  ;; returns false if elem is not in list. else, returns the index of elem in list. list is indexed at 0.
  (define (iter ls result)
    (cond ((null? ls) #f)
          ((equal? elem (car ls)) result)
          (else
           (iter (cdr ls) (+ result 1)))))
  (if (find elem list)
      (iter list 0)
      #f))

(define (higher a b list)  ;; returns the element that is higher in the list. if one does not exist, then returns false.
  (let ((i (index a list))
        (j (index b list)))
    (if (and i j)
        (if (>= i j) a b)
        #f)))

(define (lower a b list)
  (let ((i (index a list))
        (j (index b list)))
    (if (and i j)
        (if (< i j) a b)
        #f)))

(define (right-pad number len)
  (let* ((nstr (number->string number))
         (diff (- len (string-length nstr)))
         (pads (if (> diff 0) (make-string diff #\0) "")))
    (string-append nstr pads)))

(define (u x)
  ;; converts usd to sgd
  (/ x 0.74))

(* 0.74 (+ 96.58 (u (+ 16.59 83.93 99.54 22.08 23.30 50.36))))
(u 259)

(define a (sort (list 96.58 (u 16.59) (u 83.93) (u 99.54) (u 22.08) (u 23.30) (u 50.36)) <)) 
(define b (sort (list 32.41 30.83 138.93 117.18 99.55 23.24 67.91) <))

(define (add args)
  (define (iter args result)
    (if (null? args)
        result
        (iter (cdr args) (+ (car args) result))))
  (iter args 0))


(define (access index ls)
  (define (iter ls count)
    (cond ((null? ls) #f)
          ((= index count) (car ls))
          (else (iter (cdr ls) (+ count 1)))))
  (iter ls 0))

(display "POS . cryp")
(newline)
(map (lambda (i)
       (cons (access (index i a) b) i))
     a)

(display "POS sum")
(newline)
(add b)

(display "cryp sum")
(newline)
(add a)

(display "POS in")
(newline)
350.97

(display "LOSS")
(newline)
(- 350.97 (add b))

pi
        
;(+ 32.41 30.83 138.93 117.18 99.55 23.24 67.91)