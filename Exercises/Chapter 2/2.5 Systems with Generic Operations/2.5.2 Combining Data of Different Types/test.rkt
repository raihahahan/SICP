#lang scheme
(define (find item list)
  (cond ((null? list) #f)
        ((equal? item (car list)) #t)
        (else
         (find item (cdr list)))))

(define a (list #t 0 10 2 'sadsad 'df))
(find #f a)

(range -4 1)

(apply max (map (lambda (x) (* x x)) (range -4 1)))

(memq 5 (list 1 2 3 4 5))
(null? #f)

(define (index elem list)
  (define (iter ls result)
    (cond ((null? ls) #f)
          ((eq? elem (car ls)) result)
          (else
           (iter (cdr ls) (+ result 1)))))
  (if (find elem list)
      (iter list 0)
      #f))

(define (higher a b list)
  (let ((i (index a list))
        (j (index b list)))
    (if (and i j)
        (if (>= i j) a b)
        #f)))

(higher 2 7 (list 2 3 4 5 6))

(index 0 (list 1 2 3 4 5 6))
      
      
      
   