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
      
      
   