#lang racket
(define (find-keyval list key val)
  (cond ((null? list) #f)
        ((and (eq? (car (car list)) key)
              (eq? (cdr (car list)) val))
         #t)
        (else
         (find-keyval (cdr list) key val))))

(define a (list
 (cons 'a 'b)
 (cons 'c 'd)
 (cons 'a 'f)))

(find-keyval a 'a 'b)
(find-keyval a 'c 'd)
(find-keyval a 'f 'g)

(set! b 20)


         
             