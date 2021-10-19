#lang racket
(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (make r a)
  (attach-tag 'polar (cons r a)))
(define z (make 2 3))

(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "BAD")))

(define (test a . b)
  ;;(let ((type-tags (map (lambda (x) (car x)) b)))
   ;; type-tags))
  (map contents b))
(test z)
(display "23\n")
(define (sum a b) (+ a b))
(define make11 





(test 'real-part z)