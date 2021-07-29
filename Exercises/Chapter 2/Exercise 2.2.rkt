#lang racket/base
;; exercise 2.2

;; Second layer of data abstraction: making use of cons car and cdr to make constructors and selectors
(define (make-segment start end)
  (cons start end))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))

(define (make-point x y)
  (cons x y))

(define (x-point point)
  (car point))

(define (y-point point)
  (cdr point))

;; Third layer of data abstraction
(define (midpoint-segment segment)
  (let ((start (start-segment segment))
        (end (end-segment segment)))
    (let ((x-A (x-point start))
          (y-A (y-point start))
          (x-B (x-point end))
          (y-B (y-point end)))
    (make-point (/ (+ x-A x-B) 2)
                (/ (+ y-A y-B) 2)))))

(define (print-point p)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define my-start-segment (make-point 0 3))
(define my-end-segment (make-point 5 5))
(define my-segment (make-segment my-start-segment my-end-segment))

;; Test
;; Fourth layer of data abstraction
(define my-midpoint (midpoint-segment my-segment))
(print-point my-midpoint)

;; Output
; (5/2,4)