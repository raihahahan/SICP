#lang racket/base
(define x (cons 1 2))
(define y (cons 3 4))
(define z (cons x y))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

; Second layer of data abstraction barrier 
(define (make-rat n d)
  (let ((g (gcd (abs n) (abs d))))
    (cond ((and (< n 0) (< d 0)) (cons (/ (abs n) g) (/ (abs d) g)))
          ((< d 0) (cons (/ (* -1 n) g) (/ (abs d) g)))
          (else (cons (/ n g) (/ d g)))
          )))

(define (numer x) (car x))
(define (denom x) (cdr x))

; Third layer of data abstraction barrier
(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))


(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(define one-half (make-rat 1 -2))
;(print-rat one-half)
(define one-third (make-rat 1 3))

;; Tests
; Fourth layer of data abstraction: manipulating rational numbers
(print-rat (add-rat one-half one-third))
(print-rat (mul-rat one-half one-third))
(print-rat (add-rat one-third one-third))

;; Output
;-1/6
;-1/6
;2/3