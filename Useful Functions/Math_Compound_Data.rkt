#lang racket/base
;; Math compound data

; 1. Vectors

;; Vector implementation
; 1st layer: constructors and selectors make-vect, xcor-vect, ycor-vect

(define (make-vect x y)
  (cons x y))

(define (xcor-vect vector)
  (car vector))

(define (ycor-vect vector)
  (cdr vector))

; 2nd layer: procedures add-vect, sub-vect, scale-vect

(define (add-vect v1 v2)
  (let ((x1 (xcor-vect v1))
        (x2 (xcor-vect v2))
        (y1 (ycor-vect v1))
        (y2 (ycor-vect v2)))
    (make-vect (+ x1 x2)
               (+ y1 y2))))

(define (sub-vect v1 v2)
  (let ((x1 (xcor-vect v1))
        (x2 (xcor-vect v2))
        (y1 (ycor-vect v1))
        (y2 (ycor-vect v2)))
    (make-vect (- x1 x2)
               (- y1 y2))))

(define (scale-vect s vect)
  (let ((x (xcor-vect vect))
        (y (ycor-vect vect)))
    (make-vect (* s x)
               (* s y))))