#lang racket/base
(#%require sicp-pict)
(define wave einstein)

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

(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter
         (make-frame new-origin
                     (sub-vect (m corner1) new-origin)
                     (sub-vect (m corner2) new-origin)))))))

;; Exercise 2.50
(define (flip-horiz painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))

(define (rotate_counterclockwise_90 painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))

(define (rotate_cc_180 painter)
  (transform-painter painter
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 1.0)
                     (make-vect 1.0 0.0)))

(define (rotate_cc_270 painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))

;; Attempt with loop
(define (repeat func times initial)
  (if (= times 0)
      initial
      (repeat func (- times 1) (func initial))))

(define (rotate_counterclockwise_180 painter)
  (repeat rotate_counterclockwise_90 2 painter))

(define (rotate_counterclockwise_270 painter)
  (repeat rotate_counterclockwise_90 3 painter))


  
        
        





        




