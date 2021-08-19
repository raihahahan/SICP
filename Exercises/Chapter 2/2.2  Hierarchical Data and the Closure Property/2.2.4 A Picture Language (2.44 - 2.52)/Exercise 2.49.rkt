#lang sicp
(#%require sicp-pict)

;; Required procedures

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

;; Frame Implementation
(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define (origin-frame frame)
  (car frame))

(define (edge1-frame frame)
  (cadr frame))

(define (edge2-frame frame)
  (caddr frame))

(define (frame-coord-map frame)
  (lambda (v)
    (add-vect ;; to add vectors
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v)
                           (edge1-frame frame))
               (scale-vect (ycor-vect v)
                           (edge2-frame frame))))))

;; Painter Implementation
(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (draw-line
        ((frame-coord-map frame) (start-segment segment))
        ((frame-coord-map frame) (end-segment segment))))
     segment-list)))

;; The segments are given using coordinates with respect to the unit square.

;; For each segment in the list, the painter transforms the segment endpoints with the frame coordinate map and draws a line between the transformed points.

;; Segment Implementation
(define (make-segment start end)
  (cons (make-vect 0 start)
        (make-vect 0 end)))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))

;; START

;; Define draw-line
(define (draw-line start end)
  (define (iter n)
    (display "—")
    (if (= n (- end 1))
        (display "—")
        (iter (+ n 1))))
  (iter start))

(draw-line 1 10)

; a. The painter that draws the outline of the designated frame.


; b. The painter that draws an ``X'' by connecting opposite corners of the frame.

; c. The painter that draws a diamond shape by connecting the midpoints of the sides of the frame.

; d. The wave painter.


 





         