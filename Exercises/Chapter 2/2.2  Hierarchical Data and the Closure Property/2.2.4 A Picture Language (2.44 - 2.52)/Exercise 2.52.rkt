#lang sicp
(#%require sicp-pict)
(define wave einstein)

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
  ;; used to shift and scale images to fit the frame
  ;; map transforms the unit square into the frame by mapping the vector v = (x, y) to the vector sum.
  (lambda (v)
    (add-vect ;; to add vectors
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v)
                           (edge1-frame frame))
               (scale-vect (ycor-vect v)
                           (edge2-frame frame))))))


;; Painter Implementation
;(define (segments->painter segment-list)
 ; (lambda (frame)
  ;  (for-each
   ;  (lambda (segment)
    ;   (draw-line
     ;   ((frame-coord-map frame) (start-segment segment))
      ;  ((frame-coord-map frame) (end-segment segment))))
    ; segment-list)))

;; The segments are given using coordinates with respect to the unit square.

;; For each segment in the list, the painter transforms the segment endpoints with the frame coordinate map and draws a line between the transformed points.

;; Segment Implementation
(define (make-segment start end)
  (cons start end))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))

;; Exercise 2.52

; a. add some segments to wave painter
(define (wave-painter frame)
   (let ((origin (origin-frame frame))
        (edge1 (edge1-frame frame))
        (edge2 (edge2-frame frame)))
     ;; 21 segments total: arbitrarily guess the coords
     ;; plotted coordinated using Desmos
     ; 1. (0, 0.5), (0.125, 0.5)
     ; 2. (0.125,0.5), (0.25, 0.65)
     ; 3. (0.25, 0.65), (0.325, 0.6)
     ; 4. (0.325, 0.6), (0.25, 0)
     ; 5. (0.25, 0), (0.35, 0)
     ; 6. (0.35, 0), (0.5, 0.2)
     ; 7. (0.5, 0.2), (0.65, 0)
     ; 8. (0.65, 0), (0.75, 0)
     ; 9. (0.75, 0), (0.65, 0.55)
     ; 10. (0.65, 0.55), (1, 0.15)
     ; 11. (1, 0.15), (1, 0.4)
     ; 12. (1, 0.4), (0.8, 0.75)
     ; 13. (0.8, 0.75), (0.65, 0.75)
     ; 14. (0.65, 0.75), (0.725, 0.9)
     ; 15. (0.725, 0.9), (0.65, 1)
     ; 16. (0.65, 1), (0.4, 1)
     ; 17. (0.4, 1), (0.325, 0.9)
     ; 18. (0.325, 0.9), (0.4, 0.75)
     ; 19. (0.4, 0.75), (0.25, 0.75)
     ; 20. (0.25, 0.75), (0.125, 0.7)
     ; 21. (0.125, 0.7), (0, 0.9)
     (let ((s1 (make-segment (make-vect 0 0.5) (make-vect 0.125 0.5)))
           (s2 (make-segment (make-vect 0.125 0.5) (make-vect 0.25 0.65)))
           (s3 (make-segment (make-vect 0.25 0.65) (make-vect 0.325 0.6)))
           (s4 (make-segment (make-vect 0.325 0.6) (make-vect 0.25 0)))
           (s5 (make-segment (make-vect 0.25 0) (make-vect 0.35 0)))
           (s6 (make-segment (make-vect 0.35 0) (make-vect 0.5 0.2)))
           (s7 (make-segment (make-vect 0.5 0.2) (make-vect 0.65 0)))
           (s8 (make-segment (make-vect 0.65 0) (make-vect 0.75 0)))
           (s9 (make-segment (make-vect 0.75 0) (make-vect 0.65 0.55)))
           (s10 (make-segment (make-vect 0.65 0.55) (make-vect 1 0.15)))
           (s11 (make-segment (make-vect 1 0.15) (make-vect 1 0.4)))
           (s12 (make-segment (make-vect 1 0.4) (make-vect 0.8 0.75)))
           (s13 (make-segment (make-vect 0.8 0.75) (make-vect 0.65 0.75)))
           (s14 (make-segment (make-vect 0.65 0.75) (make-vect 0.725 0.9)))
           (s15 (make-segment (make-vect 0.725 0.9) (make-vect 0.65 1)))
           (s16 (make-segment (make-vect 0.65 1) (make-vect 0.4 1)))
           (s17 (make-segment (make-vect 0.4 1) (make-vect 0.325 0.9)))
           (s18 (make-segment (make-vect 0.325 0.9) (make-vect 0.4 0.75)))
           (s19 (make-segment (make-vect 0.4 0.75) (make-vect 0.25 0.75)))
           (s20 (make-segment (make-vect 0.25 0.75) (make-vect 0.125 0.7)))
           (s21 (make-segment (make-vect 0.125 0.7) (make-vect 0 0.9)))
           (s22 (make-segment (make-vect 0.0 0.0) (make-vect 0.85 0.85)))) ;; added a segment across the wave painter
       (segments->painter (list s1 s2 s3 s4 s5 s6 s7
                                s8 s9 s10 s11 s12 s13
                                s14 s15 s16 s17 s18 s19 s20 s21 s22)) frame)))

; b. change pattern constructed by corner-split
(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))

(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))

;;;; using only one copy of the up-split and right-split images instead of two
(define (corner-split-1 painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left up)
              (bottom-right right)
              (corner (corner-split-1 painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))

; c. modify the version of square-limit that uses square-of-four so as to assemble the corners in a different pattern
(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
          (bottom (beside (bl painter) (br painter))))
      (below bottom top))))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))

(define (identity x) x)

(define (flip-v-h x) (flip-vert (flip-horiz x)))

(define (square-limit painter n)
  (let ((quarter (corner-split painter n)))
    ((square-of-four flip-horiz
                    identity
                    flip-v-h
                    flip-vert) quarter)))

(paint (square-limit wave 3))
          
                                          
