#lang sicp
(#%require sicp-pict)
(define wave einstein)

(define (square x)
  ;; takes an argument number x
  ;; returns the square of x
  (* x x))

(define (fixed-point f first-guess)
  ;; takes as argument a procedure f and number first-guess
  ;; Wikipedia: a fixed point of a function is an element
  ;;            of the function's doman that is mapped to
  ;;            itself by the function
  ;; eg. a is a fixed point of f if f(a) = a
  (define (close-enough? v1 v2)
    ;; internal procedure definition for tolerance of guess
    (< (abs (- v1 v2)) 0.000001))
  (define (try guess)
    ;; internal procedure definition to test for a given guess
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (deriv g)
  ;; procedure which finds the derivative of a given procedure g with respect to x
  (define dx 0.00001)
  (lambda (x)
      (/ (- (g (+ x dx)) (g x))
         dx)))

(define (newtons-transform g)
  ;; helper function to be used in newtons-method procedure
  ;; takes as argument a procedure g
  (lambda (x)
    (- x (/ (g x)
            ((deriv g) x)))
            )
       )

(define (newtons-method g guess)
  ;; Wikipedia: Newton's method is a powerful technique—in general the convergence is quadratic: as the method converges on the root, the difference between the root and the approximation is squared (the number of accurate digits roughly doubles) at each step.
  ;; takes as argument a procedure g and number guess
  (fixed-point (newtons-transform g) guess))

(define (sqrt x)
  ;; square root procedure which uses the special case of newton's method
  ;; takes as argument a number x
  ;; returns sqrt of x
  (newtons-method (lambda (y) (- (square y) x)) 1.0))

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
  (cons start end))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))

;; START

;; Define draw-line
(define (gradient segment)
  (let ((start (start-segment segment))
        (end (end-segment segment)))
    (let ((x1 (xcor-vect start))
          (y1 (ycor-vect start))
          (x2 (xcor-vect end))
          (y2 (ycor-vect end)))
      (/ (- y2 y1)
         (- x2 x1)))))

(define (horizontal? segment)
  (= (gradient segment) 0))

(define (vertical? segment)
   (let ((start (start-segment segment))
        (end (end-segment segment)))
    (let ((x1 (xcor-vect start))
          (y1 (ycor-vect start))
          (x2 (xcor-vect end))
          (y2 (ycor-vect end)))
      (and (not (= y2 y1))
           (= x2 x1)))))

(define (length segment)
  (let ((start (start-segment segment))
        (end (end-segment segment)))
    (let ((x1 (xcor-vect start))
          (y1 (ycor-vect start))
          (x2 (xcor-vect end))
          (y2 (ycor-vect end)))
      (sqrt (+ (square (- x2 x1))
               (square (- y2 y1)))))))

(define (draw-line start end)
  (let ((line (make-segment start end)))
    (let ((len (length line)))
      (let ((disp (cond ((vertical? line) "|\n")
                        ((horizontal? line) "—")                        
                        ((> (gradient line) 0) "/\n")
                        ((< (gradient line) 0) "\\\n"))))                    
        (define (iter n)
          (display disp)
          (if (> n len)
              (display disp)
              (iter (+ n 1))))
        (iter 0)
        (newline)
        ))))

(define v1 (make-vect -10 12))
(define v2 (make-vect 0 0))
(define seg1 (make-segment v1 v2))
(draw-line v1 v2)

; a. The painter that draws the outline of the designated frame.
(define (frame-paint frame)
  (let ((origin (origin-frame frame))
        (edge1 (edge1-frame frame))
        (edge2 (edge2-frame frame)))
    (let ((tL (add-vect origin edge1))
          (bR (add-vect origin edge2))
          (bL origin))
      (let ((tR (add-vect tL edge2)))
        (let ((s1 (make-segment tL bL))
              (s2 (make-segment bR bL))
              (s3 (make-segment tR tL))
              (s4 (make-segment tR bR)))
          (segments->painter (list s1
                                   s2
                                   s3
                                   s4)) frame)))))
                                            
; b. The painter that draws an ``X'' by connecting opposite corners of the frame.
(define (x-paint frame)
  (let ((origin (origin-frame frame))
        (edge1 (edge1-frame frame))
        (edge2 (edge2-frame frame)))
     (let ((tL (add-vect origin edge1))
          (bR (add-vect origin edge2))
          (bL origin))
      (let ((tR (add-vect tL edge2)))
        (let ((s1 (make-segment tL bR))
              (s2 (make-segment tR bL)))
          (segments->painter (list s1
                                   s2)) frame)))))
                                  

; c. The painter that draws a diamond shape by connecting the midpoints of the sides of the frame.
(define (diamond-painter frame)
  (let ((origin (origin-frame frame))
        (edge1 (edge1-frame frame))
        (edge2 (edge2-frame frame)))
     (let ((tL (add-vect origin edge1))
          (bR (add-vect origin edge2))
          (bL origin))
       (let ((tR (add-vect tL edge2)))
         (let ((t1 (midpoint tL bL))
               (t2 (midpoint tR tL))
               (t3 (midpoint tR bL))
               (t4 (midpoint bR bL)))
           (let ((s1 (make-segment t1 t2))
                 (s2 (make-segment t2 t3))
                 (s3 (make-segment t3 t4))
                 (s4 (make-segment t4 t1)))
             (segments->painter (list s1
                                      s2
                                      s3
                                      s4)) frame))))))

; d. The wave painter.
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
           (s21 (make-segment (make-vect 0.125 0.7) (make-vect 0 0.9))))
       (segments->painter (list s1 s2 s3 s4 s5 s6 s7
                                s8 s9 s10 s11 s12 s13
                                s14 s15 s16 s17 s18 s19 s20 s21)) frame)))
                                
           
     



 





         