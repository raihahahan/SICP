#lang racket/base
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

(define (square x)
  (* x x))

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


;; Exercise 2.3

;; Consists of 3 different implementations
; 1. get bottom left and top right points
; 2. get two perpendicular segments
; 3. get two parallel segments of same length
; 4. codology's method: https://codology.net/post/sicp-solution-exercise-2-3/

;; degree to radian converter
(define (deg-to-rad angle)
  (/ (* angle 3.14159265359) 180))

;; gradient library
(define (gradient segment)
  (let ((start (start-segment segment))
        (end (end-segment segment)))
    (let ((x-A (x-point start))
          (y-A (y-point start))
          (x-B (x-point end))
          (y-B (y-point end)))
  (/ (- y-B y-A)
     (- x-B x-A)))))

;; length library
(define (length segment)
  (let ((start (start-segment segment))
        (end (end-segment segment)))
    (let ((x-A (x-point start))
          (y-A (y-point start))
          (x-B (x-point end))
          (y-B (y-point end)))
  (sqrt
          (+
           (square (- x-A x-B))
           (square (- y-A y-B)))))))

;; First Implementation: get bottom left and top right points

; 1st layer of abstraction
(define (make-length-width length width)
  (cons length width))
(define (get-length rectangle)
  (car rectangle))
(define (get-width rectangle)
  (cdr rectangle))

; 2nd layer of abstraction
(define (get-diagonal rectangle)
  (sqrt (+ (square (get-length rectangle))
           (square (get-width rectangle)))))

(define (make-rect-1 bottom-left top-right bottom-right)
  (let ((diagonal (make-segment bottom-left top-right))
        (lower-diagonal (make-segment bottom-left bottom-right)))
    (let ((grad-diagonal (gradient diagonal))
          (grad-lower-diagonal (gradient lower-diagonal))
          (len-diagonal (length diagonal)))
      (let ((angle (atan grad-diagonal))
            (phi (atan grad-lower-diagonal)))
        (let ((theta (- angle phi)))
          (let ((width (* len-diagonal (sin theta)))
                (length (* len-diagonal (cos theta))))
            (make-length-width length width)
            ))))))
(define (get-length-rect rectangle)
  (get-length rectangle))
(define (get-width-rect rectangle)
  (get-width rectangle))

(define my-rect-1 (make-rect-1 (make-point 3 3) (make-point 5 7) (make-point 6 4)))

;; END OF FIRST IMPLEMENTATION

; 2nd implementation: get two perpendicular segments
(define (make-rect-2 segment-1 segment-2)
  (let ((grad-1 (gradient segment-1))
        (grad-2 (gradient segment-2)))
    (if (= (* grad-1 grad-2) -1)
        (make-length-width (length segment-1) (length segment-2))
        (display "INVALID"))))

(define my-rect-2 (make-rect-2 (make-segment (make-point 2 6) (make-point 3 3))
                               (make-segment (make-point 2 6) (make-point 5 7))))

;; END OF SECOND IMPLEMENTATION

; 3rd implementation: get two parallel segments of same length
(define (make-rect-3 segment-1 segment-2)
  (let ((grad-1 (gradient segment-1))
        (grad-2 (gradient segment-2))
        (len-1 (length segment-1))
        (len-2 (length segment-2)))
    (if (and (= len-1 len-2)
             (= grad-1 grad-2))
        (make-length-width len-1 len-2)
        (display "INVALID")
        )))

(define my-rect-3 (make-rect-3 (make-segment (make-point 2 6) (make-point 5 7))
                               (make-segment (make-point 3 3) (make-point 6 4))))

;; END OF THIRD IMPLEMENTATION

; 4th implementation: codology's method
(define (make-rect origin height width angle)
  (cons (cons height width) (cons origin angle)))

(define (origin-rect r) (car (cdr r)))
(define (angle-rect  r) (cdr (cdr r)))

; Public interface
(define (height-rect r) (car (car r)))
(define (width-rect  r) (cdr (car r)))

(define (perimeter-rect r)
  (* 2 (+ (width-rect r) (height-rect r))))

(define (area-rect r)
  (* (width-rect r) (height-rect r)))
 
;; END OF FORTH IMPLEMENTATION

; area and perimeter procedures to be tested on all implementations of rectangle
(define (area rectangle)
  (* (get-length-rect rectangle)
     (get-width-rect rectangle)))

(define (perimeter rectangle)
  (+ (* 2 (get-length-rect rectangle))
     (* 2 (get-width-rect rectangle))))

;; TESTS
(define (display-output rectangle stringName)
  (newline)
  (display "rect name: ")
  (display stringName)
  (newline)
  (display "length: ")
  (display (get-length-rect rectangle))
  (newline)
  (display "width: ")
  (display (get-width-rect rectangle))
  (newline)
  (display "diagonal: ")
  (display (get-diagonal rectangle))
  (newline)
  (display "area: ")
  (display (area rectangle))
  (newline)
  (display "perimeter: ")
  (display (perimeter rectangle))
  (newline)
  )

(display-output my-rect-1 "rect-1")
(display-output my-rect-2 "rect-2")
(display-output my-rect-3 "rect-3")

(define codology (make-rect (make-point 3 3) 3.162277660168388 3.162277660168388 (atan (/ 1 3))))
 (newline)
  (display "rect name: rect-4 ")
  (newline)
  (display "length: ")
  (display (width-rect codology))
  (newline)
  (display "width: ")
  (display (height-rect codology))
  (newline)
  (display "area: ")
  (display (area-rect codology))
  (newline)
  (display "perimeter: ")
  (display (perimeter-rect codology))
  (newline)

;; OUTPUT
;rect name: rect-1
;length: 3.1622776601683795
;width: 3.162277660168379
;diagonal: 4.47213595499958
;area: 10.0
;perimeter: 12.649110640673516

;rect name: rect-2
;length: 3.1622776601683795
;width: 3.1622776601683795
;diagonal: 4.47213595499958
;area: 10.000000000000002
;perimeter: 12.649110640673518

;rect name: rect-3
;length: 3.1622776601683795
;width: 3.1622776601683795
;diagonal: 4.47213595499958
;area: 10.000000000000002
;perimeter: 12.649110640673518

;rect name: rect-4 
;length: 3.162277660168388
;width: 3.162277660168388
;area: 10.000000000000055
;perimeter: 12.649110640673552
