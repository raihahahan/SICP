#lang racket

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1)
                 (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1)
                 trials-passed))))
  (iter trials 0))

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(define (square x)
  (* x x))

(define (predicate-circle x y)
  (let ((cx 0)
        (cy 0)
        (r 1))
    (<= (+ (square (- x cx))
           (square (- y cy)))
        (square r))))

(define (area-of-rectangle x1 x2 y1 y2)
  (let ((L (- x2 x1))
        (B (- y2 y1)))
    (* L B)))
       
(define (estimate-integral P x1 x2 y1 y2 trials)
  (* (* (monte-carlo trials (lambda () (P (random-in-range x1 x2) (random-in-range y1 y2))))
        (area-of-rectangle x1 x2 y1 y2)) 1.0))

(estimate-integral predicate-circle -1 1 -1 1 10000000)
; 3.00001


