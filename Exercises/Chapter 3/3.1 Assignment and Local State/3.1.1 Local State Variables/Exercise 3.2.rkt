#lang racket
(define (make-monitored f)
  ;; initial state, local variable
  (let ((counter 0))
    (lambda (g)

      ;; immediately dispatch on type
      (define (mf j)
        (cond ((eq? j 'how-many-calls?)
               counter)
              ((eq? j 'reset-count)
               (set! counter 0))
              (else
               (begin
                 (set! counter (+ counter 1))
                 (f j)
                 ))))                
    (mf g))))

(define (make-monitored-1 f)
   ;; initial state, local variable
  (let ((counter 0))
   
    ;; operations that manages the state of local variable
    (define (increment arg)
      (begin
        (set! counter (+ counter 1))
        (f arg)))
    (define (return-calls)
      counter)
    (define (reset-calls)
      (set! counter 0))

    ;; dispatch on operation type
    (define (dispatch m)
      (cond ((eq? m 'how-many-calls?) (return-calls))
            ((eq? m 'reset-count) (reset-calls))
            (else
             (increment m))))

    ;; returns dispatch object
    dispatch))
    

(define s (make-monitored sqrt))
(s 100)
(s 'how-many-calls?)
(s 'reset-count)
(s 'how-many-calls?)
(s 900)
(s 1000)
(s 'how-many-calls?)

;10
;1
;0
;30
;1
;31.622776601683793
;2

'**********************************

(define a (make-monitored-1 sqrt))
(a 100)
(a 'how-many-calls?)
(a 'reset-count)
(a 'how-many-calls?)
(a 900)
(a 1000)
(a 'how-many-calls?)

;10
;1
;0
;30
;31.622776601683793
;2
  
      