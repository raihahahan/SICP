#lang racket

(define rand
  (let ((x 1))
    
    (define (generate)
      (define (rand-update x)
        (let ((a 3)
              (b 7)
              (m 13))
          (remainder (+ (* a x) b) m)))
        (set! x (rand-update x))
        x)

    (define (reset)
      (lambda (val) (set! x val)))

    (define (dispatch m)
      (cond ((eq? m 'generate) (generate))
            ((eq? m 'reset) (reset))
            (else
             (error "Procedure not found."))))
    dispatch))

(rand 'generate)
(rand 'generate)
(rand 'generate)
(rand 'generate)
((rand 'reset) 123)
(rand 'generate)
(rand 'generate)
(rand 'generate)
(rand 'generate)

;10
;11
;1
;10
;12
;4
;6
;12
      
      