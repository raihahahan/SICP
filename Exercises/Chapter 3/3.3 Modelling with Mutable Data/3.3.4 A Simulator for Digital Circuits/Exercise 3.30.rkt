#lang racket
(define (ripple-carry-adder an bn sn c-in)
  (define (iter an bn sn c-in c-out)
    (if (null? an)
        sn
        (begin   
          (full-adder (car an) (car bn) c-in (car sn) c-out)
          (iter (cdr an) (cdr bn) (cdr sn) c-out (make-wire)))))
  (iter an bn sn c-in (make-wire)))

;; delay:
;; n full adders = 2n half adders
;;               = 2n * (2*and-gate + 1*or-gate + 1*inverter)