#lang racket
(define (or-gate a b output)
  (let ((c (make-wire)) (d (make-wire)) (e (make-wire)) (f (make-wire)))
    (inverter a c)
    (inverter b d)
    (and-gate c d e)
    (inverter e f)
  'ok))

;; the delay is sum of the and-gate delay and twice the inverter delay.