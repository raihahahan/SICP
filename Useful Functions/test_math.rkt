#lang racket

(define (golden-ratio trials)
  (define (iter count result)
    (cond ((= count 1) result)
          (else
           (iter (- count 1) (+ 1 (/ 1.0 result))))))
  (iter trials 1))



(define (golden-ratio-recur trials)
  (if (= trials 1)
      1
      (+ 1 (/ 1.0 (golden-ratio-recur (- trials 1))))))

(golden-ratio 1000)
(golden-ratio-recur 1000)
(/ (+ 1 (sqrt 5)) 2)

(define (make-decrementer balance)
  (lambda (amount)
    (- balance amount))
    )

(define (make-withdraw balance)
  (lambda (amount)
    (set! balance (- balance amount))
    balance))

(define D1 (make-decrementer 25))
(define D2 (make-decrementer 25))
(define W1 (make-withdraw 25))
(define W2 (make-withdraw 25))

(W1 10)
(W1 10)
(W2 10)

