#lang racket
(define (make-account username balance password)
  
  (define (withdraw pw amount)
    (cond
      ((not (eq? pw password)) "Incorrect password")
      ((>= balance amount)
       (begin
         (set! balance (- balance amount))
         balance))
      (else
       "Insufficient funds")))
  
  (define (deposit pw amount)
    (cond
      ((not (eq? pw password)) "Incorrect password")
      (else
       (begin
         (set! balance (+ balance amount))
         balance))))
  
  (define (dispatch pw m)
    (cond
      ((eq? m 'withdraw) (lambda (amount) (withdraw pw amount)))
      ((eq? m 'deposit) (lambda (amount) (deposit pw amount)))
      (else
       (error "Unknown request: MAKE-ACCOUNT"
              m))))
  dispatch)

(define acc (make-account 100 'secret-password))
((acc 'secret-password 'withdraw) 40)
((acc 'some-other-password 'deposit) 50)

;60
;"Incorrect password"
        
   