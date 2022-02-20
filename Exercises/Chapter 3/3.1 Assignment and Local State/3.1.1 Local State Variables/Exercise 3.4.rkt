#lang racket
(define (make-account balance password)
  ;; define local variable
  (let ((attempts 0))

    ;; Operations that work on the states of local variables (i.e balance and attempts)
    (define (withdraw pw amount)
      (cond
        ((not (eq? pw password))
         (begin
           (increment-attempts)
           "Incorrect password"))
        ((>= balance amount)
         (begin
           (reset-attempts)
           (set! balance (- balance amount))
           balance))
        (else
         "Insufficient funds")))
  
    (define (deposit pw amount)
      (cond
        ((not (eq? pw password))
         (begin
           (increment-attempts)
           "Incorrect password"))
        (else
         (begin
           (reset-attempts)
           (set! balance (+ balance amount))
           balance))))

    ;; Helper procedures
    (define (increment-attempts)
      (set! attempts (+ attempts 1)))

    (define (reset-attempts)
      (set! attempts 0))

    (define (call-the-cops)
      "The cops have been called.")

    ;; Dispatch on operation type
    (define (dispatch pw m)
      (cond
        ((>= attempts 7) (lambda (arg) (call-the-cops)))
        ((eq? m 'withdraw) (lambda (amount) (withdraw pw amount)))
        ((eq? m 'deposit) (lambda (amount) (deposit pw amount)))
        (else
         (error "Unknown request: MAKE-ACCOUNT"
                m))))

        dispatch))

(define acc (make-account 100 'secret-password))
((acc 'secret-password 'withdraw) 40)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'secret-password 'withdraw) 40)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)

;60

;"Incorrect password"
;"Incorrect password"
;"Incorrect password"
;"Incorrect password"
;"Incorrect password"
;"Incorrect password"

;20

;"Incorrect password"
;"Incorrect password"
;"Incorrect password"
;"Incorrect password"
;"Incorrect password"
;"Incorrect password"
;"Incorrect password"

;"The cops have been called."
;"The cops have been called."
;"The cops have been called."
;"The cops have been called."
;"The cops have been called."
;"The cops have been called."
;"The cops have been called."
        
   