#lang racket
;; EXPLANATION OF SOLUTION

; There are 2 main local variables with state: balance and password
; Balance is shared amongst the accounts in joint-accounts (or not shared if it is not joint).
; Password is unique to each account.
; We create an account with (create-account) and give our balance and password.
; Withdrawing and depositing works the same way as 3.3, where the procedure checks the password and do the necessary operations.
; To add joint account, simply return the procedure itself (recursively), with a new password variable but same balance state.

(define (make-joint acc1 acc1-pw acc2-pw)
  ((acc1 acc1-pw 'add-joint) acc2-pw))

(define (make-account balance password)
  
  (define (withdraw amount)
      (cond
        ((>= balance amount)
         (begin
           (set! balance (- balance amount))
           balance))
        (else
         "Insufficient funds")))
  
    (define (deposit amount)
      (begin
        (set! balance (+ balance amount))
        balance))
  
  (define (dispatch pw m)
    (cond
      ((not (eq? pw password)) (lambda (a) "Incorrect password"))
      ((eq? m 'withdraw) withdraw)
      ((eq? m 'deposit) deposit)
      ((not (eq? pw password)) (lambda (a) "Incorrect password"))
      ((eq? m 'add-joint) (lambda (new-pw) (make-account balance new-pw)))
      (else
         (error "Unknown request: MAKE-ACCOUNT"
                m))))
    dispatch)
          
(define paul (make-account 100 'paul))
((paul 'paul 'withdraw) 10)
((paul 'pasul 'withdraw) 10)
(define pete
  (make-joint paul 'paul 'new-pete))
((pete 'new-pete 'withdraw) 10)
        
   