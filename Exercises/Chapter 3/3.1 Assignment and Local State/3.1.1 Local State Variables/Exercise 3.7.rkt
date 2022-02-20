#lang racket

(define (make-joint acc1 acc1-pw acc2-pw)
  (define (withdraw amount)
    ((acc1 acc1-pw 'withdraw) amount))

  (define (deposit amount)
    ((acc1 acc1-pw 'deposit) amount))

   (define (dispatch pw m)
    (cond
      ((or (not (eq? acc2-pw pw))
           (not (acc1 acc1-pw 'auth)))
       (lambda (a) "Incorrect password"))
      ((eq? m 'withdraw) withdraw)
      ((eq? m 'deposit) deposit)
      (else
         (error "Unknown request: MAKE-ACCOUNT"
                m))))
    dispatch)
 

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
      ((eq? m 'auth) (eq? pw password))
      (else
         (error "Unknown request: MAKE-ACCOUNT"
                m))))
    dispatch)

(define paul (make-account 100 'paul))
((paul 'paul 'withdraw) 10) ; 90
((paul 'pasul 'withdraw) 10) ; "Incorrect password"
(define pete-wrong
  (make-joint paul 'pausl 'new-pete)) 
(define pete
  (make-joint paul 'paul 'new-pete)) 
((pete 'new-pete 'withdraw) 10)  ; 80
((paul 'paul 'deposit) 200) ; 280
((pete 'new-pete 'deposit) 0) ; 280
((paul 'paul 'deposit) 0) ; 280
        
   