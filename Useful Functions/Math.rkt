#lang racket/base
(require racket/trace)
(define (runtime) (current-inexact-milliseconds))
;; Contents

; even?
; expt (b^n)
; square 
; average 
; fixed-point 
; cube 
; newtons-transform
; newtons-method
; sqrt (using newtons-method)
; cbrt (cube root)
; prime?
; expmod (a modulo b)

(define (even? x)
  ;; takes an argument number x
  ;; checks if number is even
  (= (remainder x 2) 0))

(define (square x)
  ;; takes an argument number x
  ;; returns the square of x
  (* x x))

(define (expt base n)
  ;; takes as arguments base and n (types: number)
  ;; returns b^n
  (cond ((= n 0) 1)
        ((even? n) (expt (square base) (/ n 2)))
        (else (* base (expt base (- n 1))))))

(define (average x y)
  ;; takes an arguments numbers x and y 
  ;; returns average
  (/ (+ x y) 2))

(define (fixed-point f first-guess)
  ;; takes as argument a procedure f and number first-guess
  ;; Wikipedia: a fixed point of a function is an element
  ;;            of the function's doman that is mapped to
  ;;            itself by the function
  ;; eg. a is a fixed point of f if f(a) = a
  (define (close-enough? v1 v2)
    ;; internal procedure definition for tolerance of guess
    (< (abs (- v1 v2)) 0.000001))
  (define (try guess)
    ;; internal procedure definition to test for a given guess
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (deriv g)
  ;; procedure which finds the derivative of a given procedure g with respect to x
  (define dx 0.00001)
  (lambda (x)
      (/ (- (g (+ x dx)) (g x))
         dx)))

(define (cube x) (* x x x))

(define (newtons-transform g)
  ;; helper function to be used in newtons-method procedure
  ;; takes as argument a procedure g
  (lambda (x)
    (- x (/ (g x)
            ((deriv g) x)))
            )
       )

(define (newtons-method g guess)
  ;; Wikipedia: Newton's method is a powerful technique—in general the convergence is quadratic: as the method converges on the root, the difference between the root and the approximation is squared (the number of accurate digits roughly doubles) at each step.
  ;; takes as argument a procedure g and number guess
  (fixed-point (newtons-transform g) guess))

(define (sqrt x)
  ;; square root procedure which uses the special case of newton's method
  ;; takes as argument a number x
  ;; returns sqrt of x
  (newtons-method (lambda (y) (- (square y) x)) 1.0))


(define (cbrt x)
  ;; takes as argument a number x
  ;; returns its cube root
  (define (cube-improve guess x)
    (/ (+ (/ x
             (square guess))
          (* 2 guess))
       3))
  (define (cube-good-enough? previous-guess-cube guess)
    (< (abs (/ (- previous-guess-cube guess) guess)) 0.0000000000001))
  (define (cbrt-iter guess x)
    (if (cube-good-enough? guess (cube-improve guess x))
        guess
        (cbrt-iter (cube-improve guess x) x)))
  (cbrt-iter 1.0 x))

(define (prime? n)
  ;; takes as argument a number n
  ;; returns #t if n is prime, else #f
  (define (smallest-divisor n)
    (find-divisor n 2))
  (define (find-divisor n test-divisor)
    (cond ((> (square test-divisor) n) n)
          ((= (remainder n test-divisor) 0) test-divisor)
          (else (find-divisor n (+ test-divisor 1)))))
  (cond ((< n 2) #f)
        ((= (smallest-divisor n) n) #t)
        (else #f)))

(define (expmod base exp m)
  ;; takes as arguments numbers base, exp, m (types: number)
  ;; returns base^exp modulo m
  (cond ((= exp 0) 1) ;;1 mod n = 1
        ((even? exp) ;; if exp is even
         (remainder (square (expmod base (/ exp 2) m)) m)) ;; fast expt method for even exponent. returns remainder i.e (a^(n/2))^2modn
         (else
          (remainder (* base (expmod base (- exp 1) m)) m)))) ;; fast expt method for odd exponent. returns remainder i.e (a(a^(n-1)))modn





  

