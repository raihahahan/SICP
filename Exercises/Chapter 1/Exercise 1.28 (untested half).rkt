#lang racket/base
;; Exercise 1.28
(define (square x)
  (* x x))

(define (expmod base exp m)
  ;; takes as arguments numbers base, exp, m (types: number)
  ;; returns base^exp modulo m
  (cond ((= exp 0) 1) ;;1 mod n = 1
        ((even? exp) ;; if exp is even
         (remainder (square (expmod base (/ exp 2) m)) m)) ;; fast expt method for even exponent. returns remainder i.e (a^(n/2))^2modn
         (else
          (remainder (* base (expmod base (- exp 1) m)) m)))) ;; fast expt method for odd exponent. returns remainder i.e (a(a^(n-1)))modn

(define (miller-rabbi-test n)
  (define (miller-rabbi-iter a n) ; a^(n-1) congruent 1 mod n
    (cond ((= n 1) #f) ; n = 1 is not prime
          ((= a 1) #t) ; 1 is congruent to 1 mod n
          ((= (new-expmod a (- n 1) n) 0) #f)
          ((= (new-expmod a (- n 1) n) 1) (miller-rabbi-iter (- a 1) n))
          (else #f)))
  (miller-rabbi-iter (- n 1) n))

(define (my-fermat-test n)
  (define (my-fermat-test-iter a n)
    (cond ((= a 1) #t)
          ((= (expmod a n n) a) (my-fermat-test-iter (- a 1) n))
          (else #f)))
  (my-fermat-test-iter (- n 1) n))

(define (new-expmod base exp m)
  (define (nontrivial-checker a m)
  (if (and
       (and (not (= a 1)) (= (- m 1)))
       (= (remainder (square base) m) (expmod 1 1 m)))
      0
      (remainder (square (new-expmod base (/ exp 2) m)) m)))
  (cond ((= exp 0) 1) ;;1 mod n = 1
        ((even? exp) ;; if exp is even
         (nontrivial-checker base m)) ;; fast expt method for even exponent. returns remainder i.e (a^(n/2))^2modn
         (else
          (remainder (* base (new-expmod base (- exp 1) m)) m)))) ;; fast expt method for odd exponent. returns remainder i.e (a(a^(n-1)))modn

;; Tests
(define testing 1729)
(display "test: ")
(display testing)
(newline)
(display "miller-rabbi: ")
(miller-rabbi-test testing)
(display "fermat: ")
(my-fermat-test testing)

;; Output
;test: 1729
;miller-rabbi: #f
;fermat: #t