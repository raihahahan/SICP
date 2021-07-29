#lang racket/base
(require racket/trace)
(define (runtime) (current-inexact-milliseconds))

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

(define (timed-prime-test n)
  (newline)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))
      (display "")
      ))

(define (report-prime elapsed-time)
  (display "***")
  (display elapsed-time))

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


;; exercise 1.24

;; 1. define procedure for a^n mod n
(define (expmod base exp m)
  (cond ((= exp 0) 1) ;;1 mod n = 1
        ((even? exp) ;; if exp is even
         (remainder (square (expmod base (/ exp 2) m)) m)) ;; fast expt method for even exponent. returns remainder i.e (a^(n/2))^2modn
         (else
          (remainder (* base (expmod base (- exp 1) m)) m)))) ;; fast expt method for odd exponent. returns remainder i.e (a(a^(n-1)))modn

;; 2. define procedure for fermat's test. choose random number 1 <= a <= n-1 and check whether a^nmodn = a
(define (fermat-test n) ;; procedure which does fermat's test with random 1 <= a <= n-1
  (define (try-it a) ;; procedure which returns true if a^nmodn = a, which checks if n is prime.
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

;; 3. define procedure that runs the test given number of times, as specified by parameter.
(define (fast-prime? n times)
  (cond ((= times 0) #t) ;; if test runs 0 times, then n is prime
        ((fermat-test n) (fast-prime? n (- times 1))) ;; does fermat test, if true, then runs fast-prime test again until n times. if all n times, fermat test returns true, then we have confidence that n is most probably prime.
        (else #f)))

(define (fast-timed-prime-test n)
  (newline)
  (fast-start-prime-test n (runtime)))

(define (fast-start-prime-test n start-time)
  (if (fast-prime? n 20)
      (report-prime (- (runtime) start-time))
      (display "")
      ))

(define (fast-search-iter lower upper count counter)
  (cond ((= counter 3) (display ""))
        ((even? count)
         (fast-search-iter lower upper (+ count 1) counter))
        ((fast-prime? count 20)
         (newline)
         (display count)
         (fast-timed-prime-test count)
         (fast-search-iter lower upper (+ count 1) (+ counter 1)))
        (else
         (fast-search-iter lower upper (+ count 1) counter))))


(define (fast-search-for-primes lower upper)
  (fast-search-iter lower upper lower 0))

;; Tests
(fast-search-for-primes 1001 10000)
(newline)
(fast-search-for-primes 10001 100000)
(newline)
(fast-search-for-primes 100001 1000000)
(newline)
(fast-search-for-primes 1000001 10000000)
(newline)

;; Output
;1009
;***0.034423828125
;1013
;***0.0419921875
;1019
;***0.034423828125

;10007
;***0.04052734375
;10009
;***0.039794921875
;10037
;***0.042236328125

;100003
;***0.046875
;100019
;***0.05029296875
;100043
;***0.051025390625

;1000003
;***0.053466796875
;1000033
;***0.053466796875
;1000037
;***0.056396484375

(define (my-fermat-test n)
  (define (my-fermat-test-iter a n)
    (cond ((= a 1) #t)
          ((= (expmod a n n) a) (my-fermat-test-iter (- a 1) n))
          (else #f)))
  (my-fermat-test-iter (- n 1) n))