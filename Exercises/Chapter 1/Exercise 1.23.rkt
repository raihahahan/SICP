#lang racket/base
(require racket/trace)
(define (runtime) (current-inexact-milliseconds))

(define (square x)
  (* x x))

(define (find-divisor n test-divisor)
    (cond ((> (square test-divisor) n) n)
          ((= (remainder n test-divisor) 0) test-divisor)
          (else (find-divisor n (+ test-divisor 1)))))

(define (prime? n)
  ;; takes as argument a number n
  ;; returns #t if n is prime, else #f
  (define (smallest-divisor n)
    (find-divisor n 2))
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

;; exercise 1.23
(define (next input)
  (if (= input 2)
      3
      (+ input 2)))

(define (new-smallest-divisor n)
  (new-find-divisor n 2))

(define (new-find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (new-find-divisor n (next test-divisor)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (new-timed-prime-test n)
  (newline)
  (new-start-prime-test n (runtime)))

(define (new-start-prime-test n start-time)
  (if (new-prime? n)
      (report-prime (- (runtime) start-time))
      (display "")
      ))

(define (new-prime? n)
  (cond ((< n 2) #f)
        ((= (new-smallest-divisor n) n) #t)
        (else #f)))


;; Tests

;; Uncomment 4 lines below to debug find-divisor and new-find-divisor procedures
;;(trace find-divisor) 
;;(trace new-find-divisor)
;;(find-divisor 1009 2)
;;(new-find-divisor 1009 2)


;1009
;1013
;1019

;10007
;10009
;10037

;1000003
;1000033
;1000037

(display "New timed prime test")
(newline)
(new-timed-prime-test 1009)
(new-timed-prime-test 1013)
(new-timed-prime-test 1019)
(new-timed-prime-test 10007)
(new-timed-prime-test 10009)
(new-timed-prime-test 10037)
(new-timed-prime-test 100003)
(new-timed-prime-test 100019)
(new-timed-prime-test 100043)
(newline)
(display "************************")
(newline)
(newline)
(display "Old timed prime test")
(newline)
(timed-prime-test 1009)
(timed-prime-test 1013)
(timed-prime-test 1019)
(timed-prime-test 10007)
(timed-prime-test 10009)
(timed-prime-test 10037)
(timed-prime-test 100003)
(timed-prime-test 100019)

;; Output

;New timed prime test

;***0.00390625
;***0.003173828125
;***0.002685546875
;***0.0048828125
;***0.0048828125
;***0.005126953125
;***0.012939453125
;***0.012939453125
;***0.012451171875
;************************

;Old timed prime test

;***0.00634765625
;***0.002685546875
;***0.001953125
;***0.005859375
;***0.00537109375
;***0.005859375
;***0.0166015625
;***0.016357421875