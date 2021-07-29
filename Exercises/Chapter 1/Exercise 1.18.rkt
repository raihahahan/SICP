#lang racket/base
#lang racket/base
(require racket/trace)

;; Exercise 1.18

(define (mul-iter a product count)
  (cond ((= count 0) a)
        ((even? count) (mul-iter a (* 2 product) (/ count 2)))
        (else (mul-iter (+ a product) product (- count 1)))))

(define (even? x)
  (= (remainder x 2) 0))

(define (mul b n)
  (mul-iter 0 b n))

;; Test
(trace mul-iter)
(mul 13 238999)

;; Output
;>(mul-iter 0 13 238999)
;>(mul-iter 13 13 238998)
;>(mul-iter 13 26 119499)
;>(mul-iter 39 26 119498)
;>(mul-iter 39 52 59749)
;>(mul-iter 91 52 59748)
;>(mul-iter 91 104 29874)
;>(mul-iter 91 208 14937)
;>(mul-iter 299 208 14936)
;>(mul-iter 299 416 7468)
;>(mul-iter 299 832 3734)
;>(mul-iter 299 1664 1867)
;>(mul-iter 1963 1664 1866)
;>(mul-iter 1963 3328 933)
;>(mul-iter 5291 3328 932)
;>(mul-iter 5291 6656 466)
;>(mul-iter 5291 13312 233)
;>(mul-iter 18603 13312 232)
;>(mul-iter 18603 26624 116)
;>(mul-iter 18603 53248 58)
;>(mul-iter 18603 106496 29)
;>(mul-iter 125099 106496 28)
;>(mul-iter 125099 212992 14)
;>(mul-iter 125099 425984 7)
;>(mul-iter 551083 425984 6)
;>(mul-iter 551083 851968 3)
;>(mul-iter 1403051 851968 2)
;>(mul-iter 1403051 1703936 1)
;>(mul-iter 3106987 1703936 0)
;<3106987
;3106987
