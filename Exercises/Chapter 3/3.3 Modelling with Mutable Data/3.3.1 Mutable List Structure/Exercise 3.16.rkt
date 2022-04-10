#lang sicp
(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))

; 1. 3 pairs
(define a (list 'a 'b 'c))
; |1|*|->|2|*|->|3|/|

(count-pairs a) ; 3

; 2. 4 pairs
(define x (cons 3 '()))
(define y (cons x x))
(define z (cons y '()))
; z->|*|/|
;      |
;      V
;    |*|*|
;      |
;      V
;    |3|/|

(count-pairs z) ; 4

; 3. 7 pairs
(define c (cons 1 2))
(define d (cons c c))
(define e (cons d d))
; e->|*|*|
;    | |
;    V V
;   |*|*|
;    | |
;    V V
;   |1|2|

(count-pairs e) ; 7

; 4. never return
(define (make-cycle x)
  (define (last-pair x)
    (if (null? (cdr x))
        x
        (last-pair (cdr x))))
  (set-cdr! (last-pair x) x))

(define f (make-cycle a))
; |1|*|->|2|*|->|3|*|-
;  ^                 |
;  |----------------|
  
