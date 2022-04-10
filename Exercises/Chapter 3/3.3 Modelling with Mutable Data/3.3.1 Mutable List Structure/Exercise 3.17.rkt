#lang sicp
(define a (list 'a 'b 'c))
; |1|*|->|2|*|->|3|/|


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


; 4. never return
(define (make-cycle x)
  (define (last-pair x)
    (if (null? (cdr x))
        x
        (last-pair (cdr x))))
  (set-cdr! (last-pair x) x)
  x
  )

(define f (make-cycle (list 1 2 3)))
; |1|*|->|2|*|->|3|*|-
;  ^                 |
;  |----------------|

(define (mark-pair pair)
  (set! pair (cons 'm pair))
  pair)

(define (mark pair)
  (set-car! pair 'm))

(define (count-pairs x)
  (cond ((not (pair? x)) 0)
        ((eq? (car x) 'm) 0)
        (else
         (let ((curr_car (car x))
               (curr_cdr (cdr x)))
           (mark x)
           (+ (count-pairs curr_car)
              (count-pairs curr_cdr)
              1)))))


(count-pairs a) ;3
(count-pairs z) ;3
(count-pairs e) ;3
(count-pairs f) ;3

  