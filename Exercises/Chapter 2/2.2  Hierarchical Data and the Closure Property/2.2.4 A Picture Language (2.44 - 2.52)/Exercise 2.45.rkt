#lang sicp
;; Exercise 2.45
(#%require sicp-pict)
(define wave einstein)

(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
          (bottom (beside (bl painter) (br painter))))
      (below bottom top))))

;; START

;; 1st solution
(define (split op1 op2)
  ;; returns a procedure which accepts two arguments
  ;; args: n (number), painter (painter)
  (define (iter m p)
    (if (= m 0)
        p
        (let ((smaller (iter (- m 1) p)))
          (op1 p (op2 smaller smaller)))))
  (lambda (n painter)
    (iter n painter)))

;; 2nd solution
(define (split-1 op1 op2)
  ;; returns a procedure which accepts two arguments
  ;; args: n (number), painter (painter)
  (lambda (n painter)
    (if (= n 0)
        painter
        (let ((smaller ((split-1 op1 op2) (- n 1) painter)))
          (op1 painter (op2 smaller smaller))))))

(define r-split (split beside below))
(define r-split-1 (split-1 beside below))

(paint (r-split 5 wave))
(paint (r-split-1 5 wave))



  

        
               