#lang racket/base
;; Exercise 2.32

;; For the tests below to work, just remove the comment out list and cons

;; List
;(define (make-mobile left right)
 ; (list left right))
;(define (make-branch length structure)
  ;(list length structure))
;(define (left-branch mobile)
 ; (car mobile))
;(define (right-branch mobile)
 ; (car (cdr mobile)))
;(define (branch-length branch)
 ; (car branch))
;(define (branch-structure branch)
;  (car (cdr branch)))

;; Cons                  
(define (make-mobile left right)
  (cons left right))
(define (make-branch length structure)
  (cons length structure))
(define (left-branch mobile)
  (car mobile))
(define (right-branch mobile)
  (cdr mobile))
(define (branch-length branch)
  (car branch))
(define (branch-structure branch)
  (cdr branch))

;; total-weight
(define (total-weight mobile)
  ;; total-weight = total-weight-left + total-weight-right
  ;; base case 1: mobile null => weight == 0
  ;; base case 2: mobile is not a pair => weight == weight of branch
  (cond ((null? mobile) 0)
        ((not (pair? mobile)) mobile)
        (else (+ (total-weight (branch-structure (left-branch mobile)))
                 (total-weight (branch-structure (right-branch mobile)))))))

;; torque
(define (torque branch)
  (* (total-weight (branch-structure branch)) (branch-length branch)))

;; balanced?
(define (balanced? mobile)
  ;; balanced === torque-left = torque-right
  (= (torque (left-branch mobile)) (torque (right-branch mobile))))
;; Test
(define mobile-1 (make-mobile (make-branch 2 3) (make-branch 2 3)))
(define mobile-2 (make-mobile (make-branch 3 4) (make-branch 3 4)))
(define mobile-3 (make-mobile (make-branch 3 mobile-1) (make-branch 4 mobile-2)))

(total-weight mobile-1)
(total-weight mobile-2)
(total-weight mobile-3)
(balanced? mobile-1)
(balanced? mobile-2)
(balanced? mobile-3)

;; List
;6
;8
;14
;#t
;#t
;#f
;>

;; Cons
;6
;8
;14
;#t
;#t
;#f
;> 
  