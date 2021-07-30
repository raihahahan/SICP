#lang racket/base
(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))
(list 1 (list 2 (list 3 4)))

;; Exercise 2.25
(define one (list 1 3 (list 5 7) 9))
(car (cdr (car (cdr (cdr one))))) ; 7

(define two (list (list 7)))
(car (car two)) ; 7

(define three (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))
(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr three))))))))))))

;; Exercise 2.26
(define x (list 1 2 3))
(define y (list 4 5 6))

(define (append l1 l2)
  (if (null? l1)
      l2
      (cons (car l1) (append (cdr l1) l2))))

; 1. (1 2 3 4 5 6)
; 2. ((1 2 3) 4 5 6))
; 3. ((1 2 3) (4 5 6))

;; Draw box and pointer diagram for clearer illustration

(append x y)
(cons x y)
(list x y)

;; Exercise 2.27
(define (reverse-1 l)
  (if (null? l)
      l
      (append (reverse-1 (cdr l)) (list (car l)))))

(define (deep-reverse l)
  ;; base cases:
  ;; 1. empty list: return l
  ;; 2. all elements in l are not sequences: return (reverse-1 l)
  ;; Recursion: append deep-reverse of (cdr l) with list of deep-reverse of (car l)
  (cond ((null? l) l)
        ((not (any_seq? l)) (reverse-1 l))
        (else (append (deep-reverse (cdr l)) (list (deep-reverse (car l)))))))

(define (list-length l)
  ;; takes as argument a list l
  ;; returns the length of list
  (if (null? l)
      0
      (+ 1 (list-length (cdr l)))))

(define (any_seq? l)
  ;; Helper function to check if any elements in list l is a sequence
  ;; returns boolean value
  (cond
    ((null? l) #f)
    ((pair? (car l)) #t)
    ((not (pair? (car l))) (any_seq? (cdr l)))))

(define (map-1 p l) ; p === procedure passed to each element of list
  (if (null? l)
      '()
      (cons (p (car l))
            (map-1 p (cdr l)))))

(define (deep-reverse-1 l)
  (if (pair? l)
      (reverse (map deep-reverse-1 l))
      l))

(define z (list (list 1 2) (list 3 4)))
(define z1 (list (list (list 1 2) (list 3 4)) (list 5 6)))
z1

(display "**********")
(newline)
z1
(reverse-1 z1)
(deep-reverse z1)
(deep-reverse-1 z1)

;; End of exercise 2.27

;; Exercise 2.28

(define (fringe l)
  ;; takes as argument a tree represented as a list
  ;; returns a list whose elements are all the leaves of the tree arranged in left to right order
  ;; Workings:
  ; Base case(s):
  ; 1. no elements in list is a sequence: return list
  ; 2. empty list: return list
  ; Else: append the fringe of (car l) to fringe of (cdr l)
  (cond ((null? l) l)
        ((not (any_seq? l)) l)
        ((not (pair? (car l))) (append (list (car l)) (fringe (cdr l))))
        (else (append (fringe (car l)) (fringe (cdr l))))))

;; Tests
(define tree-1 (list (list 1 2) (list 3 4)))
(define tree-2 (list (list 1 2 (list 3 4 5)) (list 6 7) 8 9))
(define tree-3 (list 1 2 3 (list 4 5 6 7) (list 8 (list 9 (list 10 11 (list 12)))) 13))
(define tree-4 (list '()))
(define tree-5 (list 1 2))

(display "*********")
(newline)
(fringe tree-1)
(fringe tree-2)
(fringe tree-3)
(fringe tree-4)
(fringe tree-5)

;; Output
;*********
;'(1 2 3 4)
;'(1 2 3 4 5 6 7 8 9)
;'(1 2 3 4 5 6 7 8 9 10 11 12 13)
;'(())
;'(1 2)

;; End of Exercise 2.28

;; Exercise 2.29
(define (make-mobile left right)
  (list left right))

(define (make-branch len structure)
  (list len structure))

;; a. create selectors for left-branch and right-branch and branch-length and branch-structure
(define (get-left-branch mobile)
  ;; takes as argument a mobile of type list
  ;; returns a mobile of type list or number
  (car mobile))

(define (get-right-branch mobile)
  (car (cdr mobile)))

(define (get-length branch)
  ;; takes as argument the branch of a mobile
  ;; returns length
  (car branch))


(define (branch-structure mobile)
  (car (cdr mobile)))


;; Tests
(define test-left (make-branch 10 (list 1 2 3)))
(define test-right (make-branch 12 (list 12 3)))
(define test-branch (make-mobile test-left test-right))
test-left
(get-left-branch test-branch)
test-right
(get-right-branch test-branch)

;; Output
;'(10 (1 2 3))
;'(10 (1 2 3))
;'(12 (12 3))
;'(12 (12 3))


;; b. define total-weight: returns total weight of mobile
;; Helper functions
(define (is-mobile? branch)
  ;; takes as argument a branch of a mobile
  ;; returns true if structure of branch is mobile
  ;; else false
  (pair? (branch-structure branch)))

(define (get-weight non-mobile-branch)
  ;; get weight of a non-mobile branch
  (car (cdr non-mobile-branch)))

(define (accumulate l)
  ;; takes as argument a flat list
  ;; returns the sum of values of the elements in the list
  (if (null? l)
      0
      (+ (car l) (accumulate (cdr l)))))
      
;; Method 1:
; 1. define a helper function called accumulator which iterates through a flat list and returns the total value of the leafs
; 2. define a helper function which takes a mobile as argument and returns a list of only the weights
;; Workings:
; 1. if both left and right branch are not mobile, then return list of weight of left and weight of right
; 2. if only left is not mobile, then append list of weight of left to the right branch
(define (total-weight-1 mobile)
  (define (weight-list mobile)
    (let ((left (get-left-branch mobile))
          (right (get-right-branch mobile)))
      (let ((l-mobile (is-mobile? left))
            (r-mobile (is-mobile? right)))
        (cond ((and (not l-mobile) (not r-mobile)) (list (get-weight left) (get-weight right))) ;; both not mobile
              ((and l-mobile r-mobile) (append (weight-list left) (weight-list right))) ;; both mobile
              ((not l-mobile) (append (list (get-weight left)) (weight-list right))) ;; only right mobile
              ((not r-mobile) (append (list (get-weight right)) (weight-list left))))))) ;; only left mobile
  (accumulate (weight-list mobile)))

;; Method 2:
; 1. Base case: if both left and right branch are not mobile, then return sum of weight of left and right
; 2. if both mobile, then return sum of total-weight left and total-weight right
; 3. if left/right mobile while right/left not mobile, then return sum of total-weight left/right + weight of right/left
(define (total-weight-2 mobile)
  (let ((left (get-left-branch mobile))
        (right (get-right-branch mobile)))
    (let ((l-mobile (is-mobile? left))
          (r-mobile (is-mobile? right)))
  (cond ((and (not l-mobile) (not r-mobile)) (+ (get-weight left) (get-weight right))) ;; both not mobile
        ((and l-mobile r-mobile) (+ (total-weight-2 left) (total-weight-2 right))) ;; both mobile
        ((not l-mobile) (+ (get-weight left) (total-weight-2 right))) ;; only right mobile
        ((not r-mobile) (+ (get-weight right) (total-weight-2 left))))))) ;; only left mobile

;; Tests:
(define left-1 (make-branch 10 20))
(define right-1 (make-branch 10 30))
(define mobile-1 (make-mobile left-1 right-1))

(define left-2 (make-branch 10 40))
(define right-2 (make-branch 10 50))
(define mobile-2 (make-mobile left-2 right-2))

(define left-3 mobile-1)
(define right-3 mobile-2)
(define mobile-3 (make-mobile left-3 right-3))

;; Method 1:
(total-weight-1 mobile-1)
(total-weight-1 mobile-2)
(total-weight-1 mobile-3)
; Output:
;50
;90
;140

;; Method 2:
(total-weight-2 mobile-1)
(total-weight-2 mobile-2)
(total-weight-2 mobile-3)
; Output:
;50
;90
;140

; c. balanced <= torque applied by top-left branch == torque applied by top-right branch
(define (torque branch)
  ;; takes as argument the torque of a non-mobile-branch
  ;; returns torque
  (let ((s (get-length branch))
        (F (get-weight branch)))
    (* F s)))

;; Method 1:
(define (is-balanced? mobile)
  (let ((left (get-left-branch mobile))
        (right (get-right-branch mobile)))
    (let ((l-mobile (is-mobile? left))
          (r-mobile (is-mobile? right)))
  (cond ((and (not l-mobile) (not r-mobile)) (= (torque left) (torque right))) ;; both not mobile
        ((and l-mobile r-mobile) (and (is-balanced? left) (is-balanced? right))) ;; both mobile
        ((not l-mobile) (and (is-balanced? right) (= (* (total-weight-2 right) (get-length right)) (torque left)))) ;; only right mobile
        ((not r-mobile) (and (is-balanced? left) (= (* (total-weight-2 left) (get-length left)) (torque right)))))))) ;; only left mobile


;; Method 2:
; define torque of an entire mobile branch
; check if torque of left == torque of right

(define (recur-torque branch)
  (if (not (is-mobile? branch))
      (* (get-length branch) (get-weight branch))
      (+ (recur-torque (get-left-branch branch))
         (recur-torque (get-right-branch branch)))))

(define (is-balanced-2? mobile)
  (= (recur-torque (get-left-branch mobile)) (recur-torque (get-right-branch mobile))))

;; Tests:
(define left-4 (make-branch 10 10))
(define right-4 (make-branch 5 20))
(define mobile-4 (make-mobile left-4 right-4))

(define left-5 (make-branch 20 5))
(define right-5 (make-branch 25 4))
(define mobile-5 (make-mobile left-5 right-5))

(define left-6 mobile-4)
(define right-6 mobile-5)
(define mobile-6 (make-mobile left-6 right-6))

;;Method 1:
(is-balanced? mobile-1)
(is-balanced? mobile-2)
(is-balanced? mobile-3)
(is-balanced? mobile-4)
(is-balanced? mobile-5)
(is-balanced? mobile-6)

;; Output:
;#f
;#f
;#f
;#t
;#t
;#t

;; Method 2:
(is-balanced-2? mobile-1)
(is-balanced-2? mobile-2)
(is-balanced-2? mobile-3)
(is-balanced-2? mobile-4)
(is-balanced-2? mobile-5)
(is-balanced-2? mobile-6)

;; Output:
;#f
;#f
;#f
;#t
;#t
;#t
      
;; d. change representation of mobiles
(define (make-mobile-2 left right)
  (cons left right))

(define (make-branch-2 len structure)
  (cons len structure))

;;; left-branch, right-branch, branch-length, branch-structure
(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (cdr mobile))

(define (branch-len branch)
  (car branch))

(define (branch-struct branch)
  (cdr branch))

;;; total-weight
(define (new-is-mobile? branch)
  (pair? (branch-struct branch)))

(define (new-get-weight branch)
  ;; takes as argument a non-mobile branch
  (branch-struct branch))

(define (total-weight-3 mobile)
  (let ((left (left-branch mobile))
        (right (right-branch mobile)))
    (let ((l-mobile (new-is-mobile? left))
          (r-mobile (new-is-mobile? right)))
      (cond ((and (not l-mobile) (not r-mobile)) (+ (new-get-weight left) (new-get-weight right))) ;; both not mobile
        ((and l-mobile r-mobile) (+ (total-weight-3 left) (total-weight-3 right))) ;; both mobile
        ((not l-mobile) (+ (new-get-weight left) (total-weight-3 right))) ;; only right mobile
        ((not r-mobile) (+ (new-get-weight right) (total-weight-3 left))))))) ;; only left mobile


;;; balanced check
(define (recur-torque-1 branch)
  (if (not (new-is-mobile? branch))
      (* (branch-len branch) (new-get-weight branch))
      (+ (recur-torque-1 (left-branch branch))
         (recur-torque-1 (right-branch branch)))))

(define (is-balanced-3? mobile)
  (= (recur-torque-1 (left-branch mobile)) (recur-torque-1 (right-branch mobile))))
      
;; Tests:
(define left-7 (make-branch-2 10 20))
(define right-7 (make-branch-2 10 30))
(define mobile-7 (make-mobile-2 left-7 right-7))

(define left-8 (make-branch-2 10 40))
(define right-8 (make-branch-2 10 50))
(define mobile-8 (make-mobile-2 left-8 right-8))

(define left-9 mobile-7)
(define right-9 mobile-8)
(define mobile-9 (make-mobile-2 left-9 right-9))

(define left-10 (make-branch-2 10 10))
(define right-10 (make-branch-2 5 20))
(define mobile-10 (make-mobile-2 left-10 right-10))

(define left-11 (make-branch-2 20 5))
(define right-11 (make-branch-2 25 4))
(define mobile-11 (make-mobile-2 left-11 right-11))

(define left-12 mobile-10)
(define right-12 mobile-11)
(define mobile-12 (make-mobile-2 left-12 right-12))

(total-weight-3 mobile-7)
(total-weight-3 mobile-8)
(total-weight-3 mobile-9)
(total-weight-3 mobile-10)
(total-weight-3 mobile-11)
(total-weight-3 mobile-12)

;; Output:
;50
;90
;140
;30
;9
;39

(is-balanced-3? mobile-7)
(is-balanced-3? mobile-8)
(is-balanced-3? mobile-9)
(is-balanced-3? mobile-10)
(is-balanced-3? mobile-11)
(is-balanced-3? mobile-12)

;; Output:
;#f
;#f
;#f
;#t
;#t
;#t