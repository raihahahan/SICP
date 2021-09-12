#lang scheme
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))

(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((= x (entry set)) true)
        ((< x (entry set))
         (element-of-set? x (left-branch set)))
        ((> x (entry set))
         (element-of-set? x (right-branch set)))))

(define (adjoin-set x set)
  (cond ((null? set) (make-tree '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree (entry set)
                    (adjoin-set x (left-branch set))
                    (right-branch set)))
        ((> x (entry set))
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin-set x (right-branch set))))))

;; Exercise 2.64
(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2))) ;; computes the size (no. of elements) of the left side of the tree
        (let ((left-result (partial-tree elts left-size))) ;; constructs a partial tree of the size of the left side of the tree but with elements from the elts list
          (let ((left-tree (car left-result)) ;; returns a local variable called left-tree which is the car of the above partial tree
                (non-left-elts (cdr left-result)) ;; rest of the partial tree elements
                (right-size (- n (+ left-size 1)))) ;; computes the size (no. of elements) of the right side of the tree
            (let ((this-entry (car non-left-elts)) ;; 
                  (right-result (partial-tree (cdr non-left-elts)
                                              right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-elts))))))))


;; Definition of quotient:
; What / does is totally unlike what the usual // or quotient operator does. In most Lisp systems, quotient behaves like / except when dividing integers, in which case it behaves like truncate of two arguments; this behavior is mathematically intractable, leading to such anomalies as
; (quotient 1.0 2.0) => 0.5   but   (quotient 1 2) => 0
;; In practice quotient is used only when one is sure that both arguments are integers, or when one is sure that at least one argument is a floating-point number. / is tractable for its purpose and works for any numbers --> https://www.cs.cmu.edu/Groups/AI/html/cltl/clm/node125.html

;; (partial-tree elts n) breakdown:
;; For a given integer n, we provide the argument elts of type list with at least n arguments. This returns an object pair with the car being the first n elements and cdr the remaining elements. Hence, the base case is when n == 0, where the procedure simply returns an object pair containing an empty tree, with the remaining elements of the elts list.
;;

