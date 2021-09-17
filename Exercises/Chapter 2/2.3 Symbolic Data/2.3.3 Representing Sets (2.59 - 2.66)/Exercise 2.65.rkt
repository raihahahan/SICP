#lang scheme
;; Tree Procedures
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))

(define (make-tree entry left right)
  (list entry left right))

;; Ordered list procedures
(define (union-set s1 s2)
  (cond
    ((and (null? s1) (null? s2)) '())
    ((and (null? s1) (not (null? s2))) s2)
    ((and (null? s2) (not (null? s1))) s1)
    ((< (car s1) (car s2)) (cons (car s1) (union-set (cdr s1) s2)))
    ((= (car s1) (car s2)) (cons (car s1) (union-set (cdr s1) (cdr s2))))
    (else (union-set (cdr s1) s2))))

(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()    
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1
                     (intersection-set (cdr set1)
                                       (cdr set2))))
              ((< x1 x2)
               (intersection-set (cdr set1) set2))
              ((< x2 x1)
               (intersection-set set1 (cdr set2)))))))

;; Exercise 2.65

;; O(n) implementation of converting a binary tree into a list
(define (tree->list tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))


;; O(n) implementation of converting an ordered list into a binary search tree
(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts)
                                              right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-elts))))))))

;; Method 1:
; 1. Convert both binary trees into ordered lists ==> O(n)
; 2. Union/Intersect the 2 ordered lists from exercise 2.62 ==> O(n)
; 3. Convert the ordered list into a binary tree ==> O(n)
; Total: O(n)

(define (tree->list->tree tree1 tree2 p)
  (let ((list1 (tree->list tree1))
        (list2 (tree->list tree2)))
    (let ((mod-list (p list1 list2)))
      (list->tree mod-list))))

(define (union-tree tree1 tree2)
  (tree->list->tree tree1 tree2 union-set))

(define (intersection-tree tree1 tree2)
  (tree->list->tree tree1 tree2 intersection-set))

; Test
(define tree1 (make-tree
               7
               (make-tree 3
                          (make-tree 1 '() '())
                          (make-tree 5 '() '()))
               (make-tree 9
                          '()
                          (make-tree 11 '() '()))))

;     7
;   /   \
;  3     9
; / \     \
;1   5     11

(define tree2 (make-tree
               6
               (make-tree 4
                          (make-tree 2 '() '())
                          (make-tree 5 '() '()))
               (make-tree 10
                          (make-tree 8
                                     '()
                                     (make-tree 9 '() '()))
                          (make-tree 12 '() '()))))

;      6
;    /   \
;   4     10
;  / \    / \
; 2  5   8   12
;         \
;          9
                           
(union-tree tree1 tree2)
;'(6 (2 (1 () ()) (4 () (5 () ()))) (9 (8 () ()) (10 () (12 () ()))))

;         6
;       /   \
;      2     9
;     / \    / \
;    1  4   8   10
;        \        \
;         5       12

(intersection-tree tree1 tree2)
; (5 () (9 () ()))

;        5
;         \
;          9

;; TODO: implement such procedures for binary trees without using converting the tree to list to tree