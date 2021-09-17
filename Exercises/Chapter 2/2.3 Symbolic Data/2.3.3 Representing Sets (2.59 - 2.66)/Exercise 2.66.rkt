#lang racket
;; Exercise 2.66
; Strategy:
;  To implement binary tree data structure for testing:
;   - Implement an ordered linked list containing records represented as object pairs (key record).
;   - Convert the ordered list into a binary tree using Exercise 2.65
;  Implement a binary search tree procedure to lookup for an element with order of growth of O(logn)
; Assumption(s):
;   - List is ordered


;; Constructors and selectors for a single record
(define (make-record record key)
  (cons key record))
(define (key record)
  (car record))
(define (value record)
  (cdr record))

;; Constructors and selectors for a binary tree
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))

(define (make-tree entry left right)
  (list entry left right))

;; Procedures for a binary tree
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

;; O(logn) implementation of searching a binary search tree
(define (search-tree tree _key)
  (cond ((null? tree) #f)
        ((= _key (key (entry tree))) (value (entry tree)))
        ((< _key (key (entry tree))) (search-tree (left-branch tree) _key))
        (else (search-tree (right-branch tree) _key))))

(define (lookup set-of-records my-key)
  (search-tree set-of-records my-key))

; Test

;; Tree implementation
(define record1 (cons 1 "foo"))
(define record2 (cons 3 "bar"))
(define record3 (cons 5 "test"))
(define record4 (cons 7 "math"))
(define record5 (cons 9 "sicp"))
(define record6 (cons 13 "racket"))
(define record7 (cons 21 "scheme"))
(define record8 (cons 31 "lisp"))
(define record9 (cons 78 "python"))
(define record10 (cons 89 "js"))

(define my-records-set
         (list record1
               record2
               record3
               record4
               record5
               record6
               record7
               record8
               record9
               record10))

(define records-tree
  (list->tree my-records-set))

;; Search test
(lookup records-tree 1)
(lookup records-tree 3)
(lookup records-tree 5)
(lookup records-tree 7)
(lookup records-tree 9)
(lookup records-tree 13)
(lookup records-tree 21)
(lookup records-tree 31)
(lookup records-tree 78)
(lookup records-tree 89)

;; "foo"
;; "bar"
;; "test"
;; "math"
;; "sicp"
;; "racket"
;; "scheme"
;; "lisp"
;; "python"
;; "js"

(lookup records-tree 190)
(lookup records-tree 12)
;; #f
;; #f