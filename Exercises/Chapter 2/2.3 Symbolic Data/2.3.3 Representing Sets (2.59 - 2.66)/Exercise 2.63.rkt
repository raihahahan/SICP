#lang racket
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

;; START
(define (tree->list-1 tree)
  ;; appends left-branch-converted-into-list with right-branch-converted-into-list
  ;; recursive process
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1 (right-branch tree))))))

(define (tree->list-2 tree)
  ;; Tail recursive
  ;; 2 states:
  ;;   1. tree
  ;;   2. result-list
  ;; tree -> lb -> lb*lb -> lb*lb*lb...*lb -> null
  ;; argument tree tends to null.
  ;; For the first application of copy-to-list, tree traverses to the left branch until it reaches null.
  ;; The result-list of the above application is the cons of the entry of tree and the second application of copy-to-list with args right branch and result-list
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))  
  (copy-to-list tree '()))


;; RESULTS

;; a. After conducting a few written tests, the end result of both procedures are the same. The rough explanations of how each procedure works are stated above.

;; From community scheme wiki: Both procedures produce the same result because they both are in-order traversals.
;; credits: http://community.schemewiki.org/?sicp-ex-2.63

;; TEST
(define tree1
  (make-tree 7
             (make-tree 3
                        (make-tree 1 '() '())
                        (make-tree 5 '() '()))
             (make-tree 9
                        '()
                        (make-tree 11 '() '()))))

(tree->list-1 tree1)
(tree->list-2 tree1)
;; '(1 3 5 7 9 11)
;; '(1 3 5 7 9 11)

(define tree2
  (make-tree 7
             (make-tree 3
                        (make-tree 1 '() '())
                        (make-tree 5 '() '()))
             (make-tree 9
                        '()
                        (make-tree 11 '() '()))))

(tree->list-1 tree2)
(tree->list-2 tree2)

;; '(1 3 5 7 9 11)
;; '(1 3 5 7 9 11)

(define tree3
  (make-tree 3
             (make-tree 1 '() '())
             (make-tree 7
                        (make-tree 5 '() '())
                        (make-tree 9
                                   '()
                                   (make-tree 11 '() '())))))

(tree->list-1 tree3)
(tree->list-2 tree3)

;; '(1 3 5 7 9 11)
;; '(1 3 5 7 9 11)

(define tree4
  (make-tree 5
             (make-tree 3
                        (make-tree 1 '() '())
                        '())
             (make-tree 9
                        (make-tree 7 '() '())
                        (make-tree 11 '() '()))))

(tree->list-1 tree4)
(tree->list-2 tree4)

;; '(1 3 5 7 9 11)
;; '(1 3 5 7 9 11)

;; b.

;; tree->list-1
; This procedure applies itself twice, one for left-branch and the other for right-branch.
; This is a divide and conquer algorithm. We are dividing the procedure into two sub-procedures. As the binary tree is balanced, the average number of steps required grows to log(n).
; However, for each of the log(n) steps, it takes linear time to apply append. Hence, it grows to n*log(n);
; Time complexity: O(n*log(n);
; Space complexity: O(n);

;; tree->list-2
; This procedure handles one node from the left and right branches each until the base case of tree == NULL is reached.
; Time complexity: O(n);  
; Space complexity: O(n);