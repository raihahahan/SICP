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
        (let ((left-result (partial-tree elts left-size))) ;; constructs a partial tree of the size of the left side of the tree with elements from the elts list
          (let ((left-tree (car left-result)) ;; left-branch of final tree
                (non-left-elts (cdr left-result)) ;; rest of the left partial tree elements
                (right-size (- n (+ left-size 1)))) ;; computes the size (no. of elements) of the right side of the tree
            (let ((this-entry (car non-left-elts)) ;; returns the entry of the final tree.
                  (right-result (partial-tree (cdr non-left-elts) ;; constructs a partial tree of the size of the right side of the tree with elements from the elts list
                                              right-size)))
              (let ((right-tree (car right-result)) ;; right-branch of final tree
                    (remaining-elts (cdr right-result))) ;; other elements not in the final tree
                (cons (make-tree this-entry left-tree right-tree) ;; finally, merge both the left subtree and right subtree to form the entire tree. the remaining elements that do not manage to be in the tree are in the remaining-elts list
                      remaining-elts))))))))

;; (partial-tree elts n) breakdown (not part of answer paragraph):
;; For a given integer n, we provide the argument elts of type list with at least n arguments. This returns an object pair with the car being the first n elements as an n-element tree, and cdr being the remaining elements. Hence, the base case is when n == 0, where the procedure simply returns an object pair containing an empty tree, with the remaining elements of the elts list.
;; partial-tree breaks down the problem into 2 subproblems: the left partial tree and right partial tree. Once the partial subtrees are constructed, the procedure merges both subtrees into a single tree, and cons it together with the list of remaining elements which do not fit into the tree.
;; left-size is just the size of n-1 divided by 2 represented as an integer. This size is then used in the construction of the left partial-tree, as seen in the declaration of the local variable called left-result. left-result applies the partial-tree again recursively, which is the smaller subproblem stated above.
;; left-tree is simply the car of left-result. Note that partial-tree is just a data obect containing the tree and excess elements. Hence, it makes sense to retrieve the left tree by applying a car on the left partial-tree.
;; non-left-elts is the excess elements of the left partial-tree. This is because the left-partial tree was constructed by applying the procedure partial-tree with the same list of elements, but the size of the left side of the tree (to be precise, half). Similarly, right-size is just the computation of the right side of the tree, which is the total original length minus the left-size of the tree.
;; this-entry returns the entry of the final tree. This is because when we constructed the partial left tree, we did not take into account the entry of the final tree. Instead, we let the remaining elements be both the entry and the right partial tree. Hence, this-entry is the car of non-left-elts, while right-result is the partial-tree with the cdr of no-left-elts as the list to be converted into a tree, and right size is just the size of the right partial-tree.
;; Next, right-tree can then be constructed by taking car of right-result, and remaining-elts is the excess elements from the right-partial tree which do not fit into the right-tree.
;; Finally, we have reached the merging state. As we have already constructed the entire left subtree, entry and right subtree, together with the excess elements that do not fit into the entire tree, we can simply make a tree by merging the left subtree, right subtree and entry together. Next, we just cons this tree with the remaining elements list and we are done.

;; a) FINAL EXPLANATION:
; partial-tree creates two subtrees (left and right) and one root-node, together with a list of remaining elements not being able to be part of the tree.
; The procedure starts by computing the size of the left partial-tree in order to create a left partial-tree recursively using the same list of elements but the size of the left partial-tree as arguments.
; The procedure then constructs the root node and right partial-tree by applying the partial-tree procedure for the remaining elements, using the similar approach as above.
; For each of the two steps above, the sub-tree is created by applying car (for left tree) and cdr (for right-tree) on the returned value of the partial-tree.
; Finally, the procedure merges the left sub-tree, root node and right sub-tree together to form an entire tree.
; The end result is a balanced binary search tree as the list is sorted.

(list->tree '(1 3 5 7 9 11))
;; (5 (1 () (3 () ())) (9 (7 () ()) (11 () ())))

;;           5
;         /     \
;        1      9
;         \    / \
;         3   7  11

;; b) Assume that the list and pair operations have growth of O(1). (eg. cons, list, cdr, car).

;; The calling tree of a list of size n is as shown below. This includes the empty trees.
;         n
;        / \
;     n/2   n/2
;     / \   / \
;  n/4 n/4 n/4 n/4

;; The final pairing operation, make-tree which has order of growth of O(1), is invoked as many times as the number of nodes in the tree, including the empty tree.
;; Let Sn be the number of nodes as a function of the integer n.
; S1 = 1 = 2(1) -1
; S2 = 3 = 2(2) - 1
; S3 = 7 = 2(3) - 1

; Hence, Sn = 2n - 1.
; Order of growth is O(n)