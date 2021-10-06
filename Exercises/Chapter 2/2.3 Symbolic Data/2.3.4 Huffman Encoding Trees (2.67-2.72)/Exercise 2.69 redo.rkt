#lang racket
;; Exercise 2.69

;; STRATEGY (with inspiration from scheme wiki)
; 1. (make-leaf-set) returns a list of leaf ordered from lowest to highest frequency.
; 2. Merge the first item (car pairs), which is the lowest frequency of the list, with the first item of the cdr of the pairs. This way, we are merging the two lowest frequency leaves.
; 3. Next, rearrange the leaves to reflect the new frequencies of each leaf after the merge in 2.
; 4. Continue this until the (cdr pairs) is null, where we have already merged all the leaves.


; Constructors and selectors for individual nodes
(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define (leaf? object)
  (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

; Constructors and selectors for tree
(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))
(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))
(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))
(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

;;; Sets of weighted elements ;;;
(define (adjoin-set x set)
  ;; to construct an ordered set
  ;; argument set is an ordered set, x is a node to be added into the set
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set)) ;; this implies that x has the lowest weight and we simply add x to the start of the set
        (else (cons (car set) ;; else, (car set) has lowest weight 
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  ;; argument pairs is a list of symbol-frequency pairs
  ;; constructs an initial ordered set of leaves ready to be merged according to Huffman algorithm
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair) ;; symbol
                               (cadr pair)) ;; frequency
                    (make-leaf-set (cdr pairs))))))

;; ALGORITHM 

(define (successive-merge pairs)
  (if (null? (cdr pairs))
      (car pairs)
      (successive-merge (adjoin-set
                         (make-code-tree (car pairs) (cadr pairs))
                         (cddr pairs)))))

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

;;; TEST ;;;

(define l1 (list 'A 8))
(define l2 (list 'B 3))
(define l3 (list 'C 1))
(define l4 (list 'D 1))
(define l5 (list 'E 1))
(define l6 (list 'F 1))
(define l7 (list 'G 1))
(define l8 (list 'H 1))

(define set-pairs (list l1 l2 l3 l4 l5 l6 l7 l8))

(generate-huffman-tree set-pairs)
;;'((leaf A 8) ((((leaf H 1) (leaf G 1) (H G) 2) ((leaf F 1) (leaf E 1) (F E) 2) (H G F E) 4) (((leaf D 1) (leaf C 1) (D C) 2) (leaf B 3) (D C B) 5) (H G F E D C B) 9) (A H G F E D C B) 17)
        
;; CODE BREAKDOWN
;; (adjoin-set (make-code-tree (car ordered) (cadr ordered)) (cddr ordered)) ::::: (adjoin-set x set)
;; x is a node to be added into set
;; set is an ordered set.
;; (make-code-tree (car ordered) (cadr ordered)) is the merger of the lowest frequency element with the second lowest frequency element.
;; We then want to merge this merged node with the remaining elements of the ordered set, and retrieve an ordered set overall.
;; Hence, we use adjoin-set to sort the elements accordingly.
;; (cddr ordered) is the rest of the sorted elements not involved in the merge.
;; This new ordered set will be the new argument in successive merge.
;; The base case is when the lowest frequency element is the only element in the list, and we return this element.
