#lang racket
(require racket/trace)
;; Exercise 2.69

;; STRATEGY ;;
; Use iteration with two states:
; (merge-iter pairs result)
; result is the tree after the pairs have been merged
; pairs is the list of ordered leaves which are not yet merged
; If pairs is empty and the tree length is not 1, then combine both result and pairs together again and repeat iteration.
; Pairs must always be ordered. This is to ensure that the smallest-difference procedure works.
; When pairs is empty and the tree length is 1, then return result.

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

;; Helper functions specific to successive-merge
(define (full-list pair result)
  ;; returns the full list of the pairs and result after combining them
  (leaves->pairs (append pair (new-set result))))

(define (find-smallest-full list)
  ;; returns the smallest difference in the list between 2 numbers
  (define (last list)
    (cadr list))      
  (let ((numbers (map last list)))
    (define (smallest-diff-num num smallest)
      (cond
        ((null? num) smallest)
        ((null? (cdr num)) smallest)
        ((> (abs (- (car num) (cadr num))) smallest) (smallest-diff-num (cdr num) smallest))
        (else (smallest-diff-num (cdr num) (abs (- (car num) (cadr num)))))))
    (smallest-diff-num numbers (abs (- (car numbers) (cadr numbers)))))) 

(define (new-set pairs)
  ;; to transform the result state into a representation which can be used for (leaves->pairs) procedure
  (define (transform pairs)
    (define (iter result)
      (if (= (length result) 2)
          result
          (iter (cdr result))))
    (iter pairs))
  (make-leaf-set (map transform pairs)))

(define (result->pairs pairs)
  ;; transforms the result argument into pairs
  (define (iter result)
    (if (= (length result) 2)
        result
        (iter (cdr result))))
  (iter pairs))

(define (leaves->pairs leave-set)
  ;; transforms leaves into pairs
  (define (iter item)
    (cdr item))
  (map iter leave-set))


;;; PROCEDURES ;;; 

(define (successive-merge pairs)
  (define (merge-iter pairs result)
    (let ((diff
           (cond ((= (length pairs) 1) (+ (find-smallest-full (full-list pairs result)) 1)) ;; if length of pairs is 1, then let the difference be greater than the smallest difference so that we can go to the last conditional statement indicated by (*)
                 ((null? pairs) 0) ;; if pairs is empty, then let the difference be any arbitrary number as we will always go to the first conditional flow below indicated by (**)
                 (else (abs (- (weight (car pairs)) (weight (cadr pairs))))))) ;; else, diff will be the actual difference between 2 adjacent numbers
          (smallest
           (cond ((null? pairs) 0) ;; if pairs is empty, then let smallest be any arbitrary number as we will always reach (**)
                 (else (find-smallest-full (full-list pairs result)))))) ;; else, smallest will be the actual smallest difference between 2 adjacent numbers
      (cond
        ((and (= (length result) 1) (null? pairs)) result) ;; final result (explained above in strategy) ------ (**)
        ((= diff smallest) (merge-iter (cddr pairs) (cons (make-code-tree (car pairs) (cadr pairs)) result))) ;; if diff = smallest, then merge into tree
        ((> diff smallest) (merge-iter (make-leaf-set (append (map result->pairs result) (leaves->pairs pairs))) '()))))) ;; ------ (*)
  (trace merge-iter) ;; for debugging
  (merge-iter pairs '())) ;; tail recursive process

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
(define ordered (make-leaf-set set-pairs))
(generate-huffman-tree set-pairs)

;; '(((leaf A 8) (leaf (((H G) (F E)) ((D C) B)) 9) (A (((H G) (F E)) ((D C) B))) 17))