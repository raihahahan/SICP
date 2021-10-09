#lang racket
;;; HUFFMAN ENCODING TREES ;;;
;; REDO ENCODING: previous version was too complicated
;;; Representing Huffman Encoding Trees ;;;

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

;;; Decoding procedure ;;;
(define (decode bits tree)
  ;; takes as args a list of 0's and 1's, with a Huffman tree
  (define (decode-1 bits current-branch)
    ;; takes 2 args: list of remaining bits and current position in the tree
    ;; keeps moving down the tree, choosing left or right branch according to the bit value (0 or 1 respectively)
    ;; when reaches a leaf, it returns the symbol at the leaf as the next symbol in the message and cons it with the result of decoding the rest of the message starting back from the root of the tree 
    ;; else, if not a leaf, then go down the next-branch and decode the remaining bits
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "Bad bit -- CHOOSE-BRANCH" bit))))

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
                    (make-leaf (cdr pairs))))))


;; EXERCISE 2.68
(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (encode-symbol symbol tree)
  (cond
    ((null? tree) '())
    ((memq symbol (symbols (left-branch tree)))
         (if (leaf? (left-branch tree))
             (list '0)
             (append (list '0) (encode-symbol symbol (left-branch tree)))))
    ((memq symbol (symbols (right-branch tree)))
         (if (leaf? (right-branch tree))
             (list '1)
             (append (list '1) (encode-symbol symbol (right-branch tree)))))
    (else (error "SYMBOL NOT FOUND" symbol))))


;; TEST 
(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree (make-leaf 'D 1)
                                   (make-leaf 'C 1)))))

(encode '(A D A B B C A) sample-tree)
;; '(0 1 1 0 0 1 0 1 0 1 1 1 0)
         