#lang racket
;; HUFFMAN ENCODING ALGORITHM ;;
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
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set)) 
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair) 
                               (cadr pair))
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

;; ENCODING ALGORITHM
(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (encode-symbol symbol tree)
  (cond
    ((null? tree) (error "SYMBOL NOT FOUND" symbol))
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




