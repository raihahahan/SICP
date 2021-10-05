#lang racket
(require racket/trace)
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
;; START
;; Helper functions
(define (good-enough? pairs)
  (= (length pairs) 1))

(define (merge leaf1 leaf2)
  (let ((l1 (make-leaf (car leaf1) (cadr leaf1)))
        (l2 (make-leaf (car leaf2) (cadr leaf2))))
    (let ((s1 (symbol-leaf l1))
          (s2 (symbol-leaf l2))
          (w1 (weight-leaf l1))
          (w2 (weight-leaf l2)))
      (let ((weight (+ w1 w2)))
        (cond ((and (pair? s1) (pair? s2))
               (make-leaf (append s1 s2) weight))
              ((and (pair? s1) (not (pair? s2)))
               (make-leaf (append s1 (list s2)) weight))
              ((and (pair? s2) (not (pair? s1)))
               (make-leaf (append s2 (list s1)) weight))
              (else
               (make-leaf (list s1 s2) weight)))))))

(define (to-merge pairs)
  (define (iter diff pair)
    (cond
      ((null? pair) '())
      ((null? (cadr pair)) (iter (+ diff 1) pairs))
      ((= (length pair) 2)
       (cons (list (symbol-leaf (car pair)) (weight-leaf (car pair)))
             (list (list (symbol-leaf (cadr pair)) (weight-leaf (cadr pair))))))
      ((= (abs (- (weight-leaf (car pair))
                  (weight-leaf (cadr pair))))
          diff) (cons (list (symbol-leaf (car pair)) (weight-leaf (car pair)))
                      (list (list (symbol-leaf (cadr pair)) (weight-leaf (cadr pair))))))
      (else
       (iter diff (cdr pair)))))
  (iter 0 pairs))

(define (complement pairs excl)
  (let ((s1 (caar excl))
        (s2 (caadr excl)))
  (cond
    ((null? pairs) '())
    ((equal? (symbol-leaf (car pairs)) s1) (cddr pairs))
    (else (cons (car pairs) (complement (cdr pairs) excl))))))


(define (successive-merge pairs)
  (cond ((null? pairs) pairs)
        ((= (length pairs) 2) (make-leaf '() 0))
        (else
         (let ((p (to-merge pairs))) ;; cons pair containing two (symbol-frequency) pairs to be merged
           (let ((new-pairs (complement pairs p))) ;; list of (symbol-frequency) pairs that do not contain the two pairs above;
             (let ((new-merge (to-merge (complement new-pairs (to-merge new-pairs))))
                   (new-complement (complement new-pairs (to-merge new-pairs))))
               (let ((merge-1 (to-merge new-complement)))
                 ;;new-complement
                 ;(length (complement new-complement merge-1))
                 (make-code-tree (successive-merge new-pairs) (merge (car p) (cadr p)))
                 )))))))

         

(define (generate-huffman-tree pairs)
  (trace successive-merge)
  (successive-merge (make-leaf-set pairs)))

(define l1 (list 'A 8))
(define l2 (list 'B 3))
(define l3 (list 'C 1))
(define l4 (list 'D 1))
(define l5 (list 'E 1))
(define l6 (list 'F 1))
(define l7 (list 'G 1))
(define l8 (list 'H 1))

(define set-pairs (list l1 l2 l3 l4 l5 l6 l7 l8))
;;(make-leaf (car (car (to-merge set-pairs))) (cadr (car (to-merge set-pairs))))
;;(complement set-pairs (to-merge set-pairs))
;;(trace generate-huffman-tree)
(generate-huffman-tree set-pairs)