#lang sicp

;; Assume that the keys are ordered numerically.
;; For any given pair of a list of keys and a value, the list of keys have to be converted into a binary tree.
;; In lookup procedure, lookup 

(define (make-table same-key?)
  (let ((local-table '()))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; HELPERS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    (define (get-key record)
      (car record))

    (define (get-value record)
      (cdr record))

    (define (get-record node)
      (entry node))
    

     ;;;;;;;;;;;;;;; TREE PROCEDURES, CONSTRUCTORS, SELECTORS ;;;;;;;;;;;;;;;

    ;; (list (cons key value) left-branch right-branch))
    
    (define (entry tree) (car tree)) 
    (define (left-branch tree) (cadr tree)) 
    (define (right-branch tree) (caddr tree)) 
  
    (define (make-tree entry left right) 
      (list entry left right)) 
  
    (define (adjoin-set x set) 
      (cond ((null? set) (make-tree x '() '())) 
            ((= (car x) (car (entry set))) set) 
            ((< (car x) (car (entry set))) 
             (make-tree (entry set) 
                        (adjoin-set x (left-branch set)) 
                        (right-branch set))) 
            ((> (car x) (car (entry set))) 
             (make-tree (entry set) 
                        (left-branch set) 
                        (adjoin-set x (right-branch set))))))
    
     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
    (define (lookup key)
      (define (iter key records)
        (cond ((null? records) #f)
              ((= key (car (entry records))) (entry records))
              ((< key (car (entry records))) (iter key (left-branch records)))
              ((> key (car (entry records))) (iter key (right-branch records)))))
        (iter key local-table))
                
    
    (define (insert! key value) 
      (let ((record (lookup key)))
        (if record
            (set-cdr! record value)
            (set! local-table (adjoin-set (cons key value) local-table)))))
        
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) (lambda (key) (if (lookup key) (cdr (lookup key)) #f)))
            ((eq? m 'insert-proc!) insert!)
            ((eq? m 'print-table) local-table)
            (else (error "Unknown operation: TABLE" m))))
    dispatch))

(define a (make-table equal?))
(define get (a 'lookup-proc))
(define put (a 'insert-proc!))

(put 1 'a)
(a 'print-table)
(get 1)
(put 2 'b)
(put 4 'c)
(put 3 'd)
(put 3 'e)
(get 3)
(put 10 'g)
(get 10)
(put 43 'a)
(put 42 'b)
(put 41 'c)
(put 67 'z)
(put 88 'e)
(get 88)
(a 'print-table)
(get 392)

;((1 . a) () ())
;a
;e
;g
;e
;((1 . a) () ((2 . b) () ((4 . c) ((3 . e) () ()) ((10 . g) () ((43 . a) ((42 . b) ((41 . c) () ()) ()) ((67 . z) () ((88 . e) () ())))))))
;#f