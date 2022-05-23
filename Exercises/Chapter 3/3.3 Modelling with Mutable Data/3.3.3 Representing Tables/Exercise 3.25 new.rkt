#lang sicp

(define (make-table same-key?)
  (let ((local-table (list '*table*)))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; HELPERS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    (define (get-key table) (caaar table)) 
    (define (get-value item)  
      (if (pair? (car item)) 
          (if (null? (cdar item)) 
              #f 
              (cdar item)) 
          #f)) 
    (define (change-value item value) (set-cdr! (car item) value)) 
    (define (add-subtable table key value) (set-cdr! table  
                                                     (cons (cons (cons key value) 
                                                                 '()) 
                                                           (cdr table))))
    (define (is-subtable record)
      (pair? (cdr record)))
    
     (define (assoc key table) 
         (cond ((null? table) #f)  
               ((equal? key (get-key table)) (car table)) 
               (else (assoc key (cdr table)))))
    
     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
    (define (lookup key-list)
      (define (iter keys subtable)
        (if (null? keys) ;; if no more keys and still searching, then item is not inside
            #f
            (let ((record (assoc (car keys) (cdr subtable))))
              (if record
                  (if (is-subtable record) ;; if record is a subtable
                      (iter (cdr keys) record) ;; then search in that subtable
                      (get-value record)) ;; else, it is a record. then get its value
                  #f))))
        (iter key-list local-table))
    
    (define (insert! keys value) 
      (define (iter keys value subtable) 
        (let ((record (assoc (car keys) (cdr subtable))))
          (if (null? (cdr keys)) ;; we are at the last key of the list
              (if record
                  (change-value record value) ;; if record exist, change the value
                  (add-subtable subtable (car keys) value)) ;; else add subtable with record
              (if record
                  (iter (cdr keys) value record) ;; if record exists, then insert value into record
                  (begin (add-subtable subtable (car keys) '()) ;; else create a new subtable and insert into that subtable
                         (iter (cdr keys) value (cadr subtable)))))))
      (iter keys value local-table))
        
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            ((eq? m 'print-table) local-table)
            (else (error "Unknown operation: TABLE" m))))
    dispatch))

(define a (make-table equal?))
(define get (a 'lookup-proc))
(define put (a 'insert-proc!))
(define state (a 'print-table))

(put (list 1 2) 'k1)
(get (list 1))
(put (list 2 4 5) 'k3)
(get (list 2 4 5))
(put (list 'a 'b 'c 'd) 'e)
(get (list 'a 'b))
(get (list 'a 'b 'c 'd))
state

;#f
;k3
;#f
;e
;(*table* ((a) ((b) ((c) ((d . e))))) ((2) ((4) ((5 . k3)))) ((1) ((2 . k1))))
