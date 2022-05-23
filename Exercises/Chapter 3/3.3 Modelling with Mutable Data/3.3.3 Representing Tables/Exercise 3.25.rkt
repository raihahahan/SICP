#lang sicp

(define (assoc key records)
  (cond ((null? records) #f)
        ((equal? key (caar records)) (car records))
        (else (assoc key (cdr records)))))

(define (make-table)
  (let ((local-table (list '*table*)))
    
    (define (lookup keys)
      (define (internal-lookup keys table)
         (if (null? keys)
          #f
          (let ((subtable
                 (assoc (car keys) (cdr table))))
            (if subtable
                (if (not (pair? (cdr subtable)))
                    (cdr subtable)
                    (internal-lookup (cdr keys) subtable))
                #f))))
      (internal-lookup keys local-table))

    (define (insert! keys value)
      
      (define (make-subtable keys)
        (if (null? (cdr keys))
            (cons (car keys) value)
            (cons (list (car keys)) (make-subtable (cdr keys)))))
      
      (define (internal-insert! keys table)
        (if (null? (cdr keys))
            (set-cdr! table
                      (cons (cons (car keys) value)
                            (cdr table)))
            (let ((subtable
                   (assoc (car keys) (cdr table))))
              (if subtable
                  (internal-insert! (cdr keys) subtable)
                  (set-cdr! table
                            (cons (make-subtable keys)
                                  (cdr table)))))))
      
      (internal-insert! keys local-table)
      local-table)
                                 
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            ((eq? m 'print-table) local-table)
            (else (error "Unknown operation: TABLE" m))))
    dispatch))
  
(define a (make-table))
(define get (a 'lookup-proc))
(define put (a 'insert-proc!))

(a 'print-table)
(get (list 'a))
(put (list 'a 'b 'c 'd) 'e)
(get (list 'a 'b 'c 'd))
(get (list 'j))