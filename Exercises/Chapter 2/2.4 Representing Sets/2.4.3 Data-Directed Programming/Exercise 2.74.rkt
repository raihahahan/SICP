#lang scheme
;; Exercise 2.74

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; GET and PUT procedures ;; Credits: https://stackoverflow.com/questions/5499005/how-do-i-get-the-functions-put-and-get-in-sicp-scheme-exercise-2-78-and-on
(define global-array '())

(define (make-entry k v) (list k v))
(define (key entry) (car entry))
(define (value entry) (cadr entry))

(define (put op type item)
  (define (put-helper k array)
    (cond ((null? array) (list(make-entry k item)))
          ((equal? (key (car array)) k) array)
          (else (cons (car array) (put-helper k (cdr array))))))
  (set! global-array (put-helper (list op type) global-array)))

(define (get op type)
  (define (get-helper k array)
    (cond ((null? array) #f)
          ((equal? (key (car array)) k) (value (car array)))
          (else (get-helper k (cdr array)))))
  (get-helper (list op type) global-array))

(define (attach-tag type-tag content) (cons type-tag content)) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;________________________________________________________________________
;                          DIVISION FILES
;________________________________________________________________________
;             |             |              Division (Type)
;             |             |--------------------------------------------
;             |             |    'd1     |       'd2       |     'd3
;========================================================================
; Operation   | 'get-record |            |                 |   
;________________________________________________________________________

;________________________________________________________________________
;                          EMPLOYEE RECORDS
;________________________________________________________________________
;             |              |              Record (Type)
;             |              |--------------------------------------------
;             |              |    'r1     |       'r2       |     'r3
;========================================================================
; Operation   | 'get-salary  |            |                 |   
;             ___________________________________________________________
;             | 'get-address |            |                 |   
;________________________________________________________________________


;; Each of the set of records are structured differently for each division.

;; Hence, a data-directed programming table is used to apply a generic get-record procedure.

; Each get-record procedure takes in as argument the employee-id. This is because the set of records are said to be keyed on employee's name. It returs the employee record, tagged with the division name. This is because the set of records are structured differently. Hence, for each division, different procedures are used for get-salary, get-address etc.

; a.
(define (get-record employee-id division)
  (let ((proc (get 'get-record division)))
    (attach-tag division (proc employee-id))))

; b.
(define (get-employee-data record key)
  (let ((record-type (car record))
        (record-contents (cdr record)))
    ((get key record-type) record-contents)))

(define (get-salary record)
  (get-employee-data record 'salary))

; c.
(define (find-employee-record employee-id div-ls)
  (cond ((null? div-ls) #f)
        ((get-record employee-id (car div-ls))
         (get-record employee-id (car div-ls)))
        (else
         (find-employee-record employee-id (cdr div-ls)))))

; d.
;; Install each employee records into the Employee Records Table
;; Install their personnel file into the division table.
       
       
  
  
          


  




