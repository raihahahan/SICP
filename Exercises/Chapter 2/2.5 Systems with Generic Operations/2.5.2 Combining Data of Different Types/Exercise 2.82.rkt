#lang scheme
;; Exercise 2.82

;; GET and PUT procedures
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

(define global-array1 '())

(define (make-entry1 k v) (list k v))
(define (key1 entry) (car entry))
(define (value1 entry) (cadr entry))

(define (put-coercion op type item)
  (define (put-helper1 k array)
    (cond ((null? array) (list (make-entry1 k item)))
          ((equal? (key1 (car array)) k) array)
          (else (cons (car array) (put-helper1 k (cdr array))))))
  (set! global-array1 (put-helper1 (list op type) global-array1)))

(define (get-coercion op type)
  (define (get-helper1 k array)
    (cond ((null? array) #f)
          ((equal? (key1 (car array)) k) (value1 (car array)))
          (else (get-helper1 k (cdr array)))))
  (get-helper1 (list op type) global-array1))

;; Tagged Data
(define (attach-tag type-tag contents)
  (if (number? contents)
      contents  
      (cons type-tag contents)))
(define (type-tag datum)
  (cond ((number? datum) 'scheme-number)
        ((pair? datum) (car datum))
        (else
         (error "Bad tagged datum -- TYPE-TAG" datum))))
(define (contents datum)
  (cond ((number? datum) datum)
        ((pair? datum) (cdr datum))
        (else
         (error "Bad tagged datum -- CONTENTS" datum))))


(define (find item list)
  (cond ((null? list) #f)
        ((equal? item (car list)) #t)
        (else
         (find item (cdr list)))))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (define (iter level list-of-types)
        (if (null? list-of-types) ;; if after tyring to coerce each type until we reach null still doesn't work, then return an error.s
            (error "No method for these types")
            (let ((coerced (map (lambda (a) (get-coercion a level)) type-tags))) ;; coerced is a list where each element of the type-tags list is raised to the type of the level argument. level is the current type which we are coercing to at that iteration
              (if (find #f coerced) ;; if any of (get-coercion) in the above mapping returns a false,
                  (iter (cadr list-of-types) (cdr list-of-types)) ;; then we repeat the iteration. the new level will be (cadr list-of-types), and the new list to keep track of is (cdr list-of-types).
                  coerced)))) ;; else, we return the coerced list.
      (if proc
          (apply proc (map contents args)) ;; if proc exists, then simply apply proc to the argument contents
          (if (>= (length args) 2)
              (apply (lambda (a) (apply-generic op a)) (iter (car type-tags) type-tags)) ;; else, we return (apply-generic op a), where a is the coerced list above.
              (error "No method for these types")))))) ;; else, we return an error

;; Edge cases which will not work:
; TODO