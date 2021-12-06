#lang scheme
;; Helper list functions
(define (find item list)  ;; returns true if item is in list, else false.
  (cond ((null? list) #f)
        ((equal? item (car list)) #t)
        (else
         (find item (cdr list)))))

(define (index elem list)  ;; returns false if elem is not in list. else, returns the index of elem in list. list is indexed at 0.
  (define (iter ls result)
    (cond ((null? ls) #f)
          ((equal? elem (car ls)) result)
          (else
           (iter (cdr ls) (+ result 1)))))
  (if (find elem list)
      (iter list 0)
      #f))

(define (higher a b list)  ;; returns the element that is higher in the list. if one does not exist, then returns false.
  (let ((i (index a list))
        (j (index b list)))
    (if (and i j)
        (if (>= i j) a b)
        #f)))

(define (lower a b list)
  (let ((i (index a list))
        (j (index b list)))
    (if (and i j)
        (if (< i j) a b)
        #f)))


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


; complex
;   ^
;   |
; real
;   ^
;   |
; rational
;   ^
;   |
; integer

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args))) ;; type-tags is a list containing the type tags of each of the arguments passed
    (let ((proc (get op type-tags))) ;; lookup into the table using get, the operation, and the list of type tags
      (let ((higher-type (higher (car type-tags) (cadr type-tags))) ;; the type of the argument with higher type
            (lower-type (lower (car type-tags) (cadr type-tags))))  ;; the type of the argument with lower type
        (let ((raised-lower (raise-tower lower-type higher-type arithmetic-tower)))  ;; the argument with lower type after being raised to a higher type
        (if proc ;; if procedure is found,
            (apply proc (map contents args)) ;; (map contents args) returns a list of the contents of each of the arguments. apply will then apply the procedure on each of these contents.
            (if (>= (length args) 2)
                (if raised-lower ;; if raised lower exists
                    (apply-generic op raised-lower higher-type)  ;; then call apply-generic with two arguments of the same type
                    (error
                     ("No method for these types -- APPLY-GENERIC"
                      (list op type-tags))))
                (error
                     ("No method for these types -- APPLY-GENERIC"
                      (list op type-tags))))))))))

(define (install-coercion-package)
  (define (int->rat int)
    (attach-tag 'rational (make-rat int 1)))
  (define (rat->real rat)
    (attach-tag 'real (/ (numer rat) (denom rat))))
  (define (real->complex real)
    (attach-tag 'complex (make-complex-from-real-imag real 0)))
  (put-coercion 'integer 'rational int->rat)
  (put-coercion 'rational 'real rat->real)
  (put-coercion 'real 'complex real->complex)
  'done)

(install-coercion-package)

(define (install-raise-package)
  (define (int->rat int)
    (get-coercion 'integer 'rational))
  (define (rat->real rat)
    (get-coercion 'rational 'real))
  (define (real->complex real)
    (get-coercion 'real 'complex))
  
  (put 'raise 'integer int->rat)
  (put 'raise 'rational rat->real)
  (put 'raise 'real real->complex)
  'done)

(install-raise-package)

(define arithmetic-tower '(integer rational real complex))

(define (raise-tower type-from type-to tower) ;; raises the type of argument until it reaches intended type
  (let ((shortened (memq type-from tower)))  ;; cuts the tower into shorter tower where type-from is the new lowest level
    (define (iter tp-from twr)
      (cond ((null? twr) #f)  ;; if tower is empty, return false
            ((equal? tp-from type-to) tp-from)  ;; if our type reaches the intended type, then return that type
            (iter (raise tp-from) (cdr twr)))) ;; else, return the iteration with tp-from raised to it's higher level, and we further shorten the tower
    (if shortened
        (iter type-from shortened)
        #f)))
;; the use of the tower argument above is to let us check if we have reached the highest possible type level. it also makes it easier for us to manage the hierarchy of types.
;; without the tower argument, we'll have to make a manual checker to check if there are any possible higher levels, possibly through the use of get-coercion.
             
(define (raise type)
  (apply-generic 'raise type))


