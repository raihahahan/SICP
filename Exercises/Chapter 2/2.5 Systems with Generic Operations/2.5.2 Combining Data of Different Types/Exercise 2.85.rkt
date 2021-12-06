#lang scheme
;; Exercise 2.85
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
      (if proc ;; if procedure is found,
          (apply proc (map contents args)) ;; (map contents args) returns a list of the contents of each of the arguments. apply will then apply the procedure on each of these contents.
          (error
           ("No method for these types -- APPLY-GENERIC"
            (list op type-tags)))))))

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

(define (install-coercion-package-project)
  (define (complex->real complex)
    (attach-tag 'real (real-part complex)))
  (define (real->int real)
    (attach-tag 'integer (round real)))
  (define (rat->int real)
    (attach-tag 'integer (round (numer real) / (denom real))))

  (put-coercion 'complex 'real complex->real)
  (put-coercion 'real 'int real->int)
  (put-coercion 'rat 'int rat->int))

(install-coercion-package-project)

(define (install-project-package)
  (define (complex->real complex)
    (get-coercion 'complex 'real))
  (define (real->int real)
    (get-coercion 'real 'int))
  (define (rat->int rat)
    (get-coercion 'rat 'int))

  (put 'project 'complex complex->real)
  (put 'project 'real real->int)
  (put 'project 'rat rat->int))

(install-raise-package)
(install-project-package)

(define tower '(integer rational real complex))

(define (raise type)
  (apply-generic 'raise type))

(define (project type)
  (apply-generic 'project type))

;; START

;; Description:
; Put coercion procedures for complex->real, real->int, rat->int.
; In arithmetic data table, put 'project procedures by getting procedures from coercion table. This abstraction barrier allows us to modify the implementation of the coercion without further modifying the arithmetic table.
; isLowerExist checks if num can be lowered.
; drop will check isLowerExist first, and if true, then it will drop the number down the type.

;; From exercise 2.79. For this exercise, I'll only focus on the new lowering procedures. Hence, I will not be implementing equ? procedure

(define (equ? x y)
  (apply-generic 'equ? x y))

(define (isLowerExist num)
  (let ((type (type-tag num))
        (content (contents num)))
    (let ((lowered ((project type) content)))
      (let ((newType (type lowered)))
        (let ((raisedBack ((raise newType) lowered)))
          (let ((old (contents lowered))
                (new (contents raisedBack)))
            (equ? old new)))))))

(define (drop num)
  (if (isLowerExist num)
      ((project (type-tag num)) num)
      num))
      
      
    