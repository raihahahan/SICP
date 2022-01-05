#lang scheme
;; Exercise 2.85

;; Helper math procedures
(define (square x)
  (* x x))

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
  (cond ((number? datum)
         (if (exact? datum) 'integer 'real))   
        ((pair? datum) (car datum))
        (else
         (error "Bad tagged datum -- TYPE-TAG" datum))))
(define (contents datum)
  (cond ((number? datum) datum)
        ((pair? datum) (cdr datum))
        (else
         (error "Bad tagged datum -- CONTENTS" datum))))

;; Generic arithmetic procedures
(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

;;************************ COMPLEX NUMBERS SYSTEM ************************;;

;;; RECTANGULAR
(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a) 
    (cons (* r (cos a)) (* r (sin a))))

  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular 
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular 
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'SUCCESS---RECTANGULAR-PACKAGE)

;;; POLAR
(define (install-polar-package)
  ;; internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y) 
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar 
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'SUCCESS--POLAR-PACKAGE)

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

(install-rectangular-package)
(install-polar-package)

;;************************ ORDINARY PRIMITIVE NUMBERS ************************;;
(define (install-scheme-number-package)
  (define (equ? x y)
    (equal? x y))
  (define (=zero? x)
    (= x 0))
                   
  (put 'add '(real real)
       (lambda (x y) (+ x y)))
  (put 'add '(integer integer)
       (lambda (x y) (+ x y)))
  (put 'sub '(real real)
       (lambda (x y) (- x y)))
   (put 'sub '(integer integer)
       (lambda (x y) (- x y)))
  (put 'mul '(real real)
       (lambda (x y) (* x y)))
  (put 'mul '(integer integer)
       (lambda (x y) (* x y)))
  (put 'div '(real real)
       (lambda (x y) (/ x y)))
  (put 'div '(integer integer)
       (lambda (x y) (/ x y)))
  (put 'make 'scheme-number
       (lambda (x) x))
  (put 'equ? '(real real) equ?)
  (put 'equ? '(integer integer) equ?)
  (put '=zero? '(scheme-number) =zero?)
  'SUCCESS---PRIMITIVE-NUMBERS)

; Constructor for (tagged) ordinary numbers
(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))


;;************************ RATIONAL PACKAGE ************************;;
(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))
  (define (equ? x y)
    (equal? x y))
  (define (=zero? x)
    (= (numer x) 0))
  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))

  ;; interface rational numbers selectors and to rest of system
  (put 'numer 'rational numer)
  (put 'denom 'rational denom)
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  
  (put 'equ? '(rational rational) equ?)
  (put '=zero? '(rational) =zero?)
  'SUCCESS---RATIONAL-NUMBERS)

; Constructor for (tagged) rational numbers
(define (make-rational n d)
  ((get 'make 'rational) n d))

;;************************ TRIGO PACKAGE ************************;;
(define (install-trigo-package)
  (define (tag to-attach x)
    (attach-tag to-attach x))

  (define (numer x)
    ((get 'numer 'rational) x))

  (define (denom x)
    ((get 'denom 'rational) x))
  
  (define (sine-ordinary x)
    (sin x))

  (define (sine-rational x)
    (let ((numerator  (numer x))
          (denominator (denom x)))
      (sin (/ numerator denominator))))

  (define (cosine-ordinary x)
    (cos x))

  (define (cosine-rational x)
    (let ((numerator  (numer x))
          (denominator (denom x)))
      (cos (/ numerator denominator))))

  (put 'sine 'scheme-number sine-ordinary)
  (put 'sine 'rational sine-rational)
  (put 'cosine 'scheme-number cosine-ordinary)
  (put 'cosine 'rational cosine-rational)

  'SUCCESS--TRIGO-PACKAGE)

(install-trigo-package)

(define (sine x) (apply-generic 'sine x))
(define (cosine x) (apply-generic 'cosine x))

;lll
  
  ;; TODO : TRIGO PACKAGE

;;************************ COMPLEX PACKAGE ************************;;
(define (install-complex-package)
  ;; imported procedures from rectangular and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))
  ;; internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag (add (real-part z1) (real-part z2))
                         (add (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (sub (real-part z1) (real-part z2))
                         (sub (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (mul (magnitude z1) (magnitude z2))
                       (add (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (div (magnitude z1) (magnitude z2))
                       (sub (angle z1) (angle z2))))
  (define (equ? x y)
    (let ((type-x (type-tag x))
          (type-y (type-tag y)))
      (let ((mag-diff (abs (- (magnitude x)
                              (magnitude y)))))
        (if (not (equal? type-x type-y))
            (<= mag-diff 0.1)
            (if (equal? type-x 'rectangular)
                (and (equal? (* 1.0 (real-part x)) (* 1.0 (real-part y)))
                     (equal? (* 1.0 (imag-part x)) (* 1.0 (imag-part y))))
                (equal? x y))))))

  (define (=zero? x)
    (<= (magnitude x) 0.0001))
        
    ;; interface to rest of the system
  (define (tag z) (attach-tag 'complex z))
  
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)
  
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  (put 'equ? '(complex complex) equ?)
  (put '=zero? '(complex) =zero?)
  'SUCCESS---COMPLEX-PACKAGE)

; Constructor for (tagged) complex numbers
(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

;; Install packages
(install-scheme-number-package)
(install-rational-package)
(install-complex-package)

;; Equality procedure
(define (equ? x y)
  (apply-generic 'equ? x y))

;;************************ COERCION ************************;;

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

;;************************ COERCION TABLE: RAISE ************************;;
(define (install-coercion-package-raise)
  (define numer (get 'numer 'rational))
  (define denom (get 'denom 'rational))
  
  (define (int->rat int)
    (make-rational (contents int) 1))
  (define (rat->real rat)
    (attach-tag 'real (* (/ (numer (contents rat)) (denom (contents rat))) 1.0)))
  (define (real->complex real)
    (make-complex-from-real-imag (contents real) 0))
  
  (put-coercion 'integer 'rational int->rat)
  (put-coercion 'rational 'real rat->real)
  (put-coercion 'real 'complex real->complex)
  'SUCCESS---RAISE-PACKAGE)

(install-coercion-package-raise)

;;************************ INSTALL RAISE PROCEDURES INTO RELEVANT ARITHMETIC INDICES ************************;;
(define (install-raise-package)
  (define (int->rat int)
    (get-coercion 'integer 'rational))
  (define (rat->real rat)
    (get-coercion 'rational 'real))
  (define (real->complex real)
    (get-coercion 'real 'complex))
  
  (put 'raise '(integer) int->rat)
  (put 'raise '(rational) rat->real)
  (put 'raise '(real) real->complex)
  'SUCCESS---INSTALLED-RAISED-PROCEDURES)

;;************************ COERCION TABLE: PROJECT ************************;;
(define (install-coercion-package-project)
  (define numer (get 'numer 'rational))
  (define denom (get 'denom 'rational))

  (define (complex->real complex)
    (attach-tag 'real (* (real-part (contents complex)) 1.0)))
  (define (real->rat real)
    (make-rational (round (contents real)) 1))
  (define (rat->int rat)
    (attach-tag 'integer (round (/ (numer (contents rat)) (denom (contents rat))))))

  (put-coercion 'complex 'real complex->real)
  (put-coercion 'real 'rat real->rat)
  (put-coercion 'rat 'int rat->int)
  'SUCCESS---PROJECT-PACKAGE)

(install-coercion-package-project)

;;************************ INSTALL PROJECT PROCEDURES INTO ARITHMETIC INDICES ************************;;
(define (install-project-package)
  (define (complex->real complex)
    (get-coercion 'complex 'real))
  (define (real->rat real)
    (get-coercion 'real 'rat))
  (define (rat->int rat)
    (get-coercion 'rat 'int))

  (put 'project '(complex) complex->real)
  (put 'project '(real) real->rat)
  (put 'project '(rational) rat->int)

  'SUCCESS---INSTALLED-PROJECT-PROCEDURES)


(install-raise-package)
(install-project-package)

;; GENERAL RAISE AND DROP PROCEDURES
(define (raise num)
    ((apply-generic 'raise num) num))

(define (drop num)
  (let ((lowered ((apply-generic 'project num) num)))
    (let ((raisedBack (raise lowered)))
      (if (equ? num raisedBack)
          lowered
          num))))


;;************************ PROCEDURES FOR SUCCESSIVE RAISING ************************;;

;; the procedures above this section are for raising and dropping a variable's type by one level in the hierarchy.
;; the procedures below will handle the successive raising of these variables according to the provided tower

(define arithmetic-tower '(integer rational real complex))

(define (apply-generic op . args)
  ;; apply-generic coerces its arguments to have the same type by successive raising
  (let ((type-tags (map type-tag args))) ;; type-tags is a list containing the type tags of each of the arguments passed
    (let ((proc (get op type-tags))) ;; lookup into the table using get, the operation, and the list of type tags
      (let ((higher-type (if (and (>= (length args) 2) (not proc)) (higher (car type-tags) (cadr type-tags) arithmetic-tower) #f)) ;; the type of the argument with higher type
            (lower-type (if (and (>= (length args) 2) (not proc)) (lower (car type-tags) (cadr type-tags) arithmetic-tower) #f)))  ;; the type of the argument with lower type
        (let ((lowerNum (if lower-type (car (filter (lambda (item) (equal? (type-tag item) lower-type)) args)) #f)) ;; returns the argument of the lower type which will be raised
              (higherNum (if lower-type (car (filter (lambda (item) (equal? (type-tag item) higher-type)) args)) #f)))
          (let ((raised-lower (if higher-type (raise-tower higher-type arithmetic-tower lowerNum) #f)))  ;; the argument with lower type after being raised to a higher type
            (if proc ;; if procedure is found,
                (apply proc (map contents args)) ;; (map contents args) returns a list of the contents of each of the arguments. apply will then apply the procedure on each of these contents.
                (if (>= (length args) 2)
                    (if raised-lower ;; if raised lower exists
                        (apply-generic op raised-lower higherNum)  ;; then call apply-generic with two arguments of the same type
                        (error
                         ("No method for these types -- APPLY-GENERIC"
                          (list op type-tags))))
                    (error
                     ("No method for these types -- APPLY-GENERIC"
                      (list op type-tags)))))))))))

(define (raise-tower type-to tower num) ;; raises the type of argument until it reaches intended type. returns the number with raised type
  (let ((type-from (type-tag num)))
    (let ((shortened (memq type-from tower)))  ;; cuts the tower into shorter tower where type-from is the new lowest level
      (define (iter num twr)
        (let ((tp-from (type-tag num)))
          (cond ((null? twr) #f)  ;; if tower is empty, return false
                ((equal? tp-from type-to) num)  ;; if our type reaches the intended type, then return the number of raised type
                (else
                 (iter (raise num) (cdr twr)))))) ;; else, return the iteration with tp-from raised to it's higher level, and we further shorten the tower
      (if shortened
          (iter num shortened)
          #f))))

;; **************************** TEST ***************************************** ;;

;; NOTES ;;
; drop hasn't been implemented into the system, but tests are added below
; no checks has been made for raising complex, and dropping an integer
; raise and drops for complex has only been added for rectangular form. it will not work for imaginary form
; this version doesn't allow more than two arguments in the arithmetic functions

;; Test variables


(define a-int 10)
(define b-int 234)

(define a-rat (make-rational 12 134))
(define b-rat (make-rational 124 2))

(define a-real 1.0)
(define b-real 12.512)

(define a-complex (make-complex-from-real-imag 12 0))
(define b-complex (make-complex-from-real-imag 10 13))
(define c-complex (make-complex-from-mag-ang 2 5))
(define d-complex (make-complex-from-mag-ang 10 3))

(display "***************RAISE*******************")

;; (raise num): raises a number one level up the tower.
(newline)
(raise a-int)
(raise b-int)
(raise a-rat)
(raise b-rat)
(raise a-real)
(raise b-real)

; OUTPUT:
;(rational 10 . 1)
;(rational 234 . 1)
;0.08955223880597014
;62.0
;(complex rectangular 1.0 . 0)
;(complex rectangular 12.512 . 0)

(display "******************DROP******************")
(newline)

;; (drop num): drops a number one level down the tower.
(drop a-complex)
(drop b-complex)
(drop a-real)
(drop b-real)
(drop a-rat)
(drop b-rat)

; OUTPUT:
;12.0
;(complex rectangular 10 . 13)
;(rational 1.0 . 1.0)
;12.512
;(rational 6 . 67)
;62

(display "******************RAISE-TOWER******************")
(newline)

;; (raise-tower type-to tower num): raises a number successively up the tower until it reaches level type-to
(raise-tower 'complex arithmetic-tower a-int)
(raise-tower 'complex arithmetic-tower a-rat)
(raise-tower 'complex arithmetic-tower a-real)
(raise-tower 'rational arithmetic-tower a-int)
(raise-tower 'real arithmetic-tower a-int)
(raise-tower 'real arithmetic-tower a-rat)

; OUTPUT:
;(complex rectangular 10.0 . 0)
;(complex rectangular 0.08955223880597014 . 0)
;(complex rectangular 1.0 . 0)
;(rational 10 . 1)
;10.0
;0.08955223880597014

(display "******************ADDITION******************")
(newline)

;; (add x y)
(add a-int a-rat)
(add a-int a-real)
(drop (add a-int a-complex))

(add a-int b-int)
(add a-rat b-real)
(drop (add a-complex b-int))

; OUTPUT:
;(rational 676 . 67)
;11.0
;22.0
;244
;12.60155223880597
;246.0

(display "******************SUBTRACTION******************")
(newline)

;; (sub x y)
(sub a-int a-rat)
(sub a-int a-real)
(drop (sub a-int a-complex))

(sub a-int b-int)
(sub a-rat b-real)
(drop (sub a-complex b-int))

;OUTPUT:
;(rational 664 . 67)
;9.0
;-2.0
;-224
;-12.42244776119403
;-222.0

(display "******************DIVISION******************")
(newline)

;; (div x y)
(div a-int a-rat)
(div a-int a-real)
(div a-int a-complex)

(div a-int b-int)
(div a-rat b-real)
(div a-complex b-int)

;OUTPUT
;(rational 335 . 3)
;10.0
;(complex polar 1.2 . 0)
;5/117
;0.007157308088712447
;(complex polar 0.05128205128205128 . 0)

(display "******************MULTIPLICATION******************")
(newline)

;; (mul x y)
(mul a-int a-rat)
(mul a-int a-real)
(mul a-int a-complex)

(mul a-int b-int)
(mul a-rat b-real)
(mul a-complex b-int)

;OUTPUT:
;(rational 60 . 67)
;10.0
;(complex polar 120.0 . 0)
;2340
;1.1204776119402984
;(complex polar 2808.0 . 0)




    

    