#lang racket
(require racket/trace)
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
    (cons (* r (cosine a)) (* r (sine a))))

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

;;************************ INTEGER PACKAGE ************************;;
(define (install-integer-package)
  (define (equ? x y)
    (equal? x y))
  (define (=zero? x)
    (= x 0))
  (define (negate x)
    (* -1 x))

  (put 'add '(integer integer)
       (lambda (x y) (+ x y)))
   (put 'sub '(integer integer)
       (lambda (x y) (- x y)))
  (put 'mul '(integer integer)
       (lambda (x y) (* x y)))
  (put 'div '(integer integer)
       (lambda (x y) (/ x y)))
  (put 'equ? '(integer integer) equ?)
  (put '=zero? '(integer) =zero?)
  (put 'negate '(integer) negate)
  'SUCCESS---INTEGER-PACKAGE)


;;************************ REAL PACKAGE ************************;;
(define (install-real-package)
  (define (equ? x y)
    (equal? x y))
  (define (=zero? x)
    (= x 0))
  (define (negate x)
    (* -1 x))

  (put 'add '(real real)
       (lambda (x y) (+ x y)))
   (put 'sub '(real real)
       (lambda (x y) (- x y)))
  (put 'mul '(real real)
       (lambda (x y) (* x y)))
  (put 'div '(real real)
       (lambda (x y) (/ x y)))
  (put 'equ? '(real real) equ?)
  (put '=zero? '(real) =zero?)
  (put 'negate '(real) negate)
  'SUCCESS---REAL-PACKAGE)

(install-integer-package)
(install-real-package)

;;************************ END OF ORDINARY PRIMITIVE NUMBERS ************************;;

;;************************ RATIONAL PACKAGE ************************;;
(define (install-rational-package)
  ;; internal procedures
  (define (tag x) (attach-tag 'rational x))
  
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
  (define (negate x)
    (tag (make-rat (* -1 (numer x))
                   (denom x))))
  ;; interface to rest of the system
  
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
  (put 'negate '(rational) negate)
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

  (put 'sine '(integer) sine-ordinary)
  (put 'sine '(rational) sine-rational)
  (put 'sine '(real) sine-ordinary)
  
  (put 'cosine '(integer) cosine-ordinary)
  (put 'cosine '(rational) cosine-rational)
  (put 'cosine '(real) cosine-ordinary)

  'SUCCESS--TRIGO-PACKAGE)

(install-trigo-package)

(define (sine x)
  (apply-generic 'sine x))
(define (cosine x)
  (apply-generic 'cosine x))

  ;; TODO : TRIGO PACKAGE

;;************************ COMPLEX PACKAGE ************************;;
(define (install-complex-package)
  ;; imported procedures from rectangular and polar packages
  (define (tag z) (attach-tag 'complex z))
  
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

  (define (negate x)
    (if (equal? (type-tag x) 'rectangular)
        (tag (make-from-real-imag (negate (real-part x))
                                  (negate (imag-part x))))
        (tag (make-from-mag-ang (magnitude x)
                                (+ (angle x) pi)))))
    
    ;; interface to rest of the system
  
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
                     (list op type-tags
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

(define (negate x)
  (apply-generic 'negate x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; SYMBOLIC ALGEBRA

 ;;;;;;;;;;;;;;;;;;;;;;;;;;; TERM DATA MANIPULATION ;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (install-poly-term-package)
  (define (tag x) (attach-tag 'term x))
  
  ;; internal procedures
  (define (make-term order coeff) (list order coeff))
  (define (order term) (car term))
  (define (coeff term) (cadr term))

  ;; interface to rest of system
   (put 'make-term 'term
       (lambda (order coeff) (tag (make-term order coeff))))
  (put 'order '(term) order)
  (put 'coeff '(term) coeff)
  'SUCCESS---POLYNOMIAL-POLY-TERM-PACKAGE)

 ;;;;;;;;;;;;;;;;;;;;;;;;;;; SPARSE POLY TERMLIST ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (install-poly-sparse-termlist-package)
  ;; internal procedures
  (define (tag x) (attach-tag 'sparse x))

  ;; shared procedures
  (define (=zero? x)
    (if (pair? x)
         #f
         (= x 0)))
  (define (coeff term) (cadr term))
  (define (order term) (car term))
  (define (make-term order coeff)
    ((get 'make-term 'term) order coeff))
  ;; end of shared procedures

  ;; procedures that work on sparse termlists
  (define (adjoin-term term term-list)
    (let ((o (order term))
          (c (coeff term)))
    (if (=zero? c)
        (tag term-list)
        (tag (cons (make-term o c) term-list)))))
  (define (first-term term-list) (car term-list))
  (define the-empty-termlist (tag '()))
  (define (rest-terms termlist) (tag (cdr termlist)))
  (define (empty-termlist? termlist) (null? termlist))
  
  ;; interface to rest of system
  (put 'adjoin-term '(term sparse) adjoin-term)
  (put 'first-term '(sparse) first-term)
  (put 'rest-terms '(sparse) rest-terms)
  (put 'empty-termlist? '(sparse) empty-termlist?)

  'SUCCESS---POLYNOMIAL-SPARSE-TERMLIST-PACKAGE)


 ;;;;;;;;;;;;;;;;;;;;;;;;;;; DENSE POLY TERMLIST ;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
(define (install-poly-dense-termlist-package)
  ;; internal procedures
  (define (tag x) (attach-tag 'dense x))

  ;; shared procedures
  (define (rest-terms term-list) (tag (cdr term-list)))
  (define (make-term order coeff)
    ((get 'make-term 'term) order coeff))
  (define (order term) (car term))
  (define (coeff term) (cadr term)) 
  ;; end of shared procedures

  ;; procedures that work on sparse termlists
  (define (first-term term-list)
    (make-term
     (- (length term-list) 1)
     (car term-list)))

  (define the-empty-termlist (tag '()))
  (define (empty-termlist? termlist) (null? termlist))

  (define (adjoin-term term term-list)
    (let ((exponent (order term)))
      (define (iter currExp result)
        (cond
          ((= currExp exponent) (tag (cons (coeff term) (cdr (rest-terms result)))))
          ((< currExp exponent) (iter (+ currExp 1) (cons 0 result)))
          (else
           (cons (car result) (iter (- currExp 1) (cdr result))))))
      (iter (- (length term-list) 1) term-list)))
  
  ;; interface to rest of system
  (put 'adjoin-term '(term dense) adjoin-term)
  (put 'first-term '(dense) first-term)
  (put 'rest-terms '(dense) rest-terms)
  (put 'empty-termlist? '(dense) empty-termlist?)
  
  'SUCCESS---POLYNOMIAL-DENSE-TERMLIST-PACKAGE)

(install-poly-term-package)
(install-poly-sparse-termlist-package)
(install-poly-dense-termlist-package)

 ;;;;;;;;;;;;;;;;;;;;;;;;;;; GENERIC POLYNOMIAL PACKAGE ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (install-polynomial-package)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;; POLYNOMIAL SELECTORS AND CONSTRUCTORS ;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (define (make-poly variable term-list) (cons variable term-list))
                  
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  
  (define (variable? x) (symbol? x))
  (define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))
  
  (define (=zero? x)
    (if (pair? x)
         #f
         (= x 0)))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;; TERMLIST SELECTORS AND CONSTRUCTORS ;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (adjoin-term term termlist)
    (apply-generic 'adjoin-term term termlist))
  (define (first-term termlist)
    (apply-generic 'first-term termlist))
  (define (rest-terms termlist)
    (apply-generic 'rest-terms termlist))
  (define (empty-termlist? termlist)
    (apply-generic 'empty-termlist? termlist))
  (define the-empty-termlist '())
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;; TERM SELECTORS AND CONSTRUCTORS ;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (make-term order coeff)
    ((get 'make-term 'term) order coeff))
  (define (order term)
    (apply-generic 'order term))
  (define (coeff term)
    (apply-generic 'coeff term))
 
  ;;;;;;;;;;;;;;;;;;;;;;;;;;; NEGATE POLY ;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (define (negate-terms L)
    (if (empty-termlist? L) 
        L
        (let ((first (first-term L)))
          (adjoin-term (make-term (order first)
                                  (negate (coeff first)))
                       (negate-terms (rest-terms L))))))
  
  (define (negate-poly p)
    (make-poly (variable p)
               (negate-terms (term-list p))))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;; ADD POLY ;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (add-terms L1 L2)
    (cond ((empty-termlist? L1) L2)
          ((empty-termlist? L2) L1)
          (else
           (let ((t1 (first-term L1))
                 (t2 (first-term L2)))
             (cond ((> (order t1) (order t2))
                    (adjoin-term
                     t1                                                
                     (add-terms (rest-terms L1) L2)))
                   ((< (order t1) (order t2))
                    (adjoin-term
                     t2
                     (add-terms L1 (rest-terms L2))))
                   (else
                    (adjoin-term
                     (make-term (order t1)
                                (add (coeff t1) (coeff t2)))
                     (add-terms (rest-terms L1)
                                (rest-terms L2)))))))))

  
  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (add-terms (term-list p1)
                              (term-list p2)))
        (error "Polys not in same var: ADD-POLY" (list p1 p2))))


  ;;;;;;;;;;;;;;;;;;;;;;;;;;; MUL POLY ;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (mul-terms L1 L2)
    (if (empty-termlist? L1)
        L1
        (add-terms (mul-term-by-all-terms (first-term L1) L2)
                   (mul-terms (rest-terms L1) L2))))
  
  (define (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L)
        L
        (let ((t2 (first-term L)))
          (adjoin-term
           (make-term (+ (order t1) (order t2))
                      (mul (coeff t1) (coeff t2)))
           (mul-term-by-all-terms t1 (rest-terms L))))))
  
  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (mul-terms (term-list p1) (term-list p2)))
         (error "Polys not in same var: MUL-POLY" (list p1 p2))))

   ;;;;;;;;;;;;;;;;;;;;;;;;;;; SUB POLY ;;;;;;;;;;;;;;;;;;;;;;;;;;;
               
  (define (sub-terms L1 L2)
    (add-terms L1 (negate-terms L2)))

  (define (sub-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (add-poly p1 (negate p2))
        (error "Polys not in same var: MUL-POLY" (list p1 p2))))

   ;;;;;;;;;;;;;;;;;;;;;;;;;;; DIV POLY ;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (define (div-terms L1 L2)
    (if (empty-termlist? L1)
        (list '() '())
        (let ((t1 (first-term L1))
              (t2 (first-term L2)))
          (if (> (order t2) (order t1))
              (list (list (type-tag L1)'()) L1)
              (let ((new-c (div (coeff t1) (coeff t2)))
                    (new-o (- (order t1) (order t2))))
                (let ((new-quotient (make-term new-o new-c)))
                  (let ((rest-of-result                         
                         (let ((multiplied (mul-term-by-all-terms new-quotient L2)))
                           (let ((new-numer (sub-terms L1 multiplied)))
                             (div-terms new-numer L2)))))
                    (list (adjoin-term new-quotient (first rest-of-result))
                          (second rest-of-result))
                  )))))))
  
  (define (div-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (div-terms
         (term-list p1)
         (term-list p2))
        (error "different variables")
        ))
        
                                
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;:::::::::;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  ;; interface to rest of the system
  (define (tag p) (attach-tag 'polynomial p))
  (put 'negate '(polynomial) negate-poly)
  ;(put 'negate '(sparse) negate-terms)
  ;(put 'negate '(dense) negate-terms)
  (put 'add '(polynomial polynomial)
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'mul '(polynomial polynomial)  
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'sub '(polynomial polynomial)
      (lambda (p1 p2) (tag (sub-poly p1 p2))))
  (put 'div '(polynomial polynomial)
       (lambda (p1 p2) (tag (div-poly p1 p2))))
  (put '=zero? 'polynomial =zero?)
  
  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))
  'SUCCESS---POLYNOMIAL-PACKAGE)

(install-polynomial-package)

(define (make-polynomial var terms)
  ((get 'make 'polynomial) var terms))
(define (make-term order coeff)
  ((get 'make-term 'term) order coeff))

;; TEST ;;

(define a-p (make-polynomial 'x (list 'sparse (make-term 3 4) (make-term 2 7) (make-term 1 5))))
(define b-p (make-polynomial 'x (list 'sparse (make-term 3 2) (make-term 2 4))))
(define c-p (make-polynomial 'x (list 'dense 3 2 4 0 0 1 2 4)))
(define d-p (make-polynomial 'x (list 'dense 0 0 2 4 12 9)))

(define e-p (make-polynomial 'x (list 'sparse
                                      (make-term 3 (make-rational 4 5))
                                      (make-term 2 (make-complex-from-real-imag 1 2))
                                      (make-term 4 2))))

;; TEST

(define f-p (make-polynomial 'x (list 'sparse (make-term 5 1) (make-term 0 -1))))
(define g-p (make-polynomial 'x (list 'sparse (make-term 2 1) (make-term 0 -1))))
(div f-p g-p)

;; OUTPUT
;'(polynomial (sparse (term 3 1) (term 1 1) ()) (sparse (term 1 1) (term 0 -1)))


                                     




