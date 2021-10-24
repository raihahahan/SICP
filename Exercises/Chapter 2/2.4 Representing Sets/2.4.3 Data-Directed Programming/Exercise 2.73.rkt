#lang racket
;; Exercise 2.73

(require racket/trace)

;; Define Math Procedures
(define (expt base n)
  ;; takes as arguments base and n (types: number)
  ;; returns b^n
  (cond ((= n 0) 1)
        ((even? n) (expt (square base) (/ n 2)))
        (else (* base (expt base (- n 1)))))) 
(define (square x) (* x x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Deriv procedures
(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))
(define (=number? exp num)
  (and (number? exp) (= exp num)))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Deriv Procedure

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else ((get 'deriv (operator exp)) (operands exp)
                                           var))))

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

;; Exercise 2.73

; a.

;; The procedure deriv above first checks for base cases, if exp is a number or just a variable.
;; Next, it returns the necessary derivative procedure based on the table lookup. This returned deriv procedure is different from deriv, as it takes in as arguments the list of operands without the operator. This is because (get 'deriv (operator exp)) already knows what type of procedure to apply based on the operator of the original expression.
;; number? and variable? are not able to be assimilated into the table because in order to be added into the table and have the procedure accessed, we need to have the (operator exp) as the type for the table index. However, number? and variable? do not have a type and merely checks if a given expression is only a number or variable respectively.

; b. and c.

(define (install-deriv-package)
  ;; Procedures global to the deriv package ;;
  
  ; SUM ;
  (define (make-sum a1 a2)
    (cond ((=number? a1 0) a2)
          ((=number? a2 0) a1)
          ((and (number? a1) (number? a2)) (+ a1 a2))
          (else (list '+ a1 a2))))
  (define (addend s) (car s))
  (define (augend s) (cadr s)) 
  
  ; PRODUCT ;
  (define (make-product m1 m2)
    (cond ((or (=number? m1 0) (=number? m2 0)) 0)
          ((=number? m1 1) m2)
          ((=number? m2 1) m1)
          ((and (number? m1) (number? m2)) (* m1 m2))
          (else (list '* m1 m2))))
  (define (multiplier p) (car p))
  (define (multiplicand p) (cadr p))

  ; EXPONENTIATION ;
  (define (base x) (car x))
  (define (exponent x) (cadr x))
  (define (make-exponentiation base exponent)
    (cond ((=number? exponent 0) 1)
          ((=number? exponent 1) base)
          ((=number? base 1) 1)
          ((and (number? base) (number? exponent)) (expt base exponent))
          (else
           (list '** base exponent))))  

  ;; ################################# SUM PACKAGE ################################# ;;
  (define (install-deriv-sum-package)
    ;; Internal Procedures
    (define (deriv-sum exp var) 
      (make-sum (deriv (addend exp) var)
                (deriv (augend exp) var)))
    ;; Interface to the rest of the system
    (put 'deriv '+ deriv-sum) 
    (display "SUCCESS -- SUM\n"))

  ;; ################################# PRODUCT PACKAGE ################################# ;;
  (define (install-deriv-product-package) 
    ;; Internal Procedures
    (define (deriv-product exp var)
      (make-sum
       (make-product (multiplier exp)
                     (deriv (multiplicand exp) var))
       (make-product (deriv (multiplier exp) var)
                     (multiplicand exp))))

    ;; Interface to the rest of the system
    (put 'deriv '* deriv-product)
    (display "SUCCESS -- PRODUCT\n"))

  ;; ################################# EXPT PACKAGE ################################# ;;
  (define (install-deriv-expt-package)  
    ;; Internal Procedures
    (define (deriv-expt exp var)
      (make-product
       (exponent exp)
       (make-product
        (make-exponentiation (base exp)
                             (make-sum (exponent exp) -1))
        (deriv (base exp) var))))
  
    ;; Interface to the rest of the system
    (put 'deriv '** deriv-expt)
    (display "SUCCESS -- EXPONENTIATION\n"))

  (install-deriv-sum-package)
  (install-deriv-product-package)
  (install-deriv-expt-package)
  'done)

(install-deriv-package)

;; TEST ;; 
(deriv '(+ x 3) 'x) ; 1
(deriv '(** x 4) 'x) ; '(* 4 (** x 3))
(deriv '(** x 4) 'y) ; 0
(deriv '(* 3 x) 'x) ; 3
(deriv '(* (* x y) (+ x 3)) 'x) ; '(+ (* x y) (* y (+ x 3)))
(deriv '(* x y) 'x) ; 'y
(deriv '(+ x 3) 'x) ; 1

;; d.
; For each of the package installation procedure, we will have to edit the (put) procedures, by switching around the way we index the operation and type.


;; Additional remarks:

;; Diagram of the data directed table below:
;___________________________________________________________________
;             |        |              Type
;             |        |--------------------------------------------
;             |        |    '+     |       '*       |     '**
;===================================================================
; Operation   | 'deriv | deriv-sum |  deriv-product |   deriv-expt

; This data-directed programming differs from that of the complex number system. For this, the only operation to apply is 'deriv, and we have three different types: '+, '*, '**.
; We can simply access the entry by inputting 'deriv and (operator exp) as the arguments for get. We don't have to use tagged data here because the type of data is straightforward -- by looking at the type of operator that we are using.

; For the complex number system, there are four different operations and the type/representation of the data (polar and rectangular) is not straightforward.
; Hence, for that system, in order to access the selector procedures (real-part, imag-part, magnitude, and angle), we will have to use (apply-generic), which checks the tag attached to the complex number, and then apply the selector procedure to its contents.

