#lang sicp
;; ############### 1st layer ############### ;;
(define (variable? x) (symbol? x)) ;; the variables are symbols

;; ############### 2nd layer ############### ;;
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

;; ############### 3rd layer ############### ;;

;; ##### SUM ######
(define (make-sum a1 a2)
  ;; =number? checks if an expression is equal to a given number
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))

;; A sum is a list whose first element is the symbol +:
(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

;; The addend is the second item of the sum list
(define (addend s) (cadr s))

;; The augend is the third item of the sum list
(define (augend s) (caddr s))


;; ##### SUBTRACTION ######
(define (make-subtraction a1 a2)
  (cond ((=number? a1 0) (list '-a2))
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (- a1 a2))
        (else (list '- a1 a2))))

(define (subtraction? x)
  (and (pair? x) (eq? (car x) '-)))

;; The addend is the second item of the sum list
(define (subend s) (cadr s))

;; The augend is the third item of the sum list
(define (augsub s) (caddr s))

;; ##### PRODUCT ######
(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

;; A product is a list whose first element is *
(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

;; The multiplier is the second item of product list
(define (multiplier p) (cadr p))

;; The multiplicand is the third item of the product list
(define (multiplicand p) (caddr p))

;; ##### EXPONENTIATION ######
(define (expt base n)
  ;; takes as arguments base and n (types: number)
  ;; returns b^n
  (cond ((= n 0) 1)
        ((even? n) (expt (square base) (/ n 2)))
        (else (* base (expt base (- n 1))))))

(define (square x) (* x x))

(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**)))

(define (base x) (cadr x))

(define (exponent x) (caddr x))

(define (make-exponentiation base exponent)
  (cond ((=number? exponent 0) 1)
        ((=number? exponent 1) base)
        ((=number? base 1) 1)
        ((and (number? base) (number? exponent)) (expt base exponent))
        (else
         (list '** base exponent))))

;; ############### 4th layer ############### ;;
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((subtraction? exp)
         (make-subtraction (deriv (subend exp) var)
                           (deriv (augsub exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        ((exponentiation? exp)
         (make-product
          (exponent exp)
          (make-product
           (make-exponentiation (base exp)
                                (make-subtraction (exponent exp) 1))
           (deriv (base exp) var))))
        (else
         (error "unknown expression type -- DERIV" exp))))

(deriv (make-exponentiation 'x 3) 'x)
(deriv (make-exponentiation 'x 5) 'x)
(deriv (make-exponentiation 'x 2) 'x)
(deriv (make-exponentiation 'a 5) 'a)
(deriv (make-exponentiation 'a 'b) 'a)
(deriv (make-exponentiation 'a (make-sum 'a 'b)) 'a)

;(* 5 (** x 4))
;(* 2 x)
;(* 5 (** a 4))
;(* b (** a (- b 1)))
;(* (+ a b) (** a (- (+ a b) 1)))
