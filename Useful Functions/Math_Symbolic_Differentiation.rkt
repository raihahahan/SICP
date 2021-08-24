#lang sicp
;; ############### Math Operations ############### ;;
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

;; ############### Sequence Operations ############### ;;
(define (filter predicate sequence)
  (cond ((null? sequence) '())
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) '() sequence))

(define (includes predicate sequence)
  ;; returns true if any (predicate element) returns true
  (cond ((null? sequence) #f)
        ((predicate (car sequence)) #t)
        (else (includes predicate (cdr sequence)))))

(define (flatten sequence)
  ;; if element of sequence is a pair, flatten it
  (cond
    ((null? sequence) sequence)
    ((pair? (car sequence)) (append (car sequence) (flatten (cdr sequence))))
    (else (cons (car sequence) (flatten (cdr sequence))))))

;; ############### 1st layer ############### ;;
(define (variable? x) (symbol? x)) ;; the variables are symbols

;; ############### 2nd layer ############### ;;
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

;; ############### 3rd layer ############### ;;

;; ##### ADDITION ######
(define (make-sum . a)
  ;; =number? checks if an expression is equal to a given number
  ;; reduced-a is a list of all the numbers in make-sum
  (let ((flatlist (flatten a)))                             
    (define (not_number x) (not (number? x)))
    (let ((numer-a (filter number? flatlist))
          (symbol-a (filter not_number flatlist)))
    ;; define a procedure which takes in a list as argument and returns true if all the elements are numbers.
    ;; if above predicate is true, then define a procedure accumulator which adds the sum of all the numbers in the list.
    ;; else, (append (list '+) reduced-a
    ;; can make use of sequence_operations
    ;; similar steps for subtraction and product
    (if (not (includes not_number flatlist)) ;; if all elements are numbers
        (accumulate + 0 flatlist) ;; add them up all together
        (append (list '+) symbol-a (list (accumulate + 0 numer-a))))))) ;; else, (list '+ 1 2 ...)

;; A sum is a list whose first element is the symbol +:
(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))
(define (addend s) (cadr s))
(define (augend s) (make-sum (cddr s)))

;; ##### SUBTRACTION ######
(define (make-subtraction a1 a2)
  (cond ((=number? a1 0) (list '-a2))
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (- a1 a2))
        (else (list '- a1 a2))))

(define (subtraction? x)
  (and (pair? x) (eq? (car x) '-)))
(define (subend s) (cadr s))
(define (augsub s) (caddr s))

;; ##### PRODUCT ######
(define (make-product . a)
  (let ((flatlist (flatten a)))                             
    (define (not_number x) (not (number? x)))
    (let ((numer-a (filter number? flatlist))
          (symbol-a (filter not_number flatlist)))
    (if (not (includes not_number flatlist)) ;; if all elements are numbers
        (accumulate * 1 numer-a) ;; add them up all together
        (append (list '*) symbol-a (list (accumulate * 1 numer-a))))))) ;; else, (list '+ 1 2 ...)

;; A product is a list whose first element is *
(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

;; The multiplier is the second item of product list
(define (multiplier p) (cadr p))
;; The multiplicand is the third item of the product list
(define (multiplicand p) (make-product (cddr p)))

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

(deriv '(* x 2 x 3) 'x)
