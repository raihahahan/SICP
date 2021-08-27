#lang sicp
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

(define (list-length l)
  ;; takes as argument a list l
  ;; returns the length of list
  (if (null? l)
      0
      (+ 1 (list-length (cdr l)))))

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
  (let ((flatlist (if (null? (cdr a)) ;; if the argument a is a list (due to augend), 
                      (car a) ;; then we let flatlist be the first item of that list [i.e (list (list) nil))
                      a) ;; else, leave it as a
                      ))                             
    (define (not_number x) (not (number? x)))
    (let ((numer-a (filter number? flatlist)) ;; list of numbers only
          (symbol-a (filter not_number flatlist))) ;; list of symbols only
    ;; define a procedure which takes in a list as argument and returns true if all the elements are numbers.
    ;; if above predicate is true, then define a procedure accumulator which adds the sum of all the numbers in the list.
    ;; else, (append (list '+) reduced-a
    ;; can make use of sequence_operations
    ;; similar steps for subtraction and product
      (cond ((not (includes not_number flatlist)) (accumulate + 0 flatlist)) ;; if all numbers
            ((null? numer-a)
             (if (> (list-length symbol-a) 1)
                 (append (list '+) symbol-a)
                 (car symbol-a))) ;; if all symbols
            ((= (accumulate + 0 numer-a) 0) (car symbol-a))
            (else (append (list '+) symbol-a (list (accumulate + 0 numer-a))))))))

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
  (let ((flatlist (if (null? (cdr a))
                      (car a)
                      a)
                      ))
    (define (not_number x) (not (number? x)))
    (let ((numer-a (filter number? flatlist)) ;; list of numbers only
          (symbol-a (filter not_number flatlist))) ;; list of symbols only
      (define (zero? x) (and (number? x) (= x 0))) ;; checks if any element is zero
      (cond ((includes zero? numer-a) 0) ;; if any number is zero
            ((not (includes not_number flatlist)) (accumulate * 1 numer-a)) ;; if all are numbers, multiply all them tgt      
            ((or (null? numer-a) (= (accumulate + 0 numer-a) 1))
             (if (> (list-length symbol-a) 1)
                 (append (list '*) symbol-a)
                 (car symbol-a)))
                  ;; if all are symbols or numbers == 1, then we multiply the symbols
            (else (append (list '*) symbol-a (list (accumulate * 1 numer-a))))))))

;; A product is a list whose first element is *
(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

;; The multiplier is the second item of product list
(define (multiplier p)
  (if (number? p)
      p
      (cadr p)))
;; The multiplicand is the third item of the product list
(define (multiplicand p)
  (cond ((number? p) 1)
        ((null? (cdddr p)) (caddr p))
        (else (make-product (cddr p)))))

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
        ((sum? exp) ;; ADDITION ;;
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((subtraction? exp) ;; SUBTRACTION ;;
         (make-subtraction (deriv (subend exp) var)
                           (deriv (augsub exp) var)))
        ((product? exp) ;; MULTIPLICATION ;;
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        ((exponentiation? exp) ;; EXPONENTIATION ;;
         (make-product
          (exponent exp)
          (make-product
           (make-exponentiation (base exp)
                                (make-subtraction (exponent exp) 1))
           (deriv (base exp) var))))
        (else
         (error "unknown expression type -- DERIV" exp))))


;; Exercise 2.56: exponential
(define e (make-exponentiation 'x 3))
(deriv e 'x)
;; (* (** x 2) 3)

;; Exercise 2.57: arbitrary arguments for sum and product
(define t '(* x y (+ x 3)))
(deriv t 'x)
;; (+ (* x y) (* y (+ x 3)))


