#lang racket
(require racket/trace)
;; ############### Sequence Operations ############### ;;
(define (max sequence)
  (define (iter result sequence)
    (cond ((null? sequence) result)
          ((> (car sequence) result) (iter (car sequence) (cdr sequence)))
          (else (iter result (cdr sequence)))))
  (iter (car sequence) sequence))

(define (flatten sequence)
  ;; if element of sequence is a pair, flatten it
  (cond
    ((null? sequence) sequence)
    ((pair? (car sequence)) (append (car sequence) (flatten (cdr sequence))))
    (else (cons (car sequence) (flatten (cdr sequence))))))

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

(define (first-n sequence n)
  (define (iter result n)
    (cond ((null? result) '())
          ((= n 0) '())
          (else
           (append (list (car result)) (iter (cdr result) (- n 1))))))
  (iter sequence n))

(define (reverse-1 l)
  (if (null? l)
      l
      (append (reverse-1 (cdr l)) (list (car l)))))

(define (last-n sequence n)
  (let ((r1 (reverse sequence)))
    (let ((a1 (first-n r1 n)))
      (reverse a1))))

;; ############### OPERATOR PRECEDENCE ############### ;;

(define (op-table op)
  (cond ((eq? op '*) 1)
        ((eq? op '**) 2)
        ((eq? op '+) 3)
        ))

(define (op? op)
  (or (eq? op '+)
      (eq? op '*)
      (eq? op '**)))

(define (smallest-op expr) 
  (define (iter result expr)
    (cond ((null? expr) result)
          ((op? (car expr))
           (iter
            (append result (list (op-table (car expr))))
            (cdr expr)))
          (else (iter result (cdr expr)))
          )) 
  (let ((l (iter '() expr)))
    l
    (let ((m (max l)))
      (cond ((= m 1) '*)
            ((= m 2) '**)
            ((= m 3) '+)
    ))))

;; ############### 1st layer ############### ;;
(define (variable? x) (symbol? x)) ;; the variables are symbols

;; ############### 2nd layer ############### ;;
(define (same-variable? v1 v2) 
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

;; We need to find out what operator will be the last one applied to the terms should we attempt to evaluate the expression. This has to be the operator with the lowest precedence among all the visible ones. So the predicates sum? and product? will search out the lowest-precedence operator and comprare it to '+ and '* (taken from http://community.schemewiki.org/?sicp-ex-2.58)

;; ############### 3rd layer ############### ;;

;; ##### ADDITION ######
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        ((pair? a2) (append (list a1 '+) a2))
        (else (list a1 '+ a2))))
(define (sum? x)
  (eq? '+ (smallest-op x)))

(define (addend s)
  (if (not (eq? (cadr s) (smallest-op s))) ;; if first operator is not the smallest operator in expression,
      (first-n s 3)  ;; return the expression of a op b
      (car s)))

(define (augend s)
  (let ((len (list-length s)))
    (if (not (eq? (cadr s) (smallest-op s))) ;; if first op is not +
        (if (= (list-length (cddddr s)) 1)
            (car (cddddr s))
            (cddddr s))
        (if (= (list-length (cddr s)) 1)
            (caddr s)
            (cddr s)))))

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
(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list m1 '* m2))))
(define (product? x)
  (eq? '* (smallest-op x)))

(define (multiplier p)
  (if (= (op-table (cadr p)) 1)
      (car p)
      (first-n p 3)))

(define (multiplicand p)
  (let ((len (list-length p)))
  (if (= (op-table (cadr p)) 1)
      (caddr p)
      (if (= (list-length (cddddr p)) 1)
          (car (cddddr p))
          (cddddr p)
          ))))

;; ##### EXPONENTIATION ######
(define (expt base n)
  ;; takes as arguments base and n (types: number)
  ;; returns b^n
  (cond ((= n 0) 1)
        ((even? n) (expt (square base) (/ n 2)))
        (else (* base (expt base (- n 1))))))

(define (square x) (* x x))

(define (exponentiation? x)
  (and (pair? x) (eq? (cadr x) '**)))

(define (base x) (car x))

(define (exponent x)
  (if (= (list-length (cddr x)) 1)
      (caddr x)
      (cddr x)))

(define (make-exponentiation base exponent)
  (cond ((=number? exponent 0) 1)
        ((=number? exponent 1) base)
        ((=number? base 1) 1)
        ((and (number? base) (number? exponent)) (expt base exponent))
        (else
         (list base '** exponent))))

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
                                (make-sum (exponent exp) -1))
           (deriv (base exp) var))))
        (else
         (error "unknown expression type -- DERIV" exp))))

;; Exercise 2.58b
(deriv '(x + 3 * (x + y + 2)) 'x)
;4
 
(deriv '(x + 3 * (x + y + 2)) 'y)
;3
 
(deriv '(x * y + y * x) 'x)
;'(y + y)

(deriv '(x + 3 + 2 * y * (x + z)) 'x)
; 1

(deriv '(x ** 2 + x * 3 + 9) 'x)
; '((2 * x) + 3)



