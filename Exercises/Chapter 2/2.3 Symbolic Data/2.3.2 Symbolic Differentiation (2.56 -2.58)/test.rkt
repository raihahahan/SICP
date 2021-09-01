#lang racket

(require racket/trace)
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        ((exponentiation? exp)
         (let ((base (base exp))
               (exponent (exponent exp)))
           (make-product exponent
                         (make-product (make-exponentiation base (make-sum exponent -1))
                                       (deriv base var)))))
        (else
         (error "unknown expression type -- DERIV" exp))))
 
(define variable? symbol?)
 
(define (same-variable? v1 v2)
  (and (variable? v1)
       (variable? v2)
       (eq? v1 v2)))
 
(define (sum? x)
  (and (pair? x)
       (memq '+ x)))
 
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1)
              (number? a2)) (+ a1 a2))
        (else (list a1 '+ a2))))
 
(define (simple-sum? x)
  (null? (cdddr x)))
 
; 1) a sum expression:
;  a  (p +  q)       addend = p        augend = q
;  b  (p +  q +  r)  addend = p        augend = q + r
;  c  (p +  q *  r)  addend = p        augend = q * r
;  d  (p *  q +  r)  addend = p * q    augend = r
;  e  (p +  q ** r)  addend = p        augend = q ** r
;  f  (p ** q +  r)  addend = p ** q   augend = r
 
; a,b,c,e :   p + ...                 => addend is always p
; for d:
;   algebraically: p * q + r = r + p * q = r + q * p
;   symbolically:  let E = p * q + r = reverse E = r + q * p
; => the addend is the reversed augend of a reversed expression type c
; for f:
;   algebraically: p ** q + r = r + p ** q = r + (p ** p)
;   symbolically:  let E = p ** q + r = reverse E = r + q ** p
; => the addend is the reversed augend of a reversed expression type e
(define (addend x)
  (cond
    ; type a
    ((simple-sum? x)   (car x))
    ; type b, c or e
    ((eq? (cadr x) '+) (car x))
    ; type d, f
    (else (simplify-term (reverse (augend (reverse x)))))))
 
; a,b,c,e :   p + ...  => augend is the expression after the +
; for d:
;   algebraically: p * q + r = r + p * q = r + q * p
;   symbolically:  let E = p * q + r = reverse E = r + q * p
; => the augend is the reversed addend of a reversed expression type c
; for f:
;   algebraically: p ** q + r = r + p ** q = r + (p ** p)
;   symbolically:  let E = p ** q + r = reverse E = r + q ** p
; => the augend is the addend of a reversed expression type e
; Update: Just remove the first 4 terms to get the augend
(define (augend x)
  (cond
    ; type a
    ((simple-sum? x) (caddr x))
    ; type b, c or e
    ((eq? (cadr x) '+) (cddr x))
    ; type d, f
    (else (simplify-term (cddddr x)))))
 
; Complex expression terms are allowed and can contain variables, constants or sub-expressions.
; which might be a list of a single term.
(define (simplify-term x)
  (cond ((variable? x) x)
        ((null? (cdr x)) (car x))
        (else x)))
 
(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list m1 '* m2))))
 
(define (=number? exp num)
  (and (number? exp) (= exp num)))
 
(define (product? x)
  (and (pair? x)
       (not (sum? x))
       (memq '* x)))
 
(define (simple-product? x)
  (null? (cdddr x)))
 
; 2) a product expression: multiplier * multiplicand
;  a  (p *  q)        multiplier = p        multiplicand = q
;  b  (p *  q *  r)   multiplier = p        multiplicand = q * r
;  c  (p *  q ** r)   multiplier = p        multiplicand = q ** r
;  d  (p ** q *  r)   multiplier = p ** q   multiplicand = r
 
; a,b,c :  p * ... => multiplier is always p
; for d :
;   algebraically: p ** q * r = r * p ** q
;   symbolically:  let E = p ** q * r = reverse E = r * q ** p
; => the multiplier is the reversed multiplicand of a reversed expression type c
(define (multiplier x)
  (cond
    ; type a
    ((simple-product? x)   (car x))
    ; type b, c
    ((eq? (cadr x) '*) (car x))
    ; type d
    (else (simplify-term (reverse (multiplicand (reverse x)))))))
 
; a,b,c :   p * ...  => multiplicand is the expression after the *
; for d:
;   algebraically: p ** q * r = r * p ** q
;   symbolically:  let E = p ** q * r = reverse E = r * q ** p
; => the multiplicand is the reversed multiplier of a reversed expression type c
; Update: Just remove the first 4 terms to get the multiplicand
(define (multiplicand x)
  (cond
    ; type a
    ((simple-product? x) (caddr x))
    ; type b, c
    ((eq? (cadr x) '*) (cddr x))
    ; type d
    (else (simplify-term (cddddr x)))))
 
; Update: Replace = with =number? in cond
(define (make-exponentiation base exponent)
  (cond ((=number? base 0) 0)
        ((=number? exponent 0) 1)
        ((=number? exponent 1) base)
        (else (list base '** exponent))))
 
(define (exponentiation? x)
  (and (pair? x)
       (not (sum? x))
       (not (product? x))
       (memq '** x)))
 
; 3) an exponentiation expression: base ** exponent
;    (p ** q)       base = p   exponent = r
;    (p ** q ** r)  base = p   exponent = q ** r
(define (base x)
  (car x))
(define (exponent x)
  (simplify-term (cddr x)))

(trace deriv)
(deriv '(x + 3 * (x + y + 2)) 'x)
;4
 
;(deriv '(x + 3 * (x + y + 2)) 'y)
;3
 
;(deriv '(x * y + y * x) 'x)
;'(y + y)
 
; should still support part a) style nested expressions
;(deriv '(x ** (3 * (x + (y + 2)))) 'x)
;'((3 * (x + (y + 2))) * (x ** ((3 * (x + (y + 2))) + -1)))
 
;(deriv '(x + (3 * (x ** (y + 2)))) 'x)
;'(1 + (3 * ((y + 2) * (x ** ((y + 2) + -1)))))
 
;(deriv '(x + (3 * (x + (y + 2)))) 'x)
;4
;(deriv '(x + (3 * (x ** (y + 2)))) 'x)
;'(1 + (3 * ((y + 2) * (x ** ((y + 2) + -1)))))
