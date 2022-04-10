#lang sicp

(define (count-pairs x) 
  (let ((encountered '())) 
    (define (helper x) 
      (if (or (not (pair? x)) (memq x encountered)) 
          0 
          (begin 
            (set! encountered (cons x encountered)) 
            (+ (helper (car x)) 
               (helper (cdr x)) 
               1)))) 
    (helper x))) 

(define (check-cycle x)
  (let ((encountered '()))
    (define (check-recur y)
      (cond
        ((not (pair? y)) #f) ;; if not pair, then a cycle is not possible
        ((and (memq y encountered) (eq? (+ 1 (length encountered)) (* 2 (count-pairs x)))) #t) ;; if the (length + 1) of encountered is
                                                                                               ;; twice that of count pairs, AND
                                                                                               ;; the current pair is in encountered,
                                                                                               ;; then this means that we have underwent a cycles
        (else
          (begin
            (set! encountered (cons y encountered)) ;; add pair into encountered list
            (or (check-recur (car y))
                (check-recur (cdr y)))))))
    (check-recur x)))
          
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

(define (make-cycle x)
  (define (last-pair x)
    (if (null? (cdr x))
        x
        (last-pair (cdr x))))
  (set-cdr! (last-pair x) x)
  x)

(define f (make-cycle (list 1 2 3)))
(define x (cons 3 '()))
(define y (cons x x))
(define z (cons y '()))

(define x1 '(a b c)) 
(define y1 '(d e f)) 
(set-car! (cdr x1) y1) 
(set-car! x1 (cdr x1)) 
(set-cdr! (last-pair y1) (cdr y1)) 

(check-cycle x1) ;#t
(check-cycle f) ;#t
(check-cycle (list 1 2 3)) ;#f
(check-cycle z) ;#f
(check-cycle (make-cycle z)) ;#t
(check-cycle (make-cycle y)) ;#t

(define t1 (cons 'a 'b)) 
(define t2 (cons t1 t1)) 
(check-cycle t1) ;#f
(check-cycle t2) ;#f