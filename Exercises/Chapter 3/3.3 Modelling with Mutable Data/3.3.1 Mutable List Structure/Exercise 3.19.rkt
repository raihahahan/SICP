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

(define (check-cycle-func x)
  (define (check-recur y encountered)
    (cond
      ((not (pair? y)) #f) ;; if not pair, then a cycle is not possible
      ((and (memq y encountered) (eq? (+ 1 (length encountered)) (* 2 (count-pairs x)))) #t) ;; if the (length + 1) of encountered is
      ;; twice that of count pairs, AND
      ;; the current pair is in encountered,
      ;; then this means that we have underwent a cycles
      (else
       (or (check-recur (car y) (cons y encountered))
           (check-recur (cdr y) (cons y encountered))))))
  (check-recur x '()))

;(define (check-cycle-iter x)
;  (let ((finalRes '()))
 ;   (define (iter carY cdrY encountered result)
  ;    (cond
   ;     ((not (pair? carY)) (cons #f finalRes))
    ;    ((and (memq carY encountered) (eq? (+ 1 (length encountered)) (* 2 (count-pairs x)))) (cons #t finalRes)))
     ; (cond
     ;;   ((not (pair? cdrY)) (cons #f result))
      ;  ((and (memq cdrY encountered) (eq? (+ 1 (length encountered)) (* 2 (count-pairs x)))) (cons #t finalRes)))
      ;(iter

;; TODO


          
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

(define t1 (cons 'a 'b)) 
(define t2 (cons t1 t1))

(define zc (make-cycle (cons y '())))
(define yc (make-cycle (cons y '())))

;;;;;;;;;;;;; CREATE TESTS ;;;;;;;;;;;;;;;;
(define tests
  (list x1 f (list 1 2 3) z zc yc t1 t2))

(define expect
  (list #t #t #f #f #t #t #f #f))

(define (test testList expectList function)
  (cond ((null? testList) "All tests passed.")
        ((not (eq? (function (car testList)) (car expectList))) (error "A test did not pass." (list (car testList) (function (car testList)) (car expectList) function)))
        (else
         (test (cdr testList) (cdr expectList) function))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(display "Imperative-style recursive")
(display "\n")
(test tests expect check-cycle)
; "All tests passed."

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(display "Functional-style recursive")
(display "\n")
(test tests expect check-cycle-func)
; "All tests passed."

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(display "Imperative-style iterative")
(display "\n")



