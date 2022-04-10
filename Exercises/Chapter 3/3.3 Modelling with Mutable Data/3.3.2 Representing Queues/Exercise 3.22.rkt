#lang sicp

;; encapsulate data into one make-queue procedure with local state variables
(define (make-queue)
  (let ((front-ptr '())
        (rear-ptr '()))

    (define (print-queue)
      (define (iter ls result)
        (if (null? ls)
            result
            (iter (cdr ls) (cons (car ls) result))))
      (iter (reverse front-ptr) '()))

    ;;;;;;;;;;;;;;; Methods ;;;;;;;;;;;;;;;;

    ;; mutators
    (define (set-front-ptr! item) (set! front-ptr item))
    (define (set-rear-ptr! item) (set! rear-ptr item))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    ;; selectors
    (define (empty-queue?) (null? front-ptr))
    (define (front-queue)
      (if (empty-queue?)
          (error "FRONT called with an empty queue")
          front-ptr))

    ;; mutators
    (define (insert-queue! item)
      (let ((new-pair (cons item '())))
        (cond ((empty-queue?)
               (set-front-ptr! new-pair)
               (set-rear-ptr! new-pair)
               (print-queue))
              (else
               (set-cdr! rear-ptr new-pair)
               (set-rear-ptr! new-pair)
               (print-queue)))))

    (define (delete-queue!)
      (cond ((empty-queue?)
             (error "DELETE! called with an empty  queue"))
            (else
             (set-front-ptr! (cdr front-ptr))
             (print-queue))))


    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (define (dispatch m)
      (cond ((eq? m 'empty-queue?) (empty-queue?))
            ((eq? m 'front-queue) (front-queue))
            ((eq? m 'insert-queue!) insert-queue!)
            ((eq? m 'delete-queue!) (delete-queue!))
            (else
             (error "DISPATCH no dispatch of type m" m))))

    dispatch))


(define q1 (make-queue))
(q1 'empty-queue?)
((q1 'insert-queue!) 'a)
((q1 'insert-queue!) 'b)
((q1 'insert-queue!) 'c)
(q1 'delete-queue!)
;#t
;(a)
;(a b)
;(a b c)
;(b c)

             
