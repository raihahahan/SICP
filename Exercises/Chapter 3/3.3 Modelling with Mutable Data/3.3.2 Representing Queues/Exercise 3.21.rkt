#lang sicp
;; Queue Object: front-ptr | rear-ptr

;; selectors
(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))

;; mutators
(define (set-front-ptr! queue item) (set-car! queue item))
(define (set-rear-ptr! queue item) (set-cdr! queue item))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Queue structure

;; constructor
(define (make-queue) (cons '() '()))

;; selectors
(define (empty-queue? queue) (null? (front-ptr queue)))
(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with an empty queue" queue)
      (car (front-ptr queue))))

;; mutators
(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (else
           (set-cdr! (rear-ptr queue) new-pair)
           (set-rear-ptr! queue new-pair)
           queue))))

(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "DELETE! called with an empty  queue" queue))
        (else
         (set-front-ptr! queue (cdr (front-ptr queue)))
         queue)))

;; Queue is an object pair whose car is the pointer to the front of the queue,
;; cdr is the pointer to the final object pair of the queue
;; When you delete a queue item, the procedure mutates the car of the queue object pair to be the cdr of the first object pair in the queue.
;; Deleting a queue item does not modify the rear-ptr.
;; Hence, when we delete the final queue item, it seems as if there is one more item in the queue. But this is only the rear-ptr, which doesn't matter.
;; It doesn't matter because empty-queue? only checks if front-ptr is null.




(define (print-queue queue)
  (define (iter ls result)
    (if (null? ls)
        result
        (iter (cdr ls) (cons (car ls) result))))
  (iter (reverse (front-ptr queue)) '()))


;; TEST
(define q (make-queue))
(insert-queue! q 'a)
(print-queue q)
(insert-queue! q 'b)
(print-queue q)
(delete-queue! q)
(print-queue q)
(insert-queue! q 'c)
(print-queue q)