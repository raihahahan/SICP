#lang sicp
(define (make-dequeue)
  (let ((front-ptr '())
        (rear-ptr '()))

    (define (print-dequeue)
      (define (iter ls result container)
        (if (or (null? ls) (memq ls container))
            result
            (iter (cdr ls) (cons (car ls) result) (cons ls container))))
      (reverse (iter front-ptr '() '())))


    ;;;;;;;;;;;;;;; Methods ;;;;;;;;;;;;;;;;

    ;; mutators
    (define (set-front-ptr! item) (set! front-ptr item))
    (define (set-rear-ptr! item) (set! rear-ptr item))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    ;; selectors
    (define (empty-dequeue?) (and (null? front-ptr) (null? rear-ptr)))
    (define (front-dequeue)
      (if (empty-dequeue?)
          (error "FRONT called with an empty queue")
          front-ptr))
    (define (rear-dequeue)
      (if (empty-dequeue?)
          (error "REAR called with an empty queue")
          rear-ptr))

    ;; mutators
    (define (front-insert-dequeue! item)
      (let ((new-pair (cons item '())))
        (cond ((empty-dequeue?)
               (set-front-ptr! new-pair)
               (set-rear-ptr! new-pair)
               (print-dequeue))
              (else
               (set-cdr! new-pair front-ptr)
               (set-front-ptr! new-pair)
               (print-dequeue)))))
    
    (define (rear-insert-dequeue! item)
      (let ((new-pair (cons item '())))
        (cond ((empty-dequeue?)
               (set-front-ptr! new-pair)
               (set-rear-ptr! new-pair)
               (print-dequeue))
              (else
               (set-cdr! new-pair rear-ptr)
               (set-cdr! rear-ptr new-pair)
               (set-rear-ptr! new-pair)
               (print-dequeue)))))

    (define (front-delete-dequeue!)
      (cond ((empty-dequeue?)
             (error "DELETE! called with an empty  queue"))
            (else
             (set-front-ptr! (cdr front-ptr))
             (print-dequeue))))

    (define (rear-delete-dequeue!)
      (cond ((empty-dequeue?)
             (error "DELETE! called with an empty  queue"))
            (else
             (set-rear-ptr! (cdr rear-ptr))
             (set-cdr! rear-ptr '())
             (print-dequeue))))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (define (dispatch m)
      (cond ((eq? m 'empty-dequeue?) (empty-dequeue?))
            ((eq? m 'front-dequeue) (front-dequeue))
            ((eq? m 'rear-dequeue) (rear-dequeue))
            ((eq? m 'front-insert-dequeue!) front-insert-dequeue!)
            ((eq? m 'rear-insert-dequeue!) rear-insert-dequeue!)
            ((eq? m 'front-delete-dequeue!) (front-delete-dequeue!))
            ((eq? m 'rear-delete-dequeue!) (rear-delete-dequeue!))
            ((eq? m 'print-dequeue) (print-dequeue))
            (else
             (error "DISPATCH no dispatch of type m" m))))

    dispatch))

(define a (make-dequeue))
(a 'print-dequeue) ; ()
((a 'rear-insert-dequeue!) 'f) ; (f)
((a 'front-insert-dequeue!) 'a) ; (a f)
((a 'front-insert-dequeue!) 'b) ; (b a f)
((a 'rear-insert-dequeue!) 'c) ; (b a f c)
((a 'rear-insert-dequeue!) 'd) ; (b a f c d)
(a 'front-delete-dequeue!) ; (a f c d)
(a 'rear-delete-dequeue!) ; (a f c)
(a 'front-delete-dequeue!) ; (f c)