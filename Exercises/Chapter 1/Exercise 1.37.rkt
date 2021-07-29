#lang racket/base
(require racket/trace)
;; Exercise 1.37a(i)
(define (cont-frac n d k)
  ;; cont-frac procedure using recurrence relation
  (define (cont-frac-iter i)
    (define (h i)
      (cond ((= i 0) 0)
            ((= i 1) 1)
            (else (+ (*
                      (d i)
                      (h (- i 1)))
                     (*
                      (n 1)
                      (h (- i 2)))))))
    (define (r i)
      (cond ((= i 0) 1)
            ((= i (- 1)) 0)
            (else (+ (*
                      (d i)
                      (r (- i 1)))
                     (*
                      (n i)
                      (r (- i 2)))))))
    
    (define (final i)
      (/ (* (h i) (n i))
         (r i)))
    
    (final i)

    )
 
  (cont-frac-iter k)
  )

;; Test
(cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           11)

;; Output
; 0.6180555555555556

;; Exercise 1.37a(ii)

(define (new-cont-frac n d k)
  ;; cont-frac procedure using recursive process
  (define (recursive i)
    (cond ((= i k) (/ (n k) (d k)))
          (else (/ (n i)
                   (+ (d i)
                      (recursive (+ i 1)))))))
  (trace recursive)
  (recursive 1))

;; Tests
(new-cont-frac (lambda (i) 1.0)
               (lambda (i) 1.0)
               13)

(define (returnTwoMul n)
  (define (isTwoMul? n)
    (= (remainder (- n 2) 3) 0))
  (cond ((isTwoMul? n) (* 2 (+ 1 (/ (- n 2) 3))))
        (else 1)))


;(new-cont-frac (lambda (i) 1.0)
 ;              (lambda (i) (returnTwoMul i))
  ;             11)

;; Output
;>(recursive 1)
;> (recursive 2)
;> >(recursive 3)
;> > (recursive 4)
;> > >(recursive 5)
;> > > (recursive 6)
;> > > >(recursive 7)
;> > > > (recursive 8)
;> > > > >(recursive 9)
;> > > > > (recursive 10)
;> > > >[10] (recursive 11)
;> > > >[11] (recursive 12)
;> > > >[12] (recursive 13)
;< < < <[12] 1.0
;< < < <[11] 0.5
;< < < <[10] 0.6666666666666666
;< < < < < 0.6000000000000001
;< < < < <0.625
;< < < < 0.6153846153846154
;< < < <0.6190476190476191
;< < < 0.6176470588235294
;< < <0.6181818181818182
;< < 0.6179775280898876
;< <0.6180555555555556
;< 0.6180257510729613
;<0.6180371352785146
;0.6180371352785146

;>(recursive 1)
;> (recursive 2)
;> >(recursive 3)
;> > (recursive 4)
;> > >(recursive 5)
;> > > (recursive 6)
;> > > >(recursive 7)
;> > > > (recursive 8)
;> > > > >(recursive 9)
;> > > > > (recursive 10)
;> > > >[10] (recursive 11)
;< < < <[10] 0.125
;< < < < < 0.8888888888888888
;< < < < <0.5294117647058824
;< < < < 0.15315315315315314
;< < < <0.8671875
;< < < 0.5355648535564853
;< < <0.22047970479704798
;< < 0.8193499622071051
;< <0.5496468633153303
;< 0.39221117810004885
;<0.7182818352059925
;0.7182818352059925


;; exercise 1.37b
(define (cont-frac-iter n d k)
  (define (iter i result)
    (if (= i 0)
        result
        (iter
         (- i 1)
         (/ (n i)
                 (+ (d i) result))
         )))
  (trace iter)
  (iter k 0.0))

;; Test
(cont-frac-iter (lambda (i) 1.0)
              (lambda (i) 1.0)
               13)

;; Output
;>(iter 13 0.0)
;>(iter 12 1.0)
;>(iter 11 0.5)
;>(iter 10 0.6666666666666666)
;>(iter 9 0.6000000000000001)
;>(iter 8 0.625)
;>(iter 7 0.6153846153846154)
;>(iter 6 0.6190476190476191)
;>(iter 5 0.6176470588235294)
;>(iter 4 0.6181818181818182)
;>(iter 3 0.6179775280898876)
;>(iter 2 0.6180555555555556)
;>(iter 1 0.6180257510729613)
;>(iter 0 0.6180371352785146)
;<0.6180371352785146
;0.6180371352785146
