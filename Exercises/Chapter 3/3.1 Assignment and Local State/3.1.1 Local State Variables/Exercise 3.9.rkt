#lang racket

;; Recursive

; Procedure Objects in global frame

; global ----> __________________________________
; env         | factorial: -----                 |
;             |                |                 |
;             | _______________|_________________|
;                              |          ^
;                              |          | 
;                              V          | 
;                          ___    ___     |
;                         |___|  |___|----
;                           |
;                           V
;                       parameters: n
;                       body: (if (= n 1)
;                                 1
;                                (* n (factorial (- n 1))))


; Environments created by evaluating (factorial 6)

; global -------->  ______________________________________
; env              |                                      |
;                  |                                      |
;                  | _____________________________________|<----------          
; (factorial 6)          ^                                           |
;                        |              (from n:6 to n:1)            |
;               E1 --> n: 6                    ...            E --> n: 1
;                  (if (= n 1)                                (if (= n 1)
;                      1                                          1
;                      (* n                                       (* n 
;                        (factorial (- n 1))))                       (factorial (- n 1)))) 
;                     
; 6 * 120 == 720                

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Iterative

; global ----> _______________________________________
; env         | factorial: -----                      |
;             | fact-iter:     |                      |
;             |  |             |                      | 
;             |  |             |                      |
;             | _|_____________|______________________|
;                |             |          ^       ^
;                |             |          |       |
;                |             V          |       |
;                |         ___    ___     |       |
;                |        |___|  |___|----        |
;                |          |                     |
;                |          V                     |
;                |      parameters: n             |
;                |      body: (fact-iter 1 1 n)   |
;                |                                |
;                |                                |
;                |                                |
;                |                                |
;                V                                |
;            ____   ____                          |
;           |___|  |___|--------------------------
;             |
;             V
;         parameters: product, counter, max-count
;         body: (if (> counter max-count)
;                    product
;                    (fact-iter (* counter product)
;                               (+ counter 1)
;                                max-count))


; Environments created by evaluating (factorial 6)

; global -------->  ______________________________________
; env              |                                      |
;                  |                                      |
;                  | _____________________________________|<------------------------------
; (factorial 6)       ^                 ^                                                |    
;                     |                 |        (from counter:1 to counter:7)           |                        
;    E0 --> n: 6 -----        E1 --> product: 1               ...               E7 --> product: 720                    
;       (fact-iter 1 1 n)            counter: 1                                        counter: 7                                
;                                    max-count: 6                                      max-count: 6                                         
;                                    (if (> counter max-count)                          (if (> counter max-count)
;                                        product                                             product                  
;                                        (fact-iter (* counter product)                      (fact-iter (* counter product)
;                                                   (+ counter 1)                                       (+ counter 1)
;                                                    max-count))                                        max-count))