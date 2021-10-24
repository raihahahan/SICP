#lang scheme
;; Exercise 2.76

;; 1. Generic operation with explicit dispatch

; Each procedure contains predicates for the different types.
; To add a new type, we have to update all the existing procedures for the new type predicates.
; To add a new operation, we can simply write the new procedure with all the type predicates without updating existing procedures.


;; 2. Message Passing

; Each procedure contains predicates for the different operations.
; To add a new type, we can simply write the new procedure with all the operation predicates without updating existing procedures.
; To add a new operation, we have to update all the existing procedures for the new operation predicates.

;; 3. Data-directed Programming

; To add a new type and operation, we can simply write a new install procedure to add in the new operation and type.
; The operation and type can also be added separately.


;; Data directed programming allows the programmer to add new code without updating existing code.
;; When new types are often added, message passing can be used as explained above.
;; When new operations are often added, generic operaiton can be used as explained above.
;; However, data directed programming gives us the best of both worlds as we do not have to update existing code in either cases.




