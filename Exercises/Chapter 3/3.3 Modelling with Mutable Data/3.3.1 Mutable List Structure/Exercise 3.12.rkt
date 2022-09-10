#lang racket

; response 1: '(b)
; response 2: '(b c d)

; x --> | * | * | --> | * | / |
;         |             |
;         V             V
;        'a            'b
; 
; y --> | * | * | --> | * | / |
;         |             |
;         V             V
;        'c            'd

; z --> | * | * |
;         |   |
;         V   V
;         x   y

; ---------------------------------
;                 x                           y
; (append! x y)   |                           |
;                 V                           V
; w --------> | * | * | --> | * | * | --> | * | * | --> | * | / |
;               |             |             |             |
;               V             V             V             V
;              'a            'b            'c            'd