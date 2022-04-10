#lang racket

z --> |'a|*| --> |'b|*| --> |'c|*| --
         ^                          |
         |--------------------------|                           

;; (last-pair z) goes into an infinite recursion