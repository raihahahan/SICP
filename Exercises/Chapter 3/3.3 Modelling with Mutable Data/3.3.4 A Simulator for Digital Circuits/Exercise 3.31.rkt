#lang racket
; Each wire has initial signal state of 0.
; Without calling the procedure on the intialization stage, the wire signals will not have the required relationship for the circuit to work.
; Hence, calling the procedures immediately allows the circuit to initialize the wires to its correct signals for the circuit to work.