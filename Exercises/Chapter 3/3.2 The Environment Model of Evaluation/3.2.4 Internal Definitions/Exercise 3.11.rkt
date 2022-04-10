#lang racket
;; refer to png diagram.

;; Local state for acc is kept in E0.
;; When we define another account, a new environment, say, E7, will be created containing the bounded variable balance.
;; acc and acc2 will share the procedure in the data object (i.e. parameters: m, body: (cond ...), and the global environment.

;; The local states are kept distinct as each variable has their own local environment.