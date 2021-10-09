#lang racket/base
;; Exercise 2.71

;           n = 5
;       
;;      (A B C D E 31)
;;           / \
;;         /    \
;;   (E 16)     (A B C D 15)
;;                /  \
;;               /    \
;;           (D 8)  (A B C 7)
;;                  /   \
;;                 /     \
;;              (C 4)    (A B 3)
;;                         /  \
;;                        /    \
;;                      (B 2)  (A 1)



;###########################################;


;           n = 10 
;
;;  (A B C D E F G H I J 1023)
;;      /         \
;;     /           \
;; (J 256) (A B C D E F G H I 511)  
;;          /        \
;;         /          \
;;   (I 256)  (A B C D E F G H 255)      
;;              /    \
;;             /      \
;;        (H 128) (A B C D E F G 127)
;;           /        \ 
;;          /          \
;;      (G 64)    (A B C D E F 63) 
;;                  /         \
;;                 /           \
;;             (F 32)     (A B C D E 31)
;;                         / \
;;                       /    \
;;                 (E 16)     (A B C D 15)
;;                             /   \
;;                            /     \
;;                         (D 8)  (A B C 7)
;;                                /   \
;;                               /     \
;;                            (C 4)    (A B 3)
;;                                       /  \
;;                                      /    \
;;                                    (B 2)  (A 1)

;; For general n, it takes 1 bit ('1) to encode the most frequent symbol. This is because the most frequent symbol has frequency higher than all the other symbols (2^(n-1) > 2^i, where i < n-1.
;; It takes n-1 bits (10 '0's) to encode the least frequent symbol. This is because there are no symbols with the same frequency. Hence, we will have to keep going down the tree, to the right branch, until we reach a leaf to go to the least frequent symbol. This takes n-1 steps.
