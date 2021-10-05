;; HUFFMAN ENCODING TREES model

(define (successive-merge trees) 
   (let ((lightest-tree (car trees)) (heavier-trees (cdr trees))) 
     (if (null? heavier-trees) 
         lightest-tree 
         (successive-merge (adjoin-set (make-code-tree lightest-tree (car heavier-trees)) 
                                       (cdr heavier-trees)))))) 