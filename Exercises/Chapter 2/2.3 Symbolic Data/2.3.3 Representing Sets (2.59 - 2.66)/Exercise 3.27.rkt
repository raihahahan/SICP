#lang racket
; Question: Explain why memo-fib computes the nth Fibonacci number in a number of steps proportional to n.

; Assume that there are no records in the memo-table
; (memo-fib n) searches the memo-table. There are no records for n.
; Call (memo-fib (n-1)) and (memo-fib (n-2)) and n is now inserted into the table.
; (memo-fib (n-2)) searches the memo-table. There are no records for n-2.
; Call (memo-fib (n-3)) and (memo-fib (n-4)) and (n-2) is now inserted into the table.
; (memo-fib (n-1)) searches the memo-table. There are no records for n-1.
; Call (memo-fib (n-2)) and (memo-fib (n-3)) and (n-1) is now inserted into the table.
; (n-2) is already inserted into table. So (memo-fib (n-2)) returns that value.
; Notice that each (memo-fib i) is only called once for all i, 0 <= i <= n.
; So (memo-fib n) behaves linearly and has time complexity of O(n).
;; OR
; After drawing the environment model of evaluation, (f x) will never be called more than once.

; Would the scheme still work if we had simply defined memofib to be (memoize fib)?

; No.
; The current implementation recursively calls memo-fib, which is the memoized procedure and hence is able to do a table lookup.
; The new implementation recursively calls fib, so it will not do any table lookups for all intermediate calls to fib. The time complexity will be O(2^n).


