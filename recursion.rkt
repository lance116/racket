;; ***************************************************
;; Lance Yan (21179582)
;; CS 135 Fall 2025
;; Assignment 3
;; ***************************************************

;; (a) harmonic numbers

;; this function computes the nth harmonic number recursively
(define (harmonic n)
  (cond [(= n 0) 0]
        [else (+ (/ 1 n) (harmonic (- n 1)))]))

;; (b) sum of squares

;; this function computes the sum of squares using the closed-form formula: n(n+1)(2n+1)/6
(define (ss-exact n)
  (/ (* n (+ n 1) (+ (* 2 n) 1)) 6))

;; and this function computes the sum of squares recursively
(define (ss-recursive n)
  (cond [(= n 0) 0]
        [else (+ (* n n) (ss-recursive (- n 1)))]))

;; tests for harmonic numbers
(check-expect (harmonic 0) 0)
(check-expect (harmonic 1) 1)
(check-expect (harmonic 2) 3/2)
(check-expect (harmonic 3) 11/6)
(check-expect (harmonic 4) 25/12)

;; tests showing both sum of squares functions produce same results
(check-expect (ss-exact 0) (ss-recursive 0))   
(check-expect (ss-exact 1) (ss-recursive 1))  
(check-expect (ss-exact 5) (ss-recursive 5))    
(check-expect (ss-exact 10) (ss-recursive 10))   
(check-expect (ss-exact 15) (ss-recursive 15))   

;; additional individual tests for sum of squares
(check-expect (ss-exact 0) 0)   
(check-expect (ss-exact 1) 1)    
(check-expect (ss-exact 2) 5)   
(check-expect (ss-exact 3) 14)   
(check-expect (ss-exact 4) 30)     
