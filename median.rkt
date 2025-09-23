;; ***************************************************
;; Lance Yan (21179582)
;; CS 135 Fall 2025
;; Assignment #2 
;; ***************************************************

;; Original function for reference and testing
(define (median-of-3 a b c)
  (cond [(or (and (<= b a) (<= a c)) (and (<= c a) (<= a b))) a]
        [(or (and (<= a b) (<= b c)) (and (<= c b) (<= b a))) b]
        [(or (and (<= b c) (<= c a)) (and (<= a c) (<= c b))) c]))

;; Simplified median-of-3 function with minimal comparisons
(define (median-of-3-simple a b c)
  (cond [(<= a b) 
         (cond [(<= b c) b]          
               [(<= a c) c]          
               [else a])]           
        [(<= a c) a]   
        [(<= b c) c]      
        [else b]))       
        
;; Test cases
(check-expect (median-of-3-simple 1 2 3) 2)
(check-expect (median-of-3-simple 2 1 3) 2)
(check-expect (median-of-3-simple 3 2 1) 2)
(check-expect (median-of-3-simple 1 3 2) 2)
(check-expect (median-of-3-simple 2 3 1) 2)
(check-expect (median-of-3-simple 3 1 2) 2)
(check-expect (median-of-3-simple 5 5 5) 5)
(check-expect (median-of-3-simple 1 1 2) 1)
(check-expect (median-of-3-simple 1 2 1) 1)