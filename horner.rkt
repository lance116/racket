;;
;; ***************************************************
;; Lance Yan (21179582)
;; CS 135 Fall 2025
;; Assignment 04
;; ***************************************************
;;

;;
;; Question 5
;;

;; eval-poly consumes a list of numbers (representing the coefficients of a polynomial) 
;; and a value for x, and produces the result of evaluating the given polynomial 
;; at the given value of x
;; eval-poly: (listof Number) Number -> Number

(define (eval-poly coefficients x)
  (cond
    [(empty? coefficients) 0]
    [else (+ (first coefficients)
          (* x (eval-poly (rest coefficients) x)))]))


;; Tests for eval-poly
(check-within (eval-poly (cons 1.4 (cons 4 (cons 0
                         (cons 2 empty)))) 3) 67.4 0.0001)
(check-expect (eval-poly empty 10) 0)
(check-expect (eval-poly (cons 5 (cons 2 empty)) 7) 19)
(check-expect (eval-poly (cons 1 (cons 2 (cons 3 empty))) 0) 1)
(check-expect (eval-poly (cons 1/2 (cons -3 (cons 2 empty))) -2) 29/2)
(check-expect (eval-poly (cons 2 (cons -1 (cons 0 (cons 4 empty)))) 2) 32)
(check-within (eval-poly (cons 1/2 (cons -3
                         (cons 2 empty))) -2) 14.5 0.0001)
(check-within (eval-poly (cons 0 (cons 1 empty)) 2.5) 2.5 0.0001)
(check-within (eval-poly (cons 3 (cons -1 (cons 0
                                 (cons 4 empty)))) 2) 33.0 0.0001)
(check-within (eval-poly (cons -1.2 (cons 0.75 empty)) 4) 1.8 0.0001)
(check-within (eval-poly (cons 2.0 (cons -1.25
                                   (cons 0.5 empty))) -0.5) 2.75 0.0001)
(check-within (eval-poly empty 7) 0.0 0.0001)
(check-within (eval-poly (cons 10.0 empty) -12345) 10.0 0.0001)
(check-within (eval-poly (cons -0.3 (cons 0.2 (cons -0.1
                                    (cons 0.05 (cons -0.01 empty))))) 5) -1.8 0.0001)
