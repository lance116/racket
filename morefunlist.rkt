;; *******************************************;; Tests for count-down
(check-expect (count-down 9) (cons 9 (cons 8 (cons 7 (cons 6 (cons 5
              (cons 4 (cons 3 (cons 2 (cons 1 (cons 0 empty)))))))))))
(check-expect (count-down 0) (cons 0 empty))
(check-expect (count-down 1) (cons 1 (cons 0 empty)))
(check-expect (count-down 3) (cons 3 (cons 2 (cons 1 (cons 0 empty)))))
(check-expect (count-down 5) (cons 5 (cons 4 (cons 3 (cons 2 (cons 1
              (cons 0 empty))))))))**
;; Lance Yan (21179582)
;; CS 135 Fall 2025
;; Assignment 04
;; ***************************************************
;;

;;
;; Question 3. Part a.
;;

;; negate consumes a list of numbers and produces a new list 
;; where each number has been negated
;; negate : (listof Number) -> (listof Number)

(define (negate lst)
  (cond
    [(empty? lst) lst]
    [else (cons (- (first lst)) (negate (rest lst)))]))

;; Tests for negate
(check-expect (negate (cons 100 (cons -10 (cons 0 empty))))
              (cons -100 (cons 10 (cons 0 empty))))
(check-expect (negate empty) empty)
(check-expect (negate (cons 5 empty)) (cons -5 empty))
(check-expect (negate (cons -7 empty)) (cons 7 empty))
(check-expect (negate (cons 1/2 (cons -3/4 empty)))
              (cons -1/2 (cons 3/4 empty)))
(check-expect (negate (cons 3.5 (cons 0 (cons -2.25 empty))))
              (cons -3.5 (cons 0 (cons 2.25 empty))))
(check-expect (negate (cons 42 (cons -17 (cons 99 empty))))
              (cons -42 (cons 17 (cons -99 empty))))

;;
;; Question 3. Part b.
;;

;; count-down consumes a natural number and produces a list of 
;; natural numbers counting down from that number to 0 (inclusive)
;; count-down: Natural -> (listof Natural)

(define (count-down n)
  (cond
    [(= n 0) (cons 0 empty)]
    [else (cons n (count-down (- n 1)))]))

;; test cases for question 3 part b
(check-expect (count-down 0) (cons 0 empty))
(check-expect (count-down 1) (cons 1 (cons 0 empty)))
(check-expect (count-down 3) (cons 3 (cons 2 (cons 1 (cons 0 empty)))))
(check-expect (count-down 9) (cons 9 (cons 8 (cons 7 (cons 6 (cons 5
              (cons 4 (cons 3 (cons 2 (cons 1 (cons 0 empty)))))))))))

;;
;; Question 3. Part c.
;;

;; add-constant consumes a number and a list of numbers, and 
;; produces a new list where the given number has been added to each element of the list
;; add-constant: Number (listof Number) -> (listof Number)

(define (add-constant n lst)
  (cond
    [(empty? lst) empty]
    [else (cons (+ n (first lst)) (add-constant n (rest lst)))]))

;; Tests for add-constant
(check-expect (add-constant 1/2 (cons 10 (cons 4 empty)))
              (cons 21/2 (cons 9/2 empty)))
(check-expect (add-constant 5 empty) empty)
(check-expect (add-constant 0 (cons 1 (cons 2 (cons 3 empty))))
              (cons 1 (cons 2 (cons 3 empty))))
(check-expect (add-constant -3 (cons 0 (cons 3 (cons -3 empty))))
              (cons -3 (cons 0 (cons -6 empty))))
(check-expect (add-constant 10 (cons -5 (cons -15 (cons -25 empty))))
              (cons 5 (cons -5 (cons -15 empty))))
(check-expect (add-constant 2.5 (cons 1.5 (cons 0 (cons -1.5 empty))))
              (cons 4.0 (cons 2.5 (cons 1.0 empty))))

;;
;; Question 3. Part d.
;;

;; count-up consumes a natural number and produces a list of natural 
;; numbers counting up from 0 to that number (inclusive)
;; You must use only the three functions you wrote above (negate, count-down, and add-constant)
;; count-up: Natural -> (listof Natural)

(define (count-up n)
  (add-constant n (negate (count-down n))))

;; Tests for count-up

(check-expect (count-up 4) (cons 0 (cons 1 (cons 2 (cons 3 (cons 4 empty))))))
(check-expect (count-up 0) (cons 0 empty))
(check-expect (count-up 1) (cons 0 (cons 1 empty)))
(check-expect (count-up 2) (cons 0 (cons 1 (cons 2 empty))))
(check-expect (count-up 5) (cons 0 (cons 1 (cons 2 (cons 3 (cons 4 (cons 5 empty)))))))
