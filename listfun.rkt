;; ***************************************************
;; Lance Yan (21179582)
;; CS 135 Fall 2025
;; Assignment 04
;; ***************************************************
;;

;; Question 2. Part A
;;

;; double-plus-one consumes a list of numbers and produces
;; a transformed list where each number is doubled, then 1 is added
;; double-plus-one : (listof Number) -> (listof Number)

(define (double-plus-one lst)
  (cond
    [(empty? lst) empty]
    [else (cons (+ (* 2 (first lst)) 1) 
                (double-plus-one (rest lst)))]))

;; Tests for double-plus-one
(check-expect (double-plus-one empty) empty)
(check-expect (double-plus-one (cons 5 (cons -3 (cons 0 (cons 12 empty)))))
              (cons 11 (cons -5 (cons 1 (cons 25 empty)))))
(check-expect (double-plus-one (cons 7 empty)) (cons 15 empty))
(check-expect (double-plus-one (cons -2 empty)) (cons -3 empty))
(check-expect (double-plus-one (cons 1.5 empty)) (cons 4 empty))
(check-expect (double-plus-one (cons 3 (cons 6 (cons 9 empty))))
              (cons 7 (cons 13 (cons 19 empty))))
(check-expect (double-plus-one (cons -4 (cons -8 (cons -12 empty))))
              (cons -7 (cons -15 (cons -23 empty))))
(check-expect (double-plus-one (cons 20 (cons -30 (cons 40 empty))))
              (cons 41 (cons -59 (cons 81 empty))))

;;
;; Question 2. Part B
;;

;; symbol-sandwich consumes a list of symbols and produces
;; a new list where each symbol is "sandwiched" between the symbol 'bread
;; if the consumed list is empty, symbol-sandwich produces empty
;; symbol-sandwich : (listof Symbol) -> (listof Symbol)

(define (symbol-sandwich lst)
  (cond
    [(empty? lst) empty]
    [(empty? (rest lst)) 
     (cons 'bread (cons (first lst) (cons 'bread empty)))]
    [else (cons 'bread 
                (cons (first lst) 
                      (symbol-sandwich (rest lst))))]))

;; Tests for symbol-sandwich

(check-expect (symbol-sandwich empty) empty)
(check-expect (symbol-sandwich (cons 'ham (cons 'cheese (cons 'lettuce empty))))
              (cons 'bread (cons 'ham (cons 'bread (cons 'cheese (cons 'bread
                                                   (cons 'lettuce (cons 'bread empty))))))))
(check-expect (symbol-sandwich (cons 'bread empty)) 
              (cons 'bread (cons 'bread (cons 'bread empty))))
(check-expect (symbol-sandwich (cons 'turkey (cons 'mustard empty)))
              (cons 'bread (cons 'turkey (cons 'bread (cons 'mustard (cons 'bread empty))))))
(check-expect (symbol-sandwich (cons 'pickle empty)) 
              (cons 'bread (cons 'pickle (cons 'bread empty))))


;;
;; Question 2. Part C
;;

;; shuffle-rock-paper-lizard-spock consumes a list of symbols and produces a new list where:
;; every 'rock is changed to 'paper, every 'paper is changed to 'lizard,
;; every 'lizard is changed to 'rock, and every 'spock is removed
;; shuffle-rock-paper-lizard-spock : (listof Symbol) -> (listof Symbol)

(define (shuffle-rock-paper-lizard-spock lst)
  (cond
    [(empty? lst) empty]
    [(symbol=? (first lst) 'rock)
     (cons 'paper (shuffle-rock-paper-lizard-spock (rest lst)))]
    [(symbol=? (first lst) 'paper)
     (cons 'lizard (shuffle-rock-paper-lizard-spock (rest lst)))]
    [(symbol=? (first lst) 'lizard)
     (cons 'rock (shuffle-rock-paper-lizard-spock (rest lst)))]
    [(symbol=? (first lst) 'spock)
     (shuffle-rock-paper-lizard-spock (rest lst))]
    [else
     (cons (first lst) 
           (shuffle-rock-paper-lizard-spock (rest lst)))]))

;; Tests for shuffle-rock-paper-lizard-spock

(check-expect (shuffle-rock-paper-lizard-spock empty) empty)
(check-expect (shuffle-rock-paper-lizard-spock (cons 'rock (cons 'paper (cons 'lizard (cons 'spock (cons 'therock empty))))))
              (cons 'paper (cons 'lizard (cons 'rock (cons 'therock empty)))))                     
(check-expect (shuffle-rock-paper-lizard-spock (cons 'spock empty)) empty)     
(check-expect (shuffle-rock-paper-lizard-spock (cons 'rock empty))
              (cons 'paper empty))                                               
(check-expect (shuffle-rock-paper-lizard-spock (cons 'paper empty))
              (cons 'lizard empty))                                          
(check-expect (shuffle-rock-paper-lizard-spock (cons 'lizard empty))
              (cons 'rock empty))                                                
(check-expect (shuffle-rock-paper-lizard-spock (cons 'scissors empty))
              (cons 'scissors empty))                                           
(check-expect (shuffle-rock-paper-lizard-spock
               (cons 'spock (cons 'spock (cons 'spock empty))))
              empty)
(check-expect (shuffle-rock-paper-lizard-spock
              (cons 'spock (cons 'rock (cons 'spock empty))))(cons 'paper empty))
(check-expect (shuffle-rock-paper-lizard-spock
              (cons 'rock (cons 'spock (cons 'paper empty))))
              (cons 'paper (cons 'lizard empty)))

(check-expect (shuffle-rock-paper-lizard-spock(cons 'rock
              (cons 'therock (cons 'spock (cons 'paper (cons 'lizard
              (cons 'foo empty)))))))(cons 'paper(cons 'therock
              (cons 'lizard(cons 'rock(cons 'foo empty))))))

(check-expect (shuffle-rock-paper-lizard-spock (cons 'rock-paper (cons 'lizard empty)))
              (cons 'rock-paper (cons 'rock empty)))

    
    