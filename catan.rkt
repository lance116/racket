;; ***************************************************
;; Lance Yan (21179582)
;; CS 135 Fall 2025
;; Assignment 3
;; ***************************************************

;; this function creates an inventory list with counts of each resource
(define (mk-inventory lumber brick grain wool ore)
  (cons lumber (cons brick (cons grain (cons wool (cons ore empty))))))

;; helper functions to extract resources from inventory
(define (get-lumber inv) (first inv))
(define (get-brick inv) (first (rest inv)))
(define (get-grain inv) (first (rest (rest inv))))
(define (get-wool inv) (first (rest (rest (rest inv)))))
(define (get-ore inv) (first (rest (rest (rest (rest inv))))))

;; (b) count how many actions can be performed

;; this function counts how many of a given action can be performed based on inventory
(define (count-actions inv action)
  (cond [(symbol=? action 'road) (count-roads inv)]
        [(symbol=? action 'settlement) (count-settlements inv)]
        [(symbol=? action 'city) (count-cities inv)]
        [(symbol=? action 'card) (count-cards inv)]))

;; helper functions for counting each action type

(define (count-roads inv)
  (min (get-lumber inv) (get-brick inv)))
(define (count-settlements inv)
  (min (get-lumber inv) (get-brick inv) (get-grain inv) (get-wool inv)))
;; helper function for integer division (replacement for floor)
(define (int-div n d)
  (cond [(< n d) 0]
        [else (+ 1 (int-div (- n d) d))]))

(define (count-cities inv)
  (min (int-div (get-grain inv) 2) (int-div (get-ore inv) 3)))
(define (count-cards inv)
  (min (get-grain inv) (get-wool inv) (get-ore inv)))

;; (c) convert gold to maximize roads
(define (convert-gold-road inv gold)
  (convert-gold-helper inv gold (get-lumber inv) (get-brick inv)))

;; helper function for gold conversion
(define (convert-gold-helper inv gold current-lumber current-brick)
  (balance-and-distribute inv gold current-lumber current-brick 
                         (max 0 (- current-brick current-lumber))
                         (max 0 (- current-lumber current-brick))))

;; balance resources then distribute remaining gold
(define (balance-and-distribute inv gold lumber brick lumber-deficit brick-deficit)
  (distribute-remaining inv gold lumber brick lumber-deficit brick-deficit
                       (min gold (+ lumber-deficit brick-deficit))))

;; distribute remaining gold after balancing
(define (distribute-remaining inv gold lumber brick lumber-deficit brick-deficit gold-to-balance)
  (finalize-inventory inv 
                     (+ lumber (min lumber-deficit gold-to-balance))
                     (+ brick (min brick-deficit (- gold-to-balance lumber-deficit)))
                     (- gold gold-to-balance)))

;; create final inventory with distributed gold
(define (finalize-inventory inv new-lumber new-brick remaining-gold)
  (mk-inventory (+ new-lumber (int-div remaining-gold 2))
                (+ new-brick (- remaining-gold (int-div remaining-gold 2)))
                (get-grain inv)
                (get-wool inv)
                (get-ore inv)))

;; test data
(define demo-inv (mk-inventory 3 5 9 7 30))

;; tests for mk-inventory and helper functions
(check-expect (mk-inventory 1 2 3 4 5) (cons 1 (cons 2 (cons 3 (cons 4 (cons 5 empty))))))
(check-expect (get-lumber demo-inv) 3)
(check-expect (get-brick demo-inv) 5)
(check-expect (get-grain demo-inv) 9)
(check-expect (get-wool demo-inv) 7)
(check-expect (get-ore demo-inv) 30)

;; tests for count-actions
(check-expect (count-actions demo-inv 'road) 3)      
(check-expect (count-actions demo-inv 'settlement) 3)  
(check-expect (count-actions demo-inv 'city) 4)       
(check-expect (count-actions demo-inv 'card) 7)        

;; additional tests for count-actions
(check-expect (count-actions (mk-inventory 0 0 0 0 0) 'road) 0)
(check-expect (count-actions (mk-inventory 10 5 0 0 0) 'road) 5)
(check-expect (count-actions (mk-inventory 1 1 1 1 1) 'settlement) 1)
(check-expect (count-actions (mk-inventory 0 0 6 0 9) 'city) 3)

;; tests for convert-gold-road
(check-expect (convert-gold-road (mk-inventory 0 0 1 2 3) 10) 
              (mk-inventory 5 5 1 2 3))
(check-expect (convert-gold-road (mk-inventory 0 20 6 5 4) 10) 
              (mk-inventory 10 20 6 5 4))

;; additional tests for convert-gold-road
(check-expect (convert-gold-road (mk-inventory 2 5 0 0 0) 4) 
              (mk-inventory 4 7 0 0 0))  
(check-expect (convert-gold-road (mk-inventory 5 2 0 0 0) 5) 
              (mk-inventory 6 5 0 0 0))  