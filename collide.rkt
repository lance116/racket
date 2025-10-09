;; ***************************************************
;; Lance Yan (21179582)
;; CS 135 Fall 2025
;; Assignment 05
;; ***************************************************

;; (a) State: (list x y direction max-battery cur-battery)
;; A State is a 5-element list with coordinates, direction, and battery info

;; (b) Helper functions
(define (mk-state x y direction max-battery cur-battery)
  (list x y direction max-battery cur-battery))

(define (get-x state) (first state))
(define (get-y state) (first (rest state)))
(define (get-direction state) (first (rest (rest state))))
(define (get-max-battery state) (first (rest (rest (rest state)))))
(define (get-cur-battery state) (first (rest (rest (rest (rest state))))))

;; robot-ctl: State Command -> State
;; processes robot command with battery management
(define (robot-ctl state command)
  (cond
    [(= (get-cur-battery state) 0) 
     (mk-state (get-x state) (get-y state) (get-direction state)
               (get-max-battery state) (get-max-battery state))]
    [(symbol=? command 'turn-left) 
     (mk-state (get-x state) (get-y state) (turn-left-dir (get-direction state))
               (get-max-battery state) (- (get-cur-battery state) 1))]
    [(symbol=? command 'turn-right) 
     (mk-state (get-x state) (get-y state) (turn-right-dir (get-direction state))
               (get-max-battery state) (- (get-cur-battery state) 1))]
    [(symbol=? command 'forward) (move-forward-battery state)]
    [else state]))

(define (turn-left-dir direction)
  (cond [(symbol=? direction 'North) 'West]
        [(symbol=? direction 'West) 'South]
        [(symbol=? direction 'South) 'East]
        [(symbol=? direction 'East) 'North]))

(define (turn-right-dir direction)
  (cond [(symbol=? direction 'North) 'East]
        [(symbol=? direction 'East) 'South]
        [(symbol=? direction 'South) 'West]
        [(symbol=? direction 'West) 'North]))

(define (move-forward-battery state)
  (local [(define new-x (calculate-new-x (get-x state) (get-direction state)))
          (define new-y (calculate-new-y (get-y state) (get-direction state)))
          (define new-battery (- (get-cur-battery state) 1))]
    (cond
      [(and (>= new-x 0) (<= new-x 10) (>= new-y 0) (<= new-y 10))
       (mk-state new-x new-y (get-direction state) (get-max-battery state) new-battery)]
      [else
       (mk-state (get-x state) (get-y state) (get-direction state)
                 (get-max-battery state) new-battery)])))

(define (calculate-new-x x direction)
  (cond [(symbol=? direction 'East) (+ x 1)]
        [(symbol=? direction 'West) (- x 1)]
        [else x]))

(define (calculate-new-y y direction)
  (cond [(symbol=? direction 'North) (+ y 1)]
        [(symbol=? direction 'South) (- y 1)]
        [else y]))

;; Tests for robot-ctl
(check-expect (robot-ctl (mk-state 5 5 'North 3 2) 'turn-left) (mk-state 5 5 'West 3 1))
(check-expect (robot-ctl (mk-state 5 5 'North 3 2) 'forward) (mk-state 5 6 'North 3 1))
(check-expect (robot-ctl (mk-state 5 5 'North 3 0) 'forward) (mk-state 5 5 'North 3 3))

;; (c) collide?: (listof Command) (listof Command) State State -> Bool
;; checks if robots collide without battery constraints
(define (collide? commands0 commands1 state0 state1)
  (cond
    [(and (= (get-x state0) (get-x state1)) (= (get-y state0) (get-y state1))) true]
    [(and (empty? commands0) (empty? commands1)) false]
    [else
     (local [(define new-state0 (cond [(empty? commands0) state0]
                                      [else (robot-ctl-no-battery state0 (first commands0))]))
             (define new-state1 (cond [(empty? commands1) state1]
                                      [else (robot-ctl-no-battery state1 (first commands1))]))
             (define new-commands0 (cond [(empty? commands0) empty] [else (rest commands0)]))
             (define new-commands1 (cond [(empty? commands1) empty] [else (rest commands1)]))]
       (collide? new-commands0 new-commands1 new-state0 new-state1))]))

(define (robot-ctl-no-battery state command)
  (cond
    [(symbol=? command 'turn-left) 
     (mk-state (get-x state) (get-y state) (turn-left-dir (get-direction state))
               (get-max-battery state) (get-cur-battery state))]
    [(symbol=? command 'turn-right) 
     (mk-state (get-x state) (get-y state) (turn-right-dir (get-direction state))
               (get-max-battery state) (get-cur-battery state))]
    [(symbol=? command 'forward) (move-forward-no-battery state)]
    [else state]))

(define (move-forward-no-battery state)
  (local [(define new-x (calculate-new-x (get-x state) (get-direction state)))
          (define new-y (calculate-new-y (get-y state) (get-direction state)))]
    (cond
      [(and (>= new-x 0) (<= new-x 10) (>= new-y 0) (<= new-y 10))
       (mk-state new-x new-y (get-direction state) (get-max-battery state) (get-cur-battery state))]
      [else state])))

;; Tests for collide?
(check-expect (collide? empty empty (mk-state 3 2 'East 5 5) (mk-state 8 2 'West 3 3)) false)
(check-expect (collide? empty (list 'forward 'forward 'forward) 
                        (mk-state 3 2 'East 5 5) (mk-state 6 2 'West 3 3)) true)
(check-expect (collide? (list 'forward 'forward 'forward) empty 
                        (mk-state 3 2 'East 5 5) (mk-state 6 2 'West 3 3)) true)
(check-expect (collide? (list 'forward) (list 'forward) 
                        (mk-state 4 8 'South 5 5) (mk-state 6 7 'North 3 3)) false)
(check-expect (collide? (list 'turn-right) (list 'turn-left) 
                        (mk-state 0 0 'East 5 5) (mk-state 1 1 'West 3 3)) false)

;; (d) collide-b?: (listof Command) (listof Command) State State -> Bool
;; checks if robots collide with battery constraints
(define (collide-b? commands0 commands1 state0 state1)
  (cond
    [(and (= (get-x state0) (get-x state1)) (= (get-y state0) (get-y state1))) true]
    [(and (empty? commands0) (empty? commands1)) false]
    [else
     (local [(define result0 (execute-with-battery state0 commands0))
             (define result1 (execute-with-battery state1 commands1))]
       (collide-b? (second result0) (second result1) (first result0) (first result1)))]))

(define (execute-with-battery state commands)
  (cond
    [(empty? commands) (list state empty)]
    [(= (get-cur-battery state) 0)
     (list (mk-state (get-x state) (get-y state) (get-direction state)
                     (get-max-battery state) (get-max-battery state)) commands)]
    [else (list (robot-ctl state (first commands)) (rest commands))]))

;; Tests for collide-b?
(check-expect (collide-b? (list 'forward 'forward) (list 'forward 'forward 'forward) 
                          (mk-state 2 2 'North 1 1) (mk-state 4 4 'West 3 3)) false)
(check-expect (collide-b? (list 'forward 'forward) (list 'forward 'forward 'forward 'forward) 
                          (mk-state 2 2 'North 1 1) (mk-state 5 4 'West 3 3)) true)