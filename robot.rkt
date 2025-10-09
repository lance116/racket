;; ***************************************************
;; Lance Yan (21179582)
;; CS 135 Fall 2025
;; Assignment 3
;; ***************************************************

;; (a) data definitions
(define NORTH 'North)
(define SOUTH 'South)
(define EAST 'East)
(define WEST 'West)
(define FORWARD 'forward)
(define TURN-LEFT 'turn-left)
(define TURN-RIGHT 'turn-right)

;; (b) helper functions for state

;; function to create a robot state
(define (mk-state x y direction)
  (cons x (cons y (cons direction empty))))

;; function to extract x coordinate
(define (get-x state)
  (first state))

;; function to extract y coordinate
(define (get-y state)
  (first (rest state)))

;; function to extract direction
(define (get-direction state)
  (first (rest (rest state))))

;; (c) robot control function

;; function to control the robot based on command
(define (robot-ctl state command)
  (cond [(symbol=? command TURN-LEFT) (turn-left state)]
        [(symbol=? command TURN-RIGHT) (turn-right state)]
        [(symbol=? command FORWARD) (move-forward state)]
        [else state]))

;; helper functions for robot control
(define (turn-left state)
  (mk-state (get-x state) 
            (get-y state)
            (cond [(symbol=? (get-direction state) NORTH) WEST]
                  [(symbol=? (get-direction state) WEST) SOUTH]
                  [(symbol=? (get-direction state) SOUTH) EAST]
                  [(symbol=? (get-direction state) EAST) NORTH])))

;; function to turn right
(define (turn-right state)
  (mk-state (get-x state) 
            (get-y state)
            (cond [(symbol=? (get-direction state) NORTH) EAST]
                  [(symbol=? (get-direction state) EAST) SOUTH]
                  [(symbol=? (get-direction state) SOUTH) WEST]
                  [(symbol=? (get-direction state) WEST) NORTH])))

;; function that moves the robot forward one unit in its current direction
(define (move-forward state)
  (move-forward-helper state (get-x state) (get-y state) (get-direction state)))

;; helper function for move-forward
(define (move-forward-helper state current-x current-y direction)
  (calculate-new-position state current-x current-y direction 
                         (calculate-new-x current-x direction)
                         (calculate-new-y current-y direction)))

;; calculate new x coordinate based on direction
(define (calculate-new-x current-x direction)
  (cond [(symbol=? direction EAST) (+ current-x 1)]
        [(symbol=? direction WEST) (- current-x 1)]
        [else current-x]))

;; calculate new y coordinate based on direction  
(define (calculate-new-y current-y direction)
  (cond [(symbol=? direction NORTH) (+ current-y 1)]
        [(symbol=? direction SOUTH) (- current-y 1)]
        [else current-y]))

;; check bounds and create new state or return original
(define (calculate-new-position state current-x current-y direction new-x new-y)
  (cond [(and (>= new-x 0) (<= new-x 10) (>= new-y 0) (<= new-y 10))
         (mk-state new-x new-y direction)]
        [else state]))  



;; tests

;; test helper functions
(check-expect (mk-state 5 3 NORTH) (cons 5 (cons 3 (cons 'North empty))))
(check-expect (get-x (cons 5 (cons 3 (cons 'North empty)))) 5)
(check-expect (get-y (cons 5 (cons 3 (cons 'North empty)))) 3)
(check-expect (get-direction (cons 5 (cons 3 (cons 'North empty)))) 'North)

;; test turning
(check-expect (robot-ctl (mk-state 5 5 NORTH) TURN-LEFT) (mk-state 5 5 WEST))
(check-expect (robot-ctl (mk-state 5 5 NORTH) TURN-RIGHT) (mk-state 5 5 EAST))
(check-expect (robot-ctl (mk-state 5 5 WEST) TURN-LEFT) (mk-state 5 5 SOUTH))
(check-expect (robot-ctl (mk-state 5 5 SOUTH) TURN-LEFT) (mk-state 5 5 EAST))

;; test forward movement (normal cases)
(check-expect (robot-ctl (mk-state 5 5 NORTH) FORWARD) (mk-state 5 6 NORTH))
(check-expect (robot-ctl (mk-state 5 5 SOUTH) FORWARD) (mk-state 5 4 SOUTH))
(check-expect (robot-ctl (mk-state 5 5 EAST) FORWARD) (mk-state 6 5 EAST))
(check-expect (robot-ctl (mk-state 5 5 WEST) FORWARD) (mk-state 4 5 WEST))

;; test boundary cases (robot should not move out of bounds)
(check-expect (robot-ctl (mk-state 0 5 WEST) FORWARD) (mk-state 0 5 WEST)) 
(check-expect (robot-ctl (mk-state 10 5 EAST) FORWARD) (mk-state 10 5 EAST))
(check-expect (robot-ctl (mk-state 5 0 SOUTH) FORWARD) (mk-state 5 0 SOUTH)) 
(check-expect (robot-ctl (mk-state 5 10 NORTH) FORWARD) (mk-state 5 10 NORTH)) 

;; test corner cases
(check-expect (robot-ctl (mk-state 10 10 NORTH) FORWARD) (mk-state 10 10 NORTH))
(check-expect (robot-ctl (mk-state 0 0 SOUTH) FORWARD) (mk-state 0 0 SOUTH))
