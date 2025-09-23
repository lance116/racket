;; ***************************************************
;; Lance Yan (21179582)
;; CS 135 Fall 2025
;; Assignment #1
;; ***************************************************


;; Question 1 Part A
;; This function computes the Manhattan distance between two points (x1, y1) and (x2, y2).
(define (manhattan-distance x1 y1 x2 y2) (+ (abs (- x1 x2)) (abs (- y1 y2))))


;; Question 1 Part B
;; This function computes the slugging average of a baseball player.
(define (batter-slugging-average s d t hr ab) (/ (+ s (* 2 d) (* 3 t) (* 4 hr)) ab))

;; Question 1 Part C
;; This function computes the surface area of a cone given its radius r and height h.
(define (cone-area r h) (* (* pi r) (sqrt (+ (sqr r) (sqr h)))))

;; Question 1 Part D
;; This function computes the escape velocity from a celestial body given its mass M and radius r.
(define G 6.67430e-11) ; gravitational constant in m^3 kg^-1 s^-2
(define (escape M r) (sqrt (/ (* 2 G M) r))) ; escape velocity in m/s

