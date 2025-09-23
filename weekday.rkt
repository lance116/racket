;; ***************************************************
;; Lance Yan (21179582)
;; CS 135 Fall 2025
;; Assignment #2
;; ***************************************************

;; this function takes an encoded date (YYYYMMDD) and returns the day of the week
(define (date->day-of-week encoded-date)
  (day-number->day-name
   (zeller-formula (extract-day encoded-date)
                   (adjust-month (extract-month encoded-date))
                   (adjust-year (extract-month encoded-date) (extract-year encoded-date)))))

;; helper functions for date parsing
(define (extract-day encoded-date)
  (modulo encoded-date 100))

(define (extract-month encoded-date)
  (modulo (floor (/ encoded-date 100)) 100))

(define (extract-year encoded-date)
  (floor (/ encoded-date 10000)))

;; adjust month for zeller's congruence (jan=13, feb=14 of previous year)
(define (adjust-month month)
  (cond [(= month 1) 13]
        [(= month 2) 14]
        [else month]))

;; adjust year for zeller's congruence
(define (adjust-year month year)
  (cond [(or (= month 1) (= month 2)) (- year 1)]
        [else year]))

;; zeller's congruence formula
(define (zeller-formula day month year)
  (modulo (+ day
             (floor (/ (* 13 (+ month 1)) 5))
             (modulo year 100)
             (floor (/ (modulo year 100) 4))
             (floor (/ (floor (/ year 100)) 4))
             (- (* 2 (floor (/ year 100)))))
          7))

;; convert zeller's result to day name
(define (day-number->day-name h)
  (cond [(= h 0) 'Saturday]
        [(= h 1) 'Sunday]
        [(= h 2) 'Monday]
        [(= h 3) 'Tuesday]
        [(= h 4) 'Wednesday]
        [(= h 5) 'Thursday]
        [(= h 6) 'Friday]))

;; tests
(check-expect (date->day-of-week 20240924) 'Tuesday)
(check-expect (date->day-of-week 38781202) 'Monday)
(check-expect (date->day-of-week 17530101) 'Monday)
(check-expect (date->day-of-week 20000101) 'Saturday)
(check-expect (date->day-of-week 19000101) 'Monday)