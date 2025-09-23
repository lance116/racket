;; ***************************************************
;; Lance Yan (21179582)
;; CS 135 Fall 2025
;; Assignment #2
;; ***************************************************



;; this function checks if a donor blood type can donate to a recipient blood type using cond statements
(define (can-donate-to/cond? donor recipient)
  (cond [(symbol=? donor 'O-) true]
        [(symbol=? recipient 'AB+) true]
        [(symbol=? donor recipient) true]
        [(symbol=? donor 'O+)
         (cond [(symbol=? recipient 'A+) true]
               [(symbol=? recipient 'B+) true]
               [(symbol=? recipient 'AB-) true]
               [else false])]
        [(symbol=? donor 'A-)
         (cond [(symbol=? recipient 'A+) true]
               [(symbol=? recipient 'AB-) true]
               [else false])]
        [(symbol=? donor 'A+)
         (cond [(symbol=? recipient 'AB+) true]
               [else false])]
        [(symbol=? donor 'B-) 
         (cond [(symbol=? recipient 'B+) true]
               [(symbol=? recipient 'AB-) true]
               [else false])]
        [(symbol=? donor 'B+) 
         (cond [(symbol=? recipient 'AB+) true]
               [else false])]
        [else false]))


;; this version can use and/or which makes it cleaner
(define (can-donate-to/bool? donor recipient)
  (or (symbol=? donor 'O-)
      (symbol=? recipient 'AB+)
      (symbol=? donor recipient)
      (and (symbol=? donor 'O+) 
           (or (symbol=? recipient 'A+) (symbol=? recipient 'B+) (symbol=? recipient 'AB-)))
      (and (symbol=? donor 'A-) 
           (or (symbol=? recipient 'A+) (symbol=? recipient 'AB-)))
      (and (symbol=? donor 'A+) (symbol=? recipient 'AB+))
      (and (symbol=? donor 'B-) 
           (or (symbol=? recipient 'B+) (symbol=? recipient 'AB-)))
      (and (symbol=? donor 'B+) (symbol=? recipient 'AB+))))

;; some tests
(check-expect (can-donate-to/cond? 'O- 'A+) true)
(check-expect (can-donate-to/cond? 'B+ 'AB+) true)
(check-expect (can-donate-to/cond? 'A+ 'A+) true)
(check-expect (can-donate-to/cond? 'A+ 'B-) false)
(check-expect (can-donate-to/cond? 'O+ 'A+) true)
(check-expect (can-donate-to/cond? 'A- 'AB-) true)

(check-expect (can-donate-to/bool? 'O- 'A+) true)
(check-expect (can-donate-to/bool? 'B+ 'AB+) true)
(check-expect (can-donate-to/bool? 'A+ 'A+) true)
(check-expect (can-donate-to/bool? 'A+ 'B-) false)