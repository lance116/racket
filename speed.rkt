;; ***************************************************
;; Lance Yan (21179582)
;; CS 135 Fall 2025
;; Assignment #2
;; ***************************************************




;; a) 
;; Constants for conversions
(define METRES-PER-MILE 1609.344)
(define SECONDS-PER-HOUR 3600)
(define METRES-PER-SMOOT 1.7018)
(define SECONDS-PER-MILLIFORTNIGHT 1209.6)

;; (m/s->mph speed) converts speed from metres per second to miles per hour
(define (m/s->mph speed)
  (* speed (/ SECONDS-PER-HOUR METRES-PER-MILE)))


;;Tests for m/s->mph
(check-expect (m/s->mph 1609.344) 3600)
(check-expect (m/s->mph 0) 0)

;; b)
;; (mph->s/mfn speed) converts speed from miles per hour to Smoots per millifortnight
(define (mph->s/mfn speed)
  (* speed 
     (/ METRES-PER-MILE SECONDS-PER-HOUR)  ; convert mph to m/s first
     (/ SECONDS-PER-MILLIFORTNIGHT METRES-PER-SMOOT)))  ; then convert m/s to S/mfn

;; Tests for mph->s/mfn 
(check-expect (mph->s/mfn 1) (/ 3171249 1675))
(check-expect (mph->s/mfn 0) 0)
