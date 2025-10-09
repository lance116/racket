(define distance-to-origin p)
    (sqrt (+ (sqr (first p)) (sqr (second p))))


(check-expect (distance-to-origin (list 3 4)) 5)