;; ***************************************************
;; Lance Yan (21179582)
;; CS 135 Fall 2025
;; Assignment
;; ***************************************************

;; A Set is a (listof Sym) with unique symbols

;; (a) in?: Sym Set -> Bool
(define (in? element set)
  (cond
    [(empty? set) false]
    [(symbol=? element (first set)) true]
    [else (in? element (rest set))]))

;; Tests for in?
(check-expect (in? 'apple empty) false)
(check-expect (in? 'red (list 'red 'green 'blue)) true)
(check-expect (in? 'yellow (list 'red 'green 'blue)) false)
(check-expect (in? 'pizza (list 'blue 'pizza 'idea)) true)
(check-expect (in? 'car (list 'blue 'pizza 'idea)) false)


;; (b) add: Sym Set -> Set
(define (add element set)
  (cond
    [(in? element set) set]
    [else (cons element set)]))

;; Tests for add
(check-expect (add 'apple empty) (list 'apple))
(check-expect (add 'red (list 'red 'green 'blue)) (list 'red 'green 'blue))
(check-expect (add 'yellow (list 'red 'green 'blue)) (list 'yellow 'red 'green 'blue))
(check-expect (add 'pizza (list 'blue 'pizza 'idea)) (list 'blue 'pizza 'idea))
(check-expect (add 'car (list 'blue 'pizza 'idea)) (list 'car 'blue 'pizza 'idea))


;; (c) union: Set Set -> Set
(define (union set1 set2)
  (cond
    [(empty? set1) set2]
    [else (add (first set1) (union (rest set1) set2))]))

;; Tests for union
(check-expect (union empty empty) empty)
(check-expect (union (list 'a 'b) empty) (list 'a 'b))
(check-expect (union empty (list 'x 'y)) (list 'x 'y))
(check-expect (union (list 'a 'b) (list 'b 'c)) (list 'a 'b 'c))
(check-expect (union (list 'red 'blue) (list 'green 'red)) (list 'blue 'green 'red))


;; (d) intersection: Set Set -> Set
(define (intersection set1 set2)
  (cond
    [(empty? set1) empty]
    [(in? (first set1) set2) (cons (first set1) (intersection (rest set1) set2))]
    [else (intersection (rest set1) set2)]))

;; Tests for intersection
(check-expect (intersection empty empty) empty)
(check-expect (intersection (list 'a 'b) empty) empty)
(check-expect (intersection empty (list 'x 'y)) empty)
(check-expect (intersection (list 'a 'b) (list 'b 'c)) (list 'b))
(check-expect (intersection (list 'red 'blue) (list 'green 'red)) (list 'red))
(check-expect (intersection (list 'a 'b 'c) (list 'd 'e 'f)) empty)


;; (e) set=?: Set Set -> Bool
(define (set=? set1 set2)
  (and (subset? set1 set2) (subset? set2 set1)))

;; subset?: Set Set -> Bool
(define (subset? set1 set2)
  (cond
    [(empty? set1) true]
    [(in? (first set1) set2) (subset? (rest set1) set2)]
    [else false]))

;; Tests for set=?
(check-expect (set=? empty empty) true)
(check-expect (set=? (list 'a) (list 'a)) true)
(check-expect (set=? (list 'a 'b) (list 'b 'a)) true)
(check-expect (set=? (list 'red 'green 'blue) (list 'blue 'red 'green)) true)
(check-expect (set=? (list 'a 'b) (list 'a 'b 'c)) false)
(check-expect (set=? (list 'a 'b 'c) (list 'a 'b)) false)
(check-expect (set=? (list 'x 'y) (list 'a 'b)) false)


;; (f) set?: Any -> Bool
(define (set? obj)
  (and (list? obj) (all-symbols? obj) (no-duplicates? obj)))

;; all-symbols?: (listof Any) -> Bool
(define (all-symbols? lst)
  (cond
    [(empty? lst) true]
    [(symbol? (first lst)) (all-symbols? (rest lst))]
    [else false]))

;; no-duplicates?: (listof Sym) -> Bool
(define (no-duplicates? lst)
  (cond
    [(empty? lst) true]
    [(in? (first lst) (rest lst)) false]
    [else (no-duplicates? (rest lst))]))

;; Tests for set?
(check-expect (set? empty) true)
(check-expect (set? (list 'blue 'pizza 'idea)) true)
(check-expect (set? (list 'blue 'pizza 'blue)) false)
(check-expect (set? (list 'blue 1 'idea)) false)
(check-expect (set? (list 'red 'green 'blue)) true)
(check-expect (set? "not a list") false)
(check-expect (set? 42) false)
(check-expect (set? (list)) true)