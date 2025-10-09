;; ***************************************************
;; Lance Yan (21179582)
;; CS 135 Fall 2025
;; Assignment
;; ***************************************************

;; Original slow function (for reference):
;; count-types: (listof Any) -> (list Nat Nat)
;; (define (count-types lst)
;;   (cond [(empty? lst) (list 0 0)]
;;         [(symbol? (first lst))
;;          (list (add1 (first (count-types (rest lst))))
;;                (second (count-types (rest lst))))]
;;         [else (list (first (count-types (rest lst)))
;;                     (add1 (second (count-types (rest lst)))))]))

;; count-types-fast: (listof Any) -> (list Nat Nat)
(define (count-types-fast lst)
  (count-helper lst 0 0))

;; count-helper: (listof Any) Nat Nat -> (list Nat Nat)
(define (count-helper lst symbol-count non-symbol-count)
  (cond
    [(empty? lst) (list symbol-count non-symbol-count)]
    [(symbol? (first lst))
     (count-helper (rest lst) (add1 symbol-count) non-symbol-count)]
    [else
     (count-helper (rest lst) symbol-count (add1 non-symbol-count))]))

;; alt-test: Nat -> (listof Any)
(define (alt-test n)
  (cond
    [(zero? n) empty]
    [else (cons 'a (cons 1 (alt-test (sub1 n))))]))

;; Tests for count-types-fast
(check-expect (count-types-fast empty) (list 0 0))
(check-expect (count-types-fast (list 'a 'b 'c)) (list 3 0))
(check-expect (count-types-fast (list 1 2 3)) (list 0 3))
(check-expect (count-types-fast (list 'a 1 'b 2 'c 3)) (list 3 3))
(check-expect (count-types-fast (list 'symbol "string" 42 'another-symbol)) (list 2 2))
(check-expect (count-types-fast (list 'only-symbol)) (list 1 0))
(check-expect (count-types-fast (list "only-non-symbol")) (list 0 1))

;; Test with alt-test pattern
(check-expect (count-types-fast (alt-test 0)) (list 0 0))
(check-expect (count-types-fast (alt-test 1)) (list 1 1))
(check-expect (count-types-fast (alt-test 2)) (list 2 2))
(check-expect (count-types-fast (alt-test 5)) (list 5 5))


