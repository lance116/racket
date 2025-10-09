;; ***************************************************
;; Lance Yan (21179582)
;; CS 135 Fall 2025
;; Assignment
;; ***************************************************

;; (a) list-sublist: (listof Any) Nat Nat -> (listof Any)
(define (list-sublist lst m n)
  (list-sublist-helper lst m n 0))

;; list-sublist-helper: (listof Any) Nat Nat Nat -> (listof Any)
(define (list-sublist-helper lst m n current-index)
  (cond
    [(empty? lst) empty]
    [(< current-index m) 
     (list-sublist-helper (rest lst) m n (add1 current-index))]
    [(> current-index n) empty]
    [else 
     (cons (first lst) 
           (list-sublist-helper (rest lst) m n (add1 current-index)))]))

;; Tests for list-sublist
(check-expect (list-sublist (list 0 1 2 3 4 5) 2 4) (list 2 3 4))
(check-expect (list-sublist (list 'a 'b 'c 'd 'e) 0 2) (list 'a 'b 'c))
(check-expect (list-sublist (list 10 20 30 40) 1 1) (list 20))
(check-expect (list-sublist (list 1 2 3 4 5 6 7) 3 5) (list 4 5 6))
(check-expect (list-sublist (list 'x 'y 'z) 0 0) (list 'x))

;; (b) list-zipper: (listof Any) (listof Any) Nat Nat -> (listof Any)
(define (list-zipper lst1 lst2 m n)
  (list-zipper-helper lst1 lst2 m n true))

;; list-zipper-helper: (listof Any) (listof Any) Nat Nat Bool -> (listof Any)
(define (list-zipper-helper lst1 lst2 m n turn-lst1?)
  (cond
    [(and (empty? lst1) (empty? lst2)) empty]
    [(empty? lst1) lst2]
    [(empty? lst2) lst1]
    [turn-lst1? 
     (append (take-n lst1 m) 
             (list-zipper-helper (drop-n lst1 m) lst2 m n false))]
    [else 
     (append (take-n lst2 n) 
             (list-zipper-helper lst1 (drop-n lst2 n) m n true))]))

;; take-n: (listof Any) Nat -> (listof Any)
(define (take-n lst n)
  (cond
    [(or (empty? lst) (zero? n)) empty]
    [else (cons (first lst) (take-n (rest lst) (sub1 n)))]))

;; drop-n: (listof Any) Nat -> (listof Any)
(define (drop-n lst n)
  (cond
    [(or (empty? lst) (zero? n)) lst]
    [else (drop-n (rest lst) (sub1 n))]))

;; Tests for list-zipper
(check-expect (list-zipper (list 0 2 4 6 8 10) (list 1 3 5 7 9) 2 3) 
              (list 0 2 1 3 5 4 6 7 9 8 10))
(check-expect (list-zipper (list 'a 'b 'c) (list 1 2 3 4) 1 2) 
              (list 'a 1 2 'b 3 4 'c))
(check-expect (list-zipper (list 1 2 3 4) (list 'x 'y) 2 1) 
              (list 1 2 'x 3 4 'y))
(check-expect (list-zipper (list 1) (list 2) 1 1) 
              (list 1 2))

;; (c) list-insert-at: (listof Any) (listof Any) Nat -> (listof Any)
(define (list-insert-at lst1 lst2 n)
  (list-insert-helper lst1 lst2 n 0))

;; list-insert-helper: (listof Any) (listof Any) Nat Nat -> (listof Any)
(define (list-insert-helper lst1 lst2 n current-index)
  (cond
    [(= current-index n) (append lst2 lst1)]
    [(empty? lst1) lst2]
    [else (cons (first lst1) 
                (list-insert-helper (rest lst1) lst2 n (add1 current-index)))]))

;; Tests for list-insert-at
(check-expect (list-insert-at (list 0 2 4 6 8) (list 1 3 5) 2) 
              (list 0 2 1 3 5 4 6 8))
(check-expect (list-insert-at (list 'a 'b 'c) (list 'x 'y) 0) 
              (list 'x 'y 'a 'b 'c))
(check-expect (list-insert-at (list 1 2 3) (list 'inserted) 3) 
              (list 1 2 3 'inserted))
(check-expect (list-insert-at (list 1 2) (list 'a 'b) 5) 
              (list 1 2 'a 'b))  ; n beyond end, insert at end
(check-expect (list-insert-at empty (list 'x) 0) 
              (list 'x))

;; (d) list-swap: (listof Any) Nat Nat -> (listof Any)
(define (list-swap lst m n)
  (cond
    [(= m n) lst]
    [else (list-swap-helper lst m n 0)]))

;; list-swap-helper: (listof Any) Nat Nat Nat -> (listof Any)
(define (list-swap-helper lst m n current-index)
  (cond
    [(empty? lst) empty]
    [(= current-index m) 
     (cons (list-ref lst n) 
           (list-swap-helper (rest lst) m n (add1 current-index)))]
    [(= current-index n) 
     (cons (list-ref lst m) 
           (list-swap-helper (rest lst) m n (add1 current-index)))]
    [else 
     (cons (first lst) 
           (list-swap-helper (rest lst) m n (add1 current-index)))]))

;; Tests for list-swap
(check-expect (list-swap (list 0 1 2 3 4 5 6) 2 4) (list 0 1 4 3 2 5 6))
(check-expect (list-swap (list 0 1 2 3 4 5 6) 3 3) (list 0 1 2 3 4 5 6))
(check-expect (list-swap (list 'a 'b 'c 'd) 0 3) (list 'd 'b 'c 'a))
(check-expect (list-swap (list 1 2 3) 1 2) (list 1 3 2))
(check-expect (list-swap (list 'x 'y) 0 1) (list 'y 'x))
