;; ***************************************************
;; Lance Yan (21179582)
;; CS 135 Fall 2025
;; Assignment 04
;; ***************************************************
;;

;;
;; Question 4. Part a.
;;

;; Spell costs (for reference):
;; 'light: 0, 'mage-hand: 0, 'magic-missile: 1, 'shield: 1
;; 'fireball: 2, 'invisibility: 2, 'teleport: 3, 'meteor-swarm: 3

;; A Spell is represented by one of the spell symbols from the table above
;; A Spell is (anyof 'light 'mage-hand 'magic-missile 'shield
;; 'fireball 'invisibility 'teleport 'meteor-swarm)

;; A Spellbook is a list of spells the wizard will memorize
;; A Spellbook is one of:
;; * empty
;; * (cons Spell Spellbook)

;;
;; Question 4. Part b.
;;

;; valid-spellbook? consumes two arguments: available-slots and spellbook
;; produces true if the wizard has enough spell slots to memorize all the spells
;; in their spellbook, false otherwise
;; valid-spellbook?: Natural Spellbook -> Boolean

(define (valid-spellbook? available-slots spellbook)
  (cond
    [(empty? spellbook) true]
    [(symbol=? (first spellbook) 'light)
     (valid-spellbook? available-slots (rest spellbook))]
    [(symbol=? (first spellbook) 'mage-hand)
     (valid-spellbook? available-slots (rest spellbook))]
    [(symbol=? (first spellbook) 'magic-missile)
     (cond
       [(>= available-slots 1)
        (valid-spellbook? (- available-slots 1) (rest spellbook))]
       [else false])]
    [(symbol=? (first spellbook) 'shield)
     (cond
       [(>= available-slots 1)
        (valid-spellbook? (- available-slots 1) (rest spellbook))]
       [else false])]
    [(symbol=? (first spellbook) 'fireball)
     (cond
       [(>= available-slots 2)
        (valid-spellbook? (- available-slots 2) (rest spellbook))]
       [else false])]
    [(symbol=? (first spellbook) 'invisibility)
     (cond
       [(>= available-slots 2)
        (valid-spellbook? (- available-slots 2) (rest spellbook))]
       [else false])]
    [(symbol=? (first spellbook) 'teleport)
     (cond
       [(>= available-slots 3)
        (valid-spellbook? (- available-slots 3) (rest spellbook))]
       [else false])]
    [(symbol=? (first spellbook) 'meteor-swarm)
     (cond
       [(>= available-slots 3)
        (valid-spellbook? (- available-slots 3) (rest spellbook))]
       [else false])]))

;; Tests for valid-spellbook?

(check-expect (valid-spellbook? 3 (cons 'magic-missile (cons 'shield (cons 'light empty)))) true)
(check-expect (valid-spellbook? 2 (cons 'fireball (cons 'invisibility empty))) false)
(check-expect (valid-spellbook? 1 empty) true)
(check-expect (valid-spellbook? 5 (cons 'fireball (cons 'light (cons 'light empty)))) true)
(check-expect (valid-spellbook? 0 (cons 'light (cons 'mage-hand empty))) true)
(check-expect (valid-spellbook? 0 (cons 'magic-missile empty)) false)
(check-expect (valid-spellbook? 2 (cons 'shield (cons 'shield empty))) true)
(check-expect (valid-spellbook? 2 (cons 'magic-missile
                                  (cons 'shield
                                    (cons 'magic-missile empty)))) false)
(check-expect (valid-spellbook? 3 (cons 'fireball (cons 'shield empty))) true)
(check-expect (valid-spellbook? 3 (cons 'teleport empty)) true)
(check-expect (valid-spellbook? 3 (cons 'teleport (cons 'magic-missile empty))) false)
(check-expect (valid-spellbook? 3 (cons 'teleport
                                  (cons 'light (cons 'mage-hand empty)))) true)
(check-expect (valid-spellbook? 4 (cons 'invisibility (cons 'fireball empty))) true)
(check-expect (valid-spellbook? 4 (cons 'invisibility (cons 'invisibility empty))) true)
(check-expect (valid-spellbook? 6 (cons 'meteor-swarm (cons 'teleport empty))) true)
(check-expect (valid-spellbook? 6 (cons 'meteor-swarm (cons 'meteor-swarm empty))) true)
(check-expect (valid-spellbook? 6
              (cons 'magic-missile
                (cons 'magic-missile
                  (cons 'magic-missile
                    (cons 'magic-missile
                      (cons 'magic-missile
                        (cons 'magic-missile empty))))))) true)
(check-expect (valid-spellbook? 7
              (cons 'teleport
                (cons 'fireball
                  (cons 'shield
                    (cons 'magic-missile
                      (cons 'light empty)))))) true)
(check-expect (valid-spellbook? 1 (cons 'shield empty)) true)


