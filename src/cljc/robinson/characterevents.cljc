;; Functions for describing character events for npcs and the player.
(ns robinson.characterevents)

(defprotocol CharacterEvents
  (on-successful-attack [this state])
  (on-missed-attack [this state])
  (on-death [this state])
  (on-tick [this state]))

;; namespace with only a protocol gets optimized out, causing missing dependencies.
;; add a dummp def to prevent this ns from being optimized away.
#?(:cljs
(def x 1))
