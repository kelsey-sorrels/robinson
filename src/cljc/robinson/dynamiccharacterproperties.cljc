;; Functions for accessing character values that may change based on circumstance.
(ns robinson.dynamiccharacterproperties)

(defprotocol DynamicCharacterProperties
  (get-energy [this state])
  (get-speed [this state])
  (get-size [this state])
  (get-strength [this state])
  (get-toughness [this state]))

;; namespace with only a protocol gets optimized out, causing missing dependencies.
;; add a dummp def to prevent this ns from being optimized away.
#?(:cljs
(def x 1))
