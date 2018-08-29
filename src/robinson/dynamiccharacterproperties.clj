;; Functions for accessing character values that may change based on circumstance.
(ns robinson.dynamiccharacterproperties)

(defprotocol DynamicCharacterProperties
  (get-energy [this state])
  (get-speed [this state])
  (get-size [this state])
  (get-strength [this state])
  (get-dexterity [this state])
  (get-toughness [this state]))

