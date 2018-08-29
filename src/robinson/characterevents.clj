;; Functions for describing character events for npcs and the player.
(ns robinson.characterevents)

(defprotocol CharacterEvents
  (on-successful-attack [this state])
  (on-missed-attack [this state])
  (on-hit [this state])
  (on-death [this state])
  (on-tick [this state]))

