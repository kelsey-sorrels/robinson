(ns robinson.actor-protocol)

(defprotocol Actor
  (receive [this state]))

