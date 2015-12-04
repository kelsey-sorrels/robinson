;; Utility functions and functions for manipulating trap state
(ns robinson.traps
  (:require 
            [robinson.world :as rw]
            [taoensso.timbre :as log]))

(defn trigger-crushing-wall
  [state x y cell]
  ;; reveal trap cell
  ;; add trap obj to place
  state)

(defn trigger-traps
  [state]
  (let [[cell x y] (rw/player-cellxy state)]
    (case (get cell :type)
      :crushing-wall-trigger
        (trigger-crushing-wall state x y cell)
      state)))

(defn update-crushing-wall-trap
  [state trap]
  state)

(defn update-trap
  [state trap]
  (case (get trap :type)
    :crushing-wall
      (update-crushing-wall-trap state trap)
    state))

(defn current-place-traps
  [state]
  (let [place-id (rw/current-place-id state)]
    (get-in state [:world :places place-id :traps] [])))

(defn update-traps
  [state]
  (reduce update-trap (current-place-traps state))) 

