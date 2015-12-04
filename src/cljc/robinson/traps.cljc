;; Utility functions and functions for manipulating trap state
(ns robinson.traps
  (:require 
            [robinson.common :as rc]
            [robinson.world :as rw]
            [taoensso.timbre :as log]))

(defn conj-trap-in-current-place
  [state trap]
  (rw/update-current-place state (fn [place] (update place :traps (fn [traps] (conj traps trap))))))


(defn trigger-crushing-wall
  [state x y cell]
  ;; determine trap advancement mechanics
  (let [trap-direction nil ;:up :down :left :right
        trap-locations []] ; list ex: [[[x y] [x y] [x y]] [[x y] [x y] [x y]] ...]
                           ; a sequence of xy's where each element is the location of
                           ; the crushing wall as it moves. As the wall is updated
                           ; the first element is discarded. When drawn only the first
                           ; set of xys are drawn.
    (-> state
      ;; reveal trap cell
      (rw/assoc-cell x y :trap-found true)
      (rc/append-log "You find a trap")
      ;; add trap obj to place
      (conj-trap-in-current-place {:type :crushing-wall
                                   :direction trap-direction
                                   :locations trap-locations}))))

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

