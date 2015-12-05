;; Utility functions and functions for manipulating trap state
(ns robinson.traps
  (:require 
            [robinson.common :as rc]
            [robinson.random :as rr]
            [robinson.player :as rp]
            [robinson.world :as rw]
            [taoensso.timbre :as log]))

(defn conj-trap-in-current-place
  [state trap]
  (rw/update-current-place state (fn [place] (update place :traps (fn [traps] (conj traps trap))))))

(defn find-doors
  [state min-x min-y max-x max-y]
  (filter (fn [[x y]]
            (contains? #{:close-door :open-door}
                       (get (rw/get-cell state) :type)))
          (for [x (range min-x (inc max-x))
                y (range min-y (inc max-y))
                :when (or (= x min-x)
                          (= x max-x)
                          (= y min-y)
                          (= y max-y))]
            [x y])))

(defn crushing-wall-direction
  [state player-xy min-x min-y max-x max-y]
  (let [door-xys (find-doors state min-x min-y max-x max-y)
        door-xy  (rr/rand-nth door-xys)]
    ;; finding direction from player to door is the direction the wall will move
    (rc/find-point-relation (- min-x max-x) (- min-y max-y) player-xy door-xy)))

(defn make-trap-xy-sequence
   "Returns a list ex: [[[x y] [x y] [x y]] [[x y] [x y] [x y]] ...]
   a sequence of xy's where each element is the location of
   the crushing wall as it moves. As the wall is updated
   the first element is discarded. When drawn only the first
   set of xys are drawn."
  [min-x min-y max-x max-y direction]
  (case direction
    ; <--
    :left
      (for [x (range (dec max-x) min-x)]
        (for [y (range (inc min-y) max-y)]
          [x y]))
    ; -->
    :right
      (for [x (range (inc min-x) max-x)]
        (for [y (range (inc min-y) max-y)]
          [x y]))
    ;/\
    ;|
    :up
      (for [y (range (dec max-y) min-y)]
        (for [x (range (inc min-x) max-x)]
          [x y]))
    ;|
    ;\/
    :down
      (for [y (range (inc min-y) max-y)]
        (for [x (range (inc min-x) max-x)]
          [x y]))))

(defn trigger-crushing-wall
  [state x y cell]
  ;; determine trap advancement mechanics
  (let [{{min-x :min-x
          min-y :min-y
          max-x :max-x
          max-y :max-y}
        :room-bounds}  cell
        trap-direction (crushing-wall-direction state (rp/player-xy) min-x min-y max-x max-y) ;:up :down :left :right
        trap-locations (make-trap-xy-sequence min-x min-y max-x max-y trap-direction)]
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

