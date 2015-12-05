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
  (rw/update-current-place state (fn [place] (update place :traps (fn [traps] (vec (conj traps trap)))))))

(defn room-bound-xys
  [min-x min-y max-x max-y]
  (for [x (range min-x (inc max-x))
        y (range min-y (inc max-y))
        :when (or (= x min-x)
                  (= x max-x)
                  (= y min-y)
                  (= y max-y))]
    [x y]))
  
(defn find-exits
  [state min-x min-y max-x max-y]
  (log/info "room-bound-xys" (vec (room-bound-xys min-x min-y max-x max-y)))
  (filter (fn [[x y]]
            (log/info "x" x "y" y "type" (get (rw/get-cell state x y) :type))
            (contains? #{:close-door :open-door :corridor :moss-corridor :white-corridor}
                       (get (rw/get-cell state x y) :type)))
          (room-bound-xys min-x min-y max-x max-y)))

(defn crushing-wall-direction
  [state player-xy min-x min-y max-x max-y]
  (let [exit-xys (vec (find-exits state min-x min-y max-x max-y))
        _ (log/info "door-xys" exit-xys)
        exit-xy  (rr/rand-nth exit-xys)]
    ;; finding direction from player to exit is the direction the wall will move
    (case (rc/find-point-relation (- min-x max-x) (- min-y max-y) player-xy exit-xy)
      :left   :left
      :right  :right
      :top    :up
      :bottom :down)))

(defn make-trap-xy-sequence
   "Returns a list ex: [[[x y] [x y] [x y]] [[x y] [x y] [x y]] ...]
   a sequence of xy's where each element is the location of
   the crushing wall as it moves. As the wall is updated
   the first element is discarded. When drawn only the first
   set of xys are drawn."
  [min-x min-y max-x max-y direction]
  (log/info "making trap xy sequence" "min-x" min-x "min-y" min-y "max-x" max-x "max-y" max-y "direction" direction)
  (case direction
    ; <--
    :left
      (for [x (range (dec max-x) min-x -1)]
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
      (for [y (range (dec max-y) min-y -1)]
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
  (log/info "x" x "y" y "cell" cell)
  ;; TODO: only trigger once
  (let [{{min-x :min-x
          min-y :min-y
          max-x :max-x
          max-y :max-y}
        :room-bounds}  cell
        trap-direction (crushing-wall-direction state (rp/player-xy state) min-x min-y max-x max-y) ;:up :down :left :right
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
  [state idx]
  (let [place-id (rw/current-place-id state)]
    (update-in state
               [:world :places place-id :traps idx :locations]
               (comp rest vec))))

(defn update-trap
  [state [idx trap]]
  (log/info "updating trap" [idx trap])
  (case (get trap :type)
    :crushing-wall
      (update-crushing-wall-trap state idx)
    state))

(defn current-place-traps
  [state]
  (let [place-id (rw/current-place-id state)]
    (get-in state [:world :places place-id :traps] [])))

(defn update-traps
  [state]
  (as-> state state
    (reduce update-trap state (map-indexed vector (current-place-traps state)))
    (if-let [place-id (rw/current-place-id state)]
      (update-in state
                 [:world :places place-id :traps]
                 (fn [traps]
                   (reduce (fn [traps trap]
                             ;; remove expended crusing wall traps
                             (if (and (= (get trap :type) :crushing-wall)
                                      (empty? (get trap :locations)))
                               traps
                               (vec (conj traps trap))))
                           []
                           traps)))
      state)))

