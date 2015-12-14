;; Utility functions and functions for manipulating trap state
(ns robinson.traps
  (:require 
            [robinson.common :as rc]
            [robinson.random :as rr]
            [robinson.player :as rp]
            [robinson.world :as rw]
            [robinson.npc :as rnpc]
            [robinson.combat :as rcombat]
            [robinson.dynamiccharacterproperties :as dcp]
            [taoensso.timbre :as log]))

;; Trap protocol
(defrecord Trap [type
                 race
                 name
                 name-plural
                 speed
                 size
                 strength
                 dexterity
                 toughness
                 attacks]
  Object
  (toString [this] (str "#Trap" (into {} this)))
  ;; Dispatch DynaimicCharacterProperties to multi-methods
  dcp/DynamicCharacterProperties
  (get-energy [this state]
    (get this :energy))
  (get-speed [this state]
    (get this :speed))
  (get-size [this state]
    (get this :size))
  (get-strength [this state]
    (get this :strength))
  (get-dexterity [this state]
    (get this :dexterity))
  (get-toughness [this state]
    (get this :toughness)))

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
      (for [x (range max-x min-x -1)]
        (for [y (range (inc min-y) max-y)]
          [x y]))
    ; -->
    :right
      (for [x (range min-x max-x)]
        (for [y (range (inc min-y) max-y)]
          [x y]))
    ;/\
    ;|
    :up
      (for [y (range max-y min-y -1)]
        (for [x (range (inc min-x) max-x)]
          [x y]))
    ;|
    ;\/
    :down
      (for [y (range min-y max-y)]
        (for [x (range (inc min-x) max-x)]
          [x y]))))

(defn trigger-crushing-wall
  [state x y cell]
  ;; determine trap advancement mechanics
  (log/info "x" x "y" y "cell" cell)
  ;; Only trigger once every 20 turns max
  (if (> (rw/get-time state) (+ (get cell :last-triggered 0) 20))
    (let [{{min-x :min-x
            min-y :min-y
            max-x :max-x
            max-y :max-y}
          :room-bounds}  cell
          trap-direction (crushing-wall-direction state (rp/player-xy state) min-x min-y max-x max-y) ;:up :down :left :right
          trap-locations (make-trap-xy-sequence min-x min-y max-x max-y trap-direction)]
      (-> state
        ;; reveal trap cell
        (rw/assoc-cell x y :trap-found     true
                           :last-triggered (rw/get-time state))
        (rc/append-log "You find a trap")
        ;; add trap obj to place
        (conj-trap-in-current-place (assoc (Trap. ; type
                                                  :crushing-wall
                                                  ; race
                                                  :trap
                                                  ; name
                                                  "spiked wall"
                                                  ; name-plural
                                                  "spiked walls"
                                                  ; speed
                                                  1
                                                  ; size
                                                  50
                                                  ; strength
                                                  5
                                                  ; dexterity
                                                  1
                                                  ; toughness
                                                  5
                                                  ; attacks
                                                  #{:spike})
                                       ;; crushing-wall specific
                                       :direction trap-direction
                                       :locations trap-locations))))
    state))

(defn trigger-wall-darts
  [state cell]
  ;; find the first interesting object starting at the src cell and moving in the direction
  ;; toward the triggering cell.
  (let [[src-x src-y] (rc/pos->xy (get cell :src-pos))
        direction     (get cell :direction)
        obj           (rw/first-collidable-object state src-x src-y direction 5)]
    (log/info "trigger-wall-darts" direction obj)
    (cond
      (contains? obj :npc)
        (let [[x y]             (rc/pos->xy (get cell :src-pos))
              place-id          (rw/current-place-id state)
              trigger-cell-path [:world :places place-id y x]
              npc-path          (rnpc/npc->keys state (get obj :npc))]
        (rcombat/attack state trigger-cell-path  npc-path))
      (contains? obj :player)
        (let [[x y]             (rc/pos->xy (get cell :src-pos))
              place-id          (rw/current-place-id state)
              trigger-cell-path [:world :places place-id y x]]
        (rcombat/attack state trigger-cell-path [:world :player]))
      (contains? obj :cell)
        (rc/append-log state "Schwaff! The arrow wizzes past.")
      :else
        state)))

(defn trigger-if-trap
  [state [x y]]
  (let [cell (rw/get-cell state x y)]
    (case (get cell :type)
      :crushing-wall-trigger
        (trigger-crushing-wall state x y cell)
      :wall-darts-trigger
        (trigger-wall-darts state cell)
      state)))

(defn trigger-traps
  [state]
  (reduce trigger-if-trap
          state
          (conj (map (comp rc/pos->xy :pos) (rnpc/visible-npcs state))
                (rp/player-xy state))))

(defn update-crushing-wall-trap
  [state idx]
  (let [place-id (rw/current-place-id state)
        xys      (second (get-in state [:world :places place-id :traps idx :locations] []))]
    ;; are any npcs in the next set of xys?
    (cond
      (some (fn [[x y]] (rnpc/npc-at-xy state x y)) xys)
        ;; damage creatures in xys
        (reduce (fn [state [x y]]
                  (log/info "Trap attacking npc at" x y)
                  (if-let [npc (rnpc/npc-at-xy state x y)]
                    (let [npc-path (rnpc/npc->keys state npc)
                          wall-path [:world :places place-id :traps idx]]
                      (log/info "crushing wall" wall-path "attacking" npc-path)
                      (rcombat/attack state wall-path npc-path))
                    state))
                state
                xys)
      (contains? (set xys) (rp/player-xy state))
        ;; damage player
        (let [player-path [:world :player]
              wall-path [:world :places place-id :traps idx]]
          (rcombat/attack state wall-path player-path))
      ;; move crushing wall forward
      :else
      (update-in state
                 [:world :places place-id :traps idx :locations]
                 (comp rest vec)))))

(defn update-trap
  [state [idx trap]]
  {:pre [(not (nil? state))]
   :post [(not (nil? %))]}
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
    ;; clean up traps if expired
    (if-let [place-id (rw/current-place-id state)]
      (-> state
        (update-in [:world :places place-id :traps]
                   (fn [traps]
                     (reduce (fn [traps trap]
                               ;; remove expended crusing wall traps
                               (if (and (= (get trap :type) :crushing-wall)
                                        (empty? (get trap :locations)))
                                 traps
                                 (vec (conj traps trap))))
                             []
                             traps)))
        trigger-traps)
      state)))

