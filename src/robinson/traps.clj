;; Utility functions and functions for manipulating trap state
(ns robinson.traps
  (:require 
            [robinson.common :as rc]
            [robinson.random :as rr]
            [robinson.player :as rp]
            [robinson.world :as rw]
            [robinson.monstergen :as mg]
            [robinson.itemgen :as ig]
            [robinson.describe :as rdesc]
            [robinson.npc :as rnpc]
            [robinson.combat :as rcombat]
            [robinson.actors :as ractors]
            [robinson.fx :as rfx]
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
    (get this :toughness))
  (get-attack-speed [this state attack-type]
    (get this :speed))
  (get-attack-strength [this state attack-type]
    (get this :strength))
  (get-attack-dexterity [this state attack-type]
    (get this :dexterity))
  (get-attack-toughness [this state attack-type]
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
    (rc/append-log state "Click. Nothing happens.")))

(defn trigger-wall-darts
  [state x y cell]
  ;; find the first interesting object starting at the src cell and moving in the direction
  ;; toward the triggering cell.
  (let [direction     (get cell :direction)
        [src-x src-y] (rc/pos->xy (rw/adjacent-pos (get cell :src-pos) direction))
        obj           (rw/first-collidable-object state src-x src-y direction 5)
        ;; TODO: give the trap an inventory including a :range-wielded dart gun that it fires to that ranged combat works
        src-trap      (Trap. ; type
                             :dart
                             ; race
                             :trap
                             ; name
                             "dart"
                             ; name-plural
                             "darts"
                             ; speed
                             5
                             ; size
                             5
                             ; strength
                             5
                             ; dexterity
                             3
                             ; toughness
                             5
                             ; attacks
                             #{:spike})]
    (log/info "trigger-wall-darts" direction obj)
    (as-> state state
      ;; reveal trap cell
      (rw/assoc-cell state x y :trap-found     true
                               :last-triggered (rw/get-time state))
      (rc/append-log state "You find a trap")
      (let [src-pos           (get cell :src-pos)
            [x y]             (rc/pos->xy src-pos)
            place-id          (rw/current-place-id state)
            trigger-cell-path [:world :places place-id y x]
            path              (map rc/pos->xy (rc/direction->path src-pos direction 10))]
        (rfx/conj-effect
          state 
          :airborn-item
          (assoc (ig/gen-item :blowdart) :attacker src-trap)
          true
          path
          5)))))

(defn trigger-poisonous-gas
  [state x y cell]
  ;; Only trigger once every 20 turns max
  (log/info "trigger-poisonous-gas")
  (if (> (rw/get-time state) (+ (get cell :last-triggered 0) 20))
    (let [trap (assoc (Trap. ; type
                             :poisonous-gas
                             ; race
                             :trap
                             ; name
                             "poisonous gas"
                             ; name-plural
                             "poisonous gasses"
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
                             #{:poisonous-gas})
                      ;; poisonous-gas specific
                      :locations {[x y] 1})]
      (-> state
        ;; reveal trap cell
        (rw/assoc-cell x y :trap-found     true
                           :last-triggered (rw/get-time state))
        (rc/append-log "Hiss..poisonous gas is released.")
        ;; add trap obj to place
        (conj-trap-in-current-place trap)
        (as-> state
          ;; attack player if player is in gas
          (if (= [x y] (rp/player-xy state))
            (rcombat/attack state trap [:world :player])
            state)
          ;; attack npcs in gas
          (if-let [npc (rw/npc-at-xy state x y)]
            (let [npc-path (rnpc/npc->keys state npc)]
              (rcombat/attack state trap npc-path))
            state))))
    state))

(defn trigger-spike-pit
  [state x y cell]
  (log/info "trigger-spike-pit")
  (let [trap (Trap. ; type
                    :spike-pit
                    ; race
                    :trap
                    ; name
                    "spike pit"
                    ; name-plural
                    "spike pits"
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
                    #{:spike})]
    (-> state
      ;; reveal trap cell
      (rw/assoc-cell x y :trap-found     true)
      (as-> state
        ;; attack player if player is in pit
        (if (= [x y] (rp/player-xy state))
          (rcombat/attack state trap [:world :player])
          state)
        ;; attack npcs in pit
        (if-let [npc (rw/npc-at-xy state x y)]
          (let [npc-path (rnpc/npc->keys state npc)]
            (rcombat/attack state trap npc-path))
          state)))))

(defn find-snake-src-xys
  [state min-x min-y max-x max-y]
  (let [perimeter-xys (set
                        (concat (map vector (range (inc min-x) max-x) (repeat (inc min-y)))
                                (map vector (range (inc min-x) max-x) (repeat (dec max-y)))
                                (map vector (repeat (inc min-x)) (range (inc min-y) max-y))
                                (map vector (repeat (dec max-x)) (range (inc min-y) max-y))))]
    (log/info "perimeter-xys" perimeter-xys)
    (take 4 (filter (fn [[x y]]
                      (and
                        (= (get (rw/get-cell state x y) :type) :floor)
                        (nil? (rw/npc-at-xy state x y))
                        (not= (rp/player-xy state) [x y])))
                    perimeter-xys))))

(defn trigger-snakes
  [state x y cell]
  (log/info "x" x "y" y "cell" cell)
  ;; Only trigger once every 20 turns max
  (let [{{min-x :min-x
          min-y :min-y
          max-x :max-x
          max-y :max-y}
        :room-bounds}  cell
        snake-src-xys  (find-snake-src-xys state min-x min-y max-x max-y)]
    (log/info "snake-src-xys" (vec snake-src-xys))
    (-> state
      ;; reveal trap cell
      (rw/assoc-cell x y :trap-found     true
                         :last-triggered (rw/get-time state))
      (rc/append-log "You find a trap. Snakes pour out of the walls.")
      ;; add snakes to place
      (as-> state
        (reduce (fn [state [x y]] (rnpc/add-npc state (mg/gen-monster :snake) x y))
                state
                snake-src-xys)))))

(defn trigger-if-trap
  [state [x y]]
  (let [cell (rw/get-cell state x y)]
    (case (get cell :type)
      :crushing-wall-trigger
        (trigger-crushing-wall state x y cell)
      :wall-darts-trigger
        (trigger-wall-darts state x y cell)
      :poisonous-gas-trigger
        (trigger-poisonous-gas state x y cell)
      :spike-pit
        (trigger-spike-pit state x y cell)
      :snakes-trigger
        (trigger-snakes state x y cell)
      state)))

(defn trigger-traps
  [state]
  (log/info "trigger traps")
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

(defn adj-passable-xys
  [state x y]
  (conj (remove (fn [[x y]] (rw/collide? state x y {:collide-player? false
                                               :collide-npcs? false
                                               :collide-water false}))
           (rw/adjacent-xys x y))
        [x y]))

(defn update-poisonous-gas-trap 
  [state idx]
  (let [place-id    (rw/current-place-id state)
        trap        (get-in state [:world :places place-id :traps idx])
        xy->density (get trap :locations {})
        _ (log/info "xy->density" xy->density)
        target-xys  (set (mapcat (fn [[x y]] (adj-passable-xys state x y)) (keys xy->density)))
        _ (log/info "target-xys" target-xys)
        xy->density (reduce (fn [xy->density-acc [x y]]
                              ;; sum adjacent densities, divide by 4 and subtract decay
                              (let [density (reduce (fn [density src-xy]
                                                      (+ density (get xy->density src-xy 0)))
                                                    0
                                                    (adj-passable-xys state x y))
                                    density (- (/ density 5) 0.005)]
                              (if (> density 0.0)
                                (assoc xy->density-acc [x y] density)
                                xy->density-acc)))
                            {}
                            target-xys)]
    (log/info "xy->density" xy->density)
    (as->
      (assoc-in state [:world :places place-id :traps idx :locations] xy->density)
      state
      ;; attack player if player is in gas
      (if (contains? (set (keys xy->density)) (rp/player-xy state))
        (rcombat/attack state trap [:world :player])
        state)
      ;; attack npcs in gas
      (reduce (fn [state [x y]]
                (if-let [npc (rw/npc-at-xy state x y)]
                  (let [npc-path (rnpc/npc->keys state npc)]
                    (rcombat/attack state trap npc-path))
                  state))
              state
              (keys xy->density)))))

(defn update-trap
  [state [idx trap]]
  {:pre [(not (nil? state))]
   :post [(not (nil? %))]}
  (log/info "updating trap" [idx trap])
  (case (get trap :type)
    :crushing-wall
      (update-crushing-wall-trap state idx)
    :poisonous-gas
      (update-poisonous-gas-trap state idx)
    state))

(defn remove-trap?
  [trap]
  (case (get trap :type)
    :crushing-wall
      (empty? (get trap :locations))
    :poisonous-gas
      (empty? (get trap :locations))))
      
(defn update-traps
  [state]
  (log/info "update-traps")
  (as-> state state
    (reduce update-trap state (map-indexed vector (rw/current-place-traps state)))
    ;; clean up traps if expired
    (if-let [place-id (rw/current-place-id state)]
      (-> state
        (update-in [:world :places place-id :traps]
                   (fn [traps]
                     (reduce (fn [traps trap]
                               ;; remove expended crusing wall traps
                               (if (remove-trap? trap)
                                 (do
                                   (log/info "removing trap" trap)
                                   traps)
                                 (vec (conj traps trap))))
                             []
                             traps)))
        trigger-traps)
      state)))

