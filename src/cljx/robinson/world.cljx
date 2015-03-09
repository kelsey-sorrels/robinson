;; Utility functions and functions for manipulating state
(ns robinson.world
  (:require [robinson.common :as rc]
            [robinson.player :as rp]
            [robinson.viewport :as rv]
            #+clj
            [clojure.core.async :as async]
            #+cljs
            [cljs.core.async :as async]
            #+clj
            [clojure.java.io :as io]
            #+clj
            [taoensso.timbre :as log]
            #+cljs
            [shodan.console :as log :include-macros true])
  #+clj
  (:import [java.io DataInputStream DataOutputStream]))


(timbre/refer-timbre)

(defn distance-from-player
  "Calculate the distance between the player and pos."
  [state pos]
  (distance (-> state :world :player :pos) pos))

(defn adjacent-to-player?
  "Return true is pos is adjacent to the player's pos including diagonals."
  [state pos]
  (<= (distance-from-player state pos) 1.5))
 
(defn with-xygrid
  "Inclue x y values alongside elements in a grid and preserve the structure
   of the grid.

       (def grid 
         [[:a :b :c]
          [:d :e :f]
          [:g :h :i]])

   Replace each inner element with [element x y]. X and Y start at 0 and increase.

       user=> (first (first grid)) 
       :a

       user=> (first (first (with-xygrid grid)))
       [:a 0 0]"
  [grid]
  (map-indexed (fn [y line] (map-indexed (fn [x cell] [cell x y]) line)) grid))

(defn with-xy
  "Include x y values alongside elements in a grid. Do not preserve the grid
   structure.

       (def grid 
         [[:a :b :c]
          [:d :e :f]
          [:g :h :i]])
  
   Return a lazy seq of [inner-element x y].
   
       user=> (first (first grid))
       :a
   
       user=> (take 5 (with-xy grid))
       ([:a 0 0] [:b 1 0] [:c 2 0] [:d 0 1] [:e 1 1])"
  [grid]
  (mapcat concat (map-indexed (fn [y line] (map-indexed (fn [x cell] [cell x y]) line)) grid)))

(defn rect->xys
  [[x1 y1] [x2 y2]]
  (for [y (range (min y1 y2) (max y1 y2))
        x (range (min x1 x2) (max x1 x2))]
    [x y]))


(defn update-matching-cells
  "Update the cells in place for which (p cell) returns true
   by replacing it with (f cell). Nil cells are skipped."
  [place p f]
  (mapv (fn [line] (mapv (fn [cell] (if (and (not (nil? cell))
                                             (p cell))
                                        (f cell)
                                        cell))
                         line))
        place))

(defn update-in-xy
  [place x y f]
  (update-in place [y x] f))

(defn current-place-id
  "Retrieve the current place id."
  [state]
  (-> state :world :current-place))

(defn current-state
  [state]
  (get-in state [:world :current-state]))

(defn get-time
  [state]
  (get-in state [:world :time]))

(defn assoc-current-state
  [state new-state]
  (assoc-in state [:world :current-state] new-state))

(defn map-with-xy
  "Non-lazily call `(f cell x y)` for each cell in the grid."
  [f grid]
  (doall 
    (map (fn [e] (apply f e)) (with-xy grid))))

(defn pmap-with-xy
  "Non-lazily call `(f cell x y)` for each cell in the grid."
  [f grid]
  (doall 
    (pmap (fn [e] (apply f e)) (with-xy grid))))

(defn matching-keys
  [m ks]
  (let [ks-coll (map #(take % ks) (range (inc (count ks))))]
    (vec (last (remove nil? (map #(if-not (nil? (get-in m %))
                         %
                         nil) ks-coll))))))

(defn get-cell
  "Retrieve the cell at the position `[x y]` in absolute world coordinates. If `[x y]` is outside the bounds
   of the in-memory places, return `nil`."
  ([state x y]
   (let [place-id (xy->place-id state x y)]
     (get-cell state place-id x y)))
  ([state place-id x y]
   (let [[ax ay]  (place-id->anchor-xy state place-id)]
     (get-cell state place-id ax ay x y)))
  ([state place-id ax ay x y]
   (let [[px py]    [(- x ax) (- y ay)]]
     #_(log/info "get-cell" place-id ax ay x y px py)
     #_(log/info "matching-keys" (matching-keys state [:world :places place-id py px]))
     (get-in state [:world :places place-id py px]))))

(defn update-cell
  [state x y f]
  (let [place-id (xy->place-id state x y)
        [ax ay]  (place-id->anchor-xy state place-id)
        [x y]    [(- x ax) (- y ay)]]
    (update-in state [:world :places place-id y x] f)))

(defn assoc-cell-fn
  [state ks v]
  {:pre [(not (nil? (get-in state (butlast ks))))]}
  (assoc-in state ks v))

(defn assoc-cell
  [state x y & keyvals]
  (let [place-id (xy->place-id state x y)
        [ax ay]  (place-id->anchor-xy state place-id)
        [px py]    [(- x ax) (- y ay)]]
    #_(log/info "assoc-cell" "place-id" place-id "x" x "y" y "ax" ax "ay" ay "px" px "py" py "kvs" keyvals)
    (reduce (fn [state [k v]]
              #_(log/info "matching-keys" (matching-keys state [:world :places place-id py px k]))
              (assoc-cell-fn state [:world :places place-id py px k] v))
            state
            (partition 2 keyvals))))

(defn dissoc-cell
  [state x y k] 
  (let [place-id (xy->place-id state x y)
        [ax ay]  (place-id->anchor-xy state place-id)
        [x y]    [(- x ax) (- y ay)]]
    (dissoc-in state [:world :places place-id y x k])))

(defn update-cell-items
  [state x y f]
  (update-cell state x y (fn [cell] (assoc cell :items (f (or (get cell :items) []))))))

(defn conj-cell-items
  [state x y item]
  (update-cell-items state x y (fn [items] (conj items item))))

(defn assoc-cell-items
  "Adds an item to [x y] in the current place. Simple, right?"
  [state x y items]
  (log/info "Adding" items "to cell @" x y)
  (assoc-cell state x y :items items))

(defn conj-current-cell-items
  "Adds an item to the player's cell's items."
  [state item]
  (let [x (-> state :world :player :pos :x)
        y (-> state :world :player :pos :y)]
    (conj-cell-items state x y item)))

(defn assoc-current-cell-items
  "Replaces items in player's current cell."
  [state items]
  (let [x (-> state :world :player :pos :x)
        y (-> state :world :player :pos :y)]
    (assoc-cell-items state x y items)))

(defn player-cellxy
  "Retrieve the cell at which the player is located."
  [state]
  (let [x (-> state :world :player :pos :x)
        y (-> state :world :player :pos :y)]
    [(get-cell state x y) x y]))

(defn dec-cell-item-count
  "Decreases the count of an item in the player's cell."
  [state id]
  (let [[cell x y] (player-cellxy state)
        item       (first (filter #(= id (get % :id)) (get cell :items)))
        item-count (get item :count 1)]
    (cond
      (zero? item-count)
        state
      (= 1 item-count)
        (update-cell state x y (fn [cell] (remove-in cell [:items] #(= id (get % :id)))))
      :else
        (update-cell state x y (fn [cell] (map-in cell [:items] (fn [item]

      (if (= id (get item :id))
                                                                       (update-in item [:count] dec)

        item))))))))


(defn adjacent-xys
  [x y]
  [[(dec x) y]
   [(inc x) y]
   [x (dec y)]
   [x (inc y)]])

(defn adjacent-xys-ext
  [x y]
  [[(dec x) y]
   [(inc x) y]
   [x (dec y)]
   [x (inc y)]
   [(dec x) (inc y)]
   [(inc x) (inc y)]
   [(dec x) (dec y)]
   [(inc x) (dec y)]])

(defn adjacent-cells
  "Return a collection of adjacent cells (diagonals not-included) around pos.
   Pos is a map with the keys `:x` and `:y`.

   Ex: `(adjacent-cells [...] {:x 0 :y 0})`
   `[{:type :floor} {:type :water}...]`"
  [place {x :x y :y}]
  (map (fn [[x y]] (get-in place [y x]))
    (adjacent-xys x y)))

(defn adjacent-cells-ext
  "Return a collection of adjacent cells (diagonals included) around pos.
   Pos is a map with the keys `:x` and `:y`.

   Ex: `(adjacent-cells [...] {:x 0 :y 0})`
   `[{:type :floor} {:type :water}...]`"
  [place {x :x y :y}]
  (map (fn [[x y]] (get-in place [y x]))
    (adjacent-xys-ext x y)))

(defn direction->xys
  ([state direction]
   (direction->xys state direction 5))
  ([state direction distance]
   {:pre [(contains? #{:left :right :up :down} direction)]}
   (let [{x :x y :y}      (get-in state [:world :player :pos])
         {width :width
          height :height} (get state :world)
         min-x            (max 0 (- x distance))
         min-y            (max 0 (- y distance))
         max-x            (min width (+ x distance))
         max-y            (min height (+ y distance))]
     (case direction
       :left  (reverse (map (fn [x] [x y]) (range min-x x)))
       :right (map (fn [x] [x y]) (range (inc x) (inc max-x)))
       :up    (reverse (map (fn [y] [x y]) (range min-y y)))
       :down  (map (fn [y] [x y]) (range (inc y) (inc max-y)))))))

(defn direction->cells
  ([state direction]
   (direction->cells state direction 5))
  ([state direction distance]
   {:pre [(contains? #{:left :right :up :down} direction)]}
   (map (fn [[x y]] (get-cell state x y)) (direction->xys state direction distance))))

(defn direction->cellsxy
  [state direction]
  (map (fn [[x y]] [(get-cell state x y) x y])
       (direction->xys state direction)))

(defn npc-at-xy
  "npc at [x y] of the current place. Otherwise `nil`."
  [state x y]
  (first (filter (fn [npc] (and (= (-> npc :pos :x) x)
                                (= (-> npc :pos :y) y)))
                 (get-in state [:world :npcs]))))

(defn type->collide?
  [t]
  (contains? #{:vertical-wall
               :horizontal-wall
               :fire
               :lava
               :mountain
               :close-door
               :tree
               :bamboo
               :bamboo-water-collector
               :solar-still
               :palm-tree
               :fruit-tree
               :palisade
               :dry-hole
               :freshwater-hole
               :saltwater-hole} t))

(defn type->water?
  [t]
  (contains? #{:water :surf} t))

(defn type->flammable?
  [t]
  (contains? #{:tree :palm-tree :fruit-tree :bamboo :tall-grass :short-grass :campfire} t))

(defn type->shelter?
  [t]
  (contains? #{:ramada :lean-to :tarp-shelter} t))

(defn type->destroyable?
  [t]
  (contains? #{:palisade} t))

(defn type->intertidal?
  [t]
  (contains? #{:sand :surf} t))

(defn collide?
  "Return `true` if the cell at `[x y]` is non-traverable. Ie: a wall, closed door or simply does
   not exist. Cells occupied by npcs are considered non-traversable."
  ([state x y opts]
    ;; destructure opts into variables with defaults
    (let [{:keys [include-npcs?
                  collide-water?]
           :or {include-npcs? true
                collide-water? true}} opts
          cell (get-cell state x y)]
      ;(log/debug "collide? " cell x y)
      (or
        (nil? cell)
        ;; check the cell to see if it is a wall or closed door
        (type->collide? (get cell :type))
        ;; water collides?
        (and collide-water?
             (= (cell :type) :water))
        ;; not a wall or closed door, check for npcs
        (and include-npcs?
             (npc-at-xy state x y)))))

  ([state x y]
  (collide? state x y {})))

(defn collide-in-water?
  "Return `true` if the cell at `[x y]` is non-traverable. Ie: any non-water cell or simply does
   not exist. Cells occupied by npcs are considered non-traversable."
  ([state x y opts]
    ;; destructure opts into variables with defaults
    (let [{:keys [include-npcs?
                  collide-water?]
           :or {include-npcs? true
                collide-water? true}} opts
          cell (get-cell state x y)]
      ;(log/debug "collide? " cell x y)
      (or
        (nil? cell)
        ;; check the cell to see if it is a wall or closed door
        (not (type->water? (get cell :type)))
        ;; check for npcs
        (and include-npcs?
             (npc-at-xy state x y)))))

  ([state x y]
  (collide-in-water? state x y {})))


(defn first-collidable-object
 "Return a map based on the first collidable cell in cellsxy.
  If an npc is encountered first, return `{:npc npc}`.
  If a non-npc is encountered first, return `{:cell cell}`.
  If no collision occurs in any of the cells, return `{}`."
 [state direction max-distance]
 {:pre [(contains? #{:up :down :left :right} direction)]}
  (let [cellsxy (remove (fn [[cell x y]]
                          (farther-than? (xy->pos x y)
                                         (get-in state [:world :player :pos])
                                         max-distance))
                        (direction->cellsxy state direction))]
   (loop [[cell x y] (first cellsxy)
          xs         (rest cellsxy)]
     (let [npc (npc-at-xy state x y)]
       (cond
         (not (nil? npc))
           {:npc npc :pos (xy->pos x y)}
         (collide? state x y false)
           {:cell cell :pos (xy->pos x y)}
         (empty? xs)
           {}
         :else
           (recur (first xs) (rest xs)))))))

(defn player-adjacent-xys
  [state]
  (let [[x y] (player-xy state)]
    [[(dec x) y]
     [(inc x) y]
     [x (dec y)]
     [x (inc y)]]))

(defn player-adjacent-xys-ext
  [state]
  (let [[x y] (player-xy state)]
    [[(dec x) y]
     [(inc x) y]
     [x (dec y)]
     [x (inc y)]
     [(dec x) (dec y)]
     [(inc x) (inc y)]
     [(inc x) (dec y)]
     [(dec x) (inc y)]]))

(defn player-adjacent-cells
  "Return a collection of cells adjacent to the player. Does not include diagonals."
  [state]
  (map (fn [[x y]] (get-cell state x y)) (player-adjacent-xys state)))

(defn player-adjacent-cells-ext
  "Return a collection of cells adjacent to the player. Includes diagonals."
  [state]
  (map (fn [[x y]] (get-cell state x y)) (player-adjacent-xys-ext state)))

(defn cells-in-range-of-player
  "Return a collection of cells with within `distance` from the player."
  [state distance]
  (let [[x y]      (player-xy state)
        player-pos (player-pos state)]
    (map (fn [[x y]] (get-cell state x y))
         (remove (fn [pos] farther-than? player-pos pos distance)
                 (map (fn [[x y]] (xy->pos x y))
                      (rect->xys [(- x distance) (- y distance)]
                                 [(+ x distance) (+ y distance)]))))))

(defn player-adjacent-pos
  [state direction]
  (let [{x :x y :y} (player-pos state)
        x           (case direction
                      :left  (dec x)
                      :right (inc x)
                      x)
        y           (case direction
                      :up   (dec y)
                      :down (inc y)                      y)]
    {:x x :y y}))

(defn player-adjacent-cell
  [state direction]
  (apply get-cell state (pos->xy (player-adjacent-pos state direction))))


(defn player-mounted-on-raft?
  [state]
  (and (get-in state [:world :player :mounted] false)
       (contains? (set (map :id (get (first (player-cellxy state)) :items []))) :raft)))


(defn inventory-and-player-cell-items
  [state]
  (let [[cell _ _]        (player-cellxy state)
        inventory         (player-inventory state)
        cell-items        (get cell :items [])
        remaining-hotkeys (get-in state [:world :remaining-hotkeys])
        inventory         (vec (fill-missing #(not (contains? % :hotkey))
                                             #(assoc %1 :hotkey %2)
                                             remaining-hotkeys
                                             (concat inventory cell-items)))]
    inventory))

(defn inventory-and-player-cell-hotkey->item
  [state hotkey]
  (first (filter (fn [item] (= hotkey (get item :hotkey))) (inventory-and-player-cell-items state))))

(defn is-night?
  [state]
  (let [atmo   (get-in state [:data :atmo])
        frames (count atmo)
        t      (mod (get-in state [:world :time]) frames)
        frame  (nth atmo t)
        values (flatten frame)]
    (zero? (reduce + values))))

(defn is-day?
  [state]
  (not (is-night? state)))

