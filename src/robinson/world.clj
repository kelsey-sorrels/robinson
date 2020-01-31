;; Utility functions and functions for manipulating state
(ns robinson.world
  (:require [robinson.common :as rc]
            [robinson.random :as rr]
            [taoensso.timbre :as log]
            [robinson.player :as rp]
            [robinson.inventory :as ri]
            [robinson.viewport :as rv]
            [clojure.core.async :as async])
  (:import [java.io DataInputStream DataOutputStream]))


(defn is-trap-type?
  [cell-type]
  (contains? #{:crushing-wall-trigger :wall-darts-trigger :poisonous-gas-trigger :spike-pit :snakes-trigger} cell-type))

(defn distance-from-player
  "Calculate the distance between the player and pos."
  [state pos]
  (rc/distance (-> state :world :player :pos) pos))

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

(defn assoc-current-place-id
  [state place-id]
  (assoc-in state [:world :current-place] place-id))

(defn current-place-traps
  [state]
  (let [place-id (current-place-id state)]
    (get-in state [:world :places place-id :traps] [])))

(defn update-place
  [state place-id f]
  (update-in state [:world :places place-id] f))

(defn current-state
  [state]
  (get-in state [:world :current-state]))

(defn get-time
  [state]
  (get-in state [:world :time]))

(defn assoc-time
  [state t]
  (assoc-in state [:world :time] t))

(defn update-time
  [state f]
  (update-in state [:world :time] f))

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
    (map (fn [e] (apply f e)) (with-xy grid))))

(defn matching-keys
  [m ks]
  (let [ks-coll (map #(take % ks) (range (inc (count ks))))]
    (vec (last (remove nil? (map #(if-not (nil? (get-in m %))
                         %
                         nil) ks-coll))))))

(defn get-cell
  "Retrieve the cell at the position `[x y]` in absolute world coordinates. If `[x y]` is outside the bounds
   of the in-memory places, return `nil`."
  ([state pos]
    (apply get-cell state (rc/pos->xy pos)))
  ([state x y]
   (let [place-id (rv/xy->place-id state x y)]
     (get-cell state place-id x y)))
  ([state place-id x y]
   (let [[ax ay]  (rv/place-id->anchor-xy state place-id)]
     (get-cell state place-id ax ay x y)))
  ([state place-id ax ay x y]
   (let [[px py]    [(- x ax) (- y ay)]]
     #_(log/info "get-cell" place-id ax ay x y px py)
     #_(log/info "matching-keys" (matching-keys state [:world :places place-id :cells py px]))
     (get-in state [:world :places  place-id :cells py px]))))

(defn get-cell-type
  [state x y]
  (get (get-cell state x y) :type))

(defn filter-cellxys
  [state pred place-id]
  {:pre[(get-in state [:world :places place-id :cells])]}
  (let [cellxys (reduce (fn [cellxys [y line]]
                          (reduce (fn [cellxys [x cell]]
                                    (if (pred [cell x y])
                                      (conj cellxys [cell x y])
                                      cellxys))
                                  cellxys
                                  (map vector (range) line)))
                        []
                        (map vector
                             (range)
                             (get-in state [:world :places place-id :cells])))]
                             
    (log/info "found cells" cellxys)
    cellxys))

(defn update-current-place
  [state f]
  (if-let [place-id (current-place-id state)]
    (update-in state [:world :places place-id] f)
    state))
  
(defn update-cell
  ([state pos f]
    (update-cell state (rc/pos->xy pos) f))
  ([state x y f]
    (let [place-id (rv/xy->place-id state x y)
          [ax ay]  (rv/place-id->anchor-xy state place-id)
          [x y]    [(- x ax) (- y ay)]]
      (update-cell state place-id x y f)))
  ([state place-id x y f]
    (log/info "update-cell" (format "place-id[%s] y[%d] x[%d]" (str place-id) y x))
    (update-in state [:world :places place-id :cells y x] f)))

(defn update-player-cell
  [state f]
  (update-cell state (rp/player-pos state) f))

(defn assoc-cell-fn
  [state ks v]
  {:pre [(not (nil? (get-in state (butlast ks))))]}
  (assoc-in state ks v))

(defn assoc-cell
  [state x y & keyvals]
  (let [place-id (rv/xy->place-id state x y)
        [ax ay]  (rv/place-id->anchor-xy state place-id)
        [px py]    [(- x ax) (- y ay)]]
    (log/info "assoc-cell" "place-id" place-id "x" x "y" y "ax" ax "ay" ay "px" px "py" py "kvs" keyvals)
    (reduce (fn [state [k v]]
              #_(log/info "matching-keys" (matching-keys state [:world :places place-id :cells py px k]))
              (assoc-cell-fn state [:world :places place-id :cells py px k] v))
            state
            (partition 2 keyvals))))

;(assoc-cells state {[1 2] {:discovered 10} [0 0] {:discovered 10}})
(defn assoc-cells
  [state xyvs]
  (let [place-id->xyvs (group-by (fn [[[x y] _]]
                                   (let [place-id  (rv/xy->place-id state x y)]
                                     place-id))
                                 xyvs)]
    #_(log/info "reducing" place-id->xyvs)
    (reduce-kv (fn [state place-id  xyvs]
                 (let [place   (get-in state [:world :places place-id :cells])
                       [ax ay] (rv/place-id->anchor-xy state place-id)]
                   (if (some? place)
                     (assoc-in state
                             [:world :places place-id :cells]
                             (reduce (fn [place [[x y] v]]
                                       #_(log/info "updating place" place-id "x" x "y" y "with value" v)
                                       (let [px (- x ax)
                                             py (- y ay)]
                                          (update-in place [py px] (fn [cell] (merge cell v)))))
                                     place
                                     xyvs))
                     state)))
               state
               place-id->xyvs)))
                       
(defn update-cells
   [state xy-fns]
  (let [place-id->xy-fns (group-by (fn [[[x y] _]]
                                   (let [place-id  (rv/xy->place-id state x y)]
                                     place-id))
                                 xy-fns)]
    #_(log/info "reducing" place-id->xyvs)
    (update-in state [:world :places]
      (fn [places]
        (reduce-kv (fn [places place-id xy-fns]
                     (let [place   (get places place-id)
                           [ax ay] (rv/place-id->anchor-xy state place-id)]
                       (assoc places
                              place-id
                              (assoc place
                                      :cells
                                      (reduce (fn [place [[x y] f]]
                                                #_(log/info "updating place" place-id "x" x "y" y "with value" v)
                                                (let [px (- x ax)
                                                      py (- y ay)]
                                                  (if (get-in place [py px])
                                                    (update-in place [py px] f)
                                                    (do (log/error "Tried associng in unloaded place"
                                                                   place
                                                                   place-id
                                                                   px
                                                                   py
                                                                   (keys places))
                                                        place))))
                                              (get place :cells)
                                              xy-fns)))))
                   places
                   place-id->xy-fns)))))

(defn dissoc-cell
  [state x y k] 
  (let [place-id (rv/xy->place-id state x y)
        [ax ay]  (rv/place-id->anchor-xy state place-id)
        [x y]    [(- x ax) (- y ay)]]
    (rc/dissoc-in state [:world :places place-id :cells y x k])))

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
  "Retrieve the cell at which the player is located along with the x and y coordinates."
  [state]
  (let [x (-> state :world :player :pos :x)
        y (-> state :world :player :pos :y)]
    [(get-cell state x y) x y]))

(defn dec-cell-item-count
  "Decreases the count of an item in the player's cell."
  [state x y id]
  (let [cell       (get-cell state x y)
        item       (first (filter #(= id (get % :item/id)) (get cell :items)))
        item-count (get item :count 1)]
    (cond
      (zero? item-count)
        state
      (= 1 item-count)
        (update-cell-items state x y (fn [items] (remove #(= id (get % :item/id)) items)))
      :else
        (update-cell-items state x y (fn [items] (map (fn [item]
                                                        (if (= id (get item :item/id))
                                                          (update item :count dec)
                                                          item))))))))

(defn adjacent-xys
  ([pos]
   (apply adjacent-xys (rc/pos->xy pos)))
  ([x y]
   [[(dec x) y]
    [(inc x) y]
    [x (dec y)]
    [x (inc y)]]))

(defn adjacent-xys-ext
  ([pos]
   (apply adjacent-xys-ext (rc/pos->xy pos)))
  ([x y]
   [[(dec x) y]
    [(inc x) y]
    [x (dec y)]
    [x (inc y)]
    [(dec x) (inc y)]
    [(inc x) (inc y)]
    [(dec x) (dec y)]
    [(inc x) (dec y)]]))

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
   (let [[start-x start-y] (rp/player-xy state)]
     (direction->xys state start-x start-y direction distance)))
  ([state start-x start-y direction distance]
   {:pre [(contains? #{:left :right :up :down} direction)]}
   (let [{width :width
          height :height} (get state :world)
         min-x            (max -1 (- start-x distance))
         min-y            (max -1 (- start-y distance))
         max-x            (min width (+ start-x distance))
         max-y            (min height (+ start-y distance))]
     (case direction
       :left  (map (fn [x] [x start-y]) (range start-x min-x -1))
       :right (map (fn [x] [x start-y]) (range start-x max-x))
       :up    (map (fn [y] [start-x y]) (range start-y min-y -1))
       :down  (map (fn [y] [start-x y]) (range start-y max-y))))))

(defn adjacent-xy
  [x y direction]
  (let [x           (case direction
                      :left  (dec x)
                      :right (inc x)
                      :up-left (dec x)
                      :up-right (inc x)
                      :down-left (dec x)
                      :down-right (inc x)
                      x)
        y           (case direction
                      :up   (dec y)
                      :down (inc y)
                      :up-left (dec y)
                      :up-right (dec y)
                      :down-left (inc y)
                      :down-right (inc y)
                      y)]
    [x y]))

(defn adjacent-pos
  [{x :x y :y} direction]
  (apply rc/xy->pos (adjacent-xy x y direction)))

(defn player-adjacent-pos
  [state direction]
  (let [{x :x y :y} (rp/player-pos state)
        x           (case direction
                      :left  (dec x)
                      :right (inc x)
                      :up-left (dec x)
                      :up-right (inc x)
                      :down-left (dec x)
                      :down-right (inc x)
                      x)
        y           (case direction
                      :up   (dec y)
                      :down (inc y)
                      :up-left (dec y)
                      :up-right (dec y)
                      :down-left (inc y)
                      :down-right (inc y)
                      y)]
    {:x x :y y}))

(defn player-adjacent-xy
  [state direction]
  (rc/pos->xy (player-adjacent-pos state direction)))

(defn player-adjacent-cell
  [state direction]
  (apply get-cell state (rc/pos->xy (player-adjacent-pos state direction))))

(defn direction->cells
  ([state direction]
   (direction->cells state direction 5))
  ([state direction distance]
    (let [[start-x
           start-y] (player-adjacent-xy state direction)]
     (direction->cells state start-x start-y direction distance)))
  ([state start-x start-y direction distance]
   {:pre [(contains? #{:left :right :up :down} direction)]}
   (map (fn [[x y]] (println "xy" x y) (get-cell state x y)) (direction->xys state start-x start-y direction distance))))

(defn direction->cellsxy
  ([state direction distance]
    (let [[start-x
           start-y] (player-adjacent-xy state direction)]
     (direction->cellsxy state start-x start-y direction distance)))
  ([state start-x start-y direction distance]
   (map (fn [[x y]] [(get-cell state x y) x y])
        (direction->xys state start-x start-y direction distance))))

(defn npc-at-xy
  "npc at [x y] of the current place. Otherwise `nil`."
  [state x y]
  (first (filter (fn [npc] (and (= (-> npc :pos :x) x)
                                (= (-> npc :pos :y) y)))
                 (if-let [place-id (current-place-id state)]
                   (get-in state [:world :places place-id :npcs])
                   (get-in state [:world :npcs])))))

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
               :saltwater-hole
               ;; traps
               :crushing-wall
               ;; pirate ship types
               :bulkhead
               :wheel
               :bulkhead2
               :wooden-wall
               :tackle
               :canon
               :canon-trunk
               :grate
               :breach
               :table
               :mast
               :beam
               :locker
               :locker2
               :ships-wheel
               :porthole
               :chest
               :railing
               ;; ruined temple types
               :empty
               :vertical-wall-alt
               :horizontal-wall-alt
               :upper-left-1
               :upper-right-1
               :bottom-left-1
               :bottom-right-1
               :upper-left-2
               :upper-right-2
               :bottom-left-2
               :bottom-right-2
               :moss-vertical-wall
               :moss-horizontal-wall
               :moss-vertical-wall-alt
               :moss-horizontal-wall-alt
               :moss-upper-left-1
               :moss-upper-right-1
               :moss-bottom-left-1
               :moss-bottom-right-1
               :moss-upper-left-2
               :moss-upper-right-2
               :moss-bottom-left-2
               :moss-bottom-right-2
               :white-vertical-wall
               :white-horizontal-wall
               :white-vertical-wall-alt
               :white-horizontal-wall-alt
               :white-upper-left-1
               :white-upper-right-1
               :white-bottom-left-1
               :white-bottom-right-1
               :white-upper-left-2
               :white-upper-right-2
               :white-bottom-left-2
               :white-bottom-right-2} t))

(defn type->water?
  [t]
  (contains? #{:water :surf} t))

(defn type->flammable?
  [t]
  (contains? #{:tree :palm-tree :fruit-tree :bamboo :tall-grass :short-grass :campfire :log} t))

(defn type->on-fire?
  [t]
  (contains? #{:lava :fire :campfire} t))

(defn type->shelter?
  [t]
  (contains? #{:ramada :lean-to :tarp-shelter} t))

(defn type->destroyable?
  [t]
  (contains? #{:chest
               :palisade
               :tree
               :bamboo
               :palm-tree
               :fruit-tree
               :tall-grass} t))

(defn type->intertidal?
  [t]
  (contains? #{:sand :surf} t))

(defn collide-trap?
   [state x y]
   (when-let [place-id (current-place-id state)]
     (some (fn [trap]
             (case (get trap :type)
               :crushing-wall
                 (some (fn [[trap-x trap-y]]
                         (and (= x trap-x)
                              (= y trap-y)))
                       (first (get trap :locations)))
               false))
          (get-in state [:world :places place-id :traps] []))))

(defn collide?
  "Return `true` if the cell at `[x y]` is non-traverable. Ie: a wall, closed door or simply does
   not exist. Cells occupied by npcs are considered non-traversable."
  ([state x y opts]
    ;; destructure opts into variables with defaults
    (let [{:keys [include-npcs?
                  collide-player?
                  collide-water?]
           :or {include-npcs? true
                collide-player? true
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
        ;; trap collides?
        (collide-trap? state x y)
        ;; player collides?
        (and collide-player?
             (= [x y] (rp/player-xy state)))
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

(defmulti first-collidable-object 
  "Return a map based on the first collidable cell in cellsxy.
   If an npc is encountered first, return `{:npc npc}`.
   If the player is encountered first, return `{:player player}`.
   If a non-npc is encountered first, return `{:cell cell}`.
   If no collision occurs in any of the cells, return `{}`."
  (fn [& more]
    (cond (= (count more) 3)
            :direction-distance
          (and (= (count more) 5)
               (keyword? (nth more 3)))
            :direction-distance-with-startxy
          :else
            :cellsxy)))

(defmethod first-collidable-object :direction-distance
  [state direction max-distance]
    (let [[start-x
           start-y] (player-adjacent-xy state direction)]
      (first-collidable-object state start-x start-y direction max-distance)))

(defmethod first-collidable-object :direction-distance-with-startxy
  [state start-x start-y direction max-distance]
  {:pre [(contains? #{:up :down :left :right} direction)]}
  (let [cellsxy   (remove (fn [[cell x y]]
                            (rc/farther-than? (rc/xy->pos x y)
                                              (get-in state [:world :player :pos])
                                              max-distance))
                          (direction->cellsxy state start-x start-y direction max-distance))]
    (first-collidable-object state cellsxy)))

(defmethod first-collidable-object :cellsxy
  [state cellsxy]
  (println "cellsxy" cellsxy)
  (let [player-xy (rp/player-xy state)]
    (loop [[pcell px py] (first cellsxy)
           [cell x y]    (second cellsxy)
           xs            (nnext cellsxy)]
      (let [npc (npc-at-xy state x y)]
        (cond
          (= [x y] player-xy)
            {:player (rp/get-player state)}
          (not (nil? npc))
            {:npc npc :pos (rc/xy->pos x y)}
          (collide? state x y false)
            {:cell cell :pos (rc/xy->pos x y) :prev-pos (rc/xy->pos px py)}
          (is-trap-type? (get cell :type))
            {:trap cell :pos (rc/xy->pos x y)}
          (empty? xs)
            {}
          :else
            (recur [cell x y] (first xs) (rest xs)))))))

(defn player-adjacent-xys
  [state]
  (let [[x y] (rp/player-xy state)]
    [[(dec x) y]
     [(inc x) y]
     [x (dec y)]
     [x (inc y)]]))

(defn player-adjacent-xys-ext
  [state]
  (let [[x y] (rp/player-xy state)]
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
  (let [[x y]      (rp/player-xy state)
        player-pos (rp/player-pos state)]
    (map (fn [[x y]] (get-cell state x y))
         (remove (fn [pos] rc/farther-than? rp/player-pos pos distance)
                 (map (fn [[x y]] (rc/xy->pos x y))
                      (rect->xys [(- x distance) (- y distance)]
                                 [(+ x distance) (+ y distance)]))))))


(defn player-mounted-on-raft?
  [state]
  (and (get-in state [:world :player :mounted] false)
       (contains? (set (map :item/id (get (first (player-cellxy state)) :items []))) :raft)))


(defn inventory-and-player-cell-items
  [state]
  (let [[cell _ _]        (player-cellxy state)
        inventory         (ri/player-inventory state)
        cell-items        (get cell :items [])
        remaining-hotkeys (get-in state [:world :remaining-hotkeys])
        inventory         (vec (rc/fill-missing #(not (contains? % :hotkey))
                                             #(assoc %1 :hotkey %2)
                                             remaining-hotkeys
                                             (concat inventory cell-items)))]
    inventory))

(defn inventory-and-player-cell-hotkey->item
  [state hotkey]
  (first (filter (fn [item] (= hotkey (get item :hotkey))) (inventory-and-player-cell-items state))))

(def night-cache (atom {}))

(defn is-night?
  [state]
  (let [atmo   (get-in state [:data :atmo])
        frames (count atmo)
        t      (mod (get-in state [:world :time]) frames)]
    (if (contains? @night-cache (/ t 80))
      (get @night-cache (/ t 80))
      (let [frame  (nth atmo t)
            values (flatten frame)
            night? (zero? (reduce + values))]
      (swap! night-cache (fn [c] (assoc c (/ t 80) night?)))
      night?))))

(defn is-day?
  [state]
  (not (is-night? state)))

