;; Utility functions and functions for manipulating state
(ns robinson.common
  ;;(:use [robinson.mapgen :exclude [-main]])
  (:require [ clojure.data.generators :as dg]
            [taoensso.timbre :as timbre]
            [pallet.thread-expr :as tx]))

(timbre/refer-timbre)

(defn uniform-int
 ([hi] (uniform-int 0 hi))
 ([lo hi] (int (dg/uniform lo hi))))

(letfn [(arg-when- [threader arg sym condition body]
  `(let [~sym ~arg
         arg# ~arg]
    (if ~condition
      (~threader arg# ~@body)
      arg#)))]
  (defmacro arg-when->
    "Lexically assign the threaded argument to the specified symbol
    and conditionally execute the body statements.
    (-> 3
      (arg-when-> [x] (> x 2) (* x)))
    (-> 2
      (arg-when-> [x] (> x 2) (* x)))
    => 2"
    {:indent 1}
    [arg [sym] condition & body]
    (arg-when- 'clojure.core/-> arg sym condition body)))

(letfn [(arg-if- [threader arg sym condition form else-form]
  `(let [~sym ~arg
         arg# ~arg]
    (if ~condition
      (~threader arg# ~form)
      (~threader arg# ~else-form)
      arg#)))]
  (defmacro arg-if->
    "Lexically assign the threaded argument to the specified symbol
    and conditionally execute the body statements.
    (-> 3
      (arg-when-> [x] (> x 2) (* x)))
    (-> 2
      (arg-when-> [x] (> x 2) (* x)))
    => 2"
    {:indent 1}
    [arg [sym] condition form else-form]
    (arg-if- 'clojure.core/-> arg sym condition form else-form)))

(defn pos->xy
  [{x :x y :y}]
  [x y])

(defn xy->pos
  [x y]
  {:x x :y y})

(defn chebyshev-distance
  "Chebyshev/chessboard distance between 2 points"
  [p1 p2]
  (max (Math/abs (- (:x p1) (:x p2)))
       (Math/abs (- (:y p1) (:y p2)))))

(defn distance
  "Euclidean distance between 2 points"
  [p1 p2]
  (let [sq (fn [x] (* x x))]
  (Math/sqrt(+ (sq (- (:x p1) (:x p2)))
               (sq (- (:y p1) (:y p2)))))))

(defn farther-than?
  "Are the two points farther in distance than l?"
  [p1 p2 l]
  (or (> (chebyshev-distance p1 p2) l)
      (> (distance p1 p2) l)))

(defn distance-from-player
  "Calculate the distance between the player and pos."
  [state pos]
  (distance (-> state :world :player :pos) pos))

(defn adjacent-to-player?
  "Return true is pos is adjacent to the player's pos including diagonals."
  [state pos]
  (<= (distance-from-player state pos) 1.5))
 

(defn fill-missing
  "For each item in coll for which (pred item) returns true, replace that
   element with the result of (f item vcoll-item) where vcoll-item
   starts with (first vcoll) and proceeds to the next element each
   time (pred item) evaluates to true. If the collection vcoll is exhausted,
   nil will fill the remaining values.

       user=> (fill-missing
                #(not (contains? % :val))
                #(assoc %1 :val %2)
                [1 2 3]
                [{:val :a}
                 {}
                 {:val :b}
                 {:val :c}
                 {}
                 {}
                 {}])
       ({:val :a}
        {:val 1}
        {:val :b}
        {:val :c}
        {:val 2}
        {:val 3}
        {:val nil})"
    [pred f vcoll coll]
    (if (empty? coll)
          coll
          (let [x  (first coll)
                          xs (rest coll)
                          y  (first vcoll)
                          ys (rest vcoll)]
                  (if (pred x)
                            (cons (f x y) (if (empty? xs) [] (fill-missing pred f ys xs)))
                            (cons x (if (empty? xs) [] (fill-missing pred f vcoll xs)))))))

(defn fn-in
  "Applies a function to a value in a nested associative structure and an input value.
   ks sequence of keys and v is the second arguement to f. The nested value will be
   updated to (f o v) where o is the value that would be returned by (get-in m ks)."
  [f m ks v]
  (update-in m ks (fn [coll] (f coll v))))

(defn concat-in
  [m ks v]
  (fn-in concat m ks v))

(defn conj-in
  [m ks v]
  (fn-in conj m ks v))

(defn map-in
  [m ks f]
  (fn-in (fn [coll _] (if (vector? coll)
                        (vec (map f coll))
                        (map f coll)))
         m ks nil))

(defn filter-in
  [m ks f]
  (fn-in (fn [coll _] (if (vector? coll)
                        (vec (filter f coll))
                        (filter f coll))) m ks nil))

(defn some-in
  [m ks f]
  (fn-in (fn [coll _] (if (vector? coll)
                        (vec (some f coll))
                        (some f coll))) m ks nil))

(defn remove-in
  [m ks f]
  (fn-in (fn [coll _] (if (vector? coll)
                        (vec (remove f coll))
                        (remove f coll))) m ks nil))

(defn update-in-matching
  [m ks p f]
  (if (fn? p)
    (map-in m ks (fn [e] (if (p e)
                           (f e)
                           e)))
    (update-in-matching m ks (partial = p) f)))

(defn map-indexed-in-p
  "Update in m the collection accessible by (get-in m ks)
   for which (p item) returns true for an item in the collection
   is replaced with the value of (f index element) where index
   starts at zero and is incremented with each call to f."
  [m ks p f]
  (update-in m ks (fn [coll]
    (loop [i 0
           j 0
           result []]
      (debug "loop i" i "j" j "count coll" (count coll))
      (if (>= j (count coll))
        (do (debug result)
        result)
        (let [item (nth coll j)]
          (debug "item" item)
          (if (p item)
            (recur (inc i) (inc j) (conj result (f i item)))
            (recur i (inc j) (conj result item)))))))))

(defn remove-first
  "Removes the first matching element from coll."
  [e coll]
  (if (not (fn? e))
    (remove-first (partial = e) coll)
    (let [[l1 l2] (split-with (complement e) coll)]
      (concat l1 (rest l2)))))

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

(defn ascii-to-place
  "Convert an acscii representation of a grid into a grid of cells.
   Each character mapps to a cell. A cell is a map that contains a key `:type`
   with a value as shown here.

       | = :vertical-wall
       - = :horizontal-wall
       . = :floor
       + = :closed-door
       # = :corridor
       < = :down-stairs
       > = :up-stairs
       ~ = :water
       _ = :sand
       , = :dirt
       ` = :gravel
       ' = :short-grass
      \" = :tall-grass
       T = :tree

   An example collection of ascii lines.

       [\"----   ----\"
        \"|..+## |..|\"
        \"|..| ##+..|\"
        \"----   ----\"]

   extra: a list of `[[k & ks] v]` where the first element is a list of keys
          to assoc-in the place, and the second is the value to associate.

       [[[1 9 :items]
        [{:type :ring
          :name \"Ring of Power\"}]]
        [[1 1 :items]
         [{:type :scroll
          :name \"Scroll of Power\"}]]
        [[2 9]
         {:type :down-stairs
          :dest-place :1}]]"
  [ascii]
  (let [
    char-to-cell (fn [c]
      (case c
        \| {:type :vertical-wall}
        \- {:type :horizontal-wall}
        \. {:type :floor}
        \+ {:type :closed-door}
        \# {:type :corridor}
        \< {:type :up-stairs}
        \> {:type :down-stairs}
        \~ {:type :water}
        \_ {:type :sand}
        \, {:type :dirt}
        \` {:type :gravel}
        \' {:type :short-grass}
        \" {:type :tall-grass}
        \T {:type :tree}
        nil))]
    ;; convert ascii to place
    (vec (map (fn [line] (vec (map char-to-cell line))) ascii))))

(defn add-extras
  "Adds extras to a place like items, and special cell types 
   extras are in the format of `[[[x y] object] [[x y] object] &]` 
   objects are cells with a type and maybe items `{:type :floor :items []}`"
  [place extras]
  ;; create a list of functions that can be applied to assoc extras, then create a composition of
  ;; so that setting can pass through each fn in turn.
  (debug "add-extras" place extras)
  (reduce (fn [place [[x y] & r]]
           (let [args (concat [[y x]] r)
                 _ (debug "assoc-in place" args)]
                 (apply assoc-in place args))) place extras))

(defn current-place-id
  "Retrieve the current place id."
  [state]
  (-> state :world :current-place))

(defn current-place
  "Retrieve the current place."
  [state]
  (get-in state [:world :places (current-place-id state)]))

(defn current-state
  [state]
  (get-in state [:world :current-state]))

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

(defn get-cell
  "Retrieve the cell at the position `[x y]` within the given grid. If `[x y]` is outside the bounds
   of the grid, return `nil`."
  [grid x y]
  (get-in grid [y x]))

(defn get-cell-at-current-place
  [state x y]
  (get-cell (current-place state) x y))

(defn player-cellxy
  "Retrieve the cell at which the player is located."
  [state]
  (let [x (-> state :world :player :pos :x)
        y (-> state :world :player :pos :y)]
    [(get-cell (current-place state) x y) x y]))

(defn adjacent-cells
  "Return a collection of adjacent cells (diagonals not-included) around pos.
   Pos is a map with the keys `:x` and `:y`.

   Ex: `(adjacent-cells [...] {:x 0 :y 0})`
   `[{:type :floor} {:type :water}...]`"
  [place {x :x y :y}]
  (map (partial get-in place)
    [{:x (dec x) :y y}
     {:x (inc x) :y y}
     {:x x :y (dec y)}
     {:x x :y (inc y)}]))

(defn direction->cells
  [state direction]
  {:pre [(contains? #{:left :right :up :down} direction)]}
  (let [{x :x y :y} (get-in state [:world :player :pos])
        place (current-place state)]
   (vec
     (case direction
       :left  (reverse (take x (get place y)))
       :right (rest (drop x (get place y)))
       :up    (reverse (take y (map #(nth % x) place)))
       :down  (rest (drop y (map #(nth % x) place)))))))

(defn direction->xys
  [state direction]
  {:pre [(contains? #{:left :right :up :down} direction)]}
  (let [{x :x y :y} (get-in state [:world :player :pos])
        place (current-place state)
        max-x (count (first place))
        max-y (count place)]
    (case direction
      :left  (reverse (map (fn [x] [x y]) (range 0 x)))
      :right (map (fn [x] [x y]) (range (inc x) max-x))
      :up    (reverse (map (fn [y] [x y]) (range 0 y)))
      :down  (map (fn [y] [x y]) (range (inc y) max-y)))))

(defn direction->cellsxy
  [state direction]
  (map (fn [[x y]] [(get-in state [:world :places (current-place-id state) y x]) x y])
       (direction->xys state direction)))

(defn npc-at-xy
  "npc at [x y] of the current place. Otherwise `nil`."
  [state x y]
  (first (filter (fn [npc] (and (= (get npc :place) (current-place-id state))
                                (= (-> npc :pos :x) x)
                                (= (-> npc :pos :y) y)))
                 (get-in state [:world :npcs]))))


(defn collide?
  "Return `true` if the cell at `[x y]` is non-traverable. Ie: a wall, closed door or simply does
   not exist. Cells occupied by npcs are considered non-traversable."
  ([state x y opts]
    ;; destructure opts into variables with defaults
    (let [{:keys [include-npcs?
                  collide-water?]
           :or {include-npcs? true
                collide-water? true}} opts
          cell (get-cell (current-place state) x y)]
      ;(debug "collide? " cell x y)
      (or
        (nil? cell)
        ;; check the cell to see if it is a wall or closed door
        (some (fn [collision-type] (= (cell :type) collision-type)) [:vertical-wall
                                                                     :horizontal-wall
                                                                     :close-door
                                                                     :tree
                                                                     :palm-tree
                                                                     :fruit-tree
                                                                     :dry-hole
                                                                     :freshwater-hole
                                                                     :saltwater-hole])
        ;; water collides?
        (and collide-water?
             (= (cell :type) :water))
        ;; not a wall or closed door, check for npcs
        (and include-npcs?
             (npc-at-xy state x y)))))

  ([state x y]
  (collide? state x y {})))


(defn first-collidable-object
 "Return a map based on the first collidable cell in cellsxy.
  If an npc is encountered first, return `{:npc npc}`.
  If a non-npc is encountered first, return `{:cell cell}`.
  If no collision occurs in any of the cells, return `{}`."
 [state direction]
 {:pre [(contains? #{:up :down :left :right} direction)]}
  (let [cellsxy (direction->cellsxy state direction)]
   (loop [[cell x y] (first cellsxy)
          xs         (rest cellsxy)]
     (let [npc (npc-at-xy state x y)]
       (cond
         (not (nil? npc))
           {:npc npc}
         (collide? state x y false)
           {:cell cell}
         (empty? xs)
           {}
         :else
           (recur (first xs) (rest xs)))))))

(defn append-log
  "Append a message to the in-game log. The last five log messages are retained."
  ([state message]
   (append-log state message :gray))
  ([state message color]
   (assoc-in state
             [:world :log]
             (vec (take-last 23 (conj (-> state :world :log)
                                     {:text message
                                      :time (-> state :world :time)
                                      :color color}))))))

(defn ui-hint
  [state msg]
  (assoc-in state [:world :ui-hint] msg))

(defn clear-ui-hint
  [state]
  (ui-hint state nil))

(defn conj-in-cell-items
  "Adds an item to [x y] in the current place. Simple, right?"
  [state item x y]
  (conj-in state [:world :places (-> state :world :current-place) y x :items] item))

(defn conj-in-current-cell-items
  "Adds an item to the player's cell's items."
  [state item]
  (let [x (-> state :world :player :pos :x)
        y (-> state :world :player :pos :y)]
    (conj-in-cell-items state item x y)))

(defn wrap-line [size text]
  (loop [left size line [] lines []
         words (clojure.string/split text #"\s+")]
    (if-let [word (first words)]
      (let [wlen (count word)
            spacing (if (== left size) "" " ")
            alen (+ (count spacing) wlen)]
        (if (<= alen left)
          (recur (- left alen) (conj line spacing word) lines (next words))
          (recur (- size wlen) [word] (conj lines (clojure.string/join line)) (next words))))
      (when (seq line)
        (conj lines (clojure.string/join line))))))

(defmacro log-time
  "Log the time it takes to execute body."
  [msg & body]
  `(time
     (let [result# (do ~@body)]
       (println ~msg)
       result#)))

(defn log-io
  "Log function inputs and outputs by wrapping an function f."
  [msg f]
  (fn [& args]
    (let [result (apply f args)]
      (println (format "(%s %s)=>%s" msg (str args) (str result)))
      result)))


