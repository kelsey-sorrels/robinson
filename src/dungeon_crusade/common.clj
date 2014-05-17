;; Utility functions and functions for manipulating state
(ns dungeon-crusade.common
  (:use [dungeon-crusade.mapgen :exclude [-main]]
        [dungeon-crusade.itemgen :exclude [-main]])
  (:require [taoensso.timbre :as timbre]))

(timbre/refer-timbre)


(defn chebyshev-distance
  "Manhattan distance between 2 points"
  [p1 p2]
  (max (Math/abs (- (:x p1) (:x p2)))
       (Math/abs (- (:y p1) (:y p2)))))

(defn distance
  "Euclidean distance between 2 points"
  [p1 p2]
  (Math/pow (+ (Math/pow (- (:x p1) (:x p2)) 2)
               (Math/pow (- (:y p1) (:y p2)) 2))
            0.5))

(defn farther-than?
  "Are the two points farther in distance than l?"
  [p1 p2 l]
  (if (or (> (chebyshev-distance p1 p2) l)
          (> (distance p1 p2) l))
    false
    true))

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
  (fn-in (fn [coll _] (map f coll)) m ks nil))

(defn filter-in
  [m ks f]
  (fn-in (fn [coll _] (filter f coll)) m ks nil))

(defn remove-in
  [m ks f]
  (fn-in (fn [coll _] (remove f coll)) m ks nil))

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

(defn map-with-xy
  "Non-lazily call `(f cell x y)` for each cell in the grid."
  [f grid]
  (doall 
    (map (fn [e] (apply f e)) (with-xy grid))))

(defn get-xy
  "Retrieve the cell at the position `[x y]` within the given grid. If `[x y]` is outside the bounds
   of the grid, return `nil`."
  [x y grid]
  (when-first [cell (filter (fn [[cell cx cy]] (and (= x cx) (= y cy))) (with-xy grid))]
    cell))

(defn player-cellxy
  "Retrieve the cell at which the player is located."
  [state]
  (let [x (-> state :world :player :pos :x)
        y (-> state :world :player :pos :y)]
    (get-xy x y (current-place state))))

(defn player-dead?
  "Return `true` if the player has a status of `:dead`."
  [state]
  (contains? (-> state :world :player :status) :dead))
 
(defn append-log
  "Append a message to the in-game log. The last five log messages are retained."
  ([state message]
   (append-log state :log message))
  ([state log message]
   (assoc-in state
             [:world log]
             (vec (take-last 5 (conj (-> state :world :log)
                                     {:text message
                                      :time (-> state :world :time)}))))))

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

