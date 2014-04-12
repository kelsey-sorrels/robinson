;; Utility functions and functions for manipulating state
(ns dungeon-crusade.common
  (:use [dungeon-crusade.mapgen :exclude [-main]]
        [dungeon-crusade.itemgen :exclude [-main]]))

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
  (do (println "extras" extras)
  (reduce (fn [place [[x y] & r]]
           (let [args (concat [[y x]] r)
                _ (println "assoc-in place" args)]
                 (apply assoc-in place args))) place extras)))

(defn init-place-0
  "Create an example place with two rooms, some doors, two items
   and a set of down stairs that lead to place `:1`"
  []
  (add-extras (ascii-to-place
    ["----   ----"
     "|..+## |..|"
     "|..| ##+..|"
     "----   ----"])
    [[[1 9 :items] [{:type :ring   :name "Ring of Power"}]]
     [[1 1 :items] [{:type :scroll :name "Scroll of Power"}]]
     [[2 9]        {:type :down-stairs :dest-place :1}]]))

(defn init-place-1
  "Create an example place with two rooms, a door, two items
   and a set of up stairs to place `:0`"
  []
  (add-extras (ascii-to-place
    ["-----------            "
     "|.........|            "
     "|.........|            "
     "--------+--            "
     "        #              "
     "        ##   --------  "
     "         #   |......|  "
     "         ####.......|  "
     "             --------  "
     "                       "])
    [[[6 15  :items] [{:type :ring   :name "Ring of Power"}]]
     [[6 14  :items] [{:type :scroll :name "Scroll of Power"}]]
     [[7 19]         {:type :up-stairs :dest-place :0}]]))


(defn init-random-0
  "Create a random grid suitable for a starting level.
   Contains a down stairs, and identifies the starting location as
   the spot that would have been reserved for up stairs."
  []
  (let [place       (random-place 50 27)
        _ (println "place" place)
        _ (println "(place :up-stairs)" (place :up-stairs))
        _ (println "(place :down-stairs)" (place :down-stairs))
        down-stairs (assoc-in (place :down-stairs) [1 :dest-place] :1)
        starting-location [[(-> place :up-stairs first first)
                            (-> place :up-stairs first second)]
                           {:type :floor :starting-location true}]
        _ (println "starting-location" starting-location)
        place       (place :place)
        _ (println "place" place)]
    (add-extras place [down-stairs starting-location])))

(defn init-random-n
  "Creates a random grid suitable for a non-starting level.
   Contains a down stairs, an up stairs, and five random items
   placed in floor cells."
  [level]
  (let [place       (random-place 50 27)
        _ (println "place" place)
        _ (println "(place :up-stairs)" (place :up-stairs))
        _ (println "(place :down-stairs)" (place :down-stairs))
        _ (println "level" level)
        _ (println "former place id" (keyword (str (dec level))))
        up-stairs   (assoc-in (place :up-stairs) [1 :dest-place] (keyword (str (dec level))))
        _ (println "up-stairs" up-stairs)
        down-stairs (assoc-in (place :down-stairs) [1 :dest-place] (keyword (str (inc level))))
        place       (place :place)
        drops       (map (fn [pos]
                           [pos {:type :floor :items [(gen-item)]}])
                           (take 5 (shuffle
                              (map (fn [[_ x y]] [x y]) (filter (fn [[cell x y]] (and (not (nil? cell))
                                                                                      (= (cell :type) :floor)))
                                                                (with-xy place))))))
        _ (println "drops" drops)
        _ (println "place" place)]
    (add-extras place (concat [down-stairs up-stairs] drops))))

(defn test-inventory []
  (gen-items 5))

(defn init-world []
  ;; Assign hotkeys to inventory and remove from remaining hotkeys
  (let [inventory              (test-inventory)
        remaining-hotkeys      (vec (seq "abcdefghijklmnopqrstuvwxyzABCdEFGHIJKLMNOPQRSTUVWQYZ"))
        hotkey-groups          (split-at (count inventory) remaining-hotkeys)
        inventory-with-hotkeys (vec (map #(assoc %1 :hotkey %2) inventory (first hotkey-groups)))
        remaining-hotkeys      (vec (apply str (second hotkey-groups)))
        place-0                (init-random-0)
        [_ starting-x
           starting-y]         (first (filter (fn [[cell x y]] (contains? cell :starting-location))
                                              (with-xy place-0)))
        starting-pos           {:x starting-x :y starting-y}
        _ (println "starting-pos" starting-pos)]

  {:places {:0 place-0}
            ;:1 (init-place-1)}
   :current-place :0
   :time 0
   :current-state :normal
   :selected-hotkeys #{}
   :remaining-hotkeys remaining-hotkeys
   :log []
   :player {:hp 10
            :max-hp 10
            :$ 0
            :xp 0
            :level 0
            :sym "@"
            :hunger 0
            :pos starting-pos
            :inventory inventory-with-hotkeys
            :status #{}}
   :quests {}
   :npcs {:0 []}}));{:x (+ starting-x 3) :y (+ starting-y 3) :type :rat :hp 9 :attacks #{:bite :claw}}
               ;{:x 9 :y 1 :type :rat :hp 9 :attacks #{:bite :claw}}]}}))

(defn current-place [state]
  (let [current-place (-> state :world :current-place)]
    (-> state :world :places current-place)))

(defn map-with-xy [f place]
  (doall 
    (map (fn [e] (apply f e)) (with-xy place))))

(defn get-xy [x y place]
  (when-first [cell (filter (fn [[cell cx cy]] (and (= x cx) (= y cy))) (with-xy place))]
    cell))

(defn npc-at-xy [x y state]
  (let [current-place-id (-> state :world :current-place)
        npcs             (-> state :world :npcs current-place-id)]
    (some (fn [npc] (when (and (= x (-> npc :pos :x)) (= y (-> npc :pos :y))) npc)) npcs)))

(defn collide? [x y state]
  (let [cellxy (get-xy x y (current-place state))]
    (println "collide? " cellxy)
    (let [cell (first cellxy)]
      ;; check the cell to see if it is a wall or closed door
      (or
        (-> cell nil?)
        (some (fn [collision-type] (= (cell :type) collision-type)) [:vertical-wall
                                                                     :horizontal-wall
                                                                     :close-door])
        ;; not a wall or closed door, check for npcs
        (npc-at-xy x y state)))))

(defn player-cellxy [state]
  (let [x (-> state :world :player :pos :x)
        y (-> state :world :player :pos :y)]
    (get-xy x y (current-place state))))

(defn player-dead? [state]
  (contains? (-> state :world :player :status) :dead))
 
(defn append-log [state message]
  (assoc-in state [:world :log] (vec (take 5 (conj (-> state :world :log) {:text message :time (-> state :world :time)})))))

