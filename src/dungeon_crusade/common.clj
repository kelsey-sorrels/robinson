(ns dungeon-crusade.common
  (:use [dungeon-crusade.mapgen :exclude [-main]]))

(defmacro defsource
    "Similar to clojure.core/defn, but saves the function's definition in the var's
       :source meta-data."
    {:arglists (:arglists (meta (var defn)))}
    [fn-name & defn-stuff]
    `(do (defn ~fn-name ~@defn-stuff)
                (alter-meta! (var ~fn-name) assoc :source (quote ~&form))
                (var ~fn-name)))


(defn fill-missing [pred f vcoll coll]
    "For each item in coll that (pred item) returns true, replace that element with the result
       of (f item vcoll-item) where vcoll-item starts with (first vcoll) and proceeds to the next
       element each time (pred item) is true.
       Ex: user=> (fill-missing #(not (contains? % :val))
                                #(assoc %1 :val %2)
                                [1 2 3]
                                [{:val \\a} {} {:val \\b} {:val \\c} {} {} {}])
       ({:val \\a} {:val 1} {:val \\b} {:val \\c} {:val 2} {:val 3} {:val nil})"
    (if (empty? coll)
          coll
          (let [x  (first coll)
                          xs (rest coll)
                          y  (first vcoll)
                          ys (rest vcoll)]
                  (if (pred x)
                            (cons (f x y) (if (empty? xs) [] (fill-missing pred f ys xs)))
                            (cons x (if (empty? xs) [] (fill-missing pred f vcoll xs)))))))

(defn with-xygrid [place]
  (map-indexed (fn [y line] (map-indexed (fn [x cell] [cell x y]) line)) place))

(defn with-xy [place]
  (mapcat concat (map-indexed (fn [y line] (map-indexed (fn [x cell] [cell x y]) line)) place)))

(defn ascii-to-place [ascii]
  "ascii: an ascii representatin of the place
   extra: a list of [[k & ks] v] where the first element is a list of keys
          to assoc-in the place, and the second is the value to associate"
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

(defn add-extras [place extras]
  "Adds extras to a place like items, and special cell types"
  "extras are in the format of [[[x y] object] [[x y] object] &]"
  "objects are cells with a type and maybe items {:type :floor :items []}"
  ;; create a list of functions that can be applied to assoc extras, then create a composition of
  ;; so that setting can pass through each fn in turn.
  (do (println "extras" extras)
  (reduce (fn [place [[x y] & r]]
           (let [args (concat [[y x]] r)
                _ (println "assoc-in place" args)]
                 (apply assoc-in place args))) place extras)))

(defn init-place-0 []
  (add-extras (ascii-to-place
    ["----   ----"
     "|..+## |..|"
     "|..| ##+..|"
     "----   ----"])
    [[[1 9 :items] [{:type :ring   :name "Ring of Power"}]]
     [[1 1 :items] [{:type :scroll :name "Scroll of Power"}]]
     [[2 9]        {:type :down-stairs :dest-place :1}]]))

(defn init-place-1 []
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


(defn init-random-0 []
  (let [place       (random-place 30 20)
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

(defn init-random-n [level]
  (let [place       (random-place 30 20)
        _ (println "place" place)
        _ (println "(place :up-stairs)" (place :up-stairs))
        _ (println "(place :down-stairs)" (place :down-stairs))
        _ (println "level" level)
        _ (println "former place id" (keyword (str (dec level))))
        up-stairs   (assoc-in (place :up-stairs) [1 :dest-place] (keyword (str (dec level))))
        _ (println "up-stairs" up-stairs)
        down-stairs (assoc-in (place :down-stairs) [1 :dest-place] (keyword (str (inc level))))
        place       (place :place)
        _ (println "place" place)]
    (add-extras place [down-stairs up-stairs])))

(defn test-inventory []
  [{:type :food :name "Ration"          :hunger 10}
   {:type :food :name "Rotten Eyeballs" :hunger 1}])

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
    :npcs {:0 [{:x (+ starting-x 3) :y (+ starting-y 3) :type :rat :hp 9 :attacks #{:bite :claw}}
               {:x 9 :y 1 :type :rat :hp 9 :attacks #{:bite :claw}}]}}))

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
    (some (fn [npc] (when (and (= x (npc :x)) (= y (npc :y))) npc)) npcs)))

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
 
