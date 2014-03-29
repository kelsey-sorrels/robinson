(ns dungeon-crusade.common)

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


(defn ascii-to-place [ascii extras]
  "ascii: an ascii representatin of the place
   extra: a list of [[k & ks] v] where the first element is a list of keys
          to assoc-in the place, and the second is the value to associate"
  (let [
    char-to-cell (fn [c]
      (case c
        \| {:type :vertical-wall}
        \- {:type :horizontal-wall}
        \. {:type :floor}
        \+ {:type :door-closed}
        \# {:type :corridor}
        nil))
    ;; convert ascii to place
    setting (vec (map (fn [line] (vec (map char-to-cell line))) ascii))
    ;; add in extras like items, monsters, etc.
    ;; create a list of functions that can be applied to assoc extras, then create a composition of
    ;; so that setting can pass through each fn in turn.
    place ((apply comp (map (fn [e] (partial (fn [e s] (apply assoc-in s e)) e)) extras)) setting)]
    (println "setting" setting)
    (println "place" place)
    place))

(defn init-place-0 []
  (ascii-to-place
    ["----   ----"
     "|..+## |..|"
     "|..| ##+..|"
     "----   ----"]
    [[[1 9 :items] [{:type :ring   :name "Ring of Power"}]]
     [[1 1 :items] [{:type :scroll :name "Scroll of Power"}]]]))

(defn test-inventory []
  [{:type :food :name "Ration"          :hunger 10}
   {:type :food :name "Rotten Eyeballs" :hunger 1}])

(defn init-world []
  ;; Assign hotkeys to inventory and remove from remaining hotkeys
  (let [inventory (test-inventory)
        remaining-hotkeys (vec (seq "abcdefghijklmnopqrstuvwxyzABCdEFGHIJKLMNOPQRSTUVWQYZ"))
        hotkey-groups (split-at (count inventory) remaining-hotkeys)
        inventory-with-hotkeys (vec (map #(assoc %1 :hotkey %2) inventory (first hotkey-groups)))
        remaining-hotkeys (vec (apply str (second hotkey-groups)))]

  {:places {:0 (init-place-0)}
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
            :pos {:x 2 :y 1}
            :inventory inventory-with-hotkeys
            :status #{}}
    :npcs {:0 [{:x 8 :y 1 :type :rat :hp 9 :attacks #{:bite :claw}}
               {:x 9 :y 1 :type :rat :hp 9 :attacks #{:bite :claw}}]}}))

(defn current-place [state]
  (let [current-place (-> state :world :current-place)]
    (-> state :world :places current-place)))


(defn set-last-command [state command]
  (println "setting last-command " command)
  (assoc-in state [:last-command] command))

(defn get-last-command [state]
  (state :last-command))

(defn with-xy [place]
  (mapcat concat (map-indexed (fn [y line] (map-indexed (fn [x cell] [cell x y]) line)) place)))

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
                                                                     :door-closed])
        ;; not a wall or closed door, check for npcs
        (npc-at-xy x y state)))))

(defn player-cellxy [state]
  (let [x (-> state :world :player :pos :x)
        y (-> state :world :player :pos :y)]
    (get-xy x y (current-place state))))

(defn player-dead? [state]
  (contains? (-> state :world :player :status) :dead))
 
