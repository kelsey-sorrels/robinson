;; Utility functions and functions for manipulating state
(ns robinson.worldgen
  (:use 
        robinson.common
        robinson.npc
        [robinson.mapgen :exclude [-main]]
        [robinson.itemgen :exclude [-main]])
  (:require [taoensso.timbre :as timbre]
            [clisk.live :as clisk]))

(timbre/refer-timbre)

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

;; clisk utils
(defn invert [a] (clisk/v+ [1 1 1] (clisk/v* [-1 -1 -1] a)))

(defn center [f]
  (clisk/offset [-0.5 -0.5] (clisk/scale 0.5 f)))

(defn center-radius []
  (clisk/radius (center [clisk/x clisk/y])))

(defn init-island
  "Create an example place with an island, two items
   and a set of down stairs that lead to place `:1`"
  []
  (let [_    (clisk/seed-simplex-noise!)
        node (clisk/vectorize
               (clisk/vlet [c (center (invert (clisk/offset (clisk/scale 0.43 (clisk/v* [0.5 0.5 0.5] clisk/vsnoise)) clisk/radius)))]
                 (clisk/v+
                   (clisk/vif (clisk/v+ [-0.7 -0.7 -0.7]  (clisk/v* c (clisk/v+ [0.4 0.4 0.4] (clisk/scale 0.05 clisk/noise))))
                     [0 0 1]
                     [0 0 0])
                   (clisk/vif (clisk/v+ [-0.6 -0.6 -0.6]  c)
                     [0 1 0]
                     [0 0 0])
                   (clisk/vif (clisk/v+ [-0.5 -0.5 -0.5]  c)
                     [1 0 0]
                     [0 0 0]))))
        fns  (vec (map clisk/compile-fn (:nodes node)))
        max-x 55
        max-y 20]
    (add-extras
      (vec
        (map vec
          (partition max-x
            (log-time "for" (for [y (range max-y)
                  x (range max-x)
                  :let [s (vec (map #(.calc ^clisk.IFunction % (double (/ x max-x)) (double (/ y max-y)) (double 0.0) (double 0.0))
                               fns))]]
              (case s
                [0.0 0.0 0.0] {:type :water}
                [1.0 0.0 0.0] {:type :sand}
                [1.0 1.0 0.0] (case (rand-int 3)
                                0 {:type :dirt}
                                1 {:type :gravel}
                                2 {:type :floor})
                [1.0 0.0 1.0] {:type :gravel}
                [1.0 1.0 1.0] (case (rand-int 7)
                                0 {:type :tree}
                                1 {:type :palm-tree}
                                2 {:type :fruit-tree}
                                3 {:type :tall-grass}
                                4 {:type :short-grass}
                                5 {:type :floor}
                                6 {:type :gravel})))))))
    [[[(int (/ max-x 2)) (int (/ max-y 2))]      {:type :dirt :starting-location true}]
     [[(int (inc (/ max-x 2))) (int (inc (/ max-y 2)))]      {:type :down-stairs :dest-place :1}]])))


(defn init-random-0
  "Create a random grid suitable for a starting level.
   Contains a down stairs, and identifies the starting location as
   the spot that would have been reserved for up stairs."
  []
  (let [place       (random-place 50 27)
        _ (debug "place" place)
        _ (debug "(place :up-stairs)" (place :up-stairs))
        _ (debug "(place :down-stairs)" (place :down-stairs))
        down-stairs (assoc-in (place :down-stairs) [1 :dest-place] :1)
        starting-location [[(-> place :up-stairs first first)
                            (-> place :up-stairs first second)]
                           {:type :floor :starting-location true}]
        _ (debug "starting-location" starting-location)
        place       (place :place)
        _ (debug "place" place)]
    (add-extras place [down-stairs starting-location])))

(defn init-random-n
  "Creates a random grid suitable for a non-starting level.
   Contains a down stairs, an up stairs, and five random items
   placed in floor cells."
  [level]
  (let [place       (random-place 50 27)
        _ (debug "place" place)
        _ (debug "(place :up-stairs)" (place :up-stairs))
        _ (debug "(place :down-stairs)" (place :down-stairs))
        _ (debug "level" level)
        _ (debug "former place id" (keyword (str (dec level))))
        up-stairs   (assoc-in (place :up-stairs) [1 :dest-place] (keyword (str (dec level))))
        _ (debug "up-stairs" up-stairs)
        down-stairs (assoc-in (place :down-stairs) [1 :dest-place] (keyword (str (inc level))))
        place       (place :place)
        drops       (map (fn [pos]
                           [pos {:type :floor :items [(gen-item)]}])
                           (take 5 (shuffle
                              (map (fn [[_ x y]] [x y]) (filter (fn [[cell x y]] (and (not (nil? cell))
                                                                                      (= (cell :type) :floor)))
                                                                (with-xy place))))))
        cash-drops  (map (fn [pos]
                           [pos {:type :floor :items [(gen-cash (* level 10))]}])
                           (take 5 (shuffle
                              (map (fn [[_ x y]] [x y]) (filter (fn [[cell x y]] (and (not (nil? cell))
                                                                                      (= (cell :type) :floor)))
                                                                (with-xy place))))))
        _ (debug "drops" drops)
        _ (debug "cash-drops" cash-drops)
        _ (debug "place" place)]
    (add-extras place (concat [down-stairs up-stairs] drops cash-drops))))

(defn test-inventory
  "Make five random items for the player to have when creating a new world."
  []
  (gen-items 5))

(defn init-world
  "Create a randomly generated world.

   A world consists of

   * an intial place id (`:0`)
  
   * places (indexed by place id)

   * a player
  
   * a log
  
   * a time (initialized to 0)
  
   * a state (for use with state tracking (for complex input like opening doors,
   dropping items, menus)
  
   * available hotkeys (a-zA-Z)
  
   * npcs
  
   * quests (indexed by quest id)

   Not all of the places or npcs have to be generated by this function; they can be
   added during the course of the game."
  []
  ;; Assign hotkeys to inventory and remove from remaining hotkeys
  (let [inventory              (test-inventory)
        remaining-hotkeys      (vec (seq "abcdefghijklmnopqrstuvwxyzABCdEFGHIJKLMNOPQRSTUVWQYZ"))
        hotkey-groups          (split-at (count inventory) remaining-hotkeys)
        inventory-with-hotkeys (vec (map #(assoc %1 :hotkey %2) inventory (first hotkey-groups)))
        remaining-hotkeys      (vec (apply str (second hotkey-groups)))
        place-0                (init-island)
        [_ starting-x
           starting-y]         (first (filter (fn [[cell x y]] (contains? cell :starting-location))
                                              (with-xy place-0)))
        starting-pos           {:x starting-x :y starting-y}
        party-pos              (adjacent-navigable-pos place-0 starting-pos #{:corridor :open-door :floor})
        _ (debug "starting-pos" starting-pos)
        _ (debug "party-pos" party-pos)]

  {:places {:0 place-0}
            ;:1 (init-place-1)}
   :current-place :0
   :time 0
   :current-state :normal
   :selected-hotkeys #{}
   :remaining-hotkeys remaining-hotkeys
   :log []
   :dialog-log []
   :player {
            :id :player
            :name "Player"
            :race :human
            :class :ranger
            :movement-policy :entourage
            :in-party? true
            :inventory inventory-with-hotkeys
            :hp 10
            :max-hp 10
            :$ 50
            :xp 0
            :level 0
            :hunger 0
            :pos starting-pos
            :place :0
            :body-parts #{:head :neck :face :abdomen :arm :leg :foot}
            :attacks #{:punch}
            :status #{}
            ;; map from body-part to {:time <int> :damage <float>}
            :wounds {}}
   :quests {}
   :npcs [
;{
;     :id :helga
;     :name "Helga"
;     :race :human
;     :class :rogue
;     :movement-policy :entourage
;     :in-party? true
;     :inventory []
;     :hp 10
;     :max-hp 10
;     :$ 0
;     :xp 0
;     :level 0
;     :hunger 0
;     :pos (nth party-pos 0)
;     :place :0
;     :state #{}
;     :image-path "./images/helga.png"}{
;     :id :cronk
;     :name "Cronk"
;     :race :human
;     :class :wizard
;     :movement-policy :entourage
;     :in-party? true
;     :inventory []
;     :hp 10
;     :max-hp 10
;     :$ 0
;     :xp 0
;     :level 0
;     :hunger 0
;     :pos (nth party-pos 1)
;     :place :0
;     :state #{}
;     :image-path "./images/cronk.png"}{
;     :id :hans
;     :name "Hans"
;     :race :human
;     :class :fighter
;     :movement-policy :entourage
;     :in-party? true
;     :inventory []
;     :hp 10
;     :max-hp 10
;     :$ 0
;     :xp 0
;     :level 0
;     :hunger 0
;     :pos (nth party-pos 2)
;     :place :0
;     :state #{}
;     :image-path "./images/hans.png"}
]}))

(defn -main [& args]
  (let [_ (clisk/seed-simplex-noise!)
        node (clisk/vectorize
               (clisk/vlet [c (center (invert (clisk/offset (clisk/scale 0.43 (clisk/v* [0.5 0.5 0.5] clisk/vsnoise)) clisk/radius)))]
                 (clisk/v+
                   (clisk/vif (clisk/v+ [-0.7 -0.7 -0.7]  (clisk/v* c (clisk/v+ [0.4 0.4 0.4] (clisk/scale 0.05 clisk/noise))))
                     [0 0 1]
                     [0 0 0])
                   (clisk/vif (clisk/v+ [-0.6 -0.6 -0.6]  c)
                     [0 1 0]
                     [0 0 0])
                   (clisk/vif (clisk/v+ [-0.5 -0.5 -0.5]  c)
                     [1 0 0]
                     [0 0 0]))))
        fns  (vec (map clisk/compile-fn (:nodes node)))]
    (dorun
      (map (comp (partial apply str) println)
        (partition 40
          (log-time "for" (for [y (range 20)
                x (range 40)
                :let [s (vec (map #(.calc ^clisk.IFunction % (double (/ x 40)) (double (/ y 20)) (double 0.0) (double 0.0))
                             fns))]]
            (case s
              [0.0 0.0 0.0] \~
              [1.0 0.0 0.0] \_
              [1.0 1.0 0.0] (case (rand-int 3)
                              0 \,
                              1 \`
                              2 \.)
              [1.0 0.0 1.0] \.
              [1.0 1.0 1.0] (case (rand-int 5)
                              0 \T
                              1 \"
                              2 \'
                              3 \.
                              4 \`)))))))))
        


