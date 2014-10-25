;; Utility functions and functions for manipulating state
(ns robinson.worldgen
  (:use 
        robinson.common
        robinson.npc
        [robinson.mapgen :exclude [-main]])
  (:require 
            [robinson.itemgen :as ig]
            [clojure.data.generators :as dg]
            [taoensso.timbre :as timbre]
            [clisk.core :as clisk]
            [clisk.patterns :as cliskp]
            [clisk.node :as cliskn]
            [clisk.functions :as cliskf]))

(timbre/refer-timbre)

;; clisk utils
(defn invert [a] (cliskf/v+ [1 1 1] (cliskf/v* [-1 -1 -1] a)))

(defn center [f]
  (cliskf/offset [-0.5 -0.5] (cliskf/scale 0.5 f)))

(defn center-radius []
  (cliskf/radius (center [cliskf/x cliskf/y])))

(defmacro vcond
  "Takes a set of test/expr pairs. It evaluates each test one at a
  time. If a test returns logical true, cond evaluates and returns
  the value of the corresponding expr and doesn't evaluate any of the
  other tests or exprs. (cond) returns nil."
  {:added "1.0"}
  [& clauses]
  (if clauses
    (list 'cliskf/vif (first clauses)
      (if (next clauses)
        (second clauses)
        (throw (IllegalArgumentException.
          "vcond requires an even number of forms")))
      (cons 'vcond (next (next clauses))))
    [0 0 0]))

(defn init-ocean
  []
  (let [max-x 80
        max-y 26]
    (add-extras
      (vec
        (map vec
          (partition max-x
            (for [y (range max-y)
                  x (range max-x)]
                {:type :water}))))
      [])))
  
(defn init-island
  "Create an example place with an island, two items
   and a set of down stairs that lead to place `:1`"
  [seed]
  (let [_    (cliskp/seed-simplex-noise! seed)
        node (cliskf/vectorize
               (cliskf/vlet [c (center (invert (cliskf/offset (cliskf/scale 0.43 (cliskf/v* [0.5 0.5 0.5] cliskp/vsnoise)) cliskf/radius)))]
                 (vcond
                   ;; interior trees/green
                   (cliskf/v+ [-0.7 -0.7 -0.7]  (cliskf/v* c (cliskf/v+ [0.4 0.4 0.4] (cliskf/scale 0.05 cliskp/noise))))
                     [0 0.5 0]
                   ;; interior dirt/brown
                   (cliskf/v+ [-0.6 -0.6 -0.6]  c)
                     [0.3 0.2 0.1]
                   ;; shore/yellow
                   (cliskf/v+ [-0.5 -0.5 -0.5]  c)
                     [0.7 0.6 0.0]
                   ;; surf/light blue
                   (cliskf/v+ [-0.37 -0.37 -0.37]  c)
                     [0 0.5 0.6]
                   ;; else ocean
                   [1 1 1]
                     [0 0.4 0.5])))
        fns  (vec (map cliskn/compile-fn (:nodes node)))
        max-x 80
        max-y 26]
    (add-extras
      (vec
        (map vec
          (partition max-x
            (log-time "for" (for [y (range max-y)
                  x (range max-x)
                  :let [s (vec (map #(.calc ^clisk.IFunction % (double (/ x max-x)) (double (/ y max-y)) (double 0.0) (double 0.0))
                               fns))]]
              (case s
                [0.0 0.4 0.5] {:type :water}
                [0.0 0.5 0.6] {:type :surf}
                [0.7 0.6 0.0] {:type :sand}
                [0.3 0.2 0.1] (case (uniform-int 2)
                                0 {:type :dirt}
                                1 {:type :gravel})
                [0.0 0.5 0.0] (case (uniform-int 7)
                                0 {:type :tree}
                                1 {:type :palm-tree}
                                2 {:type :fruit-tree :fruit-type (dg/rand-nth [:red-fruit :orange-fruit :yellow-fruit
                                                                               :green-fruit :blue-fruit :purple-fruit
                                                                               :white-fruit :black-fruit])}
                                3 {:type :tall-grass}
                                4 {:type :short-grass}
                                5 {:type :gravel}
                                6 {:type :bamboo})))))))
    [[[(int (/ max-x 2)) (int (/ max-y 2))]      {:type :dirt :starting-location true}]])))


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
                           [pos {:type :floor :items [(ig/gen-item)]}])
                           (take 5 (dg/shuffle
                              (map (fn [[_ x y]] [x y]) (filter (fn [[cell x y]] (and (not (nil? cell))
                                                                                      (= (cell :type) :floor)))
                                                                (with-xy place))))))
        cash-drops  (map (fn [pos]
                           [pos {:type :floor :items [(ig/gen-cash (* level 10))]}])
                           (take 5 (dg/shuffle
                              (map (fn [[_ x y]] [x y]) (filter (fn [[cell x y]] (and (not (nil? cell))
                                                                                      (= (cell :type) :floor)))
                                                                (with-xy place))))))
        _ (debug "drops" drops)
        _ (debug "cash-drops" cash-drops)
        _ (debug "place" place)]
    (add-extras place (concat [down-stairs up-stairs] drops cash-drops))))

(defn init-world
  "Create a randomly generated world.

   A world consists of

   * an intial place id (`:0_0`)
  
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
  [seed]
  ;; Assign hotkeys to inventory and remove from remaining hotkeys
  (let [inventory              []
        remaining-hotkeys      (vec (seq "abcdefghijklmnopqrstuvwxyzABCdEFGHIJKLMNOPQRSTUVWQYZ"))
        hotkey-groups          (split-at (count inventory) remaining-hotkeys)
        inventory-with-hotkeys (vec (map #(assoc %1 :hotkey %2) inventory (first hotkey-groups)))
        remaining-hotkeys      (set (clojure.string/join (second hotkey-groups)))
        place-0                (init-island seed)
        [_ starting-x
           starting-y]         (first (filter (fn [[cell x y]] (contains? cell :starting-location))
                                              (with-xy place-0)))
        starting-pos           {:x starting-x :y starting-y}
        party-pos              (adjacent-navigable-pos place-0 starting-pos #{:corridor :open-door :floor})
        _ (debug "starting-pos" starting-pos)
        _ (debug "party-pos" party-pos)
       fruit-ids               [:red-fruit :orange-fruit :yellow-fruit :green-fruit :blue-fruit :purple-fruit :white-fruit :black-fruit]
       poisoned-fruit          (set (take (/ (count fruit-ids) 2) (dg/shuffle fruit-ids)))
       skin-identifiable       (set (take (/ (count poisoned-fruit) 2) (dg/shuffle poisoned-fruit)))
       tongue-identifiable     (set (take (/ (count poisoned-fruit) 2) (dg/shuffle poisoned-fruit)))]

  {:places {:0_0 place-0}
            ;:1 (init-place-1)}
   :current-place :0_0
   :time 0
   :current-state :start
   :selected-hotkeys #{}
   :remaining-hotkeys remaining-hotkeys
   :log []
   :ui-hint nil
   :dialog-log []
   :player {
            :id :player
            :name "Player"
            :race :human
            :class :ranger
            :movement-policy :entourage
            :in-party? true
            :inventory inventory-with-hotkeys
            :speed 1
            :hp 10
            :max-hp 10
            :will-to-live 100
            :max-will-to-live 100
            :$ 50
            :xp 0
            :level 0
            :hunger 0
            :max-hunger 100
            :thirst 0
            :max-thirst 100
            :pos starting-pos
            :place :0_0
            :body-parts #{:head :neck :face :abdomen :arm :leg :foot}
            :attacks #{:punch}
            :status #{}
            :stats {
              :num-animals-killed       {}
              :num-items-crafted        {}
              :num-items-harvested      {}
              :num-kills-by-attack-type {}
              :num-items-eaten          {}}
            ;; map from body-part to {:time <int> :damage <float>}
            :wounds {}}
   :fruit {
     :poisonous poisoned-fruit
     :skin-identifiable skin-identifiable
     :tongue-identifiable tongue-identifiable
   }
   :quests {}
   :npcs []}))

(defn -main [& args]
  (let [_ (cliskp/seed-simplex-noise!)
        node (cliskf/vectorize
               (cliskf/vlet [c (center (invert (cliskf/offset (cliskf/scale 0.43 (cliskf/v* [0.5 0.5 0.5] cliskp/vsnoise)) cliskf/radius)))]
                 (vcond
                   ;; interior trees/green
                   (cliskf/v+ [-0.7 -0.7 -0.7]  (cliskf/v* c (cliskf/v+ [0.4 0.4 0.4] (cliskf/scale 0.05 cliskp/noise))))
                     [0 0.5 0]
                   ;; interior dirt/brown
                   (cliskf/v+ [-0.6 -0.6 -0.6]  c)
                     [0.3 0.2 0.1]
                   ;; shore/yellow
                   (cliskf/v+ [-0.5 -0.5 -0.5]  c)
                     [0.7 0.6 0.0]
                   ;; surf/light blue
                   (cliskf/v+ [-0.37 -0.37 -0.37]  c)
                     [0 0.5 0.6]
                   ;; else ocean
                   [1 1 1]
                     [0 0.4 0.5])))

        fns  (vec (map cliskn/compile-fn (:nodes node)))]
    (clisk/show node)
    #_(dorun
      (map (comp (partial apply str) println)
        (partition 70
          (log-time "for"
            (for [y (range 28)
                  x (range 70)
                  :let [[s _ _] (vec (map #(.calc ^clisk.IFunction % (double (/ x 70)) (double (/ y 28)) (double 0.0) (double 0.0))
                                   fns))]]
              (cond
                (> s 0.9) \^
                (> s 0.7) \.
                (> s 0.5) \_
                :else \~))))))))
        


