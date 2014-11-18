;; Utility functions and functions for manipulating state
(ns robinson.worldgen
  (:use 
        robinson.common
        robinson.viewport
        robinson.world
        robinson.player
        [robinson.lineofsight :exclude [-main]]
        robinson.npc
        [robinson.mapgen :exclude [-main]]
        clojure.contrib.core)
  (:require 
            [robinson.itemgen :as ig]
            [clojure.core.async :as async]
            [clojure.java.io :as io]
            [clojure.data.generators :as dg]
            [taoensso.timbre :as timbre]
            [clisk.core :as clisk]
            [clisk.patterns :as cliskp]
            [clisk.node :as cliskn]
            [clisk.functions :as cliskf]
            [taoensso.timbre :as timbre]
            [pallet.thread-expr :as tx]
            [taoensso.nippy :as nippy])
  (:import [java.io DataInputStream DataOutputStream]))



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
        max-y 22]
    (add-extras
      (vec
        (map vec
          (partition max-x
            (for [y (range max-y)
                  x (range max-x)]
                {:type :water}))))
      [])))


(def island-node
  (cliskf/vectorize
    (cliskf/vlet [c (center (invert (cliskf/offset (cliskf/scale 0.43 (cliskf/v* [0.5 0.5 0.5] cliskp/vsnoise)) cliskf/radius)))]
      (vcond
        ;; interior jungle
        (cliskf/v+ [-0.7 -0.7 -0.7]  (cliskf/v* c (cliskf/v+ [0.4 0.4 0.4] (cliskf/scale 0.03 cliskp/noise))))
          [0 0.4 0.1]
        ;; interior trees/green
        (cliskf/v+ [-0.7 -0.7 -0.7]  (cliskf/v* c (cliskf/v+ [0.4 0.4 0.4] (cliskf/offset [0.5 0.5] (cliskf/scale 0.04 cliskp/noise)))))
          [0 0.5 0]
        ;; interior meadows
        (cliskf/v+ [-0.56 -0.56 -0.56]  (cliskf/v* c (cliskf/v+ [0.3 0.3 0.3] (cliskf/offset [-0.5 -0.5] (cliskf/scale 0.06 cliskp/noise)))))
          [0.2 0.7 0.2]
        ;; interior dirt/brown
        (cliskf/v+ [-0.55 -0.55 -0.55]  c)
          [0.3 0.2 0.1]
        ;; shore/yellow
        (cliskf/v+ [-0.5 -0.5 -0.5]  c)
          [0.7 0.6 0.0]
        ;; surf/light blue
        (cliskf/v+ [-0.42 -0.42 -0.42]  c)
          [0 0.5 0.6]
        ;; else ocean
        [1 1 1]
          [0 0.4 0.5]))))

(def island-fns
  (vec (map cliskn/compile-fn (:nodes island-node))))

(defn find-starting-pos [seed max-x max-y]
  (let [angle (dg/rand-nth (range (* 2 Math/PI)))
        radius (/ (min max-x max-y) 2)
        [cx cy] [(/ max-x 2) (/ max-y 2)]
        [x y]   [(+ (* radius (Math/cos angle)) cx)
                 (+ (* radius (Math/sin angle)) cy)]
        points  (line-segment [x y] [cx cy])
        samples (take-nth 5 points)
        _       (cliskp/seed-simplex-noise! seed)
        non-water-samples (remove
          (fn [[x y]]
            (let [s (mapv #(.calc ^clisk.IFunction % (double (/ x max-x)) (double (/ y max-y)) (double 0.0) (double 0.0))
                              island-fns)]
            (or (= s [0.0 0.4 0.5])
                (= s [0.0 0.5 0.6]))))
          samples)
        [sx sy] (first non-water-samples)]
    (xy->pos sx sy)))

(defn init-island
  "Create an island block. `x` and `y` denote the coordinates of the upper left cell in the block."
  [seed x y width height max-x max-y]
  (info "init-island" seed x y width height max-x max-y)
  (let [_    (cliskp/seed-simplex-noise! seed)]
    (binding [clisk/*anti-alias* 0]
    (vec
     (pmap vec
       (partition width
         (map (fn [[x y]]
            (let [s (mapv #(.calc ^clisk.IFunction % (double (/ x max-x)) (double (/ y max-y)) (double 0.0) (double 0.0))
                              island-fns)]
              (case s
                [0.0 0.4 0.5] {:type :water}
                [0.0 0.5 0.6] {:type :surf}
                [0.7 0.6 0.0] {:type :sand}
                [0.3 0.2 0.1] (case (uniform-int 3)
                                0 {:type :dirt}
                                1 {:type :gravel})
                                2 {:type :tall-grass}
                [0.2 0.7 0.2] (case (uniform-int 5)
                                0 {:type :dirt}
                                1 {:type :tall-grass}
                                2 {:type :tall-grass}
                                3 {:type :short-grass}
                                4 {:type :short-grass})
                ;; jungle
                [0.0 0.4 0.1] (case (uniform-int 6)
                                0 {:type :palm-tree}
                                1 {:type :fruit-tree :fruit-type (dg/rand-nth [:red-fruit :orange-fruit :yellow-fruit
                                                                               :green-fruit :blue-fruit :purple-fruit
                                                                               :white-fruit :black-fruit])}
                                2 {:type :tall-grass}
                                3 {:type :short-grass}
                                4 {:type :gravel}
                                5 {:type :bamboo})
                ;; forest
                [0.0 0.5 0.0] (case (uniform-int 5)
                                0 {:type :tree}
                                1 {:type :fruit-tree :fruit-type (dg/rand-nth [:red-fruit :orange-fruit :yellow-fruit
                                                                               :green-fruit :blue-fruit :purple-fruit
                                                                               :white-fruit :black-fruit])}
                                2 {:type :tall-grass}
                                3 {:type :short-grass}
                                4 {:type :gravel}))))
            (for [y (range y (+ y height))
                  x (range x (+ x width))]
              [x y]))))))))


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
  (let [width                  80
        height                 23
        max-x                  800
        max-y                  800
        x                      (/ max-x 2)
        y                      (/ max-y 2)
        inventory              []
        remaining-hotkeys      (vec (seq "abcdefghijklmnopqrstuvwxyzABCdEFGHIJKLMNOPQRSTUVWQYZ"))
        hotkey-groups          (split-at (count inventory) remaining-hotkeys)
        inventory-with-hotkeys (vec (map #(assoc %1 :hotkey %2) inventory (first hotkey-groups)))
        remaining-hotkeys      (set (clojure.string/join (second hotkey-groups)))
        ;; calculate place-id and viewport position using minimal state information
        
        starting-pos           (find-starting-pos seed max-x max-y)
        viewport-state         {:world {:viewport {:width width :height height}}}
        place-id               (apply xy->place-id viewport-state (pos->xy starting-pos))
        [sx sy]                (pos->xy starting-pos)
        [vx vy]                [(int (- sx (/ width 2))) (int (- sy (/ height 2)))]
        _ (debug "starting-pos" starting-pos)
        place-0                (init-island seed vx vy width height max-x max-y)
        fruit-ids              [:red-fruit :orange-fruit :yellow-fruit :green-fruit :blue-fruit :purple-fruit :white-fruit :black-fruit]
        poisoned-fruit         (set (take (/ (count fruit-ids) 2) (dg/shuffle fruit-ids)))
        skin-identifiable      (set (take (/ (count poisoned-fruit) 2) (dg/shuffle poisoned-fruit)))
        tongue-identifiable    (set (take (/ (count poisoned-fruit) 2) (dg/shuffle poisoned-fruit)))
        world
          {:seed seed
           :block-size {:width width :height height}
           :width max-x
           :height max-y
           :viewport {
             :width width
             :height height
             :pos {:x vx :y vy}}
           :places {place-id place-0}
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
             :poisonous           poisoned-fruit
             :skin-identifiable   skin-identifiable
             :tongue-identifiable tongue-identifiable
             :identified          #{}
           }
           :quests {}
           :npcs []}]
    world))



(defn load-place
  "Returns a place, not state."
  [state id]
  (info "loading" id)
  ;; load the place into state. From file if exists or gen a new random place.
  (let [place
    (if (.exists (io/as-file (format "save/%s.place.edn" (str id))))
      (log-time "read-string time" ;;(clojure.edn/read-string {:readers {'Monster mg/map->Monster}} s)))
        (with-open [o (io/input-stream (format "save/%s.place.edn" (str id)))]
          (nippy/thaw-from-in! (DataInputStream. o))))
      (let [[ax ay]            (place-id->anchor-xy state id)
            [v-width v-height] (viewport-wh state)
            w-width            (get-in state [:world :width])
            w-height           (get-in state [:world :height])]
        (log-time "init-island time" (init-island (get-in state [:world :seed])
                                                  ax ay
                                                  v-width v-height
                                                 w-width w-height))))]
      (info "loaded place. width:" (count (first place)) "height:" (count place))
      place))

(def save-place-chan (async/chan))

(async/go-loop []
  (let [[id place] (async/<! save-place-chan)]
    (info "Saving" id)
    (with-open [o (io/output-stream (format "save/%s.place.edn" (str id)))]
      (nippy/freeze-to-out! (DataOutputStream. o) place)))
    (recur))

(defn unload-place
  [state id]
  (info "unloading" id)
  (log-time "unloading"
  (async/>!! save-place-chan [id (get-in state [:world :places id])])
  (dissoc-in state [:world :places id])))

(defn load-unload-places
  [state]
  (let [[x y]             (player-xy state)
        loaded-place-ids  (keys (get-in state [:world :places]))
        visible-place-ids (visible-place-ids state x y)
        places-to-load    (clojure.set/difference (set visible-place-ids) (set loaded-place-ids))
        places-to-unload  (clojure.set/difference (set loaded-place-ids) (set visible-place-ids))]
    (info "currently loaded places:" loaded-place-ids)
    (info "visible places:" visible-place-ids)
    (info "unloading places:" places-to-unload)
    (info "loading places:" places-to-load)
    (-> state
      (as-> state
        (reduce unload-place state places-to-unload))
      (as-> state
        (reduce (fn [state [id place]]
          (assoc-in state [:world :places id] place))
          state (pmap (fn [id] [id (load-place state id)]) places-to-load))))))


(defn -main [& args]
  (let [_ (cliskp/seed-simplex-noise!)]
    (log-time "show-time" (clisk/show island-node))
    #_(dorun
      (map (comp (partial apply str) println)
        (partition 70
          (log-time "for"
            (for [y (range 28)
                  x (range 70)
                  :let [[s _ _] (vec (map #(.calc ^clisk.IFunction % (double (/ x 70)) (double (/ y 28)) (double 0.0) (double 0.0))
                                   island-fns))]]
              (cond
                (> s 0.9) \^
                (> s 0.7) \.
                (> s 0.5) \_
                :else \~))))))))
        


