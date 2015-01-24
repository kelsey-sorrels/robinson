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
            [robinson.noise :as rn]
            [robinson.itemgen :as ig]
            [clojure.core.async :as async]
            [clojure.java.io :as io]
            [clojure.data.generators :as dg]
            [taoensso.timbre :as timbre]
            [taoensso.timbre :as timbre]
            [pallet.thread-expr :as tx]
            [taoensso.nippy :as nippy])
  (:import [java.io DataInputStream DataOutputStream]))



(timbre/refer-timbre)

(defn rand-xy-in-circle
  [x y max-r]
  (let [theta (rand-nth (range (* 2 Math/PI)))
        r     (rand-nth (range max-r))]
    [(int (+ x (* r (Math/cos theta)))) (int (+ y (* r (Math/sin theta))))]))

(defn line-segments [x1 y1 x2 y2]
  #_(println "line-segments" x1 y1 x2 y2)
  (if (not (farther-than? (xy->pos x1 y1) (xy->pos x2 y2) 5))
    ;; too short to split, return direct line betweem two points.
    [[x1 y1] [x2 y2]]
    ;; subdivide
    (let [mx       (/ (+ x1 x2) 2)
          my       (/ (+ y1 y2) 2)
          r        (min 20 (distance (xy->pos x1 y1) (xy->pos mx my)))
          [rmx rmy] (rand-xy-in-circle mx my (dec r))]
      (concat (line-segments x1 y1 rmx rmy)
              (line-segments rmx rmy x2 y2)))))

;; noise utils
(defn invert [s] (+ 1 (* -1 s)))

(defn offset
  [xy-or-fn f]
  (cond (type xy-or-fn)
    vector? (let [[x y] xy-or-fn]
              (fn [[xi yi]]
                (f [(+ xi x) (+ yi y)])))
    fn?     (fn [[xi yi]]
                (let [[x y] (xy-or-fn [xi yi])]
                  (f [(+ xi x) (+ yi y)])))))

(defn scale [s f]
  (fn [[x y]]
    (f [(* s x) (* s y)])))

(defn center [f]
  (fn [[x y]]
    (offset [-0.5 -0.5] (scale 0.5 f))))

(defn distance []
  (fn [[x y]]
    #+clj  (Math/sqrt (+ (* x x) (* y y)))
    #+cljs (.sqrt js/Math (+ (* x x) (* y y)))))

(defn distance []
  (fn [[x y]]
    (Math/sqrt (+ (* x x) (* y y)))))

(defn center-distance []
  (fn [[x y]]
    (center distance)))

(defn vectorize
  [& more]
  (vec more))
  

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

(def jungle
  [0.0 0.4 0.1])

(def heavy-forest
  [0.0 0.5 0.0])

(def meadow
  [0.2 0.7 0.2])

(def swamp
  [0.4 0.2 0.1])

(def bamboo-grove
  [0.2 0.7 0.0])

(def light-forest
  [0.0 0.6 0.0])

(def rocky
  [0.5 0.5 0.5])

(def dirt
  [0.3 0.2 0.1])

(def sand
  [0.7 0.6 0.0])

(def surf
  [0.0 0.5 0.6])

(def ocean
  [0.0 0.4 0.5])

(defn vec->keyword
  [v]
  (condp = v
    jungle       :jungle
    heavy-forest :heavy-forest
    meadow       :meadow
    swamp        :swamp
    bamboo-grove :bamboo-grove
    light-forest :light-forest
    rocky        :rocky
    dirt         :dirt
    sand         :sand
    surf         :surf
    ocean        :ocean))

(def tree-node
  (cliskf/vectorize (cliskf/offset [-0.5 -0.5] (cliskf/v+ [-0.5 -0.5 -0.5] (cliskf/scale 0.01 cliskp/noise)))))

(def island-node
  (cliskf/vectorize
    (cliskf/vlet [c  (center (invert (cliskf/offset (cliskf/scale 0.43 (cliskf/v* [0.5 0.5 0.5] cliskp/vsnoise)) cliskf/radius)))
                  c1 (cliskf/offset [0.5 0.5] (cliskf/v+ [-0.5 -0.5 -0.5] (cliskf/scale 0.06 cliskp/noise)))
                  c2 (cliskf/offset [-0.5 -0.5] (cliskf/v+ [-0.5 -0.5 -0.5] (cliskf/scale 0.08 cliskp/noise)))]
      (vcond
        ;; interior biomes
        (cliskf/v+ [-0.55 -0.55 -0.55]  c)
          (vcond
            (vand c1 c2 (v> c1 c2))
            ;; interior jungle
            jungle
            (vand c1 c2)
            heavy-forest
            (vand c1 (vnot c2) (v> c1 c2))
            light-forest
            (vand c1 (vnot c2))
            bamboo-grove
            (vand (vnot c1) c2 (v> c1 c2))
            meadow
            (vand (vnot c1) c2)
            rocky
            (vand (vnot c1) (vnot c2) (v> c1 c2))
            swamp
            (vnot (vand c1 c2))
            dirt)
        ;; shore/yellow
        (cliskf/v+ [-0.5 -0.5 -0.5]  c)
            sand
        ;; surf/light blue
        (cliskf/v+ [-0.42 -0.42 -0.42]  c)
          surf
        ;; else ocean
        [1 1 1]
          ocean))))

(def tree-fns
  (vec (map cliskn/compile-fn (:nodes tree-node))))

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
            (or (= s surf)
                (= s ocean))))
          samples)
        [sx sy] (first non-water-samples)]
    (xy->pos sx sy)))

(defn find-lava-terminal-pos [seed starting-pos max-x max-y]
  (let [{x :x y :y}  starting-pos
        player-angle (Math/atan2 (- x (/ max-x 2)) (- y (/ max-y 2)))
        angle        (- player-angle 0.03)
        radius       (/ (min max-x max-y) 2)
        [cx cy]      [(/ max-x 2) (/ max-y 2)]
        [x y]        [(+ (* radius (Math/cos angle)) cx)
                      (+ (* radius (Math/sin angle)) cy)]
        points       (line-segment [x y] [cx cy])
        samples       points
        _            (cliskp/seed-simplex-noise! seed)
        non-water-samples (remove
          (fn [[x y]]
            (let [s (mapv #(.calc ^clisk.IFunction % (double (/ x max-x)) (double (/ y max-y)) (double 0.0) (double 0.0))
                              island-fns)]
            (or (= s surf)
                (= s ocean))))
          samples)
        [sx sy] (first non-water-samples)]
    (xy->pos sx sy)))

(defn init-island
  "Create an island block. `x` and `y` denote the coordinates of the upper left cell in the block."
  [state x y width height max-x max-y]
  (info "init-island" x y width height max-x max-y)
  (let [seed                  (get-in state [:world :seed])
        _                     (cliskp/seed-simplex-noise! seed)
        volcano-pos           (get-in state [:world :volcano-pos])
        lava-xys              (get-in state [:world :lava-points])]
    (binding [clisk/*anti-alias* 0]
    (vec
     (pmap vec
       (partition width
         (map (fn [[x y]]
            (let [s         (mapv #(.calc ^clisk.IFunction % (double (/ x max-x)) (double (/ y max-y)) (double 0.0) (double 0.0))
                                      island-fns)
                  t         (first (mapv #(.calc ^clisk.IFunction % (double (/ x max-x)) (double (/ y max-y)) (double 0.0) (double 0.0))
                                      tree-fns))
                  biome     (vec->keyword s)
                  ;_         (info biome t)
                  cell-type (case biome
                              :ocean         {:type :water}
                              :surf          {:type :surf}
                              :sand          {:type :sand}
                              :dirt          (case (uniform-int 3)
                                               0 {:type :dirt}
                                               1 {:type :gravel}
                                               2 {:type :short-grass})
                              :bamboo-grove  (if (< t 0.1)
                                               (dg/rand-nth [
                                                 {:type :dirt}
                                                 {:type :tall-grass}
                                                 {:type :tall-grass}
                                                 {:type :short-grass}
                                                 {:type :short-grass}])
                                               {:type :bamboo})
                              :rocky         (if (< t 0.1)
                                               (dg/rand-nth [
                                                 {:type :dirt}
                                                 {:type :tall-grass}
                                                 {:type :short-grass}
                                                 {:type :short-grass}])
                                                {:type :mountain})
                              :swamp         (dg/rand-nth [
                                               {:type :dirt}
                                               {:type :surf}
                                               {:type :tree}
                                               {:type :tall-grass}
                                               {:type :short-grass}])
                              :meadow        (dg/rand-nth [
                                               {:type :dirt}
                                               {:type :tall-grass}
                                               {:type :tall-grass}
                                               {:type :short-grass}
                                               {:type :short-grass}])
                              :jungle        (if (< t 0.1)
                                               (dg/rand-nth [
                                                 {:type :tall-grass}
                                                 {:type :short-grass}
                                                 {:type :gravel}])
                                               (dg/rand-nth [
                                                 {:type :tall-grass}
                                                 {:type :palm-tree}
                                                 {:type :fruit-tree :fruit-type (dg/rand-nth [:red-fruit :orange-fruit :yellow-fruit
                                                                                              :green-fruit :blue-fruit :purple-fruit
                                                                                              :white-fruit :black-fruit])}]))
                              :heavy-forest  (if (< t 0.1)
                                               (dg/rand-nth [
                                                 {:type :tall-grass}
                                                 {:type :tree}
                                                 {:type :fruit-tree :fruit-type (dg/rand-nth [:red-fruit :orange-fruit :yellow-fruit
                                                                                              :green-fruit :blue-fruit :purple-fruit
                                                                                              :white-fruit :black-fruit])}])
                                               (dg/rand-nth [
                                                 {:type :tall-grass}
                                                 {:type :short-grass}
                                                 {:type :gravel}]))
                              :light-forest  (if (< t 0.1)
                                               (dg/rand-nth [
                                                 {:type :tall-grass}
                                                 {:type :tree}
                                                 {:type :fruit-tree :fruit-type (dg/rand-nth [:red-fruit :orange-fruit :yellow-fruit
                                                                                              :green-fruit :blue-fruit :purple-fruit
                                                                                              :white-fruit :black-fruit])}])
                                               (dg/rand-nth [
                                                 {:type :tall-grass}
                                                 {:type :tall-grass}
                                                 {:type :short-grass}
                                                 {:type :short-grass}])))
                     cell (cond
                            ;; lava
                            (not-every? #(farther-than? (xy->pos x y) (apply xy->pos %) 3) lava-xys)
                            {:type :lava}
                            (not (farther-than? (xy->pos x y) volcano-pos 7))
                            {:type :mountain}
                            :else cell-type)]
              ;; drop initial harvestable items
              (if (or (and (= :gravel (get cell :type))
                           (= :rocky biome)
                           (= (uniform-int 0 50) 0))
                      (and (= :tree (get cell :type))
                           (= :heavy-forest biome)
                           (= (uniform-int 0 50) 0))
                      (and (= :tall-grass (get cell :type))
                           (= :meadow biome)
                           (= (uniform-int 0 50) 0))
                      (and (= :palm-tree (get cell :type))
                           (= :jungle biome)
                           (= (uniform-int 0 50) 0))
                      (and (contains? #{:gravel :tree :palm-tree :tall-grass} (get cell :type))
                           (= (uniform-int 0 200) 0)))
                (assoc cell :harvestable true)
                (if (and (= :gravel (get cell :type))
                         (not-every? #(farther-than? (xy->pos x y) (apply xy->pos %) 10) lava-xys)
                         (= (uniform-int 0 50) 0))
                  (assoc cell :harvestable true :near-lava true)
                  cell))))
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
        drops       [] #_(map (fn [pos]
                           [pos {:type :floor :items [(ig/gen-item)]}])
                           (take 5 (dg/shuffle
                              (map (fn [[_ x y]] [x y]) (filter (fn [[cell x y]] (and (not (nil? cell))
                                                                                      (= (cell :type) :floor)))
                                                                (with-xy place))))))
        cash-drops  [] #_(map (fn [pos]
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
        volcano-xy             [x y]
        lava-terminal-pos      (find-lava-terminal-pos seed starting-pos max-x max-y)
        lava-segments          (partition 2 (apply line-segments [(first volcano-xy) (second volcano-xy) (get lava-terminal-pos :x) (get lava-terminal-pos :y)]))
        lava-points            (map first lava-segments)
        _                      (info "lava-points" lava-points)
        min-state              {:world {:viewport {:width width :height height}
                                        :seed seed
                                        :volcano-pos (apply xy->pos volcano-xy)
                                        :lava-points lava-points}}
        place-id               (apply xy->place-id min-state (pos->xy starting-pos))
        [sx sy]                (pos->xy starting-pos)
        [vx vy]                [(int (- sx (/ width 2))) (int (- sy (/ height 2)))]
        _ (debug "starting-pos" starting-pos)
        place-0                (init-island min-state vx vy width height max-x max-y)
        fruit-ids              [:red-fruit :orange-fruit :yellow-fruit :green-fruit :blue-fruit :purple-fruit :white-fruit :black-fruit]
        poisoned-fruit         (set (take (/ (count fruit-ids) 2) (dg/shuffle fruit-ids)))
        skin-identifiable      (set (take (/ (count poisoned-fruit) 2) (dg/shuffle poisoned-fruit)))
        tongue-identifiable    (set (take (/ (count poisoned-fruit) 2) (dg/shuffle poisoned-fruit)))
        frog-colors            [:reg :orange :yellow :green :blue :purple]
        poisonous-frog-colors  (set (take (/ (count frog-colors) 2) (dg/shuffle frog-colors)))
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
           :volcano-pos (apply xy->pos volcano-xy)
           :lava-points lava-points
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
                    :dexterity 1
                    :speed 1
                    :size 75
                    :strength 10
                    :toughness 5
                    :hp 10
                    :max-hp 10
                    :will-to-live 100
                    :max-will-to-live 100
                    :money 50
                    :xp 0
                    :level 0
                    :hunger 0
                    :max-hunger 100
                    :thirst 0
                    :max-thirst 100
                    :pos starting-pos
                    :starting-pos starting-pos
                    :place :0_0
                    :body-parts #{:head :neck :face :abdomen :arm :leg :foot}
                    :attacks #{:punch}
                    :status #{}
                    :stats {
                      :timeline (list)
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
           :frogs {
             :poisonous          poisonous-frog-colors
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
        (log-time "init-island time" (init-island state
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
  ;; Remove all npcs in place being unloaded
  (->
    (reduce (fn [state npc] (if (= (apply xy->place-id state (pos->xy (get npc :pos)))
                                   id)
                                (remove-npc state npc)
                                state))
            state (get-in state [:world :npcs]))
    (dissoc-in [:world :places id]))))

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
          state (map (fn [id] [id (load-place state id)]) places-to-load))))))


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
        


