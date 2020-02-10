;; Utility functions and functions for manipulating state
(ns robinson.worldgen
  (:require 
            [robinson.common :as rc]
            [taoensso.timbre :as log]
            [robinson.random :as rr]
            [robinson.math :as rmath]
            [robinson.noise :as rn]
            [robinson.prism :as rprism]
            [robinson.itemgen :as ig]
            [robinson.viewport :as rv]
            [robinson.world :as rw]
            [robinson.player :as rp]
            [robinson.lineofsight :as rlos]
            [robinson.npc :as rnpc]
            [robinson.dungeons.pirateship :as psg]
            [robinson.dungeons.temple :as tg]
            [robinson.macros :as rm]
            [robinson.fs :as rfs]
            [taoensso.nippy :as nippy]
            [clojure.core.async :as async :refer [go go-loop]]
            [clojure.java.io :as io])
   (:import [java.io DataInputStream DataOutputStream]
            [robinson.player Player]))

(defn halloween? []
  (let [calendar (java.util.Calendar/getInstance)
        month    (.get calendar java.util.Calendar/MONTH)
        day-of-month (.get calendar java.util.Calendar/DAY_OF_MONTH)]
    (and (= month 10)
         (= day-of-month 31))))

(defn rand-xy-in-circle
  [x y max-r]
  (let [theta (rand-nth (range (* 2 Math/PI)))
        r     (rand-nth (range max-r))]
    [(int (+ x (* r (rmath/cos theta)))) (int (+ y (* r (rmath/sin theta))))]))

(defn line-segments [x1 y1 x2 y2]
  #_(println "line-segments" x1 y1 x2 y2)
  (if (not (rc/farther-than? (rc/xy->pos x1 y1) (rc/xy->pos x2 y2) 5))
    ;; too short to split, return direct line betweem two points.
    [[x1 y1] [x2 y2]]
    ;; subdivide
    (let [mx       (/ (+ x1 x2) 2)
          my       (/ (+ y1 y2) 2)
          r        (min 20 (rc/distance (rc/xy->pos x1 y1) (rc/xy->pos mx my)))
          [rmx rmy] (rand-xy-in-circle mx my (dec r))]
      (concat (line-segments x1 y1 rmx rmy)
              (line-segments rmx rmy x2 y2)))))

(defn add-extras
  "Adds extras to a place like items, and special cell types
   extras are in the format of `[[[x y] object] [[x y] object] &]`
   objects are cells with a type and maybe items `{:type :floor :items []}`"
  [place extras]
  ;; create a list of functions that can be applied to assoc extras, then create a composition of
  ;; so that setting can pass through each fn in turn.
  #_(log/debug "add-extras" place extras)
  (reduce (fn [place [[x y] & r]]
           (let [args (concat [[y x]] r)]
             #_(log/debug "assoc-in place" args)
             (apply assoc-in place args))) place extras))


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

(defn sample-tree [n x y]
  ((rprism/coerce (rprism/offset [-0.5 -0.5] (rprism/scale 0.1 (rprism/noise n)))) x y))

(defn sample-island
  [n x y]
  (let [c  ((rprism/coerce (rprism/scale 333.0 (rprism/offset (rprism/vnoise n) (rprism/radius)))) x y)
        c  ((rprism/coerce (rprism/offset (rprism/v+ (rprism/v* 15.0 (rprism/scale 15.0 (rprism/vnoise n)))
                                                     (rprism/v+ (rprism/v* 75.0 (rprism/scale 75.0 (rprism/vnoise n)))
                                                                (rprism/v* 150.0 (rprism/scale 150.0 (rprism/vnoise n)))))
                                          (rprism/scale 333.0 (rprism/radius)))) x y)
        ;c  ((rprism/coerce (rprism/scale 333.0 (rprism/radius))) x y)
        c1 ((rprism/coerce (rprism/offset [0.5 0.5] (rprism/scale 22 (rprism/snoise n)))) x y)
        c2 ((rprism/coerce (rprism/offset [-110.5 -640.5] (rprism/scale 26 (rprism/snoise n)))) x y)
        cgt (> (Math/abs c1) (Math/abs c2))]
    (cond
      ;; interior biomes
      (> 0.55  c)
        (cond
          (and (pos? c1) (pos? c2) cgt)
          :jungle
          (and (pos? c1) (pos? c2))
          :heavy-forest
          (and (pos? c1) (neg? c2) cgt)
          :light-forest
          (and (pos? c1) (neg? c2))
          :bamboo-grove
          (and (neg? c1) (pos? c2) cgt)
          :meadow
          (and (neg? c1) (pos? c2))
          :rocky
          (and (neg? c1) (neg? c2) cgt)
          :swamp
          :else
          :dirt)
      ;; shore/yellow
      (> 0.6  c)
          :sand
      ;; surf/light blue
      (> 0.68 c)
        :surf
      ;; else ocean
      :else
        :ocean)))

(defn find-starting-pos [seed max-x max-y]
  {:pre [(integer? seed)
         (integer? max-x)
         (integer? max-y)]}
  (loop [pos nil]
    (if (and (get pos :x)
             (get pos :y))
      (do
        (log/info "found starting position" pos)
        pos)
      (let [angle (rr/uniform-double (* 2 Math/PI))
            radius (min max-x max-y)
            [x y]   [(* radius (Math/cos angle))
                     (* radius (Math/sin angle))]
            points  (rlos/line-segment [0 0] [x y])
            samples points #_(take-nth 2 points)
            n       (rn/create-noise (rr/create-random seed))
            #_#__ (log/info "find-starting-pos samples" (vec samples))]
        ; endpoint contains ocean so somewhere there is a transition to land. find it.
        (if (= :ocean (apply sample-island n (last samples)))
          (let [spans (partition-by (fn [[x y]]
                                      (let [s (sample-island n x y)
                                            adj-types (map (fn [[x y]] (sample-island n x y))
                                                           (rw/adjacent-xys x y))]
                                        (log/debug "sample" x y s (vec adj-types))
                                        (every? (partial contains? #{:surf}) adj-types)))
                                    samples)
                [sx sy] (-> spans first last)]
            (log/debug "spans" (vec spans))
            (if (and sx sy (rc/farther-than? (rc/xy->pos 0 0) (rc/xy->pos sx sy) 20))
              (recur (rc/xy->pos sx sy))
              (recur nil)))
          (recur nil))))))

(defn find-lava-terminal-pos [seed starting-pos max-x max-y]
  {:pre [(integer? seed)
         (integer? (get starting-pos :x))
         (integer? (get starting-pos :y))
         (integer? max-x)
         (integer? max-y)]
   :post [(integer? (get % :x))
          (integer? (get % :y))]}
  (loop [pos          nil
         angle-offset 0.0]
    (if (or (nil? (get pos :x))
            (nil? (get pos :y)))
      (let [{x :x y :y}  starting-pos
            _ (log/info "seed" seed "starting-pos" starting-pos "max-x" max-x "max-y" max-y)
            player-angle (Math/atan2 x y)
            _            (log/info "player-angle" player-angle)
            angle        (- player-angle 0.05 angle-offset)
            radius       (min max-x max-y)
            [x y]        [(* radius (Math/cos angle))
                          (* radius (Math/sin angle))]
            points       (rlos/line-segment [x y] [0 0])
            samples       points
            n            (rn/create-noise (rr/create-random seed))
            non-water-samples (remove
              (fn [[x y]]
                (let [s (sample-island n x y)]
                (or (= s :surf)
                    (= s :ocean))))
              samples)
            [sx sy] (first non-water-samples)]
        (recur (rc/xy->pos sx sy) (+ angle-offset 0.3)))
      pos)))

(defn gen-island-cells
  [x y width height n volcano-pos lava-xys]
  (vec
    (for [y (range y (+ y height))]
     (vec
       (for [x (range x (+ x width))]
         (let [biome     (sample-island n x y)
               t         (sample-tree n x y)
               pos       (rc/xy->pos x y)
               ;_         (log/info biome t)
               cell-type (case biome
                           :ocean         {:type :water}
                           :surf          (if (< t 0.1)
                                            {:type :surf}
                                            (rr/rand-nth [
                                              {:type :surf}
                                              {:type :surf}
                                              {:type :surf}
                                              {:type :rocky-shore}]))
                           :sand          (if (< t 0.1)
                                            (rr/rand-nth [
                                              {:type :sand}
                                              {:type :sand}
                                              {:type :sand}
                                              {:type :sand}
                                              {:type :sand}
                                              {:type :sand}
                                              {:type :tall-grass}])
                                            (rr/rand-nth [
                                              {:type :dune}
                                              {:type :dune}
                                              {:type :sand}
                                              {:type :sand}
                                              {:type :sand}
                                              {:type :sand}
                                              {:type :tall-grass}]))
                           :dirt          (rr/rand-nth [
                                            {:type :dirt}
                                            {:type :dirt}
                                            {:type :gravel}
                                            {:type :short-grass}])
                           :bamboo-grove  (if (< t 0.1)
                                            (rr/rand-nth [
                                              {:type :dirt}
                                              {:type :tall-grass}
                                              {:type :tall-grass}
                                              {:type :short-grass}
                                              {:type :short-grass}])
                                            {:type :bamboo})
                           :rocky         (if (< t 0.1)
                                            (rr/rand-nth [
                                              {:type :dirt}
                                              {:type :tall-grass}
                                              {:type :short-grass}
                                              {:type :short-grass}])
                                             {:type :mountain})
                           :swamp         (rr/rand-nth [
                                            {:type :dirt}
                                            {:type :swamp}
                                            {:type :tree}
                                            {:type :tall-grass}
                                            {:type :short-grass}])
                           :meadow        (rr/rand-nth [
                                            {:type :dirt}
                                            {:type :tall-grass}
                                            (if (halloween?)
                                              {:type :tall-grass :items [(ig/gen-item :jack-o-lantern)]}
                                              {:type :tall-grass})
                                            {:type :short-grass}
                                            {:type :short-grass}])
                           :jungle        (if (< t 0.1)
                                            (rr/rand-nth [
                                              {:type :tall-grass}
                                              {:type :short-grass}
                                              {:type :gravel}])
                                            (rr/rand-nth [
                                              {:type :tall-grass}
                                              {:type :palm-tree}
                                              {:type :fruit-tree :fruit-type (rr/rand-nth [:red-fruit :orange-fruit :yellow-fruit
                                                                                           :green-fruit :blue-fruit :purple-fruit
                                                                                           :white-fruit :black-fruit])}]))
                           :heavy-forest  (cond 
                                            (< t 0.01)
                                              {:type :spring}
                                            (< t 0.1)
                                              (rr/rand-nth [
                                                {:type :tall-grass}
                                                {:type :tree}
                                                {:type :fruit-tree :fruit-type (rr/rand-nth [:red-fruit :orange-fruit :yellow-fruit
                                                                                             :green-fruit :blue-fruit :purple-fruit
                                                                                             :white-fruit :black-fruit])}])
                                            :else
                                              (rr/rand-nth [
                                                {:type :tall-grass}
                                                {:type :short-grass}
                                                {:type :gravel}]))
                           :light-forest  (if (< t 0.1)
                                            (rr/rand-nth [
                                              {:type :tall-grass}
                                              {:type :tree}
                                              {:type :fruit-tree :fruit-type (rr/rand-nth [:red-fruit :orange-fruit :yellow-fruit
                                                                                           :green-fruit :blue-fruit :purple-fruit
                                                                                           :white-fruit :black-fruit])}])
                                            (rr/rand-nth [
                                              {:type :tall-grass}
                                              {:type :tall-grass}
                                              {:type :short-grass}
                                              {:type :short-grass}])))
                  cell (cond
                         ;; lava
                         (not-every? #(rc/farther-than? x y (first %) (second %) 3) lava-xys)
                         {:type :lava}
                         (not (rc/farther-than? pos volcano-pos 7))
                         {:type :mountain}
                         :else cell-type)]
           ;; drop initial harvestable items
           (if (or (and (= :gravel (get cell :type))
                        (= :rocky biome)
                        (= (rr/uniform-int 0 40) 0))
                   (and (= :tree (get cell :type))
                        (= :heavy-forest biome)
                        (= (rr/uniform-int 0 40) 0))
                   (and (= :tall-grass (get cell :type))
                        (= :meadow biome)
                        (= (rr/uniform-int 0 40) 0))
                   (and (= :palm-tree (get cell :type))
                        (= :jungle biome)
                        (= (rr/uniform-int 0 20) 0))
                   (and (contains? #{:gravel :tree :palm-tree :tall-grass} (get cell :type))
                        (= (rr/uniform-int 0 160) 0)))
             (assoc cell :harvestable true)
             (if (and (= :gravel (get cell :type))
                      (not-every? #(rc/farther-than? x y (first %) (second %) 10) lava-xys)
                      (= (rr/uniform-int 0 40) 0))
               (assoc cell :harvestable true :near-lava true)
               cell))))))))

(defn drop-fruit
  [cells current-time]
  {:pre [(integer? current-time)]}
  (let [fruit-tree-xys (set
                         (remove nil?
                           (for [[y line] (map-indexed vector cells)
                                 [x cell] (map-indexed vector line)]
                             (if (= (cell :type) :fruit-tree)
                               [x y]
                               nil))))]
    (vec
      (for [[y line] (map-indexed vector cells)]
        (vec
          (for [[x cell] (map-indexed vector line)]
            (if (and (zero? (rr/uniform-int 40))
                     (not (rw/type->collide? (get cell :type))))
              (if-let [[fruit-tree-x
                        fruit-tree-y] (some fruit-tree-xys
                                            (set
                                              (shuffle
                                                (rw/adjacent-xys-ext x y))))]
                (let [fruit-tree-cell (get-in cells [fruit-tree-y
                                                     fruit-tree-x])
                      fruit (assoc (ig/id->item (get fruit-tree-cell :fruit-type))
                                   :rot-time 
                                   (+ current-time (rr/uniform-int 250 350)))]
                  (update cell :items conj fruit))
                cell)
              cell)))))))

(defn init-island
  "Create an island block. `x` and `y` denote the coordinates of the upper left cell in the block."
  [state x y width height]
  (log/info "init-island" x y width height)
  (let [seed                  (get-in state [:world :seed])
        n                     (rn/create-noise (rr/create-random seed))
        volcano-pos           (get-in state [:world :volcano-pos])
        lava-xys              (get-in state [:world :lava-points])
        starting-pos          (rp/player-starting-pos state)
        player-pos            (get-in state [:world :player :pos] starting-pos)
        encounter             (if (and starting-pos
                                       (rc/farther-than? starting-pos player-pos 30))
                                (rand-nth [:normal :pirate-ship :temple])
                                :normal)
        place {:seed seed
               :pos {:x x :y y}
               :spawned-monsters {}
               :cells           (as-> (gen-island-cells x y width height n volcano-pos lava-xys) cells
                                  (case encounter
                                    :normal
                                      cells
                                    :pirate-ship
                                      (psg/merge-cells cells)
                                    :temple
                                      ;; don't spawn temple if every cell is ocean
                                      (if (not-every? (partial = :water) (map :type (mapcat identity cells)))
                                        (tg/merge-cells cells)
                                        cells))
                                  ;; drop initial fruit
                                  (drop-fruit cells (rw/get-time state)))}]
    (if (not= encounter :normal)
      (assoc place :discovered-message encounter)
      place)))

(defn init-world
  "Create a randomly generated world.

   A world consists of

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
  {:post [(get-in % [:player :pos])]}
  ;; Assign hotkeys to inventory and remove from remaining hotkeys
  (let [width                  80
        height                 23
        ;; min-x and min-y are -400 and -400
        max-x                  400
        max-y                  400
        x                      0
        y                      0
        inventory              []
        remaining-hotkeys      rc/hotkeys
        hotkey-groups          (split-at (count inventory) remaining-hotkeys)
        inventory-with-hotkeys (vec (map #(assoc %1 :hotkey %2) inventory (first hotkey-groups)))
        remaining-hotkeys      (set (clojure.string/join (second hotkey-groups)))
        ;; calculate place-id and viewport position using minimal state information
        
        starting-pos           (find-starting-pos seed max-x max-y)
        _                      (log/info "starting-pos" starting-pos)
        volcano-xy             [x y]
        lava-terminal-pos      (find-lava-terminal-pos seed starting-pos max-x max-y)
        _                      (log/info "volcano-xy" volcano-xy "lava-terminal-pos" lava-terminal-pos)
        lava-segments          (partition 2 (apply line-segments [(first volcano-xy) (second volcano-xy) (get lava-terminal-pos :x) (get lava-terminal-pos :y)]))
        lava-points            (map first lava-segments)
        _                      (log/info "lava-points" lava-points)
        min-state              {:world {:time 25
                                        :viewport {:width width :height height}
                                        :seed seed
                                        :volcano-pos (apply rc/xy->pos volcano-xy)
                                        :lava-points lava-points}}
        place-id               (apply rv/xy->place-id min-state (rc/pos->xy starting-pos))
        [sx sy]                (rc/pos->xy starting-pos)
        [ax ay]                (rv/place-id->anchor-xy min-state (rv/xy->place-id min-state sx sy))
        [vx vy]                [(int (- sx (/ width 2))) (int (- sy (/ height 2)))]
        place-0                (init-island min-state ax ay width height)
        fruit-ids              [:red-fruit :orange-fruit :yellow-fruit :green-fruit :blue-fruit :purple-fruit :white-fruit :black-fruit]
        poisoned-fruit         (set (take (/ (count fruit-ids) 2) (rr/rnd-shuffle fruit-ids)))
        skin-identifiable      (set (take (/ (count poisoned-fruit) 2) (rr/rnd-shuffle poisoned-fruit)))
        tongue-identifiable    (set (take (/ (count poisoned-fruit) 2) (rr/rnd-shuffle poisoned-fruit)))
        frog-colors            [:reg :orange :yellow :green :blue :purple]
        poisonous-frog-colors  (set (take (/ (count frog-colors) 2) (rr/rnd-shuffle frog-colors)))
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
           :current-place nil
           :volcano-pos (apply rc/xy->pos volcano-xy)
           :lava-points lava-points
           :time (rw/get-time min-state)
           :current-state :start
           :selected-hotkeys #{}
           :selected-recipe-hotkey \a
           :remaining-hotkeys remaining-hotkeys
           :log []
           :ui-hint nil
           :dialog-log []
           :random-numbers (vec (repeatedly 10 (fn [] (rr/uniform-int 100))))
           :player (rp/gen-player
                    inventory-with-hotkeys
                    starting-pos)
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
  "Load a place from disk or generate. Two form
   `(load-place state id)` returns a place.
   `(load-place state id place-type gen-args)` returns  a 2-vector `[the state with the place added, the id of the place added]`."
  ([state id]
  (log/info "loading" id)
  ;; load the place into state. From file if exists or gen a new random place.
  (let [place
    (if (.exists (io/as-file (rfs/cwd-path (format "save/%s.place.edn" (str id)))))
      (with-open [o (io/input-stream (rfs/cwd-path (format "save/%s.place.edn" (str id))))]
        (nippy/thaw-from-in! (DataInputStream. o)))
      (let [[ax ay]            (rv/place-id->anchor-xy state id)
            [v-width v-height] (rv/viewport-wh state)
            w-width            (get-in state [:world :width])
            w-height           (get-in state [:world :height])]
        (rm/log-time "init-island time" (init-island state
                                                  ax ay
                                                  v-width v-height))))]
      (log/info "loaded place. width:" (count (first place)) "height:" (count place))
      place))
  ([state id place-type gen-args]
    ;; use id or generate 8 character hex encoded id string
    (let [id    (or id (format "%x" (long (rand (bit-shift-left 1 32)))))
          place (if (.exists (io/as-file (rfs/cwd-path (format "save/%s.place.edn" (str id)))))
                  (with-open [o (io/input-stream (rfs/cwd-path (format "save/%s.place.edn" (str id))))]
                    (nippy/thaw-from-in! (DataInputStream. o)))
                  (case place-type
                    :pirate-ship
                      ;; pass appropriate args
                      (apply psg/random-place gen-args)
                    :temple
                      ;; pass appropriate args
                      (apply tg/random-place gen-args)
                    (assert "Unknown place-type" place-type)))]
      [(assoc-in state [:world :places id] place)
       id])))

(def save-place-chan (async/chan))

(go-loop []
  (let [[id place] (async/<! save-place-chan)]
    (log/info "Saving" id)
    (with-open [o (io/output-stream (rfs/cwd-path (format "save/%s.place.edn" (str id))))]
      (nippy/freeze-to-out! (DataOutputStream. o) place))
    (recur)))

(defn unload-place
  [state id]
  (log/info "unloading" id)
  ;(rm/log-time "unloading"
  (go
    (async/>! save-place-chan [id (get-in state [:world :places id])]))
  ;; Remove all npcs in place being unloaded
  (->
    (reduce (fn [state npc] (if (= (apply rv/xy->place-id state (rc/pos->xy (get npc :pos)))
                                   id)
                                (rnpc/remove-npc state npc)
                                state))
            state (get-in state [:world :npcs]))
    (rc/dissoc-in [:world :places id])))

(defn unload-places
  [state ks]
  (reduce unload-place state ks))

(defn unload-all-places
  [state]
  (unload-places state (keys (get-in state [:world :places]))))

(defn load-unload-places
  [state]
  {:pre [(not (nil? state))]
   :post [(not (nil? %))]}
  (let [[x y]             (rp/player-xy state)
        loaded-place-ids  (keys (get-in state [:world :places]))
        visible-place-ids (rv/visible-place-ids state)
        places-to-load    (clojure.set/difference (set visible-place-ids) (set loaded-place-ids))
        places-to-unload  (clojure.set/difference (set loaded-place-ids) (set visible-place-ids))]
    (log/info "currently loaded places:" loaded-place-ids)
    (log/info "visible places:" visible-place-ids)
    (log/info "unloading places:" places-to-unload)
    (log/info "loading places:" places-to-load)
    (-> state
      (as-> state
        (reduce unload-place state places-to-unload))
      (as-> state
        (reduce (fn [state [id place]]
                  (assoc-in state [:world :places id] place))
                state
                (map (fn [id]
                       [id (load-place state id)])
                     places-to-load))))))

