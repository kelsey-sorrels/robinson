;; Functions for querying and manipulating npc state
(ns robinson.npc
  (:require [robinson.random :as rr]
            [taoensso.timbre :as log]
            [robinson.common :as rc]
            [robinson.player :as rp]
            [robinson.viewport :as rv]
            [robinson.lineofsight :as rlos]
            [robinson.random :as rr]
            [robinson.world :as rw]
            [robinson.monstergen :as mg]
            [robinson.dialog :as rdiag]
            clojure.walk))

(defn adjacent-navigable-pos
  "Return a collection of positions of `:floor` type cells centered around pos.
   Pos is a map with the keys `:x` and `:y`.
  
   Ex: `(adjacent-floor-pos [...] {:x 0 :y 0})`
   `[{:x 1 :y 1} {:x 0 :y 0}]`"
  [state pos navigable-types]
  {:pre [(= (set (keys pos)) #{:x :y})]}
  (filter (fn [{x :x y :y}]
            (let [cell (rw/get-cell state x y)]
              (and (not (nil? cell))
                   (contains? navigable-types (cell :type)))))
          (for [x (range -1 1)
                y (range -1 1)]
            {:x (+ (pos :x) x) :y (+ (pos :y) y)})))


(defn adjacent-navigable-pos-extended
  "Return a collection of positions of `:floor` type cells centered around pos.
   Pos is a map with the keys `:x` and `:y`.
  
   Ex: `(adjacent-floor-pos [...] {:x 0 :y 0})`
   `[{:x 1 :y 1} {:x 0 :y 0}]`"
  [place pos]
  {:pre [(= (set (keys pos)) #{:x :y})]}
  (filter (fn [{x :x y :y}]
            (let [cell (get-in place [y x])]
              (and (not (nil? cell))
                   (contains? #{:floor :corridor :open-door} (cell :type)))))
          (for [x (range -2 2)
                y (range -2 2)]
            {:x (+ (pos :x) x) :y (+ (pos :y) y)})))



(defn npcs-in-viewport
  "Seq of npcs at the current place."
  [state]
  {:pre [(some? state)]}
  (filter (fn [npc]
            (let [[x y] (rc/pos->xy (get npc :pos))]
              (assert (integer? x) (str "non-integer x-pos for npc" npc))
              (assert (integer? y) (str "non-integer y-pos for npc" npc))
              (rv/xy-in-viewport? state x y)))
          (get-in state [:world :npcs])))

(defn npc-by-id
  "The npc with the :id id. Nil if not found."
  [state id]
  (first (filter (fn [npc] (= (npc :id) id))
                 (get-in state [:world :npcs]))))

(defn index-of [coll item]
  (ffirst (filter  #(= (second %) item) (map-indexed vector coll))))

(defn npc->keys
  "Find the keys required to lookup the npc.
   npc and (get-in state (npc->path state npc)) refer to the same npc."
  [state npc]
  [:world :npcs (index-of (get-in state [:world :npcs]) npc)])

(defn npc-freqs-in-player-place
  [state]
  (let [place-id (rv/player-place-id state)]
    (get-in state [:world :places place-id :spawned-monsters])))

(defn add-npc
  "Add an npc to the specified place and position."
  ([state npc pos]
   {:pre [(some? state)
          (some? npc)
          (rc/position? pos)]}
    (let [[x y] (rc/pos->xy pos)]
      (add-npc state npc x y)))
  ([state npc x y]
  {:pre [(vector? (get-in state [:world :npcs]))
         (some? npc)
         (integer? x)
         (integer? y)]
   :post [(vector? (get-in % [:world :npcs]))]}
    (add-npc state npc x y nil))
  ([state npc x y buy-fn-path]
    (let [place-id (rv/xy->place-id state x y)]
      (-> state
        (update-in [:world :places place-id :spawned-monsters]
                   (fn [freqs] (update freqs (get npc :race) (fn [n] (inc (or n 0))))))
        (rc/conj-in [:world :npcs] (assoc npc :pos         (rc/xy->pos x y)
                                              :inventory   (if (contains? npc :inventory)
                                                             (npc :inventory)
                                                           [])
                                          :buy-fn-path buy-fn-path))))))
(defn remove-npc
  "Remove npc from state."
  [state npc]
  {:pre [(vector? (get-in state [:world :npcs]))]
   :post [(vector? (get-in % [:world :npcs]))]}
  (rc/remove-in state [:world :npcs] (partial = npc)))

(defn transfer-items-from-npc-to-player
  "Remove items from npc's inventory and add them to the player's inventory."
  [state npc-id item-pred]
  {:pre [(vector? (get-in state [:world :npcs]))]
   :post [(vector? (get-in % [:world :npcs]))]}
  (let [npcs                     (get-in state [:world :npcs])
        npc                      (first (filter #(= (% :id) npc-id) npcs))]
    (if npc
      (let [npc-inventory-grouped (group-by item-pred (get npc :inventory))
            new-npc-inventory     (get npc-inventory-grouped false)
            num-new-items         (count (get npc-inventory-grouped true))
            hotkey-groups         (split-at num-new-items (get-in state [:world :remaining-hotkeys]))
            new-player-inventory  (vec (concat (map (fn [i k] (assoc i :hotkey k))
                                                    (get npc-inventory-grouped true) (first hotkey-groups))
                                               (get-in state [:world :player :inventory])))]
        (-> state
          (rc/map-in [:world :npcs]
                     (fn [npc]
                       (if (= (npc :id)
                              npc-id)
                         (assoc npc :inventory new-npc-inventory)
                         npc)))
          (assoc-in [:world :player :inventory] new-player-inventory)
          (assoc-in [:world :remaining-hotkeys] (vec (second hotkey-groups)))))
      (throw #?(:clj
                (IllegalArgumentException.
                  (println-str "Could not find npc with id [" npc-id "]. Valid ids:" (map :id npcs)))
                :cljs
                (js/Error.
                  (println-str "Could not find npc with id [" npc-id "]. Valid ids:" (map :id npcs))))))))

(defn transfer-items-from-player-to-npc
  "Remove items from player's inventory and add them to the npc's inventory."
  [state npc-id item-pred]
  (let [npcs (get-in state [:world :npcs])
        npc  (first (filter #(= (% :id) npc-id) npcs))]
    (if npc
      (let [player-inventory-grouped (group-by item-pred (get-in state [:world :player :inventory]))
            new-player-inventory     (vec (get player-inventory-grouped false))
            new-npc-inventory        (vec (concat (get player-inventory-grouped true)
                                                  (get npc :inventory)))]
        (log/debug "player-inventory-grouped" player-inventory-grouped)
        (-> state
          (rc/map-in [:world :npcs]
                     (fn [npc]
                       (if (= (npc :id)
                              npc-id)
                         (assoc npc :inventory new-npc-inventory)
                         npc)))
          (assoc-in [:world :player :inventory] new-player-inventory)))
      (throw #?(:clj
                (IllegalArgumentException.
                  (println-str "Could not find npc with id [" npc-id "]. Valid ids:" (map :id npcs)))
                :cljs
                (js/Error.
                  (println-str "Could not find npc with id [" npc-id "]. Valid ids:" (map :id npcs))))))))

(defn talking-npcs
  "A seq of npcs with which the player is talking."
  [state]
  (filter (fn [npc] (contains? npc :talking)) (get-in state [:world :npcs])))

(defn update-npc-at-xy
  "Transform the npc at `[x y]` with the function f. (f npc)."
  [state x y f]
  (rc/map-in state [:world :npcs] (fn [npc] (if (and (= (-> npc :pos :x) x)
                                                     (= (-> npc :pos :y) y))
                                                (f npc)
                                                npc))))

(defn update-npc
  "Transform the npc with the function f. (f npc)."
  [state npc f]
  {:pre [(some? state)]
   :post [(some? %)]}
  (rc/update-in-matching state [:world :npcs] npc f))

(defn monster-level
  [state]
  (let [pos            (rp/player-pos state)
        starting-pos   (rp/player-starting-pos state)
        d              (rc/distance pos starting-pos)]
    (rc/bound 0 (int (+ (rr/uniform-int -2 2) (/ d 25.0))) 10)))

(defn add-npcs
  "Randomly add monsters to the current place's in floor cells."
  [state]
  (let [level      (monster-level state)
        r          (rlos/sight-distance state)
        [player-x
         player-y] (rp/player-xy state)
        xys        (rlos/perimeter-xys player-x player-y (min 5 (inc r)))
        xys        (remove (fn [[x y]] (rw/collide? state x y {:include-npcs? true
                                                               :collide-water? false})) xys)
        weighted-xys (zipmap (map (fn [[x y]]
                                    (let [cell       (rw/get-cell state x y)
                                          discovered (get cell :discovered (- (rw/get-time state) 1000))
                                          w          (min 1000 (- (rw/get-time state) discovered))]
                                      w))
                                  xys)
                             xys)]
    (log/info "monster-level" monster-level)
    (log/info "weighted-xys" weighted-xys)
    (if (seq weighted-xys)
      (let [[x y]  (rr/rand-weighted-nth weighted-xys)
            monster (mg/gen-random-monster level (get (rw/get-cell state x y) :type))]
       (log/info "Adding monster" monster "@ [" x y "] r" r)
       (add-npc state monster x y))
      state)))

(defn add-npcs-random
  "Randomly add monsters inside the viewport."
  [state]
  (let [num-npcs (reduce + (vals (npc-freqs-in-player-place state)))]
    (if (and (< (rr/uniform-int 100)
                (if (rw/is-night? state)
                   (/ 180 (inc num-npcs))
                   (/ 80 (inc num-npcs))))
             (< (count (-> state :world :npcs))
               60))
      (add-npcs state)
      state)))

(defn has-status?
  [npc test-status]
  (contains? (get npc :status) test-status))

(defn conj-status
  [npc new-status]
  (update npc :status (fn [status] (conj status new-status))))

(defn disj-status
  [npc new-status]
  (update npc :status (fn [status] (disj status new-status))))


(defn npc-health-status
  [npc]
  (let [hp (get npc :hp)
        max-hp (get (mg/gen-monster (get npc :race)) :hp)
        pct-hp (float (/ hp max-hp))]
    (cond
      (< pct-hp 0.2)
        :critical
      (< pct-hp 0.4)
        :badly-wounded
      (< pct-hp 0.6)
        :wounded
      (< pct-hp 0.8)
        :injured
      (< pct-hp 1.0)
        :bruised
      :else
        :fine)))

;;;; Special monster abilities
;; Hermit crab
(defmethod mg/do-on-hit :hermit-crab [npc state] (assert (some? state))
                                                 (-> state
                                                   (update-npc npc (fn [npc] (conj-status npc :in-shell)))
                                                   (rc/append-log "The hermit crab retreats into its shell.")))
(defmethod mg/do-get-toughness :hermit-crab [npc _] (if (has-status? npc :in-shell)
                                                       100000
                                                       (get npc :toughness)))
(defmethod mg/do-on-tick :hermit-crab [npc state] (if (has-status? npc :in-shell)
                                                     (-> state
                                                       (update-npc npc (fn [npc] (disj-status npc :in-shell)))
                                                       (rc/append-log "The hermet crab pokes out of its shell."))
                                                    state))
;; Rat
(defmethod mg/do-on-death :rat [npc state] (cond
                                             (zero? (rr/uniform-int 4))
                                               ;; spawn an additional rat
                                               (-> state
                                                 (add-npc (mg/gen-monster :rat)
                                                          (->> (rw/adjacent-xys-ext (get npc :pos))
                                                               (remove (fn [[x y]] (rw/collide? state x y {:include-npcs? true
                                                                                                           :collide-player? true
                                                                                                           :collide-water? false})))
                                                               rand-nth
                                                               (apply rc/xy->pos)))
                                                 (rc/append-log "The rat swarm intensified."))
                                             (zero? (rr/uniform-int 10))
                                               ;; spawn an additional 2 rats
                                               (-> state
                                                 (add-npc (mg/gen-monster :rat)
                                                          (->> (rw/adjacent-xys-ext (get npc :pos))
                                                               (remove (fn [[x y]] (rw/collide? state x y {:include-npcs? true
                                                                                                           :collide-player? true
                                                                                                           :collide-water? false})))
                                                               rand-nth
                                                               (apply rc/xy->pos)))
                                                 (add-npc (mg/gen-monster :rat)
                                                          (->> (rw/adjacent-xys-ext (get npc :pos))
                                                               (remove (fn [[x y]] (rw/collide? state x y {:include-npcs? true
                                                                                                           :collide-player? true
                                                                                                           :collide-water? false})))
                                                               rand-nth
                                                               (apply rc/xy->pos)))
                                                 (rc/append-log "The rat swarm intensified."))
                                             :else
                                                state))
                                               
;; Bird
;; Colored Frogs

