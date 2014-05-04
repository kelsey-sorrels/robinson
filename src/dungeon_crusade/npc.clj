;; Functions for querying and manipulating npc state
(ns dungeon-crusade.npc
  (:use clojure.walk
        dungeon-crusade.common
        [dungeon-crusade.dialog :exclude [-main]]))


(defn npc-at-xy
  "Retrieve the first npc in the current place at the position `[x y]`. If not npc exists
   at `[x y]` return `nil`."
  [x y state]
  (let [current-place-id (-> state :world :current-place)
        npcs             (filter #(= current-place-id (% :place))
                                 (-> state :world :npcs current-place-id))]
    (some (fn [npc] (when (and (= x (-> npc :pos :x)) (= y (-> npc :pos :y))) npc)) npcs)))


(defn npcs-at-current-place
  "Seq of npcs at the current place."
  [state]
  (filter (fn [npc] (= (npc :place) (current-place-id state)))
          (get-in state [:world :npcs])))

(defn npc-at-xy
  "Seq of npcs at [x y] of the current place."
  [state x y]
  (println "npcs:" (get-in state [:world :npcs]))
  (first (filter (fn [npc] (and (= (npc :place) (current-place-id state))
                                (= (-> npc :pos :x) x)
                                (= (-> npc :pos :y) y)))
                 (get-in state [:world :npcs]))))
(defn add-npc
  "Add an npc to the specified place and position."
  [state place-id npc x y]
  (conj-in state [:world :npcs] (assoc npc :pos {:x x :y y} :place place-id)))

(defn transfer-items-from-npc-to-player
  "Remove items from npc's inventory and add them to the player's inventory."
  [state npc-id item-pred]
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
          (map-in [:world :npcs]
                  (fn [npc]
                    (if (= (npc :id)
                           npc-id)
                      (assoc npc :inventory new-npc-inventory)
                      npc)))
          (assoc-in [:world :player :inventory] new-player-inventory)
          (assoc-in [:world :remaining-hotkeys] (vec (second hotkey-groups)))))
      (throw (IllegalArgumentException.
               (println-str "Could not find npc with id [" npc-id "]. Valid ids:" (map :id npcs)))))))

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
        (println "player-inventory-grouped" player-inventory-grouped)
        (-> state
          (map-in [:world :npcs]
                  (fn [npc]
                    (if (= (npc :id)
                           npc-id)
                      (assoc npc :inventory new-npc-inventory)
                      npc)))
          (assoc-in [:world :player :inventory] new-player-inventory)))
      (throw (IllegalArgumentException.
               (println-str "Could not find npc with id [" npc-id "]. Valid ids:" (map :id npcs)))))))

(defn talking-npcs
  "A seq of npcs with which the player is talking."
  [state]
  (filter (fn [npc] (contains? npc :talking)) (get-in state [:world :npcs])))

(defn update-npc-at-xy
  "Transform the npc at `[x y]` with the function f. (f npc)."
  [state x y f]
  (map-in state [:world :npcs] (fn [npc] (if (and (= (-> npc :pos :x) x)
                                                  (= (-> npc :pos :y) y))
                                             (f npc)
                                             npc))))


