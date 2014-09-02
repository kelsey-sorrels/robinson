;; Functions for querying and manipulating npc state
(ns dungeon-crusade.npc
  (:use clojure.walk
        dungeon-crusade.common
        [dungeon-crusade.dialog :exclude [-main]])
  (:require [taoensso.timbre :as timbre]))

(timbre/refer-timbre)

(defn adjacent-navigable-pos
  "Return a collection of positions of `:floor` type cells centered around pos.
   Pos is a map with the keys `:x` and `:y`.
  
   Ex: `(adjacent-floor-pos [...] {:x 0 :y 0})`
   `[{:x 1 :y 1} {:x 0 :y 0}]`"
  [place pos navigable-types]
  {:pre [(= (set (keys pos)) #{:x :y})]}
  (filter (fn [{x :x y :y}]
            (let [cell (get-in place [y x])]
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



(defn npcs-at-current-place
  "Seq of npcs at the current place."
  [state]
  (filter (fn [npc] (= (get npc :place) (current-place-id state)))
          (get-in state [:world :npcs])))

(defn npc-by-id
  "The npc with the :id id. Nil if not found."
  [state id]
  (first (filter (fn [npc] (= (npc :id) id))
                 (get-in state [:world :npcs]))))

(defn npc->keys
  "Find the keys required to lookup the npc.
   npc and (get-in state (npc->path state npc)) refer to the same npc."
  [state npc]
  [:world :npcs (.indexOf (get-in state [:world :npcs]) npc)])

(defn add-npc
  "Add an npc to the specified place and position."
  ([state place-id npc x y]
  (add-npc state place-id npc x y nil))
  ([state place-id npc x y buy-fn-path]
  (conj-in state [:world :npcs] (assoc npc :pos {:x x :y y}
                                           :place place-id
                                           :inventory (if (contains? npc :inventory)
                                                        (npc :inventory)
                                                        [])
                                           :buy-fn-path buy-fn-path))))

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
        (debug "player-inventory-grouped" player-inventory-grouped)
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

(defn update-npc
  "Transform the npc with the function f. (f npc)."
  [state npc f]
  (map-in state [:world :npcs] (fn [n] (if (= npc n)
                                           (f npc)
                                           n))))


