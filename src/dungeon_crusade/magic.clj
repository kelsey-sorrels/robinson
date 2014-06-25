;; Functions for querying and manipulating magic
(ns dungeon-crusade.magic
  (:use dungeon-crusade.common
        dungeon-crusade.combat
        dungeon-crusade.npc
        pallet.thread-expr)
  (:require [taoensso.timbre :as timbre]))

(timbre/refer-timbre)


(def magic
  [{:id :zap
    :name "zap"
    :hotkey \z
    :state :magic-direction
    :fn (fn [state collided-object]
          (-> state
            (when-> (and (not (nil? collided-object))
                       (contains? collided-object :npc))
              (do
                (println "Zapped" collided-object)
                (attack state [:world :player] (npc->keys state (collided-object :npc)))))
            (assoc-in [:world :current-state] :normal)))}])

(defn id->magic
  "Get magical ability by id."
  [id]
  (first (filter #(= id (% :id)) magic)))

(defn get-magical-abilities
  "Return a subset of magical abilities."
  [character]
  magic)

(defn do-magic
  "Perform a feat of magical ability."
  [state keyin]
  (let [valid-input (get-magical-abilities (-> state :world :player))
        options (zipmap (map :hotkey valid-input) valid-input)
        input (get options keyin)]
    (if (not (nil? input))
      (-> state
        (assoc-in [:world :current-state] (input :state))
        (assoc-in [:world :magical-ability-id] (input :id))))))

(defn directional-magic
  "Perform a feat of magic in a direction by returning a new state with
   the magical ability applied."
  [state direction]
  {:pre [(contains? #{:left :right :up :down} direction)]}
  (let [obj (first-collidable-object state direction)
        magic (id->magic (get-in state [:world :magical-ability-id]))]
    ((magic :fn) state obj)))

(defn magic-left
  "Perform a feat of directional magic to the left of the player."
  [state]
  (directional-magic state :left))

(defn magic-right
  "Perform a feat of directional magic to the right of the player."
  [state]
  (directional-magic state :right))

(defn magic-up
  "Perform a feat of directional magic up from the player."
  [state]
  (directional-magic state :up))

(defn magic-down
  "Perform a feat of directional magic down from the player."
  [state]
  (directional-magic state :down))

(defn magic-inventory
  "Perform a feat of magic on inventory."
  [state hotkey]
  state)

(defn not-cursed-items
  [state]
  [])

(defn cursed-items
  [state]
  [])

(defn id->filter-inventory-fn
  [id]
  {:pre [(contains? #{:cursed :not-cursed} id)]}
  (case id
    :cursed cursed-items
    :not-cursed not-cursed-items))
