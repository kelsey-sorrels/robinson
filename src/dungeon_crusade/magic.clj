;; Functions for querying and manipulating magic
(ns dungeon-crusade.magic
  (:use dungeon-crusade.common
        [dungeon-crusade.dialog :exclude [-main]])
  (:require [taoensso.timbre :as timbre]))

(timbre/refer-timbre)


(def magic
  [{:id :zap
    :name "zap"
    :state :direction-magic
    :fn (fn [state collided-object] state)}])

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
        options (zipmap (take (count valid-input) [\a \b \c \d \e \f]) valid-input)
        input (get options keyin)]
    (-> state
      (assoc-in [:world :current-state] (input :state))
      (assoc-in [:world :magical-ability-id] (input :id)))))

(defn directional-magic
  "Perform a feat of magic in a direction by returning a new state with
   the magical ability applied."
  [state direction]
  {:pre [(contains? #{:left :right :up :down} direction)]}
  (let [obj (first-collidable-object state direction)
        magic (id->magic (get-in state [:world :magical-ability-id]))]
    ((magic :fn) obj)))

(defn magic-left
  "Perform a feat of directional magic to the left of the player."
  [state]
  (directional-magic :left))

(defn magic-right
  "Perform a feat of directional magic to the right of the player."
  [state]
  (directional-magic :right))

(defn magic-up
  "Perform a feat of directional magic up from the player."
  [state]
  (directional-magic :up))

(defn magic-down
  "Perform a feat of directional magic down from the player."
  [state]
  (directional-magic :down))

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
