;; Functions for manipulating player state
(ns dungeon-crusade.player
  (:use     dungeon-crusade.common)
  (:require [taoensso.timbre :as timbre]
            [pallet.thread-expr :as tx]))

(timbre/refer-timbre)

(defn player-dead?
  "Return `true` if the player has a status of `:dead`."
  [state]
  (contains? (-> state :world :player :status) :dead))

(defn add-to-inventory
  "Adds `item` to player's inventory assigning hotkeys as necessary."
  [state items]
  (let [inventory (get-in state [:world :player :inventory])
        items (fill-missing #(not (or (contains? % :hotkey)
                                      (contains? (set (map :id inventory)) (get % :id))))
                            #(assoc %1 :hotkey %2)
                            (get-in state [:world :remaining-hotkeys])
                            items)
        remaining-hotkeys (vec (remove (set (map :hotkey items)) items))
        inventory (apply concat
                    (vals (merge-with (fn [item1 item2]
                                        (cond 
                                          (and (not (contains? item1 :count))
                                               (not (contains? item2 :count)))
                                            (assoc item1 :count 2)
                                          (and (contains? item1 :count)
                                               (not (contains? item2 :count)))
                                            (update-in item1 [:count] inc)
                                          (and (not (contains? item1 :count))
                                               (contains? item2 :count))
                                            (update-in item2 [:count] inc)
                                          (and (contains? item1 :count)
                                               (contains? item2 :count))
                                            (update-in item1 [:count] (get item2 :count))))
                                      (group-by :id  (concat inventory items)))))]
    (-> state
      (assoc-in [:world :player :inventory] inventory)
      (assoc-in [:world :remaining-hotkeys] remaining-hotkeys))))
        
