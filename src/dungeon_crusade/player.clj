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

(defn- merge-items
  [item1 item2]
  ;(info "merging" item1 item2)
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
      (update-in item1 [:count] (partial + (get item2 :count)))))

(defn add-to-inventory
  "Adds `item` to player's inventory assigning hotkeys as necessary."
  [state items]
  (let [inventory               (get-in state [:world :player :inventory])
        remaining-hotkeys       (get-in state [:world :remaining-hotkeys])
        inventory-hotkeys       (set (map :hotkey inventory))
        item-hotkeys            (set (remove nil? (map :hotkey items)))
        _                       (trace remaining-hotkeys items)
        _                       (trace "inventory hotkeys" (set (map :hotkey inventory)))
        _                       (trace "item hotkeys" (set (map :hotkey items)))
        inventory               (mapv
                                  (fn [items]
                                    (reduce merge-items items))
                                  (vals (group-by :id  (concat inventory items))))
        _                       (trace "new inventory hotkeys" (set (map :hotkey inventory)))
        freed-inventory-hotkeys (clojure.set/difference inventory-hotkeys (set (map :hotkey inventory)))
        freed-item-hotkeys      (clojure.set/difference item-hotkeys (set (map :hotkey inventory)))
        _                       (trace "freed-hotkeys" (clojure.set/union freed-inventory-hotkeys freed-item-hotkeys))
        remaining-hotkeys       (vec (sort (vec (clojure.set/union remaining-hotkeys freed-item-hotkeys freed-inventory-hotkeys))))
        _                       (trace "remaining-hotkeys" remaining-hotkeys)
        inventory               (vec (fill-missing #(not (contains? % :hotkey))
                                                   #(assoc %1 :hotkey %2)
                                                   remaining-hotkeys
                                                   inventory))
        remaining-hotkeys       (vec (sort (vec (clojure.set/difference (set remaining-hotkeys) (set (map :hotkey inventory))))))
        _                       (info "remaining-hotkeys" remaining-hotkeys)]
    (-> state
      (assoc-in [:world :player :inventory] inventory)
      (assoc-in [:world :remaining-hotkeys] (vec remaining-hotkeys)))))
        
