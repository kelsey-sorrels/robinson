;; Functions for manipulating player state
(ns robinson.player
  (:use     robinson.common)
  (:require [taoensso.timbre :as timbre]
            [pallet.thread-expr :as tx]))

(timbre/refer-timbre)

(defn player-dead?
  "Return `true` if the player has a status of `:dead`."
  [state]
  (contains? (-> state :world :player :status) :dead))

(defn player-wounded?
  [state]
  (seq (get-in state [:world :player :wounds])))

(defn player-poisoned?
  [state]
  (contains? (get-in state [:world :player :status]) :poisoned))

(defn player-infected?
  [state]
  (contains? (get-in state [:world :player :status]) :infected))


(defn player-pos
  "Return the position of the player."
  [state]
  (get-in state [:world :player :pos]))

(defn player-xy
  "Return `[x y]` position of player."
  [state]
  (pos->xy (player-pos state)))

(defn player-adjacent-cells
  "Return a collection of cells adjacent to the player. Does not include diagonals."
  [state]
  (adjacent-cells (current-place state) (player-pos state)))

(defn player-adjacent-xys
  [state]
  (let [[x y] (player-xy state)]
    [[(dec x) y]
     [(inc x) y]
     [x (dec y)]
     [x (inc y)]]))

(defn player-adjacent-pos
  [state direction]
  (let [{x :x y :y} (player-pos state)
        x           (case direction
                      :left  (dec x)
                      :right (inc x)
                      x)
        y           (case direction
                      :up   (dec y)
                      :down (inc y)
                      y)]
    {:x x :y y}))

(defn player-adjacent-cell
  [state direction]
  (apply get-cell-at-current-place state (pos->xy (player-adjacent-pos state direction))))

(defn- merge-items
  [item1 item2]
  (info "merging" item1 item2)
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

(defn player-inventory
  "Gets the player's inventory."
  [state]
  (get-in state [:world :player :inventory]))

(defn add-to-inventory
  "Adds `item` to player's inventory assigning hotkeys as necessary."
  [state items]
  (let [inventory               (get-in state [:world :player :inventory])
        remaining-hotkeys       (get-in state [:world :remaining-hotkeys])
        original-remaining-hotkeys remaining-hotkeys
        inventory-hotkeys       (set (map :hotkey inventory))
        ;; find hotkeys of all items we're adding to inventory
        item-hotkeys            (set (remove nil? (map :hotkey items)))
        _                       (trace remaining-hotkeys items)
        _                       (trace "inventory hotkeys" (set (map :hotkey inventory)))
        _                       (trace "item hotkeys" (set (map :hotkey items)))
        inventory               (mapv
                                  (fn [items]
                                    (reduce merge-items items))
                                  (vals (group-by :id  (concat inventory items))))
        _                       (trace "new inventory hotkeys" (set (map :hotkey inventory)))
        _                       (info "new inventory" inventory)
        ;; find the hotkeys that were previously used in inventory that are no longer in use
        freed-inventory-hotkeys (clojure.set/difference inventory-hotkeys (set (map :hotkey inventory)))
        ;; find the hotkeys that were used in the added items that are no longer in use
        freed-item-hotkeys      (clojure.set/difference item-hotkeys (set (map :hotkey inventory)))
        _                       (trace "freed-hotkeys" (clojure.set/union freed-inventory-hotkeys freed-item-hotkeys))
        ;; find all the free hotkeys that were the previous free hotkeys plus the newly freed item and inventory hotkeys.
        remaining-hotkeys       (vec (sort (vec (clojure.set/union remaining-hotkeys freed-item-hotkeys freed-inventory-hotkeys))))
        _                       (trace "remaining-hotkeys" remaining-hotkeys)
        inventory               (vec (fill-missing #(not (contains? % :hotkey))
                                                   #(assoc %1 :hotkey %2)
                                                   remaining-hotkeys
                                                   inventory))
        _                       (info "new inventory with hotkeys" inventory)
        ;; find all the free hotkeys after filling in missing hotkeys into the newly added inventory items
        remaining-hotkeys       (vec (sort (vec (clojure.set/difference (set remaining-hotkeys) (set (map :hotkey inventory))))))
        newly-assigned-hotkeys  (vec (sort (vec (clojure.set/difference (set original-remaining-hotkeys) (set remaining-hotkeys)))))
        _                       (info "newly assigned hotkeys" newly-assigned-hotkeys)]
    (-> state
      ;; TODO: append log with message about new items and their hotkeys
      (assoc-in [:world :player :inventory] inventory)
      (assoc-in [:world :remaining-hotkeys] (vec remaining-hotkeys))
      ((fn [state] (reduce (fn [state item] (let [item (first (filter (fn [i] (= (get i :id) (get item :id)))
                                                                      (get-in state [:world :player :inventory])))]
                                               (append-log state (format "%s-%c" (get item :name) (get item :hotkey)))))
                           state
                           items))))))
(defn inventory-hotkey->item
  [state hotkey]
  (first (filter (fn [item] (= hotkey (get item :hotkey))) (player-inventory state))))

(defn inventory-hotkey->item-id
  [state hotkey]
  (get (inventory-hotkey->item state hotkey) :id))

(defn inventory-id->item
  [state id]
  (first (filter (fn [item] (= id (get item :id))) (player-inventory state))))

(defn remove-from-inventory
  "Removes item with `id` from player's inventory freeing hotkeys as necessary. Effectively destroys the item."
  [state id]
  (let [item   (first (filter (fn [item] (= (get item :id) id)) (get-in state [:world :player :inventory])))
        hotkey (get item :hotkey)
        _ (info "removing item" item)
        _ (info "freeing hotkey" hotkey)]
    (-> state
      (update-in [:world :player :inventory] (log-io "inventory io" (fn [inventory]
                                                                      (vec (remove-first (fn [item] (= (get item :id) id))
                                                                                         inventory)))))
      (conj-in [:world :remaining-hotkeys] hotkey))))

(defn dec-item-count
  "Decreses the count of an item in inventory."
  [state id]
  (let [item       (inventory-id->item state id)
        item-count (get item :count 1)]
    (cond
      (zero? item-count)
        state
      (= 1 item-count)
        (remove-from-inventory state (get item :id))
      :else
        (map-in state [:world :player :inventory] (fn [item] (if (= id (get item :id))
                                                               (update-in item [:count] dec)
                                                               item))))))
(defn dec-cell-item-count
  "Decreases the count of an item in the player's cell."
  [state id]
  (let [[cell x y] (player-cellxy state)
        item       (first (filter #(= id (get % :id)) (get cell :items)))
        item-count (get item :count 1)]
    (cond
      (zero? item-count)
        state
      (= 1 item-count)
        (update-cell-xy state x y (fn [cell] (remove-in cell [:items] #(= id (get % :id)))))
      :else
        (update-cell-xy state x y (fn [cell] (map-in cell [:items] (fn [item]
                                                                     (if (= id (get item :id))
                                                                       (update-in item [:count] dec)
                                                                       item))))))))

(defn update-npc-killed
  [state npc attack]
  (let [max-will-to-live (get-in state [:world :player :max-will-to-live])
        previous-kills   (get-in state [:world :player :stats :animals-killed (get npc :race)] 0)
        dwill-to-live    (/ 10 (inc previous-kills))]
  (-> state
    (update-in [:world :player :will-to-live] (fn [will-to-live] (min max-will-to-live (+ will-to-live dwill-to-live))))
    (update-in [:world :player :stats :animals-killed] (fn [animals-killed] (merge-with + animals-killed {(get npc :race) 1})))
    (update-in [:world :player :stats :kills-by-attack-type] (fn [kills-by-attack-type] (merge-with + kills-by-attack-type {attack 1}))))))

(defn update-harvested
  [state item]
  (let [max-will-to-live   (get-in state [:world :player :max-will-to-live])
        previous-harvested (get-in state [:world :player :stats :num-items-harvested (get item :id)] 0)
        dwill-to-live      (/ 10 (inc previous-harvested))]
    (-> state
      (update-in [:world :player :will-to-live] (fn [will-to-live] (min max-will-to-live (+ will-to-live dwill-to-live))))
      (update-in [:world :player :stats :num-items-harvested] (fn [num-items-harvested] (merge-with + num-items-harvested {(get item :id) 1}))))))

(defn update-crafted
  [state item]
  (let [max-will-to-live (get-in state [:world :player :max-will-to-live])
        previous-crafted (get-in state [:world :player :stats :num-items-crafted (get item :id)] 0)
        dwill-to-live    (/ 10 (inc previous-crafted))]
    (-> state
      (update-in [:world :player :will-to-live] (fn [will-to-live] (min max-will-to-live (+ will-to-live dwill-to-live))))
      (update-in [:world :player :stats :num-items-crafted] (fn [num-items-crafted] (merge-with + num-items-crafted {(get item :id) 1}))))))

(defn update-eaten
  [state item]
  (let [max-will-to-live (get-in state [:world :player :max-will-to-live])
        previous-eaten (get-in state [:world :player :stats :num-items-eaten (get item :id)] 0)
        dwill-to-live    (/ 10 (inc previous-eaten))]
    (-> state
      (update-in [:world :player :will-to-live] (fn [will-to-live] (min max-will-to-live (+ will-to-live dwill-to-live))))
      (update-in [:world :player :stats :num-items-eaten] (fn [num-items-eaten] (merge-with + num-items-eaten {(get item :id) 1}))))))


(defn inventory-and-player-cell-items
  [state]
  (let [[cell _ _]        (player-cellxy state)
        inventory         (player-inventory state)
        cell-items        (get cell :items [])
        remaining-hotkeys (get-in state [:world :remaining-hotkeys])
        inventory         (vec (fill-missing #(not (contains? % :hotkey))
                                             #(assoc %1 :hotkey %2)
                                             remaining-hotkeys
                                             (concat inventory cell-items)))]
    inventory))

(defn inventory-and-player-cell-hotkey->item
  [state hotkey]
  (first (filter (fn [item] (= hotkey (get item :hotkey))) (inventory-and-player-cell-items state))))


