;; Functions for manipulating player inventory
(ns robinson.inventory
  (:require [robinson.common :as rc]
            [taoensso.timbre :as log]
            [robinson.random :as rr]
            clojure.set))

(defn player-inventory
  [state]
  (get-in state [:world :player :inventory]))

(defn inventory-hotkeys
  [state]
  (map :hotkey (player-inventory state)))

(defn merge-items
  [item1 item2]
  (log/info "merging" item1 item2)
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

;; ids in this set will not be coalesced into collections of existing items.
(def non-mass-ids
  #{})

(defn add-to-inventory
  "Adds `item` to player's inventory assigning hotkeys as necessary."
  [state items]
  {:pre [(sequential? items)]
   ; every item has a name and hotkey
   :post [(every? (fn [item]
                    (assert (get item :item/id) (str "item does not have id" item))
                    (assert (get item :name) (str "item does not have name" item))
                    (assert (get item :hotkey (str "item does not have hotkey" item)))
                    true)
                  (player-inventory %))
          ; every hotkey is used just once
          (every? (partial = 1)
            (->> %
              player-inventory
              :hotkey
              frequencies
              second))
          ; remaining hotkeys are just characters
          (every? char? (get-in % [:world :remaining-hotkeys]))
          ; inventory hotkeys and remaining hotkeys do not overlap
          (clojure.set/difference
            (set (map :hotkey (player-inventory %)))
            (set (get-in % [:world :remaining-hotkeys])))]}
  (let [inventory               (get-in state [:world :player :inventory])
        remaining-hotkeys       (get-in state [:world :remaining-hotkeys])
        original-remaining-hotkeys remaining-hotkeys
        inventory-hotkeys       (set (map :hotkey inventory))
        ;; find hotkeys of all items we're adding to inventory
        item-hotkeys            (set (remove nil? (map :hotkey items)))
        _                       (log/info "adding items" (vec items))
        _                       (log/debug remaining-hotkeys)
        _                       (log/debug "inventory hotkeys" (set (map :hotkey inventory)))
        _                       (log/debug "item hotkeys" (set (map :hotkey items)))
        inventory               (mapv
                                  ;; merge same items
                                  (fn [items-group]
                                    (reduce merge-items items-group))
                                  (vals (group-by hash
                                                  (concat inventory items))))
        _                       (log/debug "new inventory hotkeys" (set (map :hotkey inventory)))
        _                       (log/info "new inventory" inventory)
        ;; find the hotkeys that were previously used in inventory that are no longer in use
        freed-inventory-hotkeys (clojure.set/difference inventory-hotkeys (set (map :hotkey inventory)))
        ;; find the hotkeys that were used in the added items that are no longer in use
        freed-item-hotkeys      (clojure.set/difference item-hotkeys (set (map :hotkey inventory)))
        _                       (log/info "freed-hotkeys" (clojure.set/union freed-inventory-hotkeys freed-item-hotkeys))
        _                       (log/info "remaining-hotkeys" remaining-hotkeys)
        ;; find all the free hotkeys that were the previous free hotkeys plus the newly freed item and inventory hotkeys.
        remaining-hotkeys       (vec (sort (vec (clojure.set/union remaining-hotkeys freed-item-hotkeys freed-inventory-hotkeys))))
        _                       (log/debug "remaining-hotkeys" remaining-hotkeys)
        inventory               (vec (rc/fill-missing #(not (contains? % :hotkey))
                                                      #(assoc %1 :hotkey %2)
                                                      remaining-hotkeys
                                                      inventory))
        inventory               (->> inventory
                                  (reduce (fn [inventory item]
                                    (log/info item (set items))
                                    (conj
                                      inventory
                                      (if (some (set items) #{item (dissoc item :hotkey)})
                                        (do (log/info item)
                                          (assoc item :added-time (get-in state [:world :time])))
                                        item)))
                                    [])
                                   (sort-by :added-time)
                                   reverse
                                   vec)
        _                       (log/info "new inventory with hotkeys" inventory)
        ;; find all the free hotkeys after filling in missing hotkeys into the newly added inventory items
        remaining-hotkeys       (vec (sort (vec (clojure.set/difference (set remaining-hotkeys) (set (map :hotkey inventory))))))
        newly-assigned-hotkeys  (vec (sort (vec (clojure.set/difference (set original-remaining-hotkeys) (set remaining-hotkeys)))))
        _                       (log/info "newly assigned hotkeys" newly-assigned-hotkeys)]
    (-> state
      ;; TODO: append log with message about new items and their hotkeys
      (assoc-in [:world :player :inventory] inventory)
      (assoc-in [:world :remaining-hotkeys] (vec remaining-hotkeys))
      ((fn [state] (reduce (fn [state item] (let [item (first (filter (fn [i] (= (get i :item/id) (get item :item/id)))
                                                                      (get-in state [:world :player :inventory])))]
                                               (rc/append-log state (format "%s-%c" (get item :name) (get item :hotkey)))))
                           state
                           items))))))

(defn inventory-hotkey->item
  [state hotkey]
  (first (filter (fn [item] (= hotkey (get item :hotkey))) (player-inventory state))))

(defn inventory-hotkey->item-id
  [state hotkey]
  (get (inventory-hotkey->item state hotkey) :item/id))

(defn inventory-id->item
  [state id]
  (first (filter (fn [item] (= id (get item :item/id))) (player-inventory state))))

(defn inventory-id-freqs
  [state]
  (reduce (fn [m item]
            (assoc m
                   (get item :item/id)
                   (+ (get m (get item :item/id) 0)
                      (get item :count 1))))
          {}
          (get-in state [:world :player :inventory])))

(defn inventory-id->count
  [state id]
  (get (inventory-id-freqs state) id 0))

(defn inventory-contains?
  [state id]
  (<  0 (inventory-id->count state id)))

(defn remove-from-inventory
  "Removes item with `hotkey` from player's inventory freeing hotkeys as necessary. Effectively destroys the item."
  [state hotkey]
  (let [item   (inventory-hotkey->item state hotkey)
        _ (log/info "removing item" item)
        _ (log/info "freeing hotkey" hotkey)]
    (-> state
      (update-in [:world :player :inventory]
        (fn [inventory]
          (vec (rc/remove-first (fn [item] (= (get item :hotkey) hotkey))
               inventory))))
      (rc/conj-in [:world :remaining-hotkeys] hotkey))))

(defn update-inventory-item
  "Apply the fn f to inventory items where `(pred item)` is true."
  [state pred f & args]
  (apply rc/update-in-matching state [:world :player :inventory] pred f args))

(defn update-inventory-item-by-id
  "Apply the fn f to inventory item identified by id."
  [state id f & args]
  (apply update-inventory-item state (fn [item] (= (get item :item/id) id)) f args))

(defn update-worn-item-utility
  [state f]
  (update-inventory-item state (fn [item] (contains? item :worn)) f))

(defn dec-item-count
  "Decreses the count of an item in inventory."
  [state hotkey]
  {:pre [(char? hotkey)]}
  (let [item       (inventory-hotkey->item state hotkey)
        item-count (get item :count 1)]
    (cond
      (zero? item-count)
        state
      (= 1 item-count)
        (remove-from-inventory state hotkey)
      :else
        (rc/map-in
          state
          [:world :player :inventory]
          (fn [item]
            (cond-> item
              (= hotkey (get item :hotkey))
              (update :count dec)))))))

(defn dec-item-utility
 ([state hotkey-or-id]
  (dec-item-utility state hotkey-or-id 1))
 ([state hotkey-or-id amount]
  (log/info "decrementing utility for" hotkey-or-id)
  (let [id (cond
             (keyword? hotkey-or-id)
               hotkey-or-id
             (char? hotkey-or-id)
               (inventory-hotkey->item-id state hotkey-or-id)
             :else
               (throw (IllegalArgumentException. "hotkey-or-id was neither a keyword nor a character.")))]
  (log/info "decrementing utility for item with id" id)
  (as-> state state
    (update-inventory-item
      state
      id
      (fn [item]
        (log/info "decrementing utility for" item)
        (update-in item [:utility] (fn [utility] (- utility amount)))))
     ;; remove any broken items
    (reduce
      (fn [state item]
        (log/debug "Check to see if" item "has broken")
        (if (< (get item :utility 2) 1)
          (as-> state state
            ;; item breaks
            (dec-item-count state (get item :hotkey))
            ;; add parts
            (if (pos? (count (get item :recoverable-items)))
              (add-to-inventory state [(rr/rand-nth (get item :recoverable-items))])
              state))
          state))
      state
      (player-inventory state))))))

