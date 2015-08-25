;; Functions for manipulating player state
(ns robinson.player
  (:require [robinson.common :as rc]
            [robinson.characterevents :as ce]
            [robinson.dynamiccharacterproperties :as dcp]
            [taoensso.timbre :as log]
            [robinson.random :as rr]
            clojure.set
            #?@(:clj (
               clojure.string)
               :cljs (
               [goog.string :as gstring]
               [goog.string.format]))))

(defn format [s & args]
  #?(:clj
     (apply clojure.core/format s args)
     :cljs
     (apply gstring/format s args)))

(defrecord Player [id
                   name
                   race
                   class
                   movement-policy
                   in-party?
                   inventory
                   dexterity
                   speed
                   size
                   strength
                   toughness
                   hp
                   max-hp
                   will-to-live
                   max-will-to-live
                   money
                   xp
                   level
                   hunger
                   max-hunger
                   thirst
                   max-thirst
                   pos
                   starting-pos
                   place
                   body-parts
                   attacks
                   status
                   stats
                   wounds
                   abilities]
  Object
  (toString [this] (str "#Player" (into {} this)))
  ce/CharacterEvents
  (on-successful-attack [this state]
    state)
  (on-missed-attack [this state]
    state)
  (on-hit [this state]
    state)
  (on-death [this state]
    state)
  (on-tick [this state]
    state)
  dcp/DynamicCharacterProperties
  (get-energy [this state]
    (get this :energy))
  (get-speed [this state]
    (get this :speed))
  (get-size [this state]
    (get this :size))
  (get-strength [this state]
    (get this :strength))
  (get-toughness [this state]
    (get this :toughness)))

(defn gen-player
  [inventory starting-pos]
  (Player.
    ;id
    :player
    ;name
    "Player"
    ;race
    :human
    ;class
    :ranger
    ;movement-policy
    :entourage
    ;in-party?
    true
    ;inventory
    inventory
    ;dexterity
    1
    ;speed
    1
    ;size
    75
    ;strength
    5
    ;toughness
    5
    ;hp
    10
    ;max-hp
    10
    ;will-to-live
    100
    ;max-will-to-live
    100
    ;money
    50
    ;xp
    0
    ;level
    1
    ;hunger
    0
    ;max-hunger
    100
    ;thirst
    0
    ;max-thirst
    100
    ;pos
    starting-pos
    ;starting-pos
    starting-pos
    ;place
    nil
    ;body-parts
    #{:head :neck :face :abdomen :arm :leg :foot}
    ;attacks
    #{:punch}
    ;status
    #{}
    ;stats
    {
      :timeline (list)
      :num-animals-killed       {}
      :num-items-crafted        {}
      :num-items-harvested      {}
      :num-kills-by-attack-type {}
      :num-items-eaten          {}}
    ;; map from body-part to {:time <int> :damage <float>}
    ;wounds
    {}
    ; abilities
    [:wtl->hp
     :wtl->hunger
     :wtl->thirst]))

(defn get-player
  [state]
  (get-in state [:world :player]))

(defn get-player-value
  [state k]
  (get (get-player state) k))

(defn update-player
  [state f]
  (update-in state [:world :player] f))

(defn update-player-status
  [state f]
  (update-in state [:world :player :status] f))

(defn get-player-attribute
  [state k]
  (get (get-player state) k))

(defn assoc-player-attribute
  [state k v & kvs]
  (update-player state
                 (fn [player]
                   (apply assoc player k v kvs))))

(defn dissoc-player-attribute
  [state k & ks]
  (update-player state
                 (fn [player]
                   (apply dissoc player k ks))))

(defn player-status
  [state]
  (get-player-attribute state :status))

(defn player-status-contains?
  [state v]
  (contains? (set (player-status state)) v))

(defn conj-player-status
  [state k & ks]
  (update-player-status state
                        (fn [status]
                          (apply conj status k ks))))

(defn disj-player-status
  [state k & ks]
  (update-player-status state
                        (fn [status]
                          (apply disj status k ks))))

(defn some-player-status
  [state pred]
  (some pred (get-in state [:world :player :status])))

(defn neg-hp?
  "Return `true` if the player has negative hp."
  [state]
  (neg? (get-in state [:world :player :hp])))

(defn player-dead?
  "Return `true` if the player has a status of `:dead`."
  [state]
  (contains? (-> state :world :player :status) :dead))

(defn kill-player
  "Kill the player by setting the status to `:dead`."
  [state]
  (rc/conj-in state [:world :player :status] :dead))

(defn player-wounded?
  [state]
  (seq (get-in state [:world :player :wounds])))

(defn player-poisoned?
  [state]
  (contains? (get-in state [:world :player :status]) :poisoned))

(defn player-infected?
  [state]
  (contains? (get-in state [:world :player :status]) :infected))

(defn player-to-be-paralyzed?
  [state]
  (contains? (get-player state) :paralyzed-start-expire-time))

(defn player-paralyzed?
  [state]
  (contains? (get-player state) :paralyzed-expire-time))

(defn player-pos
  "Return the position of the player."
  [state]
  {:pre  [(get-in state [:world :player :pos])]
   :post [(integer? (get % :x))
          (integer? (get % :y))]}
  (get-in state [:world :player :pos]))

(defn player-starting-pos
  "Return the starting position of the player."
  [state]
  (get-in state [:world :player :starting-pos]))

(defn player-distance-from-starting-pos
  [state]
  (rc/distance (player-pos state) (player-starting-pos state)))

(defn player-xy
  "Return `[x y]` position of player."
  [state]
  (rc/pos->xy (player-pos state)))

(defn player-inventory
  [state]
  (get-in state [:world :player :inventory]))

(defn wielded-item
  [actor]
  (first (filter (fn [item] (contains? item :wielded))
                 (get actor :inventory))))
(defn player-hp
  [state]
  (get-in state [:world :player :hp]))

(defn player-wtl
  [state]
  (get-in state [:world :player :will-to-live]))

(defn player-hunger
  [state]
  (get-in state [:world :player :hunger]))

(defn player-thirst
  [state]
  (get-in state [:world :player :thirst]))

(defn player-max-hp
  [state]
  (get-in state [:world :player :max-hp]))

(defn player-max-wtl
  [state]
  (get-in state [:world :player :max-will-to-live]))

(defn player-max-hunger
  [state]
  (get-in state [:world :player :max-hunger]))

(defn player-max-thirst
  [state]
  (get-in state [:world :player :max-thirst]))


(defn player-xp
  [state]
  (get-in state [:world :player :xp]))

;; total xp for levels 0 - 20
(def xp
  [0
   2000
   4620
   8040
   12489
   18258
   25712
   35309
   47622
   63364
   83419
   108879
   141086
   181879
   231075
   313656
   424067
   571190
   766569
   1025154])

(def xp-to-next-level
  (mapv - (rest xp) xp))

(defn player-level
  [state]
  (let [player-xp (player-xp state)]
    (count (filter #(< % player-xp) xp))))

(defn xp-for-next-level
  [state]
  (get xp-to-next-level (player-level state)))

(defn xp-acc-for-next-level
  [state]
  (- (player-xp state) (get xp (dec (player-level state)) 0)))

(defn player-update-hp
  [state f]
  {:post [(<= 0 (player-hp %) (player-max-hp %))]}
  (update-in state [:world :player :hp] f))

(defn player-update-wtl
  [state f]
  {:post [(<= 0 (player-wtl %) (player-max-wtl %))]}
  (update-in state [:world :player :will-to-live] f))

(defn player-update-hunger
  [state f]
  {:post [(<= 0 (player-hunger %) (player-max-hunger %))]}
  (update-in state [:world :player :hunger] f))

(defn player-update-thirst
  [state f]
  {:post [(<= 0 (player-thirst %) (player-max-thirst %))]}
  (update-in state [:world :player :thirst] f))

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

(defn add-to-inventory
  "Adds `item` to player's inventory assigning hotkeys as necessary."
  [state items]
  {:pre [(sequential? items)]}
  (let [inventory               (get-in state [:world :player :inventory])
        remaining-hotkeys       (get-in state [:world :remaining-hotkeys])
        original-remaining-hotkeys remaining-hotkeys
        inventory-hotkeys       (set (map :hotkey inventory))
        ;; find hotkeys of all items we're adding to inventory
        item-hotkeys            (set (remove nil? (map :hotkey items)))
        _                       (log/debug remaining-hotkeys items)
        _                       (log/debug "inventory hotkeys" (set (map :hotkey inventory)))
        _                       (log/debug "item hotkeys" (set (map :hotkey items)))
        inventory               (mapv
                                  (fn [items]
                                    (reduce merge-items items))
                                  (vals (group-by :id  (concat inventory items))))
        _                       (log/debug "new inventory hotkeys" (set (map :hotkey inventory)))
        _                       (log/info "new inventory" inventory)
        ;; find the hotkeys that were previously used in inventory that are no longer in use
        freed-inventory-hotkeys (clojure.set/difference inventory-hotkeys (set (map :hotkey inventory)))
        ;; find the hotkeys that were used in the added items that are no longer in use
        freed-item-hotkeys      (clojure.set/difference item-hotkeys (set (map :hotkey inventory)))
        _                       (log/debug "freed-hotkeys" (clojure.set/union freed-inventory-hotkeys freed-item-hotkeys))
        ;; find all the free hotkeys that were the previous free hotkeys plus the newly freed item and inventory hotkeys.
        remaining-hotkeys       (vec (sort (vec (clojure.set/union remaining-hotkeys freed-item-hotkeys freed-inventory-hotkeys))))
        _                       (log/debug "remaining-hotkeys" remaining-hotkeys)
        inventory               (vec (rc/fill-missing #(not (contains? % :hotkey))
                                                      #(assoc %1 :hotkey %2)
                                                      remaining-hotkeys
                                                      inventory))
        _                       (log/info "new inventory with hotkeys" inventory)
        ;; find all the free hotkeys after filling in missing hotkeys into the newly added inventory items
        remaining-hotkeys       (vec (sort (vec (clojure.set/difference (set remaining-hotkeys) (set (map :hotkey inventory))))))
        newly-assigned-hotkeys  (vec (sort (vec (clojure.set/difference (set original-remaining-hotkeys) (set remaining-hotkeys)))))
        _                       (log/info "newly assigned hotkeys" newly-assigned-hotkeys)]
    (-> state
      ;; TODO: append log with message about new items and their hotkeys
      (assoc-in [:world :player :inventory] inventory)
      (assoc-in [:world :remaining-hotkeys] (vec remaining-hotkeys))
      ((fn [state] (reduce (fn [state item] (let [item (first (filter (fn [i] (= (get i :id) (get item :id)))
                                                                      (get-in state [:world :player :inventory])))]
                                               (rc/append-log state (format "%s-%c" (get item :name) (get item :hotkey)))))
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

(defn inventory-id-freqs
  [state]
  (reduce (fn [m item]
            (assoc m
                   (get item :id)
                   (get item :count 1)))
          {}
          (get-in state [:world :player :inventory])))

(defn remove-from-inventory
  "Removes item with `id` from player's inventory freeing hotkeys as necessary. Effectively destroys the item."
  [state id]
  (let [item   (first (filter (fn [item] (= (get item :id) id)) (get-in state [:world :player :inventory])))
        hotkey (get item :hotkey)
        _ (log/info "removing item" item)
        _ (log/info "freeing hotkey" hotkey)]
    (-> state
      (update-in [:world :player :inventory] (rc/log-io "inventory io" (fn [inventory]
                                                                         (vec (rc/remove-first (fn [item] (= (get item :id) id))
                                                                                            inventory)))))
      (rc/conj-in [:world :remaining-hotkeys] hotkey))))

(defn update-inventory-item
  "Apply the fn f to inventory item identified by id."
  [state id f]
  (rc/map-in state [:world :player :inventory]
    (fn [item] (if (= (get item :id) id)
                 (f item)
                 item))))

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
        (rc/map-in state [:world :player :inventory] (fn [item] (if (= id (get item :id))
                                                               (update-in item [:count] dec)
                                                               item))))))
(defn dec-item-utility
 ([state hotkey-or-id]
  (dec-item-utility state hotkey-or-id 1))
 ([state hotkey-or-id amount]
  (log/info "decrementing utility for" hotkey-or-id)
  (let [id (cond
             (keyword? hotkey-or-id)
             hotkey-or-id
             #?(:clj 
                (char? hotkey-or-id)
                :cljs 
                (string? hotkey-or-id))
                (inventory-hotkey->item-id state hotkey-or-id)
             :else
             #?(:clj
                (throw (IllegalArgumentException. "hotkey-or-id was neither a keyword nor a character."))
                :cljs
                (throw (js/Error. "hotkey-or-id was neither a keyword nor a character."))))]
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
        (log/info "Check to see if" item "has broken")
        (if (< (get item :utility 2) 1)
          (as-> state state
            ;; item breaks
            (dec-item-count state (get item :id))
            ;; add parts
            (if (pos? (count (get item :recoverable-items)))
              (add-to-inventory state [(rr/rand-nth (get item :recoverable-items))])
              state))
          state))
      state
      (player-inventory state))))))

(defn update-npc-killed
  [state npc attack]
  (let [max-will-to-live (get-in state [:world :player :max-will-to-live])
        previous-kills   (get-in state [:world :player :stats :animals-killed (get npc :race)] 0)
        dwill-to-live    (/ 10 (inc previous-kills))]
    (-> state
      (update-in [:world :player :will-to-live] (fn [will-to-live] (min max-will-to-live (+ will-to-live dwill-to-live))))
      (update-in [:world :player :stats :animals-killed] (fn [animals-killed] (merge-with + animals-killed {(get npc :race) 1})))
      (update-in [:world :player :stats :kills-by-attack-type] (fn [kills-by-attack-type] (merge-with + kills-by-attack-type {attack 1})))
      (rc/conj-in   [:world :player :stats :timeline] {:time  (get-in state [:world :time])
                                                    :type  :npc-killed
                                                    :npc    npc
                                                    :attack attack}))))

(defn update-harvested
  [state item]
  (let [max-will-to-live   (get-in state [:world :player :max-will-to-live])
        previous-harvested (get-in state [:world :player :stats :num-items-harvested (get item :id)] 0)
        dwill-to-live      (/ 10 (inc previous-harvested))]
    (-> state
      (update-in [:world :player :will-to-live] (fn [will-to-live] (min max-will-to-live (+ will-to-live dwill-to-live))))
      (update-in [:world :player :stats :num-items-harvested] (fn [num-items-harvested] (merge-with + num-items-harvested {(get item :id) 1})))
      (rc/conj-in   [:world :player :stats :timeline] {:time (get-in state [:world :time])
                                                    :type :item-harvested
                                                    :food item}))))

(defn update-crafted
  [state item]
  (let [max-will-to-live (get-in state [:world :player :max-will-to-live])
        previous-crafted (get-in state [:world :player :stats :num-items-crafted (get item :id)] 0)
        dwill-to-live    (/ 10 (inc previous-crafted))]
    (-> state
      (update-in [:world :player :will-to-live] (fn [will-to-live] (min max-will-to-live (+ will-to-live dwill-to-live))))
      (update-in [:world :player :stats :num-items-crafted] (fn [num-items-crafted] (merge-with + num-items-crafted {(get item :id) 1})))
      (rc/conj-in   [:world :player :stats :timeline] {:time (get-in state [:world :time])
                                                    :type :item-crafted
                                                    :food item}))))

(defn update-eaten
  [state item]
  (let [max-will-to-live (get-in state [:world :player :max-will-to-live])
        previous-eaten (get-in state [:world :player :stats :num-items-eaten (get item :id)] 0)
        dwill-to-live    (/ 10 (inc previous-eaten))]
    (-> state
      (update-in [:world :player :will-to-live] (fn [will-to-live] (min max-will-to-live (+ will-to-live dwill-to-live))))
      (update-in [:world :player :stats :num-items-eaten] (fn [num-items-eaten] (merge-with + num-items-eaten {(get item :id) 1})))
      (rc/conj-in   [:world :player :stats :timeline] {:time (get-in state [:world :time])
                                                    :type :food-eaten
                                                    :food item}))))

(defn update-player-died
  [state reason]
  (rc/conj-in state [:world :player :stats :timeline] {:time (get-in state [:world :time])
                                                    :type :player-died}))

(defn update-player-won
  [state]
  (rc/conj-in state [:world :player :stats :timeline] {:time (get-in state [:world :time])
                                                    :type :player-won}))

;; Player abilities
(defn ability-id->name
  [ability-id]
  (case ability-id
    :wtl->hp "wtl->hp"
    :wtl->hunger "wtl->hunger"
    :wtl->thirst "wtl->thirst"
    (assert false (format "Could not find ability with id [%s]." (str ability-id)))))

(defn player-abilities
  [state]
  (let [abilities (get-in state [:world :player :abilities])]
    (map (fn [ability-id hotkey] {:id ability-id
                                  :name (ability-id->name ability-id)
                                  :hotkey hotkey})
         abilities
         rc/hotkeys)))

(defn conj-player-ability
  [state ability-id]
  (rc/concat-in state [:world :player :abilities] [ability-id]))

(defn remove-player-ability
  [state ability-id]
  (rc/remove-in state [:world :player :abilities] (partial = ability-id)))

(defn hotkey->player-ability
  [state hotkey-in]
  (first (filter (fn [{:keys [id name hotkey]}]
                       (= hotkey hotkey-in))
                 (player-abilities state))))
(defn wtl->hp
  [state]
  (let [wtl (player-wtl state)
        cost 30]
    (if (> wtl cost)
      (-> state
        (player-update-wtl (fn [wtl] (- wtl cost)))
        (player-update-hp (fn [hp] (min (player-max-hp state)
                                        (+ hp 3))))
        (rc/append-log "You push yourself past your injuries."))
      (rc/append-log "You don't have the strength to fight off your injuries."))))

(defn wtl->hunger
  [state]
  (let [wtl (player-wtl state)
        cost 30]
    (if (> wtl cost)
      (-> state
        (player-update-wtl (fn [wtl] (- wtl 30)))
        (player-update-hunger (fn [hunger] (max 0 (- hunger cost))))
        (rc/append-log "You push yourself past your hunger."))
      (rc/append-log "You don't have the strength to fight off your hunger."))))

(defn wtl->thirst
  [state]
  (let [wtl (player-wtl state)
        cost 30]
    (if (> wtl cost)
      (-> state
        (player-update-wtl (fn [wtl] (- wtl cost)))
        (player-update-thirst (fn [thirst] (max 0 (- thirst cost))))
        (rc/append-log "You push yourself past your thirst."))
      (rc/append-log "You don't have the strength to fight off your thirst."))))

