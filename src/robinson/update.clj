;; Functions that manipulate state to do what the user commands.
(ns robinson.update
  (:use     
    clojure.pprint
    [clojure.string :only [lower-case]]
    clojure.contrib.core
    robinson.common
    robinson.player
    [robinson.dialog :exclude [-main]]
    robinson.npc
    robinson.combat
    robinson.crafting
    [robinson.magic :only [do-magic magic-left magic-down     
                                  magic-up magic-right magic-inventory]]
    [robinson.worldgen :exclude [-main]]
    robinson.lineofsight)
  (:require 
            [robinson.itemgen  :as ig]
            [robinson.monstergen :as mg]
            [robinson.startgame :as sg]
            clojure.pprint
            clojure.core.memoize
            clojure.edn
            clj-tiny-astar.path
            [clojure.data.generators :as dg]
            [pallet.thread-expr :as tx]
            [clojure.stacktrace :as st]
            [taoensso.timbre :as timbre]))

(timbre/refer-timbre)

(defn- pass-state
  [state & more]
  state)

(defn backspace-name
  [state]
  (update-in state
             [:world :player :name]
             (fn [player-name]
               (clojure.string/join (butlast player-name)))))

(defn append-name
  [state key-in]
  (update-in state
             [:world :player :name]
             (fn [player-name]
               (if (and (< (count player-name) 20)
                        (or (<= (int \A) (int key-in) (int \Z))
                        (<= (int \a) (int key-in) (int \z))
                        (contains? #{\-} key-in)))
                  (str player-name key-in)
                  player-name))))

(defn reinit-world
  "Re-initialize the value of `:world` within `state`. Used when the player
   dies and a new game is started."
  [state]
  (reduce (fn [state _] (add-npcs state 1))
          (assoc state :world (init-world (System/currentTimeMillis)))
          (range 5)))

(defn add-starting-inventory
  [state]
  (let [state              (reinit-world state)
        selected-hotkeys   (get-in state [:world :selected-hotkeys])
        start-inventory    (sg/start-inventory)
        start-inventory    (filterv #(contains? selected-hotkeys (get % :hotkey))
                                    start-inventory)
        remaining-hotkeys (reduce disj (set (get-in state [:world :remaining-hotkeys])) selected-hotkeys)]
    (-> state
      (assoc-in [:world :player :inventory] start-inventory)
      (assoc-in [:world :remaining-hotkeys] remaining-hotkeys)
      (assoc-in [:world :selected-hotkeys] #{}))))

(defn select-starting-inventory
  [state key-in]
  (if (and (char? key-in)
           (<= (int \a) (int key-in) (int \k)))
    (let [selected-hotkeys (get-in state [:world :selected-hotkeys])
          _ (info "selected-hotkeys" selected-hotkeys)
          selected-hotkeys ((if (contains? selected-hotkeys key-in)
                              disj
                              conj)
                            selected-hotkeys
                            key-in)
          _ (info "selected-hotkeys" selected-hotkeys)]
      (info "count shk" (count selected-hotkeys))
      (if (< (count selected-hotkeys) 4)
        (assoc-in state
                  [:world :selected-hotkeys]
                  selected-hotkeys)
        state))
    state))

(defn pick-up-gold
  "Vacuums up gold from the floor into player's inventory."
  [state]
  (let [place-id (current-place-id state)
        ;player-x (-> state :world :player :pos :x)
        ;player-y (-> state :world :player :pos :y)
        [{items :items} x y]  (player-cellxy state)
        ;items              (player-cell :items)
        {cash  true
         non-cash-items false}  (group-by (fn [item] (= (item :type) :$))
                                          items)
        _ (debug "picking up gold. Divided items" cash non-cash-items)
        _ (debug "calling (assoc-in state [:world" place-id y x :items"]" (or non-cash-items [])")")
        $                  (reduce + (map :amount (or cash [])))]
    (if (> $ 0)
      (-> state
        (append-log (format "You pick up %d cash." $))
        (assoc-in [:world :places place-id y x :items]
          (or non-cash-items []))
        (update-in [:world :player :$]
          (fn [player-$] (+ $ player-$))))
      state)))

(defn move
  "Move the player one space provided her/she is able. Else do combat. Else swap positions
   with party member."
  [state direction]
  {:pre  [(contains? #{:left :right :up :down :up-left :up-right :down-left :down-right} direction)
          (vector? (get-in state [:world :npcs]))]
   :post [(vector? (get-in % [:world :npcs]))]}
  (let [player-x (-> state :world :player :pos :x)
        player-y (-> state :world :player :pos :y)
        target-x (+ player-x (case direction
                               :left -1
                               :right 1
                               :up-left -1
                               :up-right 1
                               :down-left -1
                               :down-right 1
                               0))
        target-y (+ player-y (case direction
                               :up  -1
                               :down 1
                               :up-left -1
                               :up-right -1
                               :down-left 1
                               :down-right 1
                               0))]
    (cond
      (not (collide? state target-x target-y))
        (-> state
            (assoc-in [:world :player :pos :x] target-x)
            (assoc-in [:world :player :pos :y] target-y)
            (pick-up-gold))
      (= (get (npc-at-xy state target-x target-y) :in-party?) true)
        (-> state
            (assoc-in [:world :player :pos :x] target-x)
            (assoc-in [:world :player :pos :y] target-y)
            (pick-up-gold)
            (map-in [:world :npcs]
                    (fn [npc] (if (and (= (-> npc :pos :x) target-x)
                                       (= (-> npc :pos :y) target-y))
                                (-> npc
                                    (assoc-in [:pos :x] player-x)
                                    (assoc-in [:pos :y] player-y))
                                npc))))
      (and (npc-at-xy state target-x target-y)
           (every? (set (keys (npc-at-xy state target-x target-y))) #{:hp :pos :race :body-parts :inventory}))
        ;; collided with npc. Engage in combat.
        (attack state [:world :player] (npc->keys state (npc-at-xy state target-x target-y)))
      ;; collided with a wall or door, nothing to be done.
      :else
        state)))
  

(defn move-left
  "moves the player one space to the left provided he/she is able."
  [state]
  (move state :left))

(defn move-right
  "moves the player one space to the right provided he/she is able."
  [state]
  (move state :right))

(defn move-up
  "moves the player one space up provided he/she is able."
  [state]
  (move state :up))

(defn move-down
  "moves the player one space down provided he/she is able."
  [state]
  (move state :down))

(defn move-up-left
  "moves the player one space to the left and up provided he/she is able."
  [state]
  (move state :up-left))

(defn move-up-right
  "moves the player one space to the right and up provided he/she is able."
  [state]
  (move state :up-right))

(defn move-down-left
  "moves the player one space down and left provided he/she is able."
  [state]
  (move state :down-left))

(defn move-down-right
  "moves the player one space down and right provided he/she is able."
  [state]
  (move state :down-right))

(defn open-door
  "Open the door one space in the direction relative to the player's position.

   Valid directions are `:left` `:right` `:up` `:down`."
  [state direction]
  (let [player-x (-> state :world :player :pos :x)
        player-y (-> state :world :player :pos :y)
        target-x (+ player-x (case direction
                               :left -1
                               :right 1
                               0))
        target-y (+ player-y (case direction
                               :up  -1
                               :down 1
                               0))]
    (debug "open-door")
    (let [target-cell (get-cell (current-place state) target-x target-y)]
      (debug "target-cell" target-cell)
      (if (and (not (nil? target-cell)) (= (target-cell :type) :close-door))
        (let [place (-> state :world :current-place)]
          (debug "opening door")
          (debug (get-in state [:world :places place target-y target-x]))
          (-> state
            (append-log "The door creaks open")
            (assoc-in [:world :places place target-y target-x :type] :open-door)))
        state))))

(defn open-left
  "Helper function for open-door."
  [state]
  (open-door state :left))

(defn open-right
  "Helper function for open-door."
  [state]
  (open-door state :right))

(defn open-up
  "Helper function for open-door."
  [state]
  (open-door state :up))

(defn open-down
  "Helper function for open-door."
  [state]
  (open-door state :down))

(defn close-door
  "Close the door one space in the direction relative to the player's position.

   Valid directions are `:left` `:right` `:up` `:down`."
  [state direction]
  (let [player-x (-> state :world :player :pos :x)
        player-y (-> state :world :player :pos :y)
        target-x (+ player-x (case direction
                               :left -1
                               :right 1
                               0))
        target-y (+ player-y (case direction
                               :up  -1
                               :down 1
                               0))]
    (debug "close-door")
    (let [target-cell (get-cell (current-place state) target-x target-y)]
      (debug "target-cell" target-cell)
      (if (and (not (nil? target-cell)) (= (target-cell :type) :open-door))
        (let [place (-> state :world :current-place)]
          (debug "opening door")
          (-> state
            (append-log "The door closes")
            (assoc-in [:world :places place target-y target-x :type] :close-door)))
        state))))

(defn close-left
  "Helper function for close-door"
  [state]
  (close-door state :left))

(defn close-right
  "Helper function for close-door"
  [state]
  (close-door state :right))

(defn close-up
  "Helper function for close-door"
  [state]
  (close-door state :up))

(defn close-down
  "Helper function for close-door"
  [state]
  (close-door state :down))

(defn pick-up
  "Move the items identified by `:selected-hotkeys`,
   remove them from the player's cell and put them in
   the player's inventory. Add to them the hotkeys with
   which they were selected."
  [state]
    ;; find all the items in the current cell
    ;; divide them into selected and not-selected piles using the selected-hotkeys
    ;; add the selected pile to the player's inventory
    ;; return the non-selcted pile to the cell
    ;; remove selected-hotkeys from remaining-hotkeys
    ;; clear selected-hotkeys
    (let [place              (-> state :world :current-place)
          [player-cell x y]  (player-cellxy state)
          items              (vec (player-cell :items))
          remaining-hotkeys  (-> state :world :remaining-hotkeys)
          divided-items      (group-by (fn [item] (if (contains? (-> state :world :selected-hotkeys) (item :hotkey))
                                                      :selected
                                                      :not-selected))
                                       (map #(assoc %1 :hotkey %2)
                                            items
                                            (fill-missing not
                                                     (fn [_ hotkey] hotkey)
                                                     remaining-hotkeys
                                                     (map :hotkey items))))
          selected-items     (vec (divided-items :selected))
          not-selected-items (vec (divided-items :not-selected))
          remaining-hotkeys  (vec (remove #(some (partial = %) (map :hotkey selected-items)) remaining-hotkeys))]
      (debug "divided-items" divided-items)
      (debug "selected-items" selected-items)
      (debug "not-selected-items" not-selected-items)
      (let [new-state (-> state
                          (append-log (format "You pick up the item%s" (if (> (count selected-items) 1) "s" "")))
                          ;; dup the item into inventory with hotkey
                          (update-in [:world :player :inventory]
                            (fn [prev-inventory]
                              (vec (concat prev-inventory selected-items))))
                          ;; remove the item from cell
                          (assoc-in [:world :places place y x :items]
                                    not-selected-items)
                          ;;;; hotkey is no longer available
                          (assoc-in [:world :remaining-hotkeys]
                              remaining-hotkeys)
                          ;; reset selected-hotkeys
                          (assoc-in [:world :selected-hotkeys] #{}))]
        (debug "cell-items (-> state :world :places" place y x ":items)")
        new-state)))

(defn drop-item
  "Drop the item from the player's inventory whose hotkey matches `keyin`.
   Put the item in the player's current cell."
  [state keyin]
  (let [place (-> state :world :current-place)
        [player-cell x y] (player-cellxy state)
        items (-> state :world :player :inventory)
        inventory-hotkeys (map #(% :hotkey) items)
        item-index (.indexOf inventory-hotkeys keyin)]
    (if (and (>= item-index 0) (< item-index (count items)))
      (let [item (nth items item-index)
            new-state (-> state
              (append-log (format "You let the %s fall to the ground" (lower-case (get item :name))))
              ;; dup the item into cell
              (update-in [:world :places place y x :items]
                (fn [prev-items]
                  ;; wipe :wielded status from all dropped items
                  (conj prev-items (dissoc item :wielded))))
              ;; remove the item from inventory
              (assoc-in [:world :player :inventory]
               (vec (concat (subvec items 0 item-index)
                            (subvec items (inc item-index) (count items))))))]
              ;;;; hotkey is now  available
              ;(assoc-in [:world :remaining-hotkeys]
              ;  (vec (concat (subvec remaining-hotkeys 0 item-index)
              ;               (subvec remaining-hotkeys (inc item-index) (count remaining-hotkeys))))))]
        (debug "dropping at:" x y "item with index" item-index "item" item)
        (debug "new-state" new-state)
        (debug "cell-items (-> state :world :places" place y x ":items)")
        new-state)
        state)))

(defn assoc-apply-item-id
  [state id]
  (assoc-in state [:world :apply-item-id] id))

(defn get-apply-item-id
  [state]
  (get-in state [:world :apply-item-id]))

(defn select-apply-item
  "Apply the item from the player's inventory whose hotkey matches `keyin`."
  [state keyin]
  (let [item (inventory-hotkey->item state keyin)]
    (if item
      (let [id (get item :id)]
        (-> state
          (assoc-apply-item-id id)
          ((fn [state]
            (cond
              (= id :stick)
                (-> state
                  (assoc-current-state :apply-item-normal)
                  (ui-hint "Pick a direction to use the stick."))
              (ig/is-sharp? item)
                (-> state
                  (assoc-current-state :apply-item-inventory)
                  (ui-hint "Pick an item to use the sharpened stick on."))
              :else state)))))
        state)))

(defn dig-hole
  "Dig in the ground, creating a hole."
  [state]
  (assoc-in state
            (concat [:world :places (current-place-id state)]
                    (reverse (player-xy state))
                    [:type])
            (dg/rand-nth [:freshwater-hole :saltwater-hole :dry-hole])))

(defn apply-sharp-item
  "Apply a sharp item to the inventory item."
  [state item]
  (info "applying sharp item to" item)
  (case (get item :id)
    :unhusked-coconut
    (-> state
      (dec-item-count (get item :id))
      (add-to-inventory [(ig/gen-coconut)]))
    :else
    state))

(defn apply-item
  "Applies the selected item."
  [state keyin]
  (info "apply-item" [(get-apply-item-id state) keyin])
  (first-vec-match [(get-apply-item-id state) keyin]
    [:stick       \>] (dig-hole state)
    [ig/id-is-sharp? :*] (if-let [item (inventory-hotkey->item state keyin)]
                        (apply-sharp-item state item)
                        state)
    [:*           :*] (ui-hint state "You're not sure how to apply it to that.")))

(defn quaff-only-adjacent-cell
  "Drink all adjacent cells."
  [state]
  (let [place (get-in state [:world :places (current-place-id state)])
        [place water] 
        (reduce (fn [[place water] [x y]]
                  (info "testing cell for water:" (get-cell place x y))
                  (if (contains? (get-cell place x y) :water)
                    [(update-in-xy place x y (fn [cell] (assoc cell :water 0))) (+ water (get (get-cell place x y) :water 0))]
                    [place water]))
                [place 0]
                (player-adjacent-xys state))]
    (info "player-adj-xys" (player-adjacent-xys state))
    (-> state
      (assoc-in [:world :places (current-place-id state)] place)
      (update-in [:world :player :thirst] (fn [thirst] (min 0 (- thirst water)))))))

(defn quaff-select
  "Select the next state depending on what quaffable items are available."
  [state]
  (let [num-adjacent-quaffable-cells  (count
                                        (filter (fn [cell] (and (not (nil? cell))
                                                                (contains? #{:freshwater-hole :saltwater-hole} (get cell :type))
                                                                (> (get cell :water) 10)))
                                                (player-adjacent-cells state)))
        _ (info "player-adj-cells" (player-adjacent-cells state))
        quaffable-inventory-item? (some (fn [item] (contains? item :thirst)) (player-inventory state))]
    (cond
      (and (pos? num-adjacent-quaffable-cells)
           quaffable-inventory-item?)
        (assoc-current-state state :quaff-adj-or-inv)
      (= num-adjacent-quaffable-cells 1)
        (quaff-only-adjacent-cell state)
      (> num-adjacent-quaffable-cells 1)
        (-> state
          (assoc-current-state :quaff-adj)
          (ui-hint "Pick a direction to drink."))
      quaffable-inventory-item?
        (assoc-current-state state :quaff-inventory)
      :else
        (-> state
          (ui-hint "There is nothing to drink.")
          (assoc-current-state :normal)))))

(defn quaff-cell-at-pos
  [state {x :x y :y}]
  (let [water (get (get-cell-at-current-place state x y) :water)]
    (-> state
      (assoc-in [:world :places (current-place-id state) y x :water] 0)
      (update-in [:world :player :thirst] (fn [thirst] (min 0 (- thirst water)))))))

(defn quaff-up
  [state]
  (quaff-cell-at-pos state (player-adjacent-pos state :up)))

(defn quaff-down
  [state]
  (quaff-cell-at-pos state (player-adjacent-pos state :down)))

(defn quaff-left
  [state]
  (quaff-cell-at-pos state (player-adjacent-pos state :left)))

(defn quaff-right
  [state]
  (quaff-cell-at-pos state (player-adjacent-pos state :right)))

(defn quaff-inventory
  [state keyin]
  (if-let [item (inventory-hotkey->item state keyin)]
    (-> state
      (append-log (format "The %s tastes %s." (lower-case (get item :name))
                                              (dg/rand-nth ["great" "foul" "greasy" "delicious" "burnt" "sweet" "salty"])))
      (update-eaten item)
      ;; reduce hunger
      (update-in [:world :player :thirst]
        (fn [thirst]
          (let [new-thirst (- thirst (item :thirst))]
            (max 0 new-thirst))))
      ;; remove the item from inventory
      (remove-from-inventory (get item :id))
      ((fn [state]
        (case (get item :id)
          :coconut (add-to-inventory state [(ig/gen-coconut-empty)])
          state))))
     state))
     
(defn do-rest
  "NOP action. Player's hp increases a little."
  [state]
  (update-in state [:world :player]
             (fn [player] (if (< (int (player :hp)) (player :max-hp))
                            (assoc-in player [:hp] (+ (player :hp) 0.05))
                            player))))

(defn toggle-hotkey
  "Toggle mark `keyin` as a selected hotkey, or not if it already is."
  [state keyin]
  (debug "toggle-hotkey" keyin)
  (update-in state [:world :selected-hotkeys]
             (fn [hotkeys] (if (contains? hotkeys keyin)
                             (disj hotkeys keyin)
                             (conj hotkeys keyin)))))

(defn eat
  "Remove the item whose `:hotkey` equals `keyin` and subtract from the player's
   hunger the item's `:hunger` value."
  [state keyin]
  (let [items (-> state :world :player :inventory)
        inventory-hotkeys (map #(% :hotkey) items)
        item-index (.indexOf inventory-hotkeys keyin)]
    (if (and (>= item-index 0) (< item-index (count items)))
      (let [item (nth items item-index)
            new-state (-> state
              (append-log (format "The %s tastes %s." (lower-case (get item :name))
                                                      (dg/rand-nth ["great" "foul" "greasy" "delicious" "burnt" "sweet" "salty"])))
              (update-eaten item)
              ;; reduce hunger
              (update-in [:world :player :hunger]
                (fn [hunger]
                  (let [new-hunger (- hunger (item :hunger))]
                    (max 0 new-hunger))))
              ;; remove the item from inventory
              (assoc-in [:world :player :inventory]
               (vec (concat (subvec items 0 item-index)
                            (subvec items (inc item-index) (count items))))))]
              ;;;; hotkey is now  available
              ;(assoc-in [:world :remaining-hotkeys]
              ;  (vec (concat (subvec remaining-hotkeys 0 item-index)
              ;               (subvec remaining-hotkeys (inc item-index) (count remaining-hotkeys))))))]
        (debug "new-state" new-state)
        new-state)
        state)))

(defn use-stairs
  "If there are stairs in the player's cell, change the world's `:current-place`
   to the cell's `:dest-place` value."
  [state]
  (let [[player-cell x y] (player-cellxy state)
        orig-place-id     (-> state :world :current-place)]
    (if (contains? #{:down-stairs :up-stairs} (player-cell :type))
      (let [dest-place-id     (player-cell :dest-place)
            ;; manipulate state so that if there isn't a destination place, create one
            _ (debug "dest-place-id" dest-place-id)
            _ (debug "name" (name dest-place-id))
            _ (debug "int" (read-string (name dest-place-id)))
            ;; save the current place to disk
            _                 (spit (format "save/%s.place.edn" orig-place-id)
                                    (with-out-str (pprint (-> state :world :places orig-place-id))))
            ;; load the place into state. From file if exists or gen a new random place.
            state             (assoc-in state [:world :places dest-place-id]
                                (if (.exists (clojure.java.io/as-file (format "save/%s.place.edn" (str dest-place-id))))
                                  (->> (slurp (format "save/%s.place.edn" (str dest-place-id)))
                                       (clojure.edn/read-string {:readers {'Monster mg/map->Monster}}))
                                  (init-random-n (read-string (name dest-place-id)))))
 
            dest-place        (-> state :world :places dest-place-id)

            ;; find the location in the destination place that points to the original place
            dest-cellxy       (first (filter #(and (not (nil? (first %)))
                                                 (contains? #{:up-stairs :down-stairs} ((first %) :type))
                                                 (= ((first %) :dest-place) orig-place-id))
                                    (with-xy dest-place)))
            _ (debug "dest-cellxy" dest-cellxy)
            dest-x             (second dest-cellxy)
            dest-y             (last dest-cellxy)
            party-pos          (adjacent-navigable-pos-extended dest-place {:x dest-x :y dest-y})]
        (debug "dest-x" dest-x "dest-y" dest-y)
        (debug "party-pos" party-pos)
        (debug "npcs" (with-out-str (pprint (-> state :world :npcs))))
        (-> state
          ;; unload the current place
          (dissoc-in [:world :places orig-place-id])
          ;; change the place
          (assoc-in [:world :current-place] (player-cell :dest-place))
          ;; move player to cell where stairs go back to original place
          (assoc-in [:world :player :pos] {:x dest-x :y dest-y})
          ;; move all party members to new place too
          (map-indexed-in-p [:world :npcs]
                            (fn [npc] (contains? npc :in-party?))
                            (fn [i npc] (assoc npc :pos (nth party-pos i)
                                                   :place (player-cell :dest-place))))))
      state)))

(defn init-cursor
  "Initialize the selection cursor at the player's current location."
  [state]
  (let [player-pos (get-in state [:world :player :pos])]
    (assoc-in state [:world :cursor] player-pos)))

(defn search
  "Search the Moore neighborhood for hidden things and make them visible."
  [state]
  state)

(defn extended-search
  "Search the Moore neighborhood for hidden things and make them visible. Describe interesting items in the log."
  [state]
  (let [directions       [["To the north-west" -1 -1] ["To the north" 0 -1] ["To the north-east" 1 -1]
                          ["To the west" -1 0]        ["At your feet" 0 0]  ["To the east" 1 0]
                          ["To the south-west" -1 1]  ["To the south" 0 1]  ["To the south-east" 1 1]]
        describe-cell-fn (fn [state direction cell]
                           (let [items     (get cell :items [])
                                 cell-type (get cell :type)]
                             (info "describing" direction items cell-type)
                             (cond 
                               (and (empty? items)
                                    (= :tall-grass cell-type))
                                 (append-log state (format "%s is tall grass." direction))
                               (and (empty? items)
                                    (= :tree cell-type))
                                 (append-log state (format "%s is a tree." direction))
                               (empty? items)
                                 state
                               (and (= (count items) 1)
                                    (= :tall-grass cell-type))
                                 (append-log state (format "%s is a %s and tall grass." direction (get (first items) :name)))
                               (and (= (count items) 1)
                                    (= :tree cell-type))
                                 (append-log state (format "%s is a %s and a tree." direction (get (first items) :name)))
                               (and (> (count items) 1)
                                    (= :tall-grass cell-type))
                                 (append-log state (format "%s there are %s, and tall grass."
                                                           direction (clojure.string/join ", "
                                                                                          (map (fn [[item-name item-count]]
                                                                                                 (format "%s x%d" item-name item-count))
                                                                                               (frequencies (map :name items))))))
                               (and (> (count items) 1)
                                    (= :tree cell-type))
                                 (append-log state (format "%s there are %s, and a tree."
                                                           direction (clojure.string/join ", "
                                                                                          (map (fn [[item-name item-count]]
                                                                                                 (format "%s x%d" item-name item-count))
                                                                                               (frequencies (map :name items))))))
                               (= (count items) 1)
                                 (append-log state (format "%s there is a %s."
                                                           direction (clojure.string/join ", "
                                                                                          (map (fn [[item-name item-count]]
                                                                                                 (format "%s x%d" item-name item-count))
                                                                                               (frequencies (map :name items))))))
                               (> (count items) 1)
                                 (append-log state (format "%s there are %s."
                                                           direction (clojure.string/join ", "
                                                                                          (map (fn [[item-name item-count]]
                                                                                                 (format "%s x%d" item-name item-count))
                                                                                               (frequencies (map :name items))))))
                               :else
                                 state)))]
    (-> state
      (search)
      ((fn [state]
        (reduce
          (fn [state [direction dx dy]]
            (let [{x :x y :y} (get-in state [:world :player :pos])]
              (describe-cell-fn state direction (get-cell-at-current-place state (+ x dx) (+ y dy)))))
          state
          directions))))))

(defn harvest
  "Collect non-item resources from adjacent or current cell"
  [state direction]
  {:pre  [(contains? #{:left :right :up :down :center} direction)]}
  (let [player-x (-> state :world :player :pos :x)
        player-y (-> state :world :player :pos :y)
        target-x (+ player-x (case direction
                               :left -1
                               :right 1
                               0))
        target-y (+ player-y (case direction
                               :up  -1
                               :down 1
                               0))
        target-cell (get-cell-at-current-place state target-x target-y)
        harvest-items (if (not= target-cell nil)
                        (cond
                          (= (get target-cell :type) :tree)
                            (concat
                              (if (<= 2 (uniform-int 10))
                                (repeat (uniform-int 1 2) (ig/gen-stick))
                                [])
                              (if (= 0 (uniform-int 10))
                                (repeat (uniform-int 1 5) (ig/gen-plant-fiber))
                                []))
                          (= (get target-cell :type) :bamboo)
                              (if (= 0 (uniform-int 10))
                                (repeat (uniform-int 1 2) (ig/gen-bamboo))
                                [])
                          (= (get target-cell :type) :palm-tree)
                            (concat
                              (if (= 0 (uniform-int 10))
                                (repeat (uniform-int 1 2) (ig/gen-unhusked-coconut))
                                [])
                              (if (= 0 (uniform-int 15))
                                (repeat (uniform-int 1 2) (ig/gen-plant-fiber))
                                []))
                          (and (= (get target-cell :type) :tall-grass)
                               (= direction :center))
                            (concat
                              (if (= 0 (uniform-int 10))
                                (repeat (uniform-int 1 5) (ig/gen-grass))
                                [])
                              (if (= 0 (uniform-int 10))
                                (repeat (uniform-int 1 5) (ig/gen-plant-fiber))
                                []))
                          (and (= (get target-cell :type) :gravel)
                               (= direction :center))
                            (concat
                              (if (= 0 (uniform-int 10))
                                (repeat (uniform-int 1 5) (ig/gen-rock))
                                [])
                              (if (= 0 (uniform-int 20))
                                (repeat (uniform-int 1 5) (ig/gen-obsidian))
                                []))
                          :else [])
                        [])]
    (info "harvested" harvest-items)
    (if (empty? harvest-items)
      (append-log state "You don't find anything.")
      (-> state
        (add-to-inventory harvest-items)
        ((fn [state] (reduce update-harvested state harvest-items)))
        (append-log (format "You gather %s." (clojure.string/join ", " (map #(if (> (get % :count 1) 1)
                                                                                  (format "%d %s" (get % :count) (get % :name-plural))
                                                                                  (format "%s %s" (if (contains? #{\a \e \i \o} (first (get % :name)))
                                                                                                     "an"
                                                                                                     "a")
                                                                                                  (get % :name)))
                                                                              harvest-items))))))))

(defn harvest-left [state]
  (harvest state :left))

(defn harvest-right [state]
  (harvest state :right))

(defn harvest-up [state]
  (harvest state :up))

(defn harvest-down [state]
  (harvest state :down))

(defn harvest-center [state]
  (harvest state :center))

(defn wield
  "Wield the item from the player's inventory whose hotkey matches `keyin`."
  [state keyin]
  (let [items (-> state :world :player :inventory)
        inventory-hotkeys (map #(% :hotkey) items)
        item-index (.indexOf inventory-hotkeys keyin)]
    (if (and (>= item-index 0) (< item-index (count items)))
      (let [selected-item (nth items item-index)
            new-state (-> state
              (append-log (format "You wield the %s." (lower-case (get selected-item :name))))
              ;; remove :wielded from all items
              (update-in [:world :player :inventory]
                (fn [items] (mapv (fn [item] (dissoc item :wielded)) items)))
              (update-in [:world :player :inventory]
                (fn [items] (mapv (fn [item] (if (= item selected-item)
                                               (assoc item :wielded true)
                                               item)) items))))]
        new-state)
        state)))

(defn free-cursor
  "Dissassociate the cursor from the world."
  [state]
  (clojure.contrib.core/dissoc-in state [:world :cursor]))

(defn move-cursor-left
  "Move the cursor pos one space to the left keeping in mind the bounds of the current place."
  [state]
  (let [cursor-pos (get-in state [:world :cursor])
        cursor-pos (assoc cursor-pos :x (max 0 (dec (cursor-pos :x))))]
    (assoc-in state [:world :cursor] cursor-pos)))

(defn move-cursor-right
  "Move the cursor pos one space to the right keeping in mind the bounds of the current place."
  [state]
  (let [cursor-pos (get-in state [:world :cursor])
        cursor-pos (assoc cursor-pos :x (min (count (first (current-place state))) (inc (cursor-pos :x))))]
    (assoc-in state [:world :cursor] cursor-pos)))

(defn move-cursor-up
  "Move the cursor pos one space up keeping in mind the bounds of the current place."
  [state]
  (let [cursor-pos (get-in state [:world :cursor])
        cursor-pos (assoc cursor-pos :y (max 0 (dec (cursor-pos :y))))]
    (assoc-in state [:world :cursor] cursor-pos)))

(defn move-cursor-down
  "Move the cursor pos one space down keeping in mind the bounds of the current place."
  [state]
  (let [cursor-pos (get-in state [:world :cursor])
        cursor-pos (assoc cursor-pos :y (min (count (current-place state)) (dec (cursor-pos :y))))]
    (assoc-in state [:world :cursor] cursor-pos)))

(defn describe-at-cursor
  "Add to the log, a message describing the scene at the cell indicated by the
   cursor's position."
  [state]
  (let [cursor-pos (get-in state [:world :cursor])
        cell       (or (get-in (current-place state) [(cursor-pos :y) (cursor-pos :x)])
                       {:type :nil})
        npc        (first (filter (fn [npc] (and (= (cursor-pos :x)
                                                    (-> npc :pos :x))
                                                 (= (cursor-pos :y)
                                                    (-> npc :pos :y))))
                                  (npcs-at-current-place state)))
        message   (case (cell :type)
                    :floor "There is a floor here. You could put things on it if you wanted."
                    :vertical-wall "There is a wall here."
                    :horizontal-wall "There is a wall here."
                    :close-door "There is a closed door here."
                    :open-door "There is an open-door here."
                    :corridor "There is a passageway here."
                    "There is nothing here. Nothing I can tell you anyway.")]
    (-> state
        (append-log message)
        (free-cursor))))

(defn start-talking
  "Open the door one space in the direction relative to the player's position.

   Valid directions are `:left` `:right` `:up` `:down`."
  [state direction]
  (let [player-x (-> state :world :player :pos :x)
        player-y (-> state :world :player :pos :y)
        target-x (+ player-x (case direction
                               :left -1
                               :right 1
                               0))
        target-y (+ player-y (case direction
                               :up  -1
                               :down 1
                               0))]
    (debug "start-talking")
    (if-let [target-npc (npc-at-xy state target-x target-y)]
      ;; store update the npc and world state with talking
      (let [state (-> state
                      (update-npc-at-xy target-x target-y #(assoc % :talking true))
                      (assoc-current-state :talking))
            fsm (get-in state [:dialog (target-npc :id)])
            ;; get the dialog input options for the npc
            valid-input (get-valid-input fsm)]
        ;; if the first option is `nil` then advance the fsm one step.
        ;; this auto step can be used to have the npc speak first when approached.
        (if (nil? (first valid-input))
          (step-fsm state fsm nil)
          state))
      (assoc-current-state state :normal))))

(defn talk-left [state]
  (start-talking state :left))

(defn talk-right [state]
  (start-talking state :right))

(defn talk-up [state]
  (start-talking state :up))

(defn talk-down [state]
  (start-talking state :down))

(defn talk [state keyin]
  (let [npc (first (talking-npcs state))
        fsm (get-in state [:dialog (get npc :id)])
        valid-input (get-valid-input fsm)
        options (zipmap (take (count valid-input) [\a \b \c \d \e \f]) valid-input)
        input (get options keyin)
        _ (debug "Stepping fsm. valid-input:" valid-input)
        _ (debug "Stepping fsm. options:" options)
        _ (debug "Stepping fsm. input:" input)]
    (step-fsm state fsm input)))

(defn stop-talking
  "Remove :talking key from all npcs."
  [state]
  (debug "stop-talking")
  (-> state
      (map-in [:world :npcs] (fn [npc] (dissoc npc :talking)))
      (assoc-current-state :normal)))

(defn describe-inventory
  "Add to the log, a message describing the item with the `:hotkey` value
   matching `keyin`."
  [state keyin]
  (let [items (-> state :world :player :inventory)
        inventory-hotkeys (map #(% :hotkey) items)
        item-index (.indexOf inventory-hotkeys keyin)]
    (if (and (>= item-index 0) (< item-index (count items)))
      (let [item (nth items item-index)
            new-state (append-log state
                                  (case (item :type)
                                    :ring "It's round. A ring."
                                    :scroll "A scroll with some writing."
                                    :food "Something to eat."
                                    "It's like a thing or something."))]
        new-state)
        state)))

(defn start-shopping
  "Starts shopping with a specific npc."
  [state npc]
    ;; store update the npc and world state with talking
    (-> state
        (update-npc npc #(assoc % :shopping true))
        (assoc-current-state :shopping)))
    
(defn shop
  "Start shopping. Allows the player to select \\a -buy or \\b - sell."
  [state keyin]
  (case keyin
    \a (assoc-current-state state :buy)
    \b (assoc-current-state state :sell)
    :else state))

(defn buy
  "Buy an item from an npc in exchange for money."
  [state keyin]
  (let [npc       (first (talking-npcs state))
        options   (zipmap [\a \b \c \d \e \f]
                          (get npc :inventory []))
        item      (get options keyin)]
    (if (and item (< (item :price)
                     (get-in state [:world :player :$])))
      (-> state
          (update-in [:world :player :$] (fn [gold] (- gold (item :price))))
          (transfer-items-from-npc-to-player (get npc :id) (partial = item)))
      state)))


(defn sell
  "Sell an item to an npc in exchange for money."
  [state keyin]
  (let [npc       (first (talking-npcs state))
        buy-fn    (get-in state (get npc :buy-fn-path) (fn [_] nil))
        sellable-items (filter #(not (nil? (buy-fn %)))
                                (get-in state [:world :player :inventory]))
        options   (apply hash-map
                         (mapcat (fn [item] [(item :hotkey) item]) sellable-items))
        _ (debug "sellable items" sellable-items)
        _ (debug "Sell options" options)]
    (if (contains? options keyin)
      (let [item  (get options keyin)
            price (buy-fn item)
            _ (debug "current $" (get-in state [:world :player :$]))]
        (-> state
            (update-in [:world :player :$] (fn [gold] (+ gold price)))
            (transfer-items-from-player-to-npc (get npc :id) (partial = item))))
        state)))

(defn craft-select-recipe
  "Selects a craft recipe."
  [state keyin]
  (let [recipe-type      (case (current-state state)
                           :craft-weapon   :weapons
                           :craft-survival :survival)
        matching-recipes (filter (fn [recipe] (= (get recipe :hotkey) keyin))
                                 (get (get-recipes state) recipe-type))]
    (info "selecting matching recipe" matching-recipes)
    (if (empty? matching-recipes)
      (ui-hint state "Pick a valid recipe.")
      (let [recipe-path [recipe-type (get (first matching-recipes) :hotkey)]]
        (info "selecting recipe path" recipe-path)
        (assoc-in state [:world :craft-recipe-path] recipe-path)))))

(defn craft-weapon
  [state]
  (craft-select-recipe (assoc-current-state state :craft-weapon) \a))

(defn craft-survival
  [state]
  (craft-select-recipe (assoc-current-state state :craft-survival) \a))


(defn craft
  "Craft the selected recipe."
  [state]
  (let [[recipe-type hotkey] (get-in state [:world :craft-recipe-path])
        recipes (get (get-recipes state) recipe-type)
        recipe (first (filter #(= hotkey (get % :hotkey))
                              recipes))]
    (info "recipe-type" recipe-type "hotkey" hotkey "recipes" recipes "recipe" recipe)
    (if recipe
      (-> state
        (craft-recipe recipe)
        (assoc-current-state :normal))
      (append-log state "Pick a valid recipe." :white))))


(defn scroll-log
  [state keyin]
  (let [t (get-in state [:world :time])]
    (-> state
      (update-in [:world :logs-viewed] inc)
      ((fn [state]
        (let [c (count (filter #(= t (get % :time)) (get-in state [:world :log])))
              logs-viewed (get-in state [:world :logs-viewed])]
          (if (>= logs-viewed c)
            (assoc-current-state state :normal)
            state)))))))

(defn init-log-scrolling
  [state]
  (-> state
    (update-in [:world :logs]
      (fn [logs]
        (mapcat
          (fn [logs-with-same-time]
            (vec
              (reduce (fn [logs-with-same-time log]
                        (if (< (+ (count (get (last logs-with-same-time) :message))
                                  (count (get log :message)))
                                70)
                          (vec (conj (butlast logs-with-same-time)
                                      {:time (get log :time)
                                       :message (clojure.string/join " " [(get (last logs-with-same-time) :message)
                                                                          (get log :message)])}))
                          (conj logs-with-same-time log)))
                      []
                      logs-with-same-time)))
          (vals (group-by :time logs)))))
    ((fn [state]
      (let [t    (get-in state [:world :time])
            logs (filter #(= t (get % :time)) (get-in state [:world :log]))]
        (info "logs" logs)
        (-> state
          (assoc-in [:world :logs-viewed] 1)
          (tx/when-> (> (count logs) 1)
            (assoc-current-state :more-log))))))))

(defn get-hungrier-and-thirstier
  "Increase player's hunger."
  [state]
  (-> state
    (update-in [:world :player :hunger] (partial + 0.4))
    (update-in [:world :player :thirst] (partial + 0.3))
    ((fn [state] (update-in state
                            [:world :player :status]
                            (fn [status]
                              (set (remove nil?
                                   (conj status (condp <= (max (-> state :world :player :hunger)
                                                               (-> state :world :player :thirst))
                                                  100 :dead
                                                   80 :dying
                                                   60 :starving
                                                   40 :hungry
                                                  nil))))))))))
(defn decrease-will-to-live
  "Decrease the player's will-to-live depending on circumstances."
  [state]
  (-> state
    (update-in [:world :player :will-to-live]
      (fn [will-to-live]
        (let [dwtl       0.05 ;; you've been on the island. It sucks and you want to get off.
              hp         (get-in state [:world :player :hp])
              max-hp     (get-in state [:world :player :max-hp])
              _          (info "hp" hp "max-hp" max-hp)
              dwtl       (+ dwtl (if (> 0.5 (/ hp max-hp))
                                   1
                                   0))
              hunger     (get-in state [:world :player :hunger])
              max-hunger (get-in state [:world :player :max-hunger])
              _          (info "hunger" hunger "max-hunger" max-hunger)
              dwtl       (+ dwtl (if (> (/ hunger max-hunger) 0.5)
                                   1
                                   0))
              thirst     (get-in state [:world :player :thirst])
              max-thirst (get-in state [:world :player :max-thirst])
              _          (info "thirst" thirst "max-thirst" max-thirst)
              dwtl       (+ dwtl (if (> (/ thirst max-thirst) 0.5)
                                   1
                                   0))
              wounded    (not (empty? (get-in state [:world :player :wounds])))
              _          (info "wounded" wounded)
              dwtl       (+ dwtl (if wounded
                                   1
                                   0))
              poisoned   (contains? (get-in state [:world :player :status]) :poisoned)
              _          (info "poisoned" poisoned)
              dwtl       (+ dwtl (if poisoned
                                   1
                                   0))
              infected   (contains? (get-in state [:world :player :status]) :infected)
              _          (info "infected" infected)
              dwtl       (+ dwtl (if infected
                                   1
                                   0))]
        (info "dwtl" dwtl "will-to-live" will-to-live)
        (- will-to-live dwtl))))
    (update-in [:world :player :status]
               (fn [status]
                 (set (if (neg? (get-in state [:world :player :will-to-live]))
                        (conj status :dead)
                        status))))))

(defn log-will-to-live-flavor
  "Log a flavor message when will-to-live increases or decreases by a lot."
  [state prev-will-to-live]
  (let [dwtl (- prev-will-to-live (get-in state [:world :player :will-to-live]))]
    (if (> (Math/abs dwtl) 1.5)
      (if (neg? dwtl)
        (append-log state (format "You feel %s." (dg/rand-nth ["happy" "happier" "glad" "elated" "great" "good"])))
        (append-log state (format "You feel %s." (dg/rand-nth ["sad" "sadder" "down" "unhappy" "bummed" "bad"]))))
      state)))
  

(defn if-poisoned-get-hurt
  "Decrease player's hp if they are poisoned."
  [state]
  (update-in state [:world :player :hp] (fn [hp] (if (contains? (get-in state [:world :player :status]) :poisoned)
                                                   (- hp 0.1)
                                                   (min (get-in state [:world :player :max-hp])
                                                        (+ hp 0.2))))))
(defn if-wounded-get-infected
  [state]
  (let [hp                  (get-in state [:world :player :hp])
        max-hp              (get-in state [:world :player :max-hp])
        chance-of-infection (inc (/ -1 (inc (/ hp (* max-hp 20)))))] 
    (if (and (not-empty (get-in state [:world :player :wounds]))
             (< (dg/float) chance-of-infection))
      (-> state
        (update-in [:world :player :status]
          (fn [status]
              (conj status :infected)))
        (append-log "Your wounds have become infected." :green))
      state)))

(defn if-infected-get-hurt
  [state]
  (update-in state [:world :player :hp]
    (fn [hp]
      (if (contains? (get-in state [:world :player :status]) :infected)
        (- hp 0.2)
        hp))))

(defn heal
  [state]
  (-> state
    ;; heal wounds
    (tx/arg-> [state]
      (update-in [:world :player :wounds]
        (fn [wounds]
          (reduce-kv (fn [wounds body-part wound]
            (if (< (get wound :dmg) 1)
              wounds
              (assoc wounds body-part {:dmg (- (get wound :dmg) 0.1)
                                       :time (get wound :time)})))
            {}
            wounds))))
    ;; chance of poison wearing off
    (arg-when-> [state]
      (and (contains? (get-in state [:world :player :status]) :poisoned)
           (< (dg/float) 0.1))
        (update-in [:world :player :status]
          (fn [status]
              (disj status :poisoned)))
        (append-log "The poison wore off." :green))
    ;; chance of infection clearing up
    (arg-when-> [state]
      (and (contains? (get-in state [:world :player :status]) :infected)
           (< (dg/float) 0.1))
        (update-in [:world :player :status]
          (fn [status]
              (disj status :infected)))
        (append-log "The infection has cleared up." :yellow))))


;; forward declaration because update-state and repeat-cmmands are mutually recursive.
(declare update-state)

(defn repeat-commands
  [state]
  (let [command-seq (get-in state [:world :command-seq] [])]
    (info "repeating commands" command-seq)
    (reduce update-state state command-seq)))

;; update visibility
(defn update-visibility
  [state]
  (let [pos              (-> state :world :player :pos)
        sight-distance   3
        will-to-live     (get-in state [:world :player :will-to-live])
        max-will-to-live (get-in state [:world :player :max-will-to-live])
        place            (get-in state [:world :places (current-place-id state)])
        get-cell         (memoize (fn [x y] (get-in place [y x])))
        new-time         (get-in state [:world :time])
        test-cells       (for [x (range (- (get pos :x) sight-distance) (+ (get pos :x) sight-distance))
                               y (range (- (get pos :y) sight-distance) (+ (get pos :y) sight-distance))]
                            [x y])
        visible-cells    (filter (fn [[x y]] (and (not (nil? (get-cell x y)))
                                                  (not (farther-than? {:x (- (get pos :x) 0.5) :y (- (get pos :y) 0.5)}
                                                                      {:x x :y y}
                                                                      sight-distance))
                                                  (visible?
                                                    get-cell
                                                    cell-blocking?
                                                    (pos :x)
                                                    (pos :y)
                                                    x y)))
                                 test-cells)
        dwtl             (/ (reduce (fn [acc [x y]] (+ acc (- (dec new-time)
                                                           (get (get-cell x y) :discovered (- new-time 10000)))))
                                 0 visible-cells)
                            48000)
        _                (info "delta will-to-live" (float dwtl))
        will-to-live     (min (+ will-to-live dwtl)
                              max-will-to-live)]
  (-> state
    (update-in [:world :places (-> state :world :current-place)]
      (fn [place]
              (reduce (fn [place [x y]]
                        (update-in place [y x] (fn [cell] (assoc cell :discovered new-time))))
                      place
                      visible-cells)))
    (assoc-in [:world :player :will-to-live] will-to-live))))

(defn next-party-member
  "Switch (-> state :world :player) with the next npc where (-> npc :in-party?) is equal to true.
   Place the player at the end of (-> state :world :npcs)."
  [state]
  (let [npc (first (filter #(contains? % :in-party?)
                            (get-in state [:world :npcs])))
    state (-> state
      (remove-in [:world :npcs] (partial = npc))
      ;; make world npcs a vector so that conj adds the player to the end of the collection
      ;; rather than the beginning.
      (update-in [:world :npcs] vec)
      (conj-in [:world :npcs] (get-in state [:world :player]))
      (assoc-in [:world :player] npc))]
    (debug "npcs" (with-out-str (pprint (-> state :world :npcs))))
    state))

(defn
  move-to-target
  "Move `npc` one space closer to the target position if there is a path
   from the npc to the target. Returns the moved npc and not the updated state.
   `target` is a map with the keys `:x` and `:y`."
  [state npc target]
    (let [npcs                   (npcs-at-current-place state)
          ;_                      (debug "meta" (-> move-to-target var meta))
          ;_                      (debug "move-to-target npc" npc "target" target)
          npc-pos                (get npc :pos)
          npc-pos-vec            [(npc-pos :x) (npc-pos :y)]
          threshold              (get npc :range-threshold)
          npc-can-move-in-water  (mg/can-move-in-water? (get npc :race))
        
          player                 (-> state :world :player)
          player-pos-vec         [(-> player :pos :x) (-> player :pos :y)]
          place                  (current-place state)
          width                  (count (first place))
          height                 (count place)
          get-type               (memoize (fn [x y] (do
                                                      ;(debug "traversable?" x y "type" (get-in place [y x :type]))
                                                      (get-in place [y x :type]))))
          water-traversable?     (fn [[x y]]
                                   (and (< 0 x width)
                                        (< 0 y height)
                                        (not (farther-than? npc-pos {:x x :y y} threshold))
                                        (= (get-type x y) :water)))
          land-traversable?      (fn [[x y]]
                                   (and (< 0 x width)
                                        (< 0 y height)
                                        (not (farther-than? npc-pos {:x x :y y} threshold))
                                        (contains? #{:floor
                                                     :open-door
                                                     :corridor
                                                     :sand
                                                     :dirt
                                                     :gravel
                                                     :tall-grass
                                                     :short-grass}
                                                   (get-type x y))))
      
          traversable?           (if npc-can-move-in-water
                                     water-traversable?
                                     land-traversable?)
          path                   (try
                                   (debug "a* params" [width height] traversable? npc-pos-vec [(target :x) (target :y)])
                                   (clj-tiny-astar.path/a* [width height] traversable? npc-pos-vec [(target :x) (target :y)])
                                   (catch Exception e
                                     (error "Caught exception during a* traversal." npc-pos-vec [(target :x) (target :y)] e)
                                     ;(st/print-cause-trace e)
                                     nil))
          ;_                      (debug "path to target" path)
          new-pos                (if (and (not (nil? path))
                                          (> (count path) 1)
                                          ;; don't collide with player
                                          (let [new-pos (second path)]
                                            (not= ((juxt first second) new-pos)
                                                  ((juxt first second) player-pos-vec))))
                                   (second path)
                                   npc-pos-vec)
          ;_                      (debug "new-pos" new-pos)
          new-npc                (-> npc
                                     (assoc-in [:pos :x] (first new-pos))
                                     (assoc-in [:pos :y] (second new-pos)))
          ;_                      (debug "new-npc" new-npc)
        ]
      [{:x (first new-pos) :y (second new-pos)} new-npc npc]))

(defn move-to-target-in-range-or-random
  [state npc target]
  (let [threshold (get npc :range-threshold)
        npc-pos  (get npc :pos)
        distance (distance npc-pos target)
        navigable-types (if (mg/can-move-in-water? (get npc :race))
                          #{:water}
                          #{:floor
                            :corridor
                            :open-door
                            :sand
                            :dirt
                            :gravel
                            :tall-grass
                            :short-grass})]
    (if (> distance threshold)
      ;; outside of range, move randomly into an adjacent cell
      (let [target (first
                     (dg/shuffle
                       (adjacent-navigable-pos (current-place state)
                                               npc-pos
                                               navigable-types)))]
        ;(debug "distance > threshold, move randomly. target" target)
        [target
         (-> npc
           (assoc-in [:pos :x] (get target :x))
           (assoc-in [:pos :y] (get target :y)))
         npc])
      ;; inside range, move toward player
      (move-to-target state npc target))))

(defn calc-npc-next-step
  "Returns the moved npc and not the updated state. New npc pos will depend on
   the npc's `:movement-policy which is one of `:constant` `:entourage` `:follow-player` `:follow-player-in-range-or-random`."
  [state npc]
  {:pre  [(contains? #{:constant :entourage :follow-player :follow-player-in-range-or-random} (get npc :movement-policy))]
   :post [(= (count %) 3)]}
  (let [policy (get npc :movement-policy)
        pos    (-> state :world :player :pos)
        navigable-types (if (mg/can-move-in-water? (get npc :race))
                          #{:water}
                          #{:floor
                            :corridor
                            :open-door
                            :sand
                            :dirt
                            :gravel
                            :tall-grass
                            :short-grass})]
        ;_ (info "moving npc@" (get npc :pos) "with policy" policy)]
    (case policy
      :constant [nil nil npc]
      :entourage (move-to-target state
                                 npc
                                 (first
                                   (dg/shuffle
                                     (adjacent-navigable-pos (current-place state)
                                                             pos
                                                             navigable-types))))
      :follow-player (move-to-target state npc pos)
      :follow-player-in-range-or-random (move-to-target-in-range-or-random state npc pos)
      [nil nil npc])))
 
(defn update-npcs
  "Move all npcs in the current place using `move-npc`."
  [state]
  {:pre  [(vector? (get-in state [:world :npcs]))]
   :post [(vector? (get-in % [:world :npcs]))]}
  ;; do npc->player attacks if adjacent
  (let [current-place-id (current-place-id state)
        ;; increase all npcs energy by their speed value and have any adjacent npcs attack the player.
        state (reduce
                (fn [state npc]
                  ;; only update npcs that are in the current place and have an :energy value.
                  (if (and (= (get npc :place)
                              current-place-id)
                           (contains? npc :energy))
                    (let [npc-keys (npc->keys state npc)
                          ;; add speed value to energy.
                          ;_ (trace "adding speed to" (select-keys npc [:race :place :pos :speed :energy]))
                          state (update-in state (conj npc-keys :energy) (partial + (get npc :speed)))]
                      ;; adjacent hostile npcs attack player.
                      (if (and (contains? (get npc :status) :hostile)
                               (adjacent-to-player? state (get npc :pos)))
                        (let [_ (info "npc attacker" npc)]
                          (attack state npc-keys [:world :player]))
                        state))
                    state))
                state
                (get-in state [:world :npcs]))]
    (loop [state          state
           ;; find all npcs in the current place with enough energy to move (energy > 1).
           ;;; Since most npcs are moving toward the player, sort by distance
           ;;; to player. The npcs closest will be moved first, leaving a gap
           ;;; behind them allowing the next most distant npc to move forward
           ;;; to fill the gap.
           remaining-npcs (sort-by (fn [npc] (distance-from-player state (get npc :pos)))
                                   (filter (fn [npc] (and (contains? npc :energy)
                                                          (> (get npc :energy) 1)
                                                          (= (get npc :place) current-place-id)))
                                     (get-in state [:world :npcs])))
           i 5]
      ;(info "looping over" (count remaining-npcs) "npcs")
      (if (or (empty? remaining-npcs)
              (neg? i))
        ;; no more results to process, stop looping and return state
        state
        ;; In parallel, find their next step
        ;; Each element in this list has the form [new-pos new-npc npc] (a list of 3-tuples.
        ;; new-pos: a map {:x <int> :y <int>}  describing where the npc is trying to move to
        ;; or nil if the npc is not moving.
        ;; new-npc: is the npc updated to reflect the movement.
        ;; npc: is the original npc object.
        ;; we can execute pathfinding in parallel, and then serially, move each npc, checking
        ;; at each step whether new-pos is occupied and use new-npc or npc depending on the
        ;; outcome.
        ;; Filter out elements where the first value is nil.
        ;_ (info "map-result" (map (fn [npc]
        ;                               (log-time "calc-npc-next-step"
        ;                                 (first (calc-npc-next-step state npc))))
        ;                             remaining-npcs))
        (let [map-result (remove (comp nil? first)
                             (vec (pmap (fn [npc]
                                     ;(log-time "calc-npc-next-step"
                                       (calc-npc-next-step state npc));)
                                   remaining-npcs)))
              ;; update the npcs in serial
              state      (reduce
                           (fn [state [new-pos new-npc old-npc]]
                             ;(trace "reducing" new-pos (select-keys old-npc [:race :place :pos :speed :energy]))
                             ;(trace "npcs")
                             ;(doseq [npc (get-in state [:world :npcs])]
                             ;  (println (select-keys npc [:race :place :pos :speed :energy])))

                             ;; decrement energy either way
                             (update-npc state old-npc
                               (fn [npc]
                                 ;(trace "updating npc" (select-keys npc [:race :place :pos :speed :energy]))
                                 (update-in
                                   ;; no npc at the destination cell?
                                   (if (not-any? (fn [npc] (and (= (get npc :pos)
                                                                   new-pos)
                                                                (= (get npc :place)
                                                                   (get new-npc :place))))
                                                 (get-in state [:world :npcs]))
                                     ;; use the new npc value
                                     new-npc
                                     ;; otherwise there is an npc at the destination, use the old npc
                                     old-npc)
                                   [:energy]
                                   (fn [energy] (max (dec energy) 0))))))
                           state
                           map-result)
              remaining-npcs (sort-by (fn [npc] (distance-from-player state (get npc :pos)))
                               (filter (fn [npc] (and (contains? npc :energy)
                                                      (> (get npc :energy) 1)
                                                      (= (get npc :place) current-place-id)))
                                 (get-in state [:world :npcs])))]
              ;_ (trace "count remaining npcs" (count remaining-npcs))]
        (recur state remaining-npcs (dec i)))))))

(defn update-cells
  "Fill holes with a small amount of water. Drop fruit."
  [state]
  (info "filling holes")
  (-> state
    (update-in [:world :places (current-place-id state)]
      (fn [place]
        (update-matching-cells place
                               (fn [cell] (contains? #{:freshwater-hole :saltwater-hole} (get cell :type)))
                               (fn [cell] (assoc cell :water (min 20 (+ 0.1 (* (dg/float) 0.1) (get cell :water 0.0))))))))
    (update-in [:world :places (current-place-id state)]
      (fn [place]
        (update-matching-cells place
                               (fn [cell] (contains? #{:solar-still} (get cell :type)))
                               (fn [cell] (assoc cell :water (min 20 (+ 0.2 (* (dg/float) 0.1) (get cell :water 0.0))))))))))

(defn update-quests
  "Execute the `pred` function for the current stage of each quest. If 
  `(pred state)` returns `true` then execute the quest's update fn as
  `(update state) an use the result as the new state. Then set the quests new
  stage to the result of `(nextstagefn state)`."
  [state]
  (reduce (fn [state quest]
            (let [stage-id (get-in state [:world :quests (quest :id) :stage] :0)]
              (if (nil? stage-id)
                ;; Skip quest if current stage of quest is nil. Ie: it is completed.
                state
                (let [stage    (get-in quest [:stages stage-id])]
                  ;(debug "exec quest" quest)
                  ;(debug "quest-id" (quest :id))
                  ;(debug "stage-id" stage-id)
                  ;(debug "exec stage" stage)
                  (-> state
                    (tx/when-> ((stage :pred) state)
                      ((stage :update))
                      (assoc-in [:world :quests (quest :id) :stage]
                                ((stage :nextstagefn) stage))))))))
            state (-> state :quests vals)))

;; A finite state machine definition for the game state. 
;; For each starting state, define a transition symbol, a function
;; to call `(transitionfn state)` to use as the new state, and a
;; final state. It's an unfortunate naming collision between the
;; transtion table's states and the application state variable, but
;; they are indeed two different things.
(def state-transition-table
  ;;         starting      transition transition             new              advance
  ;;         state         symbol     fn                     state            time?
  (let [table {:start     {:enter     [identity               :start-inventory false]
                           :backspace [backspace-name         :start           false]
                           :else      [append-name            :start           false]}
               :start-inventory
                          {:enter     [add-starting-inventory :start-text      false]
                           :else      [select-starting-inventory
                                                              :start-inventory false]}
               :start-text
                          {:else      [(fn [state _]
                                         (do-rest state))     :normal          false]}
               :normal    {\i         [identity               :inventory       false]
                           \d         [identity               :drop            false]
                           \,         [identity               :pickup          false]
                           \e         [identity               :eat             false]
                           \o         [identity               :open            false]
                           \c         [identity               :close           false]
                           \.         [do-rest                :normal          true]
                           \h         [move-left              :normal          true]
                           \j         [move-down              :normal          true]
                           \k         [move-up                :normal          true]
                           \l         [move-right             :normal          true]
                           \y         [move-up-left           :normal          true]
                           \u         [move-up-right          :normal          true]
                           \b         [move-down-left         :normal          true]
                           \n         [move-down-right        :normal          true]
                           \q         [quaff-select           identity         false]
                           \w         [identity               :wield           false]
                           \x         [identity               :harvest         false]
                           \a         [identity               :apply           false]
                           \>         [use-stairs             :normal          true]
                           \<         [use-stairs             :normal          true]
                           \;         [init-cursor            :describe        false]
                           \s         [search                 :normal          true]
                           \S         [extended-search        :normal          true]
                           \Q         [identity               :quests          false]
                           \P         [next-party-member      :normal          false]
                           \z         [identity               :craft           true]
                           \Z         [identity               :magic           true]
                           \T         [identity               :talk            true]
                           \m         [identity               :log             false]
                           \?         [identity               :help            false]
                           \r         [repeat-commands        identity         false]
                           :escape    [identity               :quit?           false]}
               :inventory {:escape    [identity               :normal          false]}
               :describe  {:escape    [free-cursor            :normal          false]
                           \i         [free-cursor            :describe-inventory false]
                           \h         [move-cursor-left       :describe        false]
                           \j         [move-cursor-down       :describe        false]
                           \k         [move-cursor-up         :describe        false]
                           \l         [move-cursor-right      :describe        false]
                           :enter     [describe-at-cursor     :normal          false]}
               :quests    {:escape    [identity               :normal          false]}
               :describe-inventory
                          {:escape    [identity               :normal          false]
                           :else      [describe-inventory     :normal          false]}
               :drop      {:escape    [identity               :normal          false]
                           :else      [drop-item              :normal          true]}
               :apply     {:escape    [identity               :normal          false]
                           :else      [select-apply-item      identity         false]}
               :apply-item-normal
                          {:escape    [identity               :normal          false]
                           :else      [apply-item             :normal          true]}
               :apply-item-inventory
                          {:escape    [identity               :normal          false]
                           :else      [apply-item             :normal          true]}
               :pickup    {:escape    [identity               :normal          false]
                           :else      [toggle-hotkey          :pickup          false]
                           :enter     [pick-up                :normal          true]}
               :eat       {:escape    [identity               :normal          false]
                           :else      [eat                    :normal          true]}
               :quaff-adj-or-inv
                          {\i         [identity               :quaff-inv       false]
                           \h         [quaff-left             :normal          true]
                           \j         [quaff-down             :normal          true]
                           \k         [quaff-up               :normal          true]
                           \l         [quaff-right            :normal          true]}
               :quaff-adj {\h         [quaff-left             :normal          true]
                           \j         [quaff-down             :normal          true]
                           \k         [quaff-up               :normal          true]
                           \l         [quaff-right            :normal          true]}
               :quaff-inventory
                          {:escape    [identity               :normal          false]
                           :else      [quaff-inventory        :normal          true]}
               :open      {\h         [open-left              :normal          true]
                           \j         [open-down              :normal          true]
                           \k         [open-up                :normal          true]
                           \l         [open-right             :normal          true]}
               :talk      {\h         [talk-left              identity         false]
                           \j         [talk-down              identity         false]
                           \k         [talk-up                identity         false]
                           \l         [talk-right             identity         false]}
               :harvest   {\h         [harvest-left           :normal          true]
                           \j         [harvest-down           :normal          true]
                           \k         [harvest-up             :normal          true]
                           \l         [harvest-right          :normal          true]
                           \.         [harvest-center         :normal          true]
                           :escape    [identity               :normal          false]}
               :wield     {:escape    [identity               :normal          false]
                           :else      [wield                  :normal          true]}
               :talking   {:escape    [stop-talking           :normal          false]
                           :else      [talk                   identity         true]}
               :shopping  {\a         [identity               :buy             true]
                           \b         [identity               :sell            true]
                           :escape    [identity               :normal          false]}
               :buy       {:escape    [identity               :normal          false]
                           :else      [buy                    :buy             true]}
               :sell      {:escape    [identity               :normal          false]
                           :else      [sell                   :sell            true]}
               :craft     {\w         [craft-weapon           :craft-weapon    false]
                           \s         [craft-survival         :craft-survival  false]
                           :escape    [identity               :normal          false]}
               :craft-weapon
                          {:escape    [identity               :craft           false]
                           :enter     [craft                  :normal          true]
                           :else      [craft-select-recipe    identity         false]}
               :craft-survival
                          {:escape    [identity               :craft           false]
                           :enter     [craft                  :normal          true]
                           :else      [craft-select-recipe    identity         false]}
               :magic     {:escape    [identity               :normal          false]
                           :else      [do-magic               identity         true]}
               :magic-direction
                          {\h         [magic-left             identity         true]
                           \j         [magic-down             identity         true]
                           \k         [magic-up               identity         true]
                           \l         [magic-right            identity         true]}
               :magic-inventory
                          {:escape    [identity               :normal          false]
                           :else      [magic-inventory        :normal          true]}
               :help      {:else      [pass-state             :normal          false]}
               :close     {\h         [close-left             :normal          true]
                           \j         [close-down             :normal          true]
                           \k         [close-up               :normal          true]
                           \l         [close-right            :normal          true]}
               :more-log  {:else      [scroll-log             identity         false]}
               :log       {:else      [pass-state             :normal          false]}
               :dead      {\y         [identity               :start-inventory false]
                           \n         [(constantly nil)       :normal          false]}
               :quit?     {\y         [(constantly nil)       :normal          false]
                           :else      [pass-state             :normal          false]}}
        expander-fn (fn [table] table)]
    (expander-fn table)))


(defn update-state
  "Use the stage transtion table defined above to call the appropriate
   transition function and assign the appropriate final state value.
   After this, apply some common per-tick transformations:
  
   * Apply hunger
  
   * Move NPCs
  
   * Add new NPCs
  
   * Update quests
  
   * Manage the save file, deleting it upon player death
  
   * Update place's discovered cells with new visibility calculations
  
   * Increment the current time"
  [state keyin]
  (let [current-state (get-in state [:world :current-state])
        table         (get state-transition-table current-state)]
    ;(debug "current-state" current-state)
    (if (or (contains? table keyin) (contains? table :else))
      (let [[transition-fn new-state advance-time] (if (contains? table keyin) (get table keyin) (get table :else))
            ;; if the table contains keyin, then pass through transition-fn assuming arity-1 [state]
            ;; else the transition-fn takes [state keyin]. Bind keying so that it becomes arity-1 [state]
            _ (debug "current-state" (get-in state [:world :current-state]))
            _ (debug "transition-fn" transition-fn)
            transition-fn             (if (contains? table keyin)
                                        transition-fn
                                        (fn [state]
                                          (transition-fn state keyin)))
            command-seq (get-in state [:world :command-seq] [])
            _ (debug "type of npcs" (type (get-in state [:world :npcs])))
            new-time  (inc (get-in state [:world :time]))
            state     (clear-ui-hint state)
            state     (transition-fn
                        (if advance-time
                          (assoc-in state [:world :time] new-time)
                          state))
            _ (debug "current-state" (get-in state [:world :current-state]))
            _ (debug "new-state" new-state)
            _ (debug "type of npcs" (type (get-in state [:world :npcs])))
            new-state (if (keyword? new-state)
                        new-state
                        (new-state (get-in state [:world :current-state])))
            _ (assert (not (nil? new-state)))
            state     (if-not (= keyin \r)
                        (cond
                          (= current-state :normal)
                            (assoc-in state [:world  :command-seq] [keyin])
                          (contains? (hash-set current-state new-state) :more-log)
                            state
                          :else
                            (conj-in state [:world :command-seq] keyin))
                        (assoc-in state [:world :command-seq] command-seq))
            _ (info "command-seq" (get-in state [:world :command-seq] :none))
            _ (trace "player" (get-in state [:world :player]))
            _ (info "new-state" new-state)
            wtl       (get-in state [:world :player :will-to-live])]
        (some-> state
            (assoc-current-state new-state)
            (tx/when-> advance-time
              ;; do updates that don't deal with keyin
              ;; Get hungrier
              (get-hungrier-and-thirstier)
              ;; if poisoned, damage player, else heal
              (if-poisoned-get-hurt)
              (heal)
              (if-wounded-get-infected)
              (if-infected-get-hurt)
              (decrease-will-to-live)
              (update-npcs)
              (update-cells)
              ;; TODO: Add appropriate level
              (add-npcs-random 1)
              ;; update visibility
              (update-visibility)
              ;; add will-to-live flavor message
              (log-will-to-live-flavor wtl))
            (update-quests)
            (arg-when-> [state] (contains? (-> state :world :player :status) :dead)
              ((fn [state]
                (.delete (clojure.java.io/file "save/world.edn"))
                state))
              (update-in [:world :player :status]
               (fn [status]
                 (disj status :dead)))
              (assoc-current-state :dead))
              ;; delete the save game on player death
            ;; only try to start log scrolling if the state we were just in was not more-log
            (tx/when-> (not= current-state :more-log)
              (init-log-scrolling))))
      state)))

