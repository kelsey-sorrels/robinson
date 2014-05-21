;; Functions that manipulate state to do what the user commands.
(ns dungeon-crusade.update
  (:use     
    dungeon-crusade.common
    [dungeon-crusade.dialog :exclude [-main]]
    dungeon-crusade.npc
    dungeon-crusade.worldgen
    dungeon-crusade.lineofsight)
  (:require clojure.pprint
            clojure.contrib.core
            clj-tiny-astar.path
            [clojure.stacktrace :as st]
            [taoensso.timbre :as timbre]))

(timbre/refer-timbre)

(defn collide?
  "Return `true` if the cell at `[x y]` is non-traverable. Ie: a wall, closed door or simply does
   not exist."
  [x y state]
  (let [cellxy (get-xy x y (current-place state))]
    (debug "collide? " cellxy)
    (let [cell (first cellxy)]
      ;; check the cell to see if it is a wall or closed door
      (or
        (nil? cell)
        (some (fn [collision-type] (= (cell :type) collision-type)) [:vertical-wall
                                                                     :horizontal-wall
                                                                     :close-door])
        ;; not a wall or closed door, check for npcs
        (npc-at-xy state x y)))))

(defn do-combat
  "Perform combat between the player and the npc at `[x y]` and return
   the state reflecting this."
  [x y state]
  (let [npc (npc-at-xy state x y)
        player (-> state :world :player)
        current-place-id (-> state :world :current-place)
        npc-idx (.indexOf (-> state :world :npcs) npc)
        player-hp (player :hp)
        npc-hp    (npc :hp)
        npc-dmg 1
        player-dmg 1]
    (debug "fighting npc" npc "with idx" npc-idx)
    (cond
      ;; npc and player still alive?
      (and (pos? (- player-hp player-dmg))
           (pos? (- npc-hp npc-dmg)))
        (-> state
          ;; modify player hp
          (update-in [:world :player :hp]
            (fn [hp] (- hp player-dmg)))
          ;; modify npc hp
          (update-in [:world :npcs npc-idx :hp]
          (fn [hp] (- hp npc-dmg))))
      ;; npc dead?
      (not (pos? (- npc-hp npc-dmg)))
        (-> state
          ;; modify player hp
          (update-in [:world :player :hp]
            (fn [hp] (- hp player-dmg)))
          ;; remove npc
          (update-in [:world :npcs]
            (fn [npcs]
              (vec (remove #(= npc %) npcs))))
          ;; maybe add corpse
          (update-in [:world :places current-place-id y x :items]
                     (fn [items]
                       (if (zero? (rand-int 3))
                         (conj items {:type :food :name (format "%s corpse" (name (npc :type))) :hunger 10})
                         items))))
      ;; player dead?
      (not (pos? (- player-hp player-dmg)))
        ;; add dead status
        (update-in state [:world :player :status]
          (fn [status] (conj status :dead))))))

(defn move
  "Move the player one space provided her/she is able. Else do combat. Else positions
   with party member."
  [state direction]
  {:pre [(contains? #{:left :right :up :down} direction)]}
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
    (cond
      (not (collide? target-x target-y state))
        (-> state
            (assoc-in [:world :player :pos :x] target-x)
            (assoc-in [:world :player :pos :y] target-y))
      (= (get (npc-at-xy state target-x target-y) :in-party?) true)
        (-> state
            (assoc-in [:world :player :pos :x] target-x)
            (assoc-in [:world :player :pos :y] target-y)
            (map-in [:world :npcs]
                    (fn [npc] (if (and (= (-> npc :pos :x) target-x)
                                       (= (-> npc :pos :y) target-y))
                                (-> npc
                                    (assoc-in [:pos :x] player-x)
                                    (assoc-in [:pos :y] player-y))
                                npc))))
      (npc-at-xy state target-x target-y)
        ;; collided with npc. Engage in combat.
        (do-combat target-x target-y state)
      ;; collided with a wall or door, nothing to be done.
      :else
        state)))
  

(defn move-left
  "Moves the player one space to the left provided he/she is able."
  [state]
  (move state :left))

(defn move-right
  "Moves the player one space to the right provided he/she is able."
  [state]
  (move state :right))

(defn move-up
  "Moves the player one space up provided he/she is able."
  [state]
  (move state :up))

(defn move-down
  "Moves the player one space down provided he/she is able."
  [state]
  (move state :down))

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
    (let [target-cellxy (get-xy target-x target-y (current-place state))
          target-cell   (first target-cellxy)]
      (debug "target-cellxy" target-cellxy)
      (debug "target-cell" target-cell)
      (if (and (not (nil? target-cell)) (= (target-cell :type) :close-door))
        (let [place (-> state :world :current-place)]
          (debug "opening door")
          (debug (get-in state [:world :places place target-y target-x]))
          (assoc-in state [:world :places place target-y target-x :type] :open-door))
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
    (let [target-cellxy (get-xy target-x target-y (current-place state))
          target-cell   (first target-cellxy)]
      (debug "target-cellxy" target-cellxy)
      (debug "target-cell" target-cell)
      (if (and (not (nil? target-cell)) (= (target-cell :type) :open-door))
        (let [place (-> state :world :current-place)]
          (debug "opening door")
          (assoc-in state [:world :places place target-y target-x :type] :close-door))
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
              ;; dup the item into cell
              (update-in [:world :places place y x :items]
                (fn [prev-items]
                  (conj prev-items (assoc item :hotkey keyin))))
              ;; remove the item from cell
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


(defn do-rest
  "NOP action. Player's hp increases a little."
  [state]
  (update-in state [:world :player]
             (fn [player] (if (< (int (player :hp)) (player :max-hp))
                            (assoc-in player [:hp] (+ (player :hp) 0.1))
                            player))))

(defn toggle-hotkey
  "Toggle mark `keyin` as a selected hotkey, or not if it already is."
  [state keyin]
  (debug "toggle-hotkey" keyin)
  (update-in state [:world :selected-hotkeys]
             (fn [hotkeys] (if (contains? hotkeys keyin)
                             (disj hotkeys keyin)
                             (conj hotkeys keyin)))))

(defn reinit-world
  "Re-initialize the value of `:world` within `state`. Used when the player
   dies and a new game is started."
  [state]
  (assoc state :world (init-world)))

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
            state             (if (contains? (-> state :world :places) dest-place-id)
                                state
                                (assoc-in state [:world :places dest-place-id] (init-random-n (read-string (name dest-place-id)))))
 
            dest-place        (-> state :world :places dest-place-id)

            ;; find the location in the destination place that points to the original place
            dest-cellxy       (first (filter #(and (not (nil? (first %)))
                                                 (contains? #{:up-stairs :down-stairs} ((first %) :type))
                                                 (= ((first %) :dest-place) orig-place-id))
                                    (with-xy dest-place)))
            _ (debug "dest-cellxy" dest-cellxy)
            dest-x             (second dest-cellxy)
            dest-y             (last dest-cellxy)
            party-pos          (adjacent-navigable-pos dest-place {:x dest-x :y dest-y})]
        (debug "dest-x" dest-x "dest-y" dest-y)
        (debug "party-pos" party-pos)
        (-> state
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
                      (assoc-in [:world :current-state] :talking))
            fsm (get-in state [:dialog (target-npc :id)])
            ;; get the dialog input options for the npc
            valid-input (get-valid-input fsm)]
        ;; if the first option is `nil` then advance the fsm one step.
        ;; this auto step can be used to have the npc speak first when approached.
        (if (nil? (first valid-input))
          (step-fsm state fsm nil)
          state))
      (assoc-in state [:world :current-state] :normal))))

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
        fsm (get-in state [:dialog (npc :id)])
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
      (assoc-in [:world :current-state] :normal)))

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
        (assoc-in [:world :current-state] :shopping)))
    
(defn shop
  "Start shopping. Allows the player to select \\a -buy or \\b - sell."
  [state keyin]
  (case keyin
    \a (assoc-in state [:world :current-state] :buy)
    \b (assoc-in state [:world :current-state] :sell)
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
          (transfer-items-from-npc-to-player (npc :id) (partial = item)))
      state)))


(defn sell
  "Sell an item to an npc in exchange for money."
  [state keyin]
  (let [npc       (first (talking-npcs state))
        buy-fn    (get-in state (npc :buy-fn-path) (fn [_] nil))
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
            (transfer-items-from-player-to-npc (npc :id) (partial = item))))
        state)))

(defn move-to-target
  "Move `npc` one space closer to the target position if there is a path
   from the npc to the target. Returns the moved npc and not the updated state.
   `target` is a map with the keys `:x` and `:y`."
  [state npc target]
  (let [npcs (npcs-at-current-place state)
        _ (debug "move-to-target npc" npc "target" target)
        npc-pos [(-> npc :pos :x) (-> npc :pos :y)]
        player  (-> state :world :player)
        player-pos [(-> player :pos :x) (-> player :pos :y)]
        place (current-place state)
        width (count (first place))
        height (count place)
        get-type (memoize (fn [x y] (do
                                      ;(debug "traversable?" x y "type" (get-in place [y x :type]))
                                      (get-in place [y x :type]))))
        traversable? (memoize
                       (fn [[x y]]
                         (and (< 0 x width)
                              (< 0 y height)
                              (or (and (= (-> npc :pos :x) x)
                                       (= (-> npc :pos :y) y))
                                  (not-any? (fn [n] (and (= (-> n :pos :x) x)
                                                         (= (-> n :pos :y) y)))
                                            npcs))
                              (contains? #{:floor
                                           :open-door
                                           :corridor}
                                         (get-type x y)))))
        path (try
               (debug "a* params" traversable? npc-pos [(target :x) (target :y)])
               (clj-tiny-astar.path/a* traversable? npc-pos [(target :x) (target :y)])
               (catch Exception e
                 (error "Caught exception during a* traversal" e)
                 (st/print-cause-trace e)
                 nil))
        _ (debug "path to target" path)
        new-pos (if (and (not (nil? path))
                         (> (count path) 1)
                         ;; don't collide with player
                         (let [new-pos (second path)]
                           (not= ((juxt first second) new-pos)
                                 ((juxt first second) player-pos))))
                  (second path)
                  npc-pos)
        _ (debug "new-pos" new-pos)
        new-npc (-> npc
                    (assoc-in [:pos :x] (first new-pos))
                    (assoc-in [:pos :y] (second new-pos)))
        _ (debug "new-npc" new-npc)]
    [new-pos new-npc npc]))

(defn calc-npc-next-step
  "Returns the moved npc and not the updated state. New npc pos will depend on
   the npc's `:movement-policy which is one of `:constant` `:entourage` `:follow-player`."
  [state npc]
  {:pre  [(contains? #{:constant :entourage :follow-player} (npc :movement-policy))]
   :post [(= (count %) 3)]}
  (let [policy (npc :movement-policy)
        pos    (-> state :world :player :pos)
        _ (info "moving npc@" (npc :pos) "with policy" policy)]
    (case policy
      :constant [nil nil npc]
      :entourage (move-to-target state npc (first (shuffle (adjacent-navigable-pos (current-place state) pos))))
      :follow-player (move-to-target state npc pos)
      [nil nil npc])))
 
(defn move-npcs
  "Move all npcs in the current place using `move-npc`."
  [state]
  (update-in state
             [:world :npcs]
             (fn [npcs]
               (let [map-result (map (fn [npc]
                                       (if (= (npc :place)
                                              (-> state :world :current-place))
                                         (calc-npc-next-step state npc)
                                         [nil nil npc]))
                                      npcs)]
               (reduce
                 (fn [result [new-pos new-npc npc]]
                   (conj result
                         (if (or (nil? new-pos)
                                 (not (nil? (npc-at-xy state (first new-pos) (second new-pos)))))
                           npc
                           new-npc)))
                 []
                 map-result)))))

(defn add-npcs
  "Randomly add rats to the current place's in floor cells."
  [state]
  (if (and (< (rand-int 10) 4)
           (< (count (filter (fn [npc] (= (-> state :world :current-place)
                                          (npc :place)))
                             (-> state :world :npcs)))
              20))
    (let [[_ x y] (first (shuffle (filter (fn [[cell _ _]] (and (not (nil? cell))
                                                                (= (cell :type) :floor)))
                                          (with-xy (current-place state)))))]
      (add-npc state (-> state :world :current-place)
                     {:race :rat
                      :movement-policy :follow-player
                      :hp 9
                      :attacks #{:bite :claw}} x y))
    state))

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
                  (if ((stage :pred) state)
                    (-> state
                        ((stage :update))
                        ((fn [state] (assoc-in state
                                               [:world :quests (quest :id) :stage]
                                               ((stage :nextstagefn) stage)))))
                    state)))))
            state (-> state :quests vals)))

;; A finite state machine definition for the game state. 
;; For each starting state, define a transition symbol, a function
;; to call `(transitionfn state)` to use as the new state, and a
;; final state. It's an unfortunate naming collision between the
;; transtion table's states and the application state variable, but
;; they are indeed two different things.
(def state-transition-table
  ;;         starting      transition transition             new
  ;;         state         symbol     fn                     state
  (let [table {:normal    {\i        [identity               :inventory]
                           \d        [identity               :drop]
                           \,        [identity               :pickup]
                           \e        [identity               :eat]
                           \o        [identity               :open]
                           \c        [identity               :close]
                           \.        [do-rest                :normal]
                           \h        [move-left              :normal]
                           \j        [move-down              :normal]
                           \k        [move-up                :normal]
                           \l        [move-right             :normal]
                           \>        [use-stairs             :normal]
                           \<        [use-stairs             :normal]
                           \;        [init-cursor            :describe]
                           \Q        [identity               :quests]
                           \T        [identity               :talk]
                           \?        [identity               :help]
                           :escape   [identity               :quit?]}
               :inventory {:escape   [identity               :normal]}
               :describe  {:escape   [free-cursor            :normal]
                           \i        [free-cursor            :describe-inventory]
                           \h        [move-cursor-left       :describe]
                           \j        [move-cursor-down       :describe]
                           \k        [move-cursor-up         :describe]
                           \l        [move-cursor-right      :describe]
                           :enter    [describe-at-cursor     :normal]}
               :quests    {:escape   [identity               :normal]}
               :describe-inventory
                          {:escape   [identity               :normal]
                           :else     [describe-inventory     :normal]}
               :drop      {:escape   [identity               :normal]
                           :else     [drop-item              :normal]}
               :pickup    {:escape   [identity               :normal]
                           :else     [toggle-hotkey          :pickup]
                           :enter    [pick-up                :normal]}
               :eat       {:escape   [identity               :normal]
                           :else     [eat                    :normal]}
               :open      {\h        [open-left              :normal]
                           \j        [open-down              :normal]
                           \k        [open-up                :normal]
                           \l        [open-right             :normal]}
               :talk      {\h        [talk-left              identity]
                           \j        [talk-down              identity]
                           \k        [talk-up                identity]
                           \l        [talk-right             identity]}
               :talking   {:escape   [stop-talking           :normal]
                           :else     [talk                   identity]}
               :shopping  {\a        [identity               :buy]
                           \b        [identity               :sell]
                           :escape   [identity               :normal]}
               :buy       {:escape   [identity               :normal]
                           :else     [buy                    :buy]}
               :sell      {:escape   [identity               :normal]
                           :else     [sell                   :sell]}
               :help      {:else     [(fn [s _] s)           :normal]}
               :close     {\h        [close-left             :normal]
                           \j        [close-down             :normal]
                           \k        [close-up               :normal]
                           \l        [close-right            :normal]}
               :dead      {\y        [reinit-world           :normal]
                           \n        [(constantly nil)       :normal]}
               :quit?     {\y        [(constantly nil)       :normal]
                           :else     [(fn [s _] s)           :normal]}}
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
      (let [[transition-fn new-state] (if (contains? table keyin) (table keyin) (table :else))
            ;; if the table contains keyin, then pass through transition-fn assuming arity-1 [state]
            ;; else the transition-fn takes [state keyin]. Bind keying so that it becomes arity-1 [state]
            transition-fn             (if (contains? table keyin)
                                        transition-fn
                                        (fn [state]
                                          (transition-fn state keyin)))
            state                     (transition-fn state)
            _ (debug "new-state" new-state)
            new-state (if (keyword? new-state)
                        new-state
                        (new-state (get-in state [:world :current-state])))
            _ (debug "new-state" new-state)]
        (some-> state
            (assoc-in [:world :current-state] new-state)
            ;; do updates that don't deal with keyin
            ;; Get hungrier
            ((fn [state] (update-in state [:world :player :hunger] inc)))
            ((fn [state] (assoc-in state [:world :player :status]
                                  (set (remove nil?
                                         (apply conj #{} [(condp <= (-> state :world :player :hunger)
                                                           400 :dead
                                                           300 :dying
                                                           200 :starving
                                                           100 :hungry
                                                           nil)
                                                         ]))))))
            (move-npcs)
            (add-npcs)
            (update-quests)
            ((fn [state] (update-in state [:world :current-state]
                                    (fn [current-state]
                                      (if (contains? (-> state :world :player :status) :dead)
                                        ;; delete the save game on player death
                                        (do (.delete (clojure.java.io/file "world.save"))
                                            :dead)
                                        current-state)))))
            ((fn [state] (update-in state [:world :places (-> state :world :current-place)]
                                    (fn [place]
                                      (let [pos (-> state :world :player :pos)
                                            sight-distance 3
                                            get-cell (memoize (fn [x y] (get-in place [y x])))]
                                        (vec (pmap (fn [line y]
                                                     (if (< sight-distance
                                                            (Math/abs (- y (pos :y))))
                                                       line
                                                       (vec (map (fn [cell x]
                                                                   (if (and (not (nil? cell))
                                                                            (not (farther-than?
                                                                                   pos
                                                                                   {:x x :y y}
                                                                                   sight-distance))
                                                                            (visible?
                                                                              get-cell
                                                                              cell-blocking?
                                                                              (pos :x)
                                                                              (pos :y)
                                                                              x
                                                                              y))
                                                                     (assoc cell :discovered :true)
                                                                     cell))
                                                                 line (range)))))
                                                    place (range))))))))

            ((fn [state] (update-in state [:world :time] inc)))))
      state)))

