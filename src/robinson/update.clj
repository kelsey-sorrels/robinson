;; Functions that manipulate state to do what the user commands.
(ns robinson.update
  (:use     
    clojure.pprint
    [clojure.string :only [lower-case]]
    clojure.contrib.core
    robinson.common
    robinson.world
    robinson.viewport
    robinson.describe
    robinson.player
    [robinson.dialog :exclude [-main]]
    robinson.npc
    [robinson.combat :exclude [-main]]
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
            [clojure.core.async :as async]
            [clojure.java.io :as io]
            [clojure.data.generators :as dg]
            [pallet.thread-expr :as tx]
            [clojure.stacktrace :as st]
            [taoensso.timbre :as timbre]))

(timbre/refer-timbre)

(defn- pass-state
  [state & more]
  state)

(defn translate-directions
  [keyin]
  (case keyin
    (\h :numpad4) :left
    (\j :numpad2) :down
    (\k :numpad8) :up
    (\l :numpad6) :right
    (\y :numpad7) :up-left
    (\u :numpad9) :up-right
    (\b :numpad1) :down-left
    (\n :numpad3) :down-right
    keyin))

(defn is-direction?
  [keyin]
  (contains? #{:left :down :up :right} keyin))

(defn is-direction-ext?
  [keyin]
  (or (is-direction? keyin)
      (contains? #{:up-left :up-right :down-left :down-right} keyin)))

(defn sight-distance
  [state]
  (let [atmo   (get-in state [:data :atmo])
        frames (count atmo)
        t      (mod (get-in state [:world :time]) frames)
        frame  (nth atmo t)
        values (flatten frame)
        item   (inventory-id->item state :flashlight)
        on     (and item (= (get item :state) :on))
        _      (info "sight-distance. flashlight:" item "state:" on)
        values (map (fn [v] (if on (max v 100) v)) values)]
    (+ 1.5 (* 8 (/ (reduce + values) (* 255 (count values)))))))
      
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

(defn toggle-hotkey
  "Toggle mark `keyin` as a selected hotkey, or not if it already is."
  [state keyin]
  (debug "toggle-hotkey" keyin)
  (update-in state [:world :selected-hotkeys]
             (fn [hotkeys] (if (contains? hotkeys keyin)
                             (disj (set hotkeys) keyin)
                             (conj (set hotkeys) keyin)))))

(defn reinit-world
  "Re-initialize the value of `:world` within `state`. Used when the player
   dies and a new game is started."
  [state]
  (-> state
    (assoc :world (init-world (System/currentTimeMillis)))
    (load-unload-places)
    (as-> state
      (reduce (fn [state _] (add-npcs state))
              state
              (range 5)))))

(defn add-starting-inventory
  [state]
  (let [new-state          (reinit-world state)
        selected-hotkeys   (get-in state [:world :selected-hotkeys])
        _                  (info "selected-hotkeys" selected-hotkeys)
        start-inventory    (filter #(contains? (set selected-hotkeys) (get % :hotkey)) (sg/start-inventory))
        _                  (info "Adding to starting inventory:" start-inventory)
        state              (add-to-inventory new-state start-inventory)]
    state))

(defn select-starting-inventory
  [state keyin]
  (if (and (char? keyin)
           (<= (int \a) (int keyin) (int \k)))
    (let [new-state (toggle-hotkey state keyin)
          selected-hotkeys (get-in new-state [:world :selected-hotkeys])]
      (info "selected-hotkeys" selected-hotkeys)
      (if (< (count selected-hotkeys) 4)
        new-state
        state))
    state))

(defn destroy-cell
  "Destroy the cell at xy, changing its type. This is when the player bumps into things like palisades."
  [state x y]
  (assoc-cell state x y :type :dirt))

(defn pick-up-gold
  "Vacuums up gold from the floor into player's inventory."
  [state]
  (let [;;place-id (current-place-id state)
        ;player-x (-> state :world :player :pos :x)
        ;player-y (-> state :world :player :pos :y)
        [{items :items} x y]  (player-cellxy state)
        ;items              (player-cell :items)
        {cash  true
         non-cash-items false}  (group-by (fn [item] (= (item :type) :$))
                                          items)
        _ (debug "picking up gold. Divided items" cash non-cash-items)
        $                  (reduce + (map :amount (or cash [])))]
    (if (> $ 0)
      (-> state
        (append-log (format "You pick up %d cash." $))
        (assoc-cell-items state x y
          (or non-cash-items []))
        (update-in [:world :player :$]
          (fn [player-$] (+ $ player-$))))
      state)))

(defn move-outside-safe-zone
  "Move the player when they are near the edge of the map.
   If a pre-generated place does not exist, create a new one using the world seed."
  [state direction]
  (info "move-outside-safe-zone")
  (let [[player-cell x y] (player-cellxy state)
        on-raft           (player-mounted-on-raft? state)
        ;;[ox oy]           (map read-string (clojure.string/split (name orig-place-id) #"_"))
        target-x          (case direction
                            :left (dec x)
                            :right (inc x)
                            :up-left (dec x)
                            :down-left (dec x)
                            :up-right (inc x)
                            :down-right (inc x)
                            x)
        target-y          (case direction
                            :up (dec y)
                            :down (inc y)
                            :up-left (dec y)
                            :down-left (inc y)
                            :up-right (dec y)
                            :down-right (inc y)
                            y)
        target-place      (xy->place-id state target-x target-y)
        visible-place-ids (visible-place-ids state target-x target-y)
        _ (info "target-x" target-x "target-y" target-y)
        _ (info "target-place" target-place)
       vp-pos             (apply xy->pos
                            (+xy (pos->xy (get-in state [:world :viewport :pos]))
                                 (-xy [target-x target-y] [x y])))
       ;;dest-place        (-> state :world :places dest-place-id)
       {width :width
        height :height}  (get-in state [:world :viewport])]
      (debug "viewport-pos" vp-pos)
      #_(debug "npcs" (with-out-str (pprint (-> state :world :npcs))))
      (-> state
        (assoc-in [:world :viewport :pos] vp-pos)
        (load-unload-places))))
      ;(-> state
      ;  (tx/when-> on-raft 
      ;    (dec-cell-item-count :raft))
      ;  (assoc-in [:world :viewport :pos] vp-pos) 
        ;; unload the current place
        ;;(dissoc-in [:world :places orig-place-id])
        ;; change the place
        ;;(assoc-in [:world :current-place] dest-place-id)
        ;;(assoc-in [:world :places dest-place-id] dest-place)
        ;; move player
      ;  (assoc-in [:world :player :pos] {:x target-x :y target-y})
      ;  (tx/when-> on-raft
      ;    (conj-in-cell-items (ig/id->item :raft) target-x target-y)))))

(defn move
  "Move the player one space provided her/she is able. Else do combat. Else swap positions
   with party member. Else hack something down."
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
                               0))
        target-cell (get-cell state target-x target-y)]
    (info "moving to" (get target-cell :type))
    (info "inside-safe-zone?" (xy-in-safe-zone? state target-x target-y) target-x target-y)
    (info "not collide?" (not (collide? state target-x target-y {:include-npcs? false})))
    (info "not mounted-on-raft?" (not (player-mounted-on-raft? state)))
    (cond
      (and (not (collide? state target-x target-y {:include-npcs? false}))
           (not (player-mounted-on-raft? state)))
        (-> state
          (arg-when-> [state] (not (xy-in-safe-zone? state target-x target-y))
            (move-outside-safe-zone direction))
          (arg-if-> [state] (do (info "npc-at-xy?" (npc-at-xy state target-x target-y)) (npc-at-xy state target-x target-y))
            ;; collided with npc. Engage in combat.
            (attack [:world :player] (npc->keys state (npc-at-xy state target-x target-y)))
            (as-> state
              (assoc-in state [:world :player :pos :x] target-x)
              (assoc-in state [:world :player :pos :y] target-y)
              (let [cell  (get-cell state target-x target-y)
                    items (get cell :items)]
                (if (seq items)
                  (search state)
                  state))
              (pick-up-gold state))))
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
      (and (not (collide-in-water? state target-x target-y))
           (player-mounted-on-raft? state))
        (-> state
          (dec-cell-item-count :raft)
          (conj-cell-items target-x target-y (ig/id->item :raft))
          (assoc-in [:world :player :pos :x] target-x)
          (assoc-in [:world :player :pos :y] target-y)
          ;; rafting = more hunger
          (update-in [:world :player :hunger] (partial + 0.05 ))
          ((fn [state]
             (let [cell  (get-cell state target-x target-y)
                   items (get cell :items)]
               (if (seq items)
                 (search state)
                 state)))))
      ;; Hack down destroyable cells.
      (type->destroyable? (get target-cell :type))
        (destroy-cell state target-x target-y)
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
    (let [target-cell (get-cell state target-x target-y)]
      (debug "target-cell" target-cell)
      (if (and (not (nil? target-cell)) (= (target-cell :type) :close-door))
        (do
          (debug "opening door")
          (debug (get-cell state target-y target-x))
          (-> state
            (append-log "The door creaks open")
            (assoc-cell state target-y target-x :type :open-door)))
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
    (let [target-cell (get-cell state target-x target-y)]
      (debug "target-cell" target-cell)
      (if (and (not (nil? target-cell)) (= (target-cell :type) :open-door))
        (-> state
          (append-log "The door closes")
          (assoc-cell state target-y target-x :type :close-door))
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
    (let [[player-cell x y]  (player-cellxy state)
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
      (if (seq selected-items)
        (let [new-state (-> state
                          (append-log "You pick up:")
                          ;; dup the item into inventory with hotkey
                          (add-to-inventory selected-items)
                          ;; remove the item from cell
                          (assoc-current-cell-items not-selected-items)
                          ;;;; hotkey is no longer available
                          (assoc-in [:world :remaining-hotkeys]
                              remaining-hotkeys)
                          ;; reset selected-hotkeys
                          (assoc-in [:world :selected-hotkeys] #{}))]
          new-state)
        state)))

(defn drop-item
  "Drop the item from the player's inventory whose hotkey matches `keyin`.
   Put the item in the player's current cell."
  [state keyin]
  (let [[player-cell x y] (player-cellxy state)
        items (-> state :world :player :inventory)
        inventory-hotkeys (map #(% :hotkey) items)
        item-index (.indexOf inventory-hotkeys keyin)]
    (if (and (>= item-index 0) (< item-index (count items)))
      (let [item (nth items item-index)
            new-state (-> state
              (append-log (format "You let the %s fall to the ground" (lower-case (get item :name))))
              ;; dup the item into cell
              (conj-cell-items x y (dissoc item :wielded))
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
        new-state)
        state)))

(defn apply-bandage
  [state]
  (-> state
    (dec-item-count :bandage)
    (assoc-in [:world :player :hp] (get-in state [:world :player :max-hp]))
    (assoc-in [:world :current-state] :normal)
    (append-log "You apply the bandage.")))

(defn apply-flashlight
  [state]
  (let [item         (inventory-id->item state :flashlight)
        item-state   (get item :state :off)]
    (case item-state
      :on
      (-> state
        (update-inventory-item :flashlight (fn [item] (assoc item :state :off)))
        (append-log "You turn the flashlight off.")
        (assoc-in [:world :current-state] :normal))
      :off
      (if (pos? (get item :charge))
        (-> state
          (update-inventory-item :flashlight (fn [item] (assoc item :state :on)))
          (append-log "You turn the flashlight on.")
          (assoc-in [:world :current-state] :normal))
        (append-log state "You try turning the flashlight on, but nothing happens.")))))

(defn assoc-apply-item
  [state item]
  (assoc-in state [:world :apply-item] item))

(defn get-apply-item
  [state]
  (get-in state [:world :apply-item]))

(defn select-apply-item
  "Apply the item from the player's inventory whose hotkey matches `keyin`."
  [state keyin]
  (let [item (inventory-hotkey->item state keyin)]
    (if item
      (let [id (get item :id)]
        (-> state
          (assoc-apply-item item)
          ((fn [state]
            (cond
              (= id :bandage)
                (apply-bandage state)
              (= id :flashlight)
                (apply-flashlight state)
              (= id :fishing-pole)
                (-> state
                  (assoc-current-state :apply-item-normal)
                  (ui-hint "Pick a direction to use the fishing pole."))
              (contains? #{:red-frog-corpse :orange-frog-corpse :yellow-frog-corpse
                           :green-frog-corpse :blue-frog-corpse :purple-frog-corpse} id)
                (-> state
                  (assoc-current-state :apply-item-inventory)
                  (ui-hint "Pick an item to apply the frog secretion to."))
              (= id :match)
                (-> state
                  (assoc-current-state :apply-item-normal)
                  (ui-hint "Pick a direction to use the match."))
              (= id :plant-guide)
                (-> state
                  (assoc-current-state :apply-item-inventory)
                  (ui-hint "Pick an item to identify."))
              (= id :stick)
                (-> state
                  (assoc-current-state :apply-item-normal)
                  (ui-hint "Pick a direction to use the stick."))
              (= id :saw)
                (-> state
                  (assoc-current-state :apply-item-normal)
                  (ui-hint "Pick a direction to use the saw."))
              (= id :obsidian-axe)
                (-> state
                  (assoc-current-state :apply-item-normal)
                  (ui-hint "Pick a direction to use the axe."))
              (ig/is-sharp? item)
                (-> state
                  (assoc-current-state :apply-item-inventory)
                  (ui-hint "Pick an item to use the sharp item on."))
              (ig/is-fruit? item)
                (-> state
                  (assoc-current-state :apply-item-body)
                  (ui-hint "a-apply to skin, b-apply to tongue"))
              :else state)))))
        state)))

(defn dig-hole
  "Dig in the ground, creating a hole."
  [state]
  (let [[x y] (player-xy state)]
    (assoc-cell state x y :type 
      (dg/rand-nth [:freshwater-hole :saltwater-hole :dry-hole]))))

(defn apply-fishing-pole
  "Start fishing for something."
  [state direction]
  (let [player-x      (-> state :world :player :pos :x)
        player-y      (-> state :world :player :pos :y)
        target-x      (+ player-x (case direction
                                    :left -1
                                    :right 1
                                    0))
        target-y      (+ player-y (case direction
                                    :up  -1
                                    :down 1
                                    0))
        target-cell   (get-cell state target-x target-y)
        new-state     (case direction
                        :left  :fishing-left
                        :right :fishing-right
                        :up    :fishing-up
                        :down  :fishing-down)]
    (if (type->water? (get target-cell :type))
      (-> state
        (append-log "You start fishing.")
        (assoc-current-state new-state))
      (append-log state "You can't fish here."))))

(defn do-fishing
  "Fish somewhere."
  [state]
  (let [p (uniform-int 0 50)]
    ;; chance of catching a fish
    (cond
      (= p 0)
      ;; catch a fish
      (add-to-inventory state [(ig/gen-corpse (mg/gen-monster 1 :water))])
      :else
      state)))

(defn apply-match
  "Light something on fire, creating chaos."
  [state direction]
  (let [player-x      (-> state :world :player :pos :x)
        player-y      (-> state :world :player :pos :y)
        target-x      (+ player-x (case direction
                                    :left -1
                                    :right 1
                                    0))
        target-y      (+ player-y (case direction
                                    :up  -1
                                    :down 1
                                    0))
        target-cell   (get-cell state target-x target-y)]
    (cond
      (type->flammable? (get target-cell :type))
      (-> state
        (append-log (format "You strike the match and light the %s." (clojure.string/replace (name (get target-cell :type))
                                                                                            #"-"
                                                                                            " ")))
        (dec-item-count :match)
        (assoc-cell target-x target-y :type :fire :fuel (if (= (get target-cell :type)
                                                               :campfire)
                                                          (uniform-int 500 600)
                                                          (uniform-int 100 300))))
      :else
      state)))

(defn saw
  "Saw nearby tree creating logs."
  [state direction keyin]
  (let [player-x      (-> state :world :player :pos :x)
        player-y      (-> state :world :player :pos :y)
        target-x      (+ player-x (case direction
                                    :left -1
                                    :right 1
                                    0))
        target-y      (+ player-y (case direction
                                    :up  -1
                                    :down 1
                                    0))
        target-cell   (get-cell state target-x target-y)]
    (info "saw dir" direction)
    (info "sawing at" target-x target-y)
    (if (contains? #{:tree :palm-tree :fruit-tree} (get target-cell :type))
      (-> state
        (append-log "You saw the tree into logs.")
        ;; sawing = more hunger
        (update-in [:world :player :hunger] (partial + 10))
        ;; decrease item utility
        (dec-item-utility :saw)
        (update-cell target-x
                     target-y
                     (fn [cell] (-> cell
                                  (dissoc :harvestable)
                                  (assoc :type (dg/rand-nth [:dirt :gravel :tall-grass :short-grass]))
                                  (assoc :items (concat (get cell :items) 
                                                      (repeat (uniform-int 1 2) (ig/gen-log))))))))
      state)))

(defn apply-plant-guide
  "Apply a plant-guide to the inventory item."
  [state item]
  (info "applying plant guide to" item)
  (info "identified"  (get-in state [:world :fruit :identified]))
  (if (ig/is-fruit? item)
    (-> state
      (conj-in [:world :fruit :identified] (get item :id))
      (append-log (format "Identified %s." (name (get item :id)))))
    (append-log state (format "You're not able to identify the %s." (name (get item :id))))))


(defn apply-sharp-item
  "Apply a sharp item to the inventory item."
  [state item]
  (info "applying sharp item to" item)
  (case (get item :id)
    :unhusked-coconut
    (-> state
      (dec-item-count (get item :id))
      (add-to-inventory [(ig/gen-coconut)]))
    :stick
    (-> state
      (dec-item-count (get item :id))
      (add-to-inventory [(ig/gen-sharpened-stick)]))
    state))


(defn apply-fruit-to-skin
  "Apply fruit to the skin. If it is poisonous, display a message in the future."
  [state item]
  (info "skin-identifiable" (get-in state [:world :fruit]))
  (-> state
    (append-log (format "You touch the %s to your skin." (get item :name)))
    (arg-when-> [state] (ig/skin-identifiable? state item)
      (assoc-in [:world :player :skin-identify-activate-time]
                (apply min (remove nil?
                                   [(get-in state [:world :player :skin-identify-activate-time])
                                    (+ (get-time state) (uniform-int 5 10))]))))))

(defn apply-fruit-to-tongue
  "Apply fruit to the tongue If it is poisonous, display a message in the future."
  [state item]
  (info "skin-identifiable" (get-in state [:world :fruit]))
  (-> state
    (append-log (format "You touch the %s to your tongue." (get item :name)))
    (arg-when-> [state] (ig/tongue-identifiable? state item)
      (assoc-in [:world :player :tongue-identify-activate-time]
                (apply min (remove nil?
                                   [(get-in state [:world :player :tongue-identify-activate-time])
                                    (+ (get-time state) (uniform-int 5 10))]))))))

(defn apply-frog-corpse
  "Apply the secretions of a frog corpse to an inventory item."
  [state frog-corpse target-item]
  (info "poisonous frogs" (get-in state [:world :frogs]))
  (-> state
    (append-log (format "You touch the %s to the %s." (get frog-corpse :name) (get target-item :name)))
    (arg-if-> [state] (= :arrow (get target-item :id))
      (->
        (dec-item-count :arrow)
        (dec-item-count (get frog-corpse :id))
        (add-to-inventory (ig/id->item (-> (get target-item :name)
                                           (clojure.string/split #"-")
                                           first
                                           (str "-tipped-arrow")
                                           keyword))))
      (append-log "You rub it all over but nothing happens."))))

(defn apply-item
  "Applies the selected item."
  [state keyin]
  (let [item (get-apply-item state)
        trans->dir? (comp is-direction? translate-directions)]
    (info "apply-item" [item keyin])
    (info "is-direction?" ((comp is-direction? translate-directions) keyin))
    (first-vec-match [(get item :id) keyin]
      [:fishing-pole   trans->dir?] (apply-fishing-pole state (translate-directions keyin))
      [:match          trans->dir?] (-> state
                                      (apply-match (translate-directions keyin))
                                      (assoc-current-state :normal))
      [:plant-guide    :*         ] (if-let [item (inventory-hotkey->item state keyin)]
                                      (-> state
                                        (apply-plant-guide item)
                                        (assoc-current-state  :normal))
                                      state)
      [:stick          \>         ] (-> state
                                      (dig-hole)
                                      (assoc-current-state :normal))
      [:obsidian-axe   trans->dir?] (-> state
                                      (saw (translate-directions keyin) keyin)
                                      (assoc-current-state :normal))
      [:saw            trans->dir?] (-> state
                                      (saw (translate-directions keyin) keyin)
                                      (assoc-current-state :normal))
      [ig/id-is-sharp? :*         ] (if-let [item (inventory-hotkey->item state keyin)]
                                      (-> state
                                        (apply-sharp-item item)
                                        (assoc-current-state :normal))
                                      state)
      ;; apply fruit to body
      [ig/id-is-fruit? \a         ] (-> state
                                      (apply-fruit-to-skin item)
                                      (assoc-current-state :normal))
      [ig/id-is-fruit? \b         ] (-> state
                                      (apply-fruit-to-tongue item)
                                      (assoc-current-state :normal))
      [#{:red-frog-corpse
         :orange-frog-corpse
         :yellow-frog-corpse
         :green-frog-corpse
         :blue-frog-corpse
         :purple-frog-corpse}
                      :*          ] (if-let [target-item (inventory-hotkey->item state keyin)]
                                      (-> state
                                        (apply-frog-corpse (get item :id) target-item)
                                        (assoc-current-state  :normal))
                                      state)
      [:*              :*         ] (ui-hint state "You're not sure how to apply it to that."))))

(defn quaff-only-adjacent-cell
  "Drink all adjacent cells."
  [state]
  (let [[state water] 
        (reduce (fn [[state water] [x y]]
                  (info "testing cell for water:" (get-cell state x y))
                  (if (contains? (get-cell state x y) :water)
                    [(update-cell state x y (fn [cell] (assoc cell :water 0))) (+ water (get (get-cell state x y) :water 0))]
                    [state water]))
                [state 0]
                (player-adjacent-xys state))]
    (info "player-adj-xys" (player-adjacent-xys state))
    (update-in state [:world :player :thirst] (fn [thirst] (min 0 (- thirst water))))))

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
  (let [water (get (get-cell state x y) :water)]
    (-> state
      (assoc-cell state x y :water 0)
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
      ;; reduce thirst
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
  (-> state
    ;; rest better then in shelter
    (arg-when-> [state] (let [[cell _ _] (player-cellxy state)]
                          (type->shelter? (get cell :type)))
      (update-in [:world :player :will-to-live]
        (fn [will-to-live] (+ 0.05 will-to-live))))
    ;; rest better when near a fire
    (arg-when-> [state] (some #{:fire :campfire} (map :type (player-adjacent-cells state)))
      (update-in state [:world :player :will-to-live]
        (fn [will-to-live] (+ 0.05 will-to-live))))
    (update-in [:world :player]
               (fn [player] (if (< (int (player :hp)) (player :max-hp))
                              (assoc-in player [:hp] (+ (player :hp) 0.05))
                              player)))))

(defn do-sleep
  "Sleep."
  [state keyin]
  (let [d (sight-distance state)]
    (if (> d 2)
      (assoc-current-state state :normal)
      (do-rest state))))

(defn eat
  "Remove the item whose `:hotkey` equals `keyin` and subtract from the player's
   hunger the item's `:hunger` value."
  [state keyin]
  (info "poisoned fruit" (get-in state [:world :fruit :poisonous]))
  (if-let [item (inventory-and-player-cell-hotkey->item state keyin)]
    (-> state
      (append-log (format "The %s tastes %s." (lower-case (get item :name))
                                              (dg/rand-nth ["great" "foul" "greasy" "delicious" "burnt" "sweet" "salty"])))
      (update-eaten item)
      ;; reduce hunger
      (update-in [:world :player :hunger]
        (fn [hunger]
          (let [new-hunger (- hunger (item :hunger))]
            (max 0 new-hunger))))
      (arg-if-> [state] (contains? (set (map :hotkey (player-inventory state))) keyin)
        ;; remove the item from inventory
        (dec-item-count (get item :id))
        ;; remove the item from the current-cell
        (dec-cell-item-count (get item :id)))
      (arg-when-> [state] (= (get item :id) :coconut-empty)
        (add-to-inventory [(ig/gen-coconut-shell)]))
      ;; if the item was a poisonous fruit, set a poisoned timebomb
      ((fn [state]
        (if (and (ig/is-fruit? item)
                 (contains? (set (get-in state [:world :fruit :poisonous])) (get item :id)))
          (do
            (info "Ate poisoned fruit." item)
            (assoc-in state
                      [:world :player :poisoned-time]
                      (apply min (remove nil?
                                         [(get-in state [:world :player :poisoned-time])
                                          (+ (get-time state) (uniform-int 100 200))]))))
          state))))
    state))

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
                                (if (.exists (io/as-file (format "save/%s.place.edn" (str dest-place-id))))
                                  (clojure.edn/read-string {:readers {'Monster mg/map->Monster}}
                                    (slurp (format "save/%s.place.edn" (str dest-place-id))))
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
    (-> state
      (assoc-in [:world :cursor] player-pos)
      (ui-hint "ijkl to move cursor, enter to select."))))

(defn harvest
  "Collect non-item resources from adjacent or current cell"
  [state direction]
  {:pre  [(contains? #{:left :right :up :down :center} direction)]}
  (let [player-x      (-> state :world :player :pos :x)
        player-y      (-> state :world :player :pos :y)
        target-x      (+ player-x (case direction
                                    :left -1
                                    :right 1
                                    0))
        target-y      (+ player-y (case direction
                                    :up  -1
                                    :down 1
                                    0))
        target-cell   (get-cell state target-x target-y)
        harvestable   (get target-cell :harvestable false)
        harvest-items (if (not= target-cell nil)
                        (cond
                          (= (get target-cell :type) :tree)
                            (if (or harvestable
                                    (= 0 (uniform-int 1000)))
                              [(dg/rand-nth [(ig/gen-stick) (ig/gen-plant-fiber)])]
                              [])
                          (= (get target-cell :type) :bamboo)
                              (if (or harvestable
                                      (= 0 (uniform-int 1000)))
                                [(ig/gen-bamboo)]
                                [])
                          (= (get target-cell :type) :palm-tree)
                            (concat
                              (if (or harvestable
                                      (= 0 (uniform-int 1000)))
                                [(dg/rand-nth [(ig/gen-unhusked-coconut) (ig/gen-plant-fiber)])]
                                []))
                          (and (= (get target-cell :type) :tall-grass)
                               (= direction :center))
                            (concat
                              (if (or harvestable
                                      (= 0 (uniform-int 1000)))
                                [(dg/rand-nth [(ig/gen-grass) (ig/gen-plant-fiber)])]
                                []))
                          (and (= (get target-cell :type) :gravel)
                               (= direction :center))
                            (concat
                              (if (or harvestable
                                      (= 0 (uniform-int 1000)))
                                [(dg/rand-nth [(ig/gen-rock) (ig/gen-obsidian)])]
                                []))
                          :else [])
                        [])]
    (info "harvested" harvest-items)
    (if (empty? harvest-items)
      (append-log state "You don't find anything.")
      (-> state
        (add-to-inventory harvest-items)
        (update-cell target-x target-y (fn [cell] (dissoc cell :harvestable)))
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
    (-> state
      (assoc-in [:world :cursor] cursor-pos)
      (ui-hint "ijkl to move cursor, enter to select."))))

(defn move-cursor-right
  "Move the cursor pos one space to the right keeping in mind the bounds of the current place."
  [state]
  (let [cursor-pos (get-in state [:world :cursor])
        cursor-pos (assoc cursor-pos :x (min (get-in state [:world :viewport :width]) (inc (cursor-pos :x))))]
    (-> state
      (assoc-in [:world :cursor] cursor-pos)
      (ui-hint "ijkl to move cursor, enter to select."))))

(defn move-cursor-up
  "Move the cursor pos one space up keeping in mind the bounds of the current place."
  [state]
  (let [cursor-pos (get-in state [:world :cursor])
        cursor-pos (assoc cursor-pos :y (max 0 (dec (cursor-pos :y))))]
    (-> state
      (assoc-in [:world :cursor] cursor-pos)
      (ui-hint "ijkl to move cursor, enter to select."))))

(defn move-cursor-down
  "Move the cursor pos one space down keeping in mind the bounds of the current place."
  [state]
  (let [cursor-pos (get-in state [:world :cursor])
        cursor-pos (assoc cursor-pos :y (min (get-in state [:world :viewport :height]) (inc (cursor-pos :y))))]
    (-> state
      (assoc-in [:world :cursor] cursor-pos)
      (ui-hint "ijkl to move cursor, enter to select."))))

(defn describe-at-cursor
  "Add to the log, a message describing the scene at the cell indicated by the
   cursor's position."
  [state]
  (let [cursor-pos (get-in state [:world :cursor])]
    (-> state
      (append-log (describe-cell-at-xy state (cursor-pos :x) (cursor-pos :y)))
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
                           :craft-survival :survival
                           :craft-shelter :shelter
                           :craft-transportation :transportation)
        matching-recipes (filter (fn [recipe] (= (get recipe :hotkey) keyin))
                                 (get (get-recipes state) recipe-type))]
    (info "selecting matching recipe" matching-recipes)
    (if (empty? matching-recipes)
      (ui-hint state "Pick a valid recipe.")
      (let [recipe-path [recipe-type (get (first matching-recipes) :hotkey)]]
        (info "selecting recipe path" recipe-path)
        (assoc-in state [:world :craft-recipe-path] recipe-path)))))

(defn assoc-throw-item
  [state item]
  (assoc-in state [:world :throw-item] item))

(defn get-throw-item
  [state]
  (get-in state [:world :throw-item]))

(defn throw-select-inventory
  [state keyin]
  (if-let [item (inventory-and-player-cell-hotkey->item state keyin)]
    (-> state
      (ui-hint "Select direction.")
      (assoc-throw-item item))
    state))

(defn throw-selected-inventory
  [state direction]
  {:pre  [(contains? #{:left :right :up :down} direction)]}
  (let [item           (get-throw-item state)
        throw-distance 5
        obj            (first-collidable-object state direction throw-distance)]
    (cond
      (contains? obj :npc)
        ;; do attack
        (-> state
          (attack [:world :player] (npc->keys state (get obj :npc)) item)
          (conj-cell-items (get-in obj [:pos :x]) (get-in obj [:pos :y]) item)
          (dec-item-count (get item :id)))
      (contains? obj :cell)
        ;; drop item into cell before hitting colliding cell
        (-> state
          (arg-if-> [state] (= (get-in obj [:cell :type]) :fire)
            (update-cell (get-in obj [:pos :x]) (get-in obj [:pos :y]) (fn [cell] (update-in cell [:fuel] (partial + (ig/id->fuel (get item :id))))))
            (conj-cell-items (+ (get-in obj [:pos :x])
                                (case direction
                                  :left 1
                                  :right -1
                                  0))
                             (+ (get-in obj [:pos :y])
                                (case direction
                                  :up 1
                                  :down -1
                                  0))
                             (if (get item :rot-time)
                               (assoc item :rot-time (inc (get-time state)))
                               item)))
          (dec-item-count (get item :id)))
      :else
        ;; didn't hit anything, drop into cell at max-distance
        (let [[x y] (nth (direction->xys state direction) throw-distance)]
          (-> state
            (conj-cell-items x y item)
            (dec-item-count (get item :id)))))))

(defn throw-left
  [state]
  (throw-selected-inventory state :left))

(defn throw-right
  [state]
  (throw-selected-inventory state :right))

(defn throw-up
  [state]
  (throw-selected-inventory state :up))

(defn throw-down
  [state]
  (throw-selected-inventory state :down))

(defn craft-weapon
  [state]
  (craft-select-recipe (assoc-current-state state :craft-weapon) \a))

(defn craft-survival
  [state]
  (craft-select-recipe (assoc-current-state state :craft-survival) \a))

(defn craft-shelter
  [state]
  (craft-select-recipe (assoc-current-state state :craft-shelter) \a))

(defn craft-transportation
  [state]
  (craft-select-recipe (assoc-current-state state :craft-transportation) \a))


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
  [state]
  (let [t (get-in state [:world :time])]
    (info "scroll-log t" t)
    (info "scroll-log logs-viewed" (get-in state [:world :logs-viewed]))
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
    (update-in [:world :log]
      (fn [logs]
        (info "updating :world :log. logs" logs)
        (mapcat
          (fn [logs-with-same-time]
            (vec
              (reduce (fn [logs-with-same-time log]
                        (info "updating logs-with-same-time" logs-with-same-time)
                        (info "log" log)
                        (let [last-log (last logs-with-same-time)]
                          (if (and (< (+ (count (get last-log :text))
                                      (count (get log :text)))
                                    70)
                                (= (get last-log :color)
                                   (get log :color)))
                            ;; conj short lines
                            (vec (conj (vec (butlast logs-with-same-time))
                                        {:time (get log :time)
                                         :text (clojure.string/join " " [(get last-log :text)
                                                                         (get log :text)])
                                         :color (get log :color)}))
                            ;; pass long lines through
                            (vec (conj logs-with-same-time log)))))
                      []
                      logs-with-same-time)))
          (vals (group-by :time logs)))))
    ((fn [state]
      (let [t    (get-in state [:world :time])
            logs (filter #(= t (get % :time)) (get-in state [:world :log]))
            logs-viewed (get-in state [:world :logs-viewed] 1)]
        (info "logs" logs)
        (-> state
          (assoc-in [:world :logs-viewed] 1)
          (tx/when-> (and (> (count logs) 1)
                          (= logs-viewed 1))
            (assoc-current-state :more-log))))))))

(defn get-hungrier-and-thirstier
  "Increase player's hunger."
  [state]
  (let [hunger (get-in state [:world :player :hunger])
        thirst (get-in state [:world :player :thirst])]
    (as-> state state
      (if (= (current-state state) :sleep)
        (-> state
          (update-in [:world :player :hunger] (partial + 0.01))
          (update-in [:world :player :thirst] (partial + 0.05)))
        (-> state
          (update-in [:world :player :hunger] (partial + 0.05 (* 0.02 (count (player-inventory state)))))
          (update-in [:world :player :thirst] (partial + 0.1))
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
      (let [new-hunger (get-in state [:world :player :hunger])]
        (info "hunger" hunger "new-hunger" new-hunger)
        (if (< hunger 80 new-hunger)
          (append-log state "You need to eat now." :yellow)
          state))
      (let [new-thirst (get-in state [:world :player :thirst])]
        (info "thirst" thirst "new-thirst" new-thirst)
        (if (< thirst 80 new-thirst)
          (append-log state "You need to drink something now." :blue)
          state)))))


(defn get-rescued
  [state]
  (if (player-mounted-on-raft? state)
    (let [{x :x y :y}         (player-pos state)
          distance-from-start (chebyshev-distance {:x 0 :y 0}
                                                  (player-pos state))
          chance-of-rescue (max 0 (+ (/ -300.0 distance-from-start) 1.5))]
      (info "distance from 0,0" distance-from-start)
      (if (< (dg/float) chance-of-rescue)
        (assoc-in state [:world :current-state] :rescued)
        state))
    state))

(defn decrease-will-to-live
  "Decrease the player's will-to-live depending on circumstances."
  [state]
  (if (= (current-state state) :sleep)
    (let [[cell _ _] (player-cellxy state)
          bedroll?   (contains? (set (map :id (get cell :items []))) :bedroll)]
      (if bedroll?
        (update-in [:world :player :will-to-live]
          (fn [will-to-live] (+ 0.05 will-to-live)))
        state))
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
                dwtl       (+ dwtl (if (> (/ hunger max-hunger) 0.8)
                                     1
                                     0))
                thirst     (get-in state [:world :player :thirst])
                max-thirst (get-in state [:world :player :max-thirst])
                _          (info "thirst" thirst "max-thirst" max-thirst)
                dwtl       (+ dwtl (if (> (/ thirst max-thirst) 0.5)
                                     1
                                     0))
                wounded    (player-wounded? state)
                _          (info "wounded" wounded)
                dwtl       (+ dwtl (if wounded
                                     1
                                     0))
                poisoned   (player-poisoned? state)
                _          (info "poisoned" poisoned)
                dwtl       (+ dwtl (if poisoned
                                     1
                                     0))
                infected   (player-infected? state)
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
                          status)))))))

(defn log-will-to-live-flavor
  "Log a flavor message when will-to-live increases or decreases by a lot."
  [state prev-will-to-live]
  (let [dwtl (- prev-will-to-live (get-in state [:world :player :will-to-live]))]
    (if (> (Math/abs dwtl) 1.5)
      (if (neg? dwtl)
        (append-log state (format "You feel %s." (dg/rand-nth ["happy" "happier" "glad" "elated" "great" "good"])))
        (append-log state (format "You feel %s." (dg/rand-nth ["sad" "sadder" "down" "unhappy" "bummed" "bad"]))))
      state)))

(defn fruit-identify-activate
  "Display a message if the fruit being identified is poisonous."
  [state]
  (info "time" (get-time state))
  (info "skin-id-time" (get-in state [:world :player :skin-identify-activate-time]))
  (info "tongue-id-time" (get-in state [:world :player :tongue-identify-activate-time]))
  (-> state
    (arg-when-> [state]
      (let [activate-time (get-in state [:world :player :skin-identify-activate-time])]
        (and activate-time
             (< activate-time (get-time state))))
      (append-log (dg/rand-nth ["Your skin feels burning."
                                "Your skin is numb."
                                "Your skin is has a rash."
                                "Your skin hurts."]))
      (dissoc-in [:world :player :skin-identify-activate-time]))
    (arg-when-> [state]
      (let [activate-time (get-in state [:world :player :tongue-identify-activate-time])]
        (and activate-time
             (< activate-time (get-time state))))
      (append-log (dg/rand-nth ["Your tongue feels burning."
                                "Your tongue is numb."
                                "Your tongue hurts."]))
      (dissoc-in [:world :player :tongue-identify-activate-time]))))

(defn if-poisoned-get-hurt
  "Decrease player's hp if they are poisoned."
  [state]
  (info "player" (get-in state [:world :player]))
  (-> state
    (arg-when-> [state]
      (let [poisoned-time (get-in state [:world :player :poisoned-time])]
        (and poisoned-time
             (< poisoned-time (get-time state))))
      (->
        (conj-in [:world :player :status] :poisoned)
        (append-log "You vomit.")
        (update-in [:world :player :hunger] (fn [hunger] (min (+ hunger 30)
                                                              (get-in state [:world :player :max-hunger]))))
        (update-in [:world :player :thirst] (fn [thirst] (min (+ thirst 30)
                                                              (get-in state [:world :player :max-thirst]))))
        (dissoc-in [:world :player :poisoned-time])))
    (update-in [:world :player :hp] (fn [hp] (if (contains? (get-in state [:world :player :status]) :poisoned)
                                               (- hp 0.1)
                                               (min (get-in state [:world :player :max-hp])
                                                    (+ hp 0.2)))))))

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

(defn if-near-too-much-fire-get-hurt
  [state]
  (update-in state [:world :player :hp]
    (fn [hp]
      (if (>= (count (filter #(= (get % :type) :fire)
                             (player-adjacent-cells-ext state)))
              3)
        (dec hp)
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

(defn decrease-flashlight-charge
  [state]
  (if-let [flashlight (inventory-id->item state :flashlight)]
    (update-inventory-item state :flashlight (fn [flashlight] (-> flashlight
                                                                (arg-when-> [flashlight] (= (get flashlight :state :off) :on)
                                                                    (update-in [:charge] dec))
                                                                (arg-when-> [flashlight] (neg? (get flashlight :charge))
                                                                    (assoc :state :off)))))
    state))

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
        sight-distance   (float (sight-distance state))
        _ (info "player-pos" pos)
        _ (info "player place-id" (str (apply xy->place-id state (pos->xy pos))))
        _ (info "sight-distance" sight-distance)
        will-to-live     (get-in state [:world :player :will-to-live])
        max-will-to-live (get-in state [:world :player :max-will-to-live])
        get-cell-m       (memoize (fn [x y] (get-cell state x y)))
        new-time         (get-in state [:world :time])
        test-cells       (for [x (range (- (get pos :x) (int (Math/ceil sight-distance)))
                                        (+ (get pos :x) (int (Math/ceil sight-distance))))
                               y (range (- (get pos :y) (int (Math/ceil sight-distance)))
                                        (+ (get pos :y) (int (Math/ceil sight-distance))))]
                            [x y])
        visible-cells    (filter (fn [[x y]] (and (not (nil? (get-cell state x y)))
                                                  (not (farther-than? pos
                                                                      {:x x :y y}
                                                                      sight-distance))
                                                  (visible?
                                                    get-cell-m
                                                    cell-blocking?
                                                    (pos :x)
                                                    (pos :y)
                                                    x y)))
                                 test-cells)
        dwtl             (/ (reduce (fn [acc [x y]] (+ acc (- (dec new-time)
                                                           (get (get-cell state x y) :discovered (- new-time 10000)))))
                                 0 visible-cells)
                            48000)
        _                (info "delta will-to-live" (float dwtl))
        will-to-live     (min (+ will-to-live dwtl)
                              max-will-to-live)]
  (-> state
    ((fn [place]
            (reduce (fn [state [x y]]
                      (assoc-cell state x y :discovered new-time))
                    state
                    visible-cells)))
    (assoc-in [:world :player :will-to-live] will-to-live))))

(defn toggle-mount
  "Mount or unmount at the current cell."
  [state]
  (let [[{items :items} x y]  (player-cellxy state)
        mounted (get-in state [:world :player :mounted] false)]
    (cond
      (and mounted
           (contains? (set (map :id items)) :raft))
        (-> state
          (append-log "You dismount the raft.")
          (assoc-in [:world :player :mounted] false))
      (and (not mounted) (contains? (set (map :id items)) :raft))
        (-> state
          (append-log "You mount the raft.")
          (assoc-in [:world :player :mounted] true))
      (not (contains? (set (map :id items)) :raft))
        (append-log state "There is nothing to mount here."))))

      

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
    (let [npcs                   (npcs-in-viewport state)
          ;_                      (debug "meta" (-> move-to-target var meta))
          ;_                      (debug "move-to-target npc" npc "target" target)
          npc-pos                (get npc :pos)
          npc-pos-vec            [(npc-pos :x) (npc-pos :y)]
          threshold              (get npc :range-threshold)
          npc-can-move-in-water  (mg/can-move-in-water? (get npc :race))
          npc-can-move-on-land   (mg/can-move-on-land? (get npc :race))
        
          player                 (-> state :world :player)
          player-pos-vec         [(-> player :pos :x) (-> player :pos :y)]
          width                  (get-in state [:world :width])
          height                 (get-in state [:world :height])
          get-type               (memoize (fn [x y] (do
                                                      ;(debug "traversable?" x y "type" (get-in place [y x :type]))
                                                      (or (get (get-cell state x y) :type) :unknown))))
          water-traversable?     (fn water-traversable? [[x y]]
                                   (and (< 0 x width)
                                        (< 0 y height)
                                        (not (farther-than? npc-pos {:x x :y y} threshold))
                                        (contains? #{:water :surf} (get-type x y))
                                        #_(every? water-traversable? (adjacent-xys-ext x y))))
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
      
          traversable?           (cond
                                   (and npc-can-move-in-water
                                        npc-can-move-on-land)
                                   (fn [xy]
                                     (or (water-traversable? xy)
                                         (land-traversable? xy)))
                                   npc-can-move-in-water
                                     water-traversable?
                                   npc-can-move-on-land
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

(defn
  move-away-from-target
  "Move `npc` one space closer to the target position if there is a path
   from the npc to the target. Returns the moved npc and not the updated state.
   `target` is a map with the keys `:x` and `:y`."
  [state npc target]
    (let [npcs                   (npcs-in-viewport state)
          npc-pos                (get npc :pos)
          npc-pos-vec            [(npc-pos :x) (npc-pos :y)]
          [npc-x npc-y]          [(npc-pos :x) (npc-pos :y)]
          threshold              (get npc :range-threshold)
          npc-can-move-in-water  (mg/can-move-in-water? (get npc :race))
          npc-can-move-on-land   (mg/can-move-on-land? (get npc :race))
        
          player                 (-> state :world :player)
          player-pos-vec         [(-> player :pos :x) (-> player :pos :y)]
          width                  (get-in state [:world :width])
          height                 (get-in state [:world :height])
          get-type               (memoize (fn [x y] (do
                                                      #_(debug "traversable?" x y "type" (get-in place [y x :type]))
                                                      (or (get (get-cell state x y) :type) :unknown))))
          water-traversable?     (fn water-traversable? [[x y]]
                                   (and (< 0 x width)
                                        (< 0 y height)
                                        (not (farther-than? npc-pos {:x x :y y} threshold))
                                        (contains? #{:water :surf} (get-type x y))
                                        #_(every? water-traversable? (adjacent-xys-ext x y))))
          land-traversable?      (fn [[x y]]

                                        (not (farther-than? npc-pos {:x x :y y} threshold))
                                        (contains? #{:floor
                                                     :open-door
                                                     :corridor
                                                     :sand
                                                     :dirt
                                                     :gravel
                                                     :tall-grass
                                                     :short-grass}
                                                   (get-type x y)))
      
          traversable?           (cond
                                   (and npc-can-move-in-water
                                        npc-can-move-on-land)
                                   (fn [xy]
                                     (or (water-traversable? xy)
                                         (land-traversable? xy)))
                                   npc-can-move-in-water
                                     water-traversable?
                                   npc-can-move-on-land
                                     land-traversable?)
          ;; TODO: calc new-pos based on adj-navigable space (8-dir) that is farthest away from player
          ;_                      (debug "path to target" path)
          new-pos                (first
                                   (sort-by (fn [[x y]] (distance (xy->pos npc-x npc-y) (xy->pos x y)))
                                            (filter traversable?
                                                    (adjacent-xys-ext npc-x npc-y))))
          new-pos                (or new-pos
                                     [npc-x npc-y])

          ;_                      (debug "new-pos" new-pos)
          new-npc                (-> npc
                                     (assoc-in [:pos :x] (first new-pos))
                                     (assoc-in [:pos :y] (second new-pos)))
          ;_                      (debug "new-npc" new-npc)
        ]
      [{:x (first new-pos) :y (second new-pos)} new-npc npc]))

(defn move-random
  [state npc]
  (let [npc-pos  (get npc :pos)
        navigable-types (if (mg/can-move-in-water? (get npc :race))
                          #{:water :surf}
                          #{:floor
                            :corridor
                            :open-door
                            :sand
                            :dirt
                            :gravel
                            :tall-grass
                            :short-grass})]
    ;; move randomly into an adjacent cell
    (let [target (first
                   (dg/shuffle
                     (adjacent-navigable-pos state
                                             npc-pos
                                             navigable-types)))]
      ;(debug "distance > threshold, move randomly. target" target)
      [target
       (-> npc
         (assoc-in [:pos :x] (get target :x))
         (assoc-in [:pos :y] (get target :y)))
       npc])))

(defn move-to-target-in-range-or-random
  [state npc target]
  (let [threshold (get npc :range-threshold)
        npc-pos  (get npc :pos)
        distance (distance npc-pos target)
        navigable-types (if (mg/can-move-in-water? (get npc :race))
                          #{:water :surf}
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
                       (adjacent-navigable-pos state
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

(defn move-away-from-target-in-range-or-random
  [state npc target]
  (let [threshold (get npc :range-threshold)
        npc-pos  (get npc :pos)
        distance (distance npc-pos target)
        navigable-types (if (mg/can-move-in-water? (get npc :race))
                          #{:water :surf}
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
                       (adjacent-navigable-pos state
                                               npc-pos
                                               navigable-types)))]
        ;(debug "distance > threshold, move randomly. target" target)
        [target
         (-> npc
           (assoc-in [:pos :x] (get target :x))
           (assoc-in [:pos :y] (get target :y)))
         npc])
      ;; inside range, move toward player
      (move-away-from-target state npc target))))

(defn calc-npc-next-step
  "Returns the moved npc and not the updated state. New npc pos will depend on
   the npc's `:movement-policy which is one of `:constant` `:entourage` `:follow-player` `:random` `:follow-player-in-range-or-random` `:hide-from-player-in-range-or-random`."
  [state npc]
  {:pre  [(contains? #{:constant :entourage :follow-player :random :follow-player-in-range-or-random :hide-from-player-in-range-or-random} (get npc :movement-policy))]
   :post [(= (count %) 3)]}
  (let [policy      (get npc :movement-policy)
        temperament (get npc :temperament)
        pos         (-> state :world :player :pos)
        ;; modify movement policy as dictated by day or night
        policy      (cond
                      (or
                        (and (= temperament :hostile-during-day)
                             (is-day? state))
                        (and (= temperament :hostile-at-day)
                             (is-night? state)))
                      :follow-player-in-range-or-random
                      :else
                      policy)
        navigable-types (if (mg/can-move-in-water? (get npc :race))
                          #{:water :surf}
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
      :constant                            [nil nil npc]
      :entourage                           (move-to-target state
                                                           npc
                                                           (first
                                                             (dg/shuffle
                                                               (adjacent-navigable-pos state
                                                                                       pos
                                                                                       navigable-types))))
      :follow-player                       (move-to-target state npc pos)
      :random                              (move-random state npc)
      :follow-player-in-range-or-random    (move-to-target-in-range-or-random state npc pos)
      :hide-from-player-in-range-or-random (move-away-from-target-in-range-or-random state npc pos)
      [nil nil npc])))
 
(defn update-npcs
  "Move all npcs in the current place using `move-npc`."
  [state]
  {:pre  [(vector? (get-in state [:world :npcs]))]
   :post [(vector? (get-in % [:world :npcs]))]}
  ;; do npc->player attacks if adjacent
  (let [;; increase all npcs energy by their speed value and have any adjacent npcs attack the player.
        state (reduce
                (fn [state npc]
                  ;; only update npcs that are in the current place and have an :energy value.
                  (if (contains? npc :energy)
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
                (npcs-in-viewport state))]
    state
    (loop [state          state
           ;; find all npcs in the current place with enough energy to move (energy > 1).
           ;;; Since most npcs are moving toward the player, sort by distance
           ;;; to player. The npcs closest will be moved first, leaving a gap
           ;;; behind them allowing the next most distant npc to move forward
           ;;; to fill the gap.
           remaining-npcs (sort-by (fn [npc] (distance-from-player state (get npc :pos)))
                                   (filter (fn [npc] (and (contains? npc :energy)
                                                          (> (get npc :energy) 1)))
                                     (npcs-in-viewport state)))
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
                             (vec (map (fn [npc]
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
                                   ;; no npc or player at the destination cell?
                                   (if (and
                                         (not-any? (fn [npc] (and (= (get npc :pos)
                                                                      new-pos)
                                                                   (= (get npc :place)
                                                                      (get new-npc :place))))
                                                    (get-in state [:world :npcs]))
                                         (not= new-pos (player-pos state)))
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
                                                      (> (get npc :energy) 1)))
                                 (npcs-in-viewport state)))]
              ;_ (trace "count remaining npcs" (count remaining-npcs))]
        (recur state remaining-npcs (dec i)))))))

(defn update-cells
  "Fill holes with a small amount of water. Drop fruit. Drop harvest items."
  [state]
  (info "updating cells")
  (let [xys      (viewport-xys state)
        fire-xys (set (filter (fn filter-fire-cells
                        [[x y]]
                        (= (get (get-cell state x y) :type) :fire))
                        xys))]
    (reduce 
      (fn cell-reduction-fn [state [x y]] 
        (let [cell       (get-cell state x y)
              cell-type  (get cell :type)
              cell-items (get cell :items)]
          #_(info "updating cell" cell "@" x y)
          #_(info "viewport" (get-in state [:world :viewport]))
          (-> 
            (cond
                ;; update holes
                (contains? #{:freshwater-hole :saltwater-hole} cell-type)
                (update-cell state x y
                  (fn increase-hole-water [cell] (assoc cell :water (min 20 (+ 0.1 (* (dg/float) 0.1) (get cell :water 0.0))))))
                ;; update solar stills
                (contains? #{:solar-still} (get cell :type))
                (update-cell state x y
                  (fn increase-still-water [cell] (assoc cell :water (min 20 (+ 0.2 (* (dg/float) 0.1) (get cell :water 0.0))))))
                ;; drop harvest items
                (contains? #{:gravel :tree :palm-tree :tall-grass} cell-type)
                (update-cell state x y
                  (fn drop-harvest-items [cell]
                    (if (= (uniform-int 0 100000) 0)
                      (assoc cell :harvestable true)
                      cell)))
                ;; drop fruit
                (contains? #{:fruit-tree} cell-type)
                ;; chance of dropped a fruit
                (if (= (uniform-int 0 1000) 0)
                  ;; make the fruit item and find an adjacent free cell to drop it into
                  (let [item    (assoc (ig/id->item (get cell :fruit-type)) :rot-time (+ (get-time state) (uniform-int 10 30)))
                        adj-xys (remove (fn [[x y]] (or (not (xy-in-viewport? state x y))
                                                         (type->collide?
                                                           (get (get-cell state x y) :type))))
                                        (adjacent-xys x y))]
                    (info "dropping fruit" item "at [" x y "]" adj-xys)
                    (if (seq adj-xys)
                      ;; drop the fruit into the cell
                      (apply conj-cell-items state (conj (dg/rand-nth adj-xys) item))
                      state))
                  state)
                ;; update fire
                (contains? fire-xys [x y])
                (-> state
                  (update-cell x y (fn [cell] (update-in cell [:fuel] dec)))
                  ;; chance of fire spreading
                  (arg-when-> [state] (= 0 (uniform-int 0 10))
                    ;; make the fire spread and find an adjacent free cell to spread it into
                    (as-> state
                      (let [adj-xys (filter (fn [[x y]] (type->flammable?
                                                          (get (get-cell state x y) :type)))
                                            (adjacent-xys-ext x y))]
                        (info "spreading fire at [" x y "]" adj-xys)
                        (if (seq adj-xys)
                          ;; spread fire into the cell
                          (let [[x y] (dg/rand-nth adj-xys)]
                            (assoc-cell state x y :type :fire :fuel (uniform-int 10 50)))
                          state))))
                  (arg-when-> [state] (neg? (get cell :fuel 0))
                    ;; extinguish the fire
                    (->
                      (assoc-cell x y :type :dirt)
                      (dissoc-cell x y :fuel))))
                 :else
                 state)
            ;; rot fruit
            ;; TODO: only rot fruit if it is in the set of visible cells.
            (arg-when-> [state] (some (fn [item] (< (get item :rot-time (get-time state)) (get-time state)))
                                      cell-items)
              (update-cell x y (fn rot-cell-fruit [cell]
                                 (update-in cell [:items] (fn [items]
                                   (remove (fn [item] (< (get item :rot-time (get-time state))
                                                         (get-time state)))
                                            items)))))))))
      state
      xys)))

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
  ;;         starting      transition  transition             new              advance
  ;;         state         symbol      fn                     state            time?
  (let [table {:start     {:enter      [identity               :start-inventory false]
                           :backspace  [backspace-name         :start           false]
                           :else       [append-name            :start           false]}
               :start-inventory
                          {:enter      [add-starting-inventory :start-text      false]
                           :else       [select-starting-inventory
                                                               :start-inventory false]}
               :start-text
                          {:else       [(fn [state _]
                                          (do-rest state))     :normal          true]}
               :normal    {\i          [identity               :inventory       false]
                           \d          [identity               :drop            false]
                           \,          [identity               :pickup          false]
                           \e          [identity               :eat             false]
                           \o          [identity               :open            false]
                           \c          [identity               :close           false]
                           \.          [do-rest                :normal          true]
                           :left       [move-left              :normal          true]
                           :down       [move-down              :normal          true]
                           :up         [move-up                :normal          true]
                           :right      [move-right             :normal          true]
                           :up-left    [move-up-left           :normal          true]
                           :up-right   [move-up-right          :normal          true]
                           :down-left  [move-down-left         :normal          true]
                           :down-right [move-down-right        :normal          true]
                           \q          [quaff-select           identity         false]
                           \w          [identity               :wield           false]
                           \x          [identity               :harvest         false]
                           \a          [identity               :apply           false]
                           \>          [use-stairs             :normal          true]
                           \<          [use-stairs             :normal          true]
                           \;          [init-cursor            :describe        false]
                           \R          [identity               :sleep           false]
                           \s          [search                 :normal          true]
                           \S          [extended-search        :normal          true]
                           \Q          [identity               :quests          false]
                           \M          [toggle-mount           :normal          false]
                           \P          [next-party-member      :normal          false]
                           \z          [identity               :craft           true]
                           \Z          [identity               :magic           true]
                           \t          [identity               :throw-inventory false]
                           \T          [identity               :talk            true]
                           \m          [identity               :log             false]
                           \?          [identity               :help            false]
                           \r          [repeat-commands        identity         false]
                           \0          [(fn [state]
                                          (-> state
                                          (append-log "log1...........................")
                                          (append-log "log2...........................")
                                          (append-log "log3...........................")
                                          (append-log "log4...........................")
                                          (append-log "log5...........................")))
                                          :normal true]
                          \1           [(fn [state]
                                          (assoc-in state [:world :log] [])) :normal true]
                          \2           [(fn [state]
                                          (assoc-in state [:world :player :hunger] 100))
                                          :normal true]
                           :escape     [identity               :quit?           false]}
               :inventory {:escape     [identity               :normal          false]}
               :describe  {:escape     [free-cursor            :normal          false]
                           \i          [free-cursor            :describe-inventory false]
                           :left       [move-cursor-left       :describe        false]
                           :down       [move-cursor-down       :describe        false]
                           :up         [move-cursor-up         :describe        false]
                           :right      [move-cursor-right      :describe        false]
                           :enter      [describe-at-cursor     :normal          false]}
               :quests    {:escape     [identity               :normal          false]}
               :describe-inventory
                          {:escape     [identity               :normal          false]
                           :else       [describe-inventory     :normal          false]}
               :drop      {:escape     [identity               :normal          false]
                           :else       [drop-item              :normal          true]}
               :apply     {:escape     [identity               :normal          false]
                           :else       [select-apply-item      identity         false]}
               :apply-item-normal
                          {:escape     [identity               :normal          false]
                           :else       [apply-item             identity         true]}
               :apply-item-inventory
                          {:escape     [identity               :normal          false]
                           :else       [apply-item             :normal          true]}
               :apply-item-body
                          {:escape     [identity               :normal          false]
                           \a          [(fn [state]
                                        (apply-item state \a)) :normal          true]
                           \b          [(fn [state]
                                        (apply-item state \b))
                                                               :normal          true]}
               :pickup    {:escape     [identity               :normal          false]
                           :else       [toggle-hotkey          :pickup          false]
                           :enter      [pick-up                :normal          true]}
               :eat       {:escape     [identity               :normal          false]
                           :else       [eat                    :normal          true]}
               :quaff-adj-or-inv
                          {\i          [identity               :quaff-inv       false]
                           :left       [quaff-left             :normal          true]
                           :down       [quaff-down             :normal          true]
                           :up         [quaff-up               :normal          true]
                           :right      [quaff-right            :normal          true]}
               :quaff-adj {:left       [quaff-left             :normal          true]
                           :down       [quaff-down             :normal          true]
                           :up         [quaff-up               :normal          true]
                           :right      [quaff-right            :normal          true]}
               :quaff-inventory
                          {:escape     [identity               :normal          false]
                           :else       [quaff-inventory        :normal          true]}
               :open      {:left       [open-left              :normal          true]
                           :down       [open-down              :normal          true]
                           :up         [open-up                :normal          true]
                           :right      [open-right             :normal          true]}
               :talk      {:left       [talk-left              identity         false]
                           :down       [talk-down              identity         false]
                           :up         [talk-up                identity         false]
                           :right      [talk-right             identity         false]}
               :harvest   {:left       [harvest-left           :normal          true]
                           :down       [harvest-down           :normal          true]
                           :up         [harvest-up             :normal          true]
                           :right      [harvest-right          :normal          true]
                           \>          [harvest-center         :normal          true]
                           :escape     [identity               :normal          false]}
               :wield     {:escape     [identity               :normal          false]
                           :else       [wield                  :normal          true]}
               :talking   {:escape     [stop-talking           :normal          false]
                           :else       [talk                   identity         true]}
               :shopping  {\a          [identity               :buy             true]
                           \b          [identity               :sell            true]
                           :escape     [identity               :normal          false]}
               :buy       {:escape     [identity               :normal          false]
                           :else       [buy                    :buy             true]}
               :sell      {:escape     [identity               :normal          false]
                           :else       [sell                   :sell            true]}
               :craft     {\w          [craft-weapon           :craft-weapon    false]
                           \s          [craft-survival         :craft-survival  false]
                           \c          [craft-shelter          :craft-shelter   false]
                           \t          [craft-transportation   :craft-transportation
                                                                                false]
                           :escape     [identity               :normal          false]}
               :craft-weapon
                          {:escape     [identity               :craft           false]
                           :enter      [craft                  :normal          true]
                           :else       [craft-select-recipe    identity         false]}
               :craft-survival
                          {:escape     [identity               :craft           false]
                           :enter      [craft                  :normal          true]
                           :else       [craft-select-recipe    identity         false]}
               :craft-shelter
                          {:escape     [identity               :craft           false]
                           :enter      [craft                  :normal          true]
                           :else       [craft-select-recipe    identity         false]}
               :craft-transportation
                          {:escape     [identity               :craft           false]
                           :enter      [craft                  :normal          true]
                           :else       [craft-select-recipe    identity         false]}
               :throw-inventory
                          {:escape     [identity               :normal          false]
                           :else       [throw-select-inventory :throw-direction false]}
               :throw-direction
                          {:escape     [identity               :normal          false]
                           :left       [throw-left             :normal          true]
                           :down       [throw-down             :normal          true]
                           :up         [throw-up               :normal          true]
                           :right      [throw-right            :normal          true]}
               :fishing-left
                          {\.          [do-fishing             identity         true]
                           :else       [pass-state             :normal          false]}
               :fishing-right
                          {\.          [do-fishing             identity         true]
                           :else       [pass-state             :normal          false]}
               :fishing-up
                          {\.          [do-fishing             identity         true]
                           :else       [pass-state             :normal          false]}
               :fishing-down
                          {\.          [do-fishing             identity         true]
                           :else       [pass-state             :normal          false]}
               :magic     {:escape     [identity               :normal          false]
                           :else       [do-magic               identity         true]}
               :magic-direction
                          {:left       [magic-left             identity         true]
                           :down       [magic-down             identity         true]
                           :up         [magic-up               identity         true]
                           :right      [magic-right            identity         true]}
               :magic-inventory
                          {:escape     [identity               :normal          false]
                           :else       [magic-inventory        :normal          true]}
               :sleep     {:else       [do-sleep               identity         true]}
               :help      {:else       [pass-state             :normal          false]}
               :close     {:left       [close-left             :normal          true]
                           :down       [close-down             :normal          true]
                           :up         [close-up               :normal          true]
                           :right      [close-right            :normal          true]}
               :more-log  {:enter      [scroll-log             identity         false]
                           :escape     [scroll-log             identity         false]
                           :space      [scroll-log             identity         false]}
               :log       {:else       [pass-state             :normal          false]}
               :rescued   {\y          [identity               :start-inventory false]
                           \n          [(constantly nil)       :normal          false]}
               :dead      {\y          [identity               :start-inventory false]
                           \n          [(constantly nil)       :normal          false]}
               :quit?     {\y          [(constantly nil)       :normal          false]
                           :else       [pass-state             :normal          false]}}
        expander-fn (fn [table] table)]
    (expander-fn table)))

(def translate-direction-states
  #{:normal
    :describe
    :quaff-adj-or-inv
    :open
    :talk
    :harvest
    :throw-direction
    :magic-direction
    :close})

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
        table         (get state-transition-table current-state)
        keyin         (if (contains? translate-direction-states current-state)
                        (translate-directions keyin)
                        keyin)]
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
            state     (if state
                        (if-not (= keyin \r)
                          (cond
                            (= current-state :normal)
                              (assoc-in state [:world  :command-seq] [keyin])
                            (contains? (hash-set current-state new-state) :more-log)
                              state
                            :else
                              (conj-in state [:world :command-seq] keyin))
                          (assoc-in state [:world :command-seq] command-seq))
                        state)
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
              ;; Chance of rescue
              (get-rescued)
              ;; Show fruit identify messages if applicable
              (fruit-identify-activate)
              ;; if poisoned, damage player, else heal
              (if-poisoned-get-hurt)
              (heal)
              (decrease-flashlight-charge)
              (if-wounded-get-infected)
              (if-infected-get-hurt)
              (if-near-too-much-fire-get-hurt)
              (decrease-will-to-live)
              (update-npcs)
              (update-cells)
              ((fn [state] (info "Done updating cells") state))
              ;; TODO: Add appropriate level
              (add-npcs-random)
              ;; update visibility
              (update-visibility)
              ;; add will-to-live flavor message
              (log-will-to-live-flavor wtl))
            (update-quests)
            (arg-when-> [state] (contains? (-> state :world :player :status) :dead)
              ((fn [state]
                 (doseq [file (filter (fn [file] (re-matches #".*edn" (.getName file))) (file-seq (io/file "save")))]
                   (.delete file))
                state))
              (update-in [:world :player :status]
               (fn [status]
                 (disj status :dead)))
              (assoc-current-state :dead))
              ;; delete the save game on player death
            ;; only try to start log scrolling if the state we were just in was not more-log
            (tx/when-> (and current-state
                            (not= current-state :more-log))
              (init-log-scrolling))))
      state)))

