;; Functions that manipulate state to do what the user commands.
(ns robinson.update
  (:require 
            [robinson.common :as rc]
            [robinson.random :as rr]
            [robinson.world :as rw]
            [robinson.viewport :as rv]
            [robinson.describe :as rdesc]
            [robinson.dialog :as rdiag]
            [robinson.player :as rp]
            [robinson.math :as rmath]
            [robinson.itemgen  :as ig]
            [robinson.monstergen :as mg]
            [robinson.startgame :as sg]
            robinson.devtools
            [clojure.string :refer [lower-case]]
            ;[robinson.dialog :refer []]
            [robinson.npc :as rnpc]
            [robinson.combat :as rcombat]
            [robinson.crafting :as rcraft]
            [robinson.magic :refer [do-magic magic-left magic-down
                                    magic-up magic-right magic-inventory]]
            [robinson.worldgen :as rworldgen]
            [robinson.lineofsight :as rlos]
            [robinson.feedback :as rf]
            robinson.macros
            #?@(:clj (
                [robinson.macros :as rm]
                clojure.pprint
                clojure.edn
                [clojure.core.async :as async]
                [clojure.data.json :as json]
                [clojure.java.io :as io]
                [taoensso.timbre :as log]
                clj-tiny-astar.path
                [clj-http.client :as http]
                [clojure.stacktrace :as st]
                clojure.inspector
                clojure.string)
                :cljs (
                [robinson.macros :as rm :include-macros true]
                [cljs.core.async :as async]
                clojure.walk
                cljs.reader
                [taoensso.timbre :as log :include-macros true]
                [goog.string.format]))))


(defn format [s & args]
  #?(:clj
     (apply clojure.core/format s args)
     :cljs
     (apply gstring/format s args)))

(defn- pass-state
  [state & more]
  state)

(defn delete-save-game []
  ;; delete the save game on player death
  (doseq [file (filter (fn [file] (re-matches #".*edn" (.getName file))) (file-seq (io/file "save")))]
    (log/info "Deleting" file)
    (.delete file)))
  
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

(defn inc-time
  [state]
  (update-in state [:world :time] inc))

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
  (log/debug "toggle-hotkey" keyin)
  (update-in state [:world :selected-hotkeys]
             (fn [hotkeys] (if (contains? hotkeys keyin)
                             (disj (set hotkeys) keyin)
                             (conj (set hotkeys) keyin)))))

(defn system-time-millis []
  #?(:clj
     (System/currentTimeMillis)
     :cljs
     (.getMilliseconds (js/Date.))))

;; update visibility
(defn update-visibility
  [state]
  {:pre [(not (nil? state))]
   :post [(not (nil? %))]}
  (let [pos              (-> state :world :player :pos)
        {px :x
         py :y}          pos
        sight-distance   (float (rlos/sight-distance state))
        _ (log/info "player-pos" pos)
        _ (log/info "player place-id" (str (apply rv/xy->place-id state (rc/pos->xy pos))))
        _ (log/info "sight-distance" sight-distance)
        _ (log/info "loaded place ids" (keys (get-in state [:world :places])))
        will-to-live     (get-in state [:world :player :will-to-live])
        max-will-to-live (get-in state [:world :player :max-will-to-live])
        new-time         (get-in state [:world :time])
        visible-cells (rlos/visible-xys px py sight-distance (fn [[x y]]
                                                               (let [cell      (rw/get-cell state x y)
                                                                     blocking? (rlos/cell-blocking? cell)]
                                                                 ;(log/info "cell place-id" (str (rv/xy->place-id state x y)))
                                                                 ;(log/debug "blocking?" x y cell blocking?)
                                                                 blocking?)))
        _ (log/info "visible-cells" visible-cells)
        dwtl             (/ (reduce (fn [acc [x y]] (+ acc (- (dec new-time)
                                                              (get (rw/get-cell state x y) :discovered (- new-time 10000)))))
                                 0 visible-cells)
                            888000)
        _                (log/info "delta will-to-live" (float dwtl))
        will-to-live     (min (+ will-to-live dwtl)
                              max-will-to-live)]
  (as-> state state
    (rw/assoc-cells state (zipmap visible-cells (repeat {:discovered new-time})))
    (assoc-in state [:world :player :will-to-live] will-to-live))))

(defn add-starting-inventory
  [state selected-hotkeys]
  {:pre  [(not (nil? state))]
   :post [(not (nil? %))]}
  (let [_                  (log/info "selected-hotkeys" selected-hotkeys)
        start-inventory    (filter #(contains? (set selected-hotkeys) (get % :hotkey)) (sg/start-inventory))
        _                  (log/info "Adding to starting inventory:" start-inventory)
        state              (rp/add-to-inventory state start-inventory)]
    state))

(defn reinit-world
  "Re-initialize the value of `:world` within `state`. Used when starting a new game
   from fresh or after the player has died."
  [state]
  (let [selected-hotkeys (get-in state [:world :selected-hotkeys])
        player-name      (rp/get-player-attribute state :name)]
    ;; remove any previously generated chunks
    (delete-save-game)
    (loop []
      (let [state (-> state
                    (assoc :world (loop []
                                    (if-let [w (try
                                                 (rworldgen/init-world (system-time-millis))
                                                 (catch #?@(:clj  (Throwable e)
                                                            :cljs (js/Error e))
                                                   (log/error e)
                                                   nil))]
                                        w
                                      (recur))))
                    (rworldgen/load-unload-places))
           cell-type (get-in (rw/player-cellxy state) [0 :type])]
        (log/info "reinit-world player cell type" cell-type)
        (if (contains? #{:tree :palm-tree :fruit-tree :bamboo :mountain :water}
                       cell-type)
          (do
            (log/warn "Scrapping world due to invalid starting cell type" cell-type)
            (recur))
          (->  state
            (add-starting-inventory selected-hotkeys)
            (rp/assoc-player-attribute :name player-name)
            (update-visibility)
            (as-> state
              (reduce (fn [state _] (rnpc/add-npcs state))
                      state
                      (range 5)))))))))

(defn select-starting-inventory
  [state keyin]
  {:pre  [(not (nil? state))]
   :post [(not (nil? %))]}
  (if (and #?(:clj
              (char? keyin)
              :cljs
              (string? keyin))
           (<= (int \a) (int keyin) (int \k)))
    (let [new-state (toggle-hotkey state keyin)
          selected-hotkeys (get-in new-state [:world :selected-hotkeys])]
      (log/info "selected-hotkeys" selected-hotkeys)
      (if (< (count selected-hotkeys) 4)
        new-state
        state))
    state))

(defn destroy-cell
  "Destroy the cell at xy, changing its type. This is when the player bumps into things like palisades."
  [state x y]
  {:pre  [(not (nil? state))]
   :post [(not (nil? %))]}
  (rw/assoc-cell state x y :type :dirt))

(defn pick-up-gold
  "Vacuums up gold from the floor into player's inventory."
  [state]
  (let [;;place-id (current-place-id state)
        ;player-x (-> state :world :player :pos :x)
        ;player-y (-> state :world :player :pos :y)
        [{items :items} x y]  (rw/player-cellxy state)
        ;items              (player-cell :items)
        {cash  true
         non-cash-items false}  (group-by (fn [item] (= (item :type) :$))
                                          items)
        _ (log/debug "picking up gold. Divided items" cash non-cash-items)
        $                  (reduce + (map :amount (or cash [])))]
    (if (> $ 0)
      (-> state
        (rc/append-log (format "You pick up %d cash." $))
        (rw/assoc-cell-items x y
          (or non-cash-items []))
        (update-in [:world :player :$]
          (fn [player-$] (+ $ player-$))))
      state)))

(defn move-outside-safe-zone
  "Move the player when they are near the edge of the map.
   If a pre-generated place does not exist, create a new one using the world seed."
  [state direction]
  {:pre  [(not (nil? state))]
   :post [(not (nil? %))]}
  (log/info "move-outside-safe-zone")
  (let [[player-cell x y] (rw/player-cellxy state)
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
        target-place      (rv/xy->place-id state target-x target-y)
        rv/visible-place-ids (rv/visible-place-ids state target-x target-y)
        _ (log/info "target-x" target-x "target-y" target-y)
        _ (log/info "target-place" target-place)
       vp-pos             (apply rc/xy->pos
                            (rv/+xy (rc/pos->xy (get-in state [:world :viewport :pos]))
                                 (rv/-xy [target-x target-y] [x y])))
       ;;dest-place        (-> state :world :places dest-place-id)
       {width :width
        height :height}  (get-in state [:world :viewport])]
      (log/debug "viewport-pos" vp-pos)
      ;#+clj
      #_(log/debug "npcs" (with-out-str (clojure.pprint/pprint (-> state :world :npcs))))
      (-> state
        (assoc-in [:world :viewport :pos] vp-pos)
        (rworldgen/load-unload-places))))
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
          (not (nil? state))
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
        target-cell (rw/get-cell state target-x target-y)]
    (log/info "moving to" target-x target-y "type:" (get target-cell :type))
    (log/info "inside-safe-zone?" (rv/xy-in-safe-zone? state target-x target-y) target-x target-y)
    (log/info "not collide?" (not (rw/collide? state target-x target-y {:include-npcs? false})))
    (log/info "not mounted-on-raft?" (not (rw/player-mounted-on-raft? state)))
    (cond
      (rp/player-paralyzed? state)
        (rc/append-log "You can not move (paralyzed).")
      (and (not (rw/collide? state target-x target-y {:include-npcs? false}))
           (not (rw/player-mounted-on-raft? state)))
        (as-> state state
          (if (not (rv/xy-in-safe-zone? state target-x target-y))
            (move-outside-safe-zone state direction)
            state)
          (if (rw/npc-at-xy state target-x target-y)
            ;; collided with npc. Engage in combat.
            (rcombat/attack state [:world :player] (rnpc/npc->keys state (rw/npc-at-xy state target-x target-y)))
            (as-> state state
              (assoc-in state [:world :player :pos :x] target-x)
              (assoc-in state [:world :player :pos :y] target-y)
              (let [cell  (rw/get-cell state target-x target-y)
                    items (get cell :items)]
                (if (seq items)
                  (rdesc/search state)
                  state)))))
      (= (get (rw/npc-at-xy state target-x target-y) :in-party?) true)
        (-> state
          (assoc-in [:world :player :pos :x] target-x)
          (assoc-in [:world :player :pos :y] target-y)
          (pick-up-gold)
          (rc/map-in [:world :npcs]
                     (fn [npc] (if (and (= (-> npc :pos :x) target-x)
                                        (= (-> npc :pos :y) target-y))
                                 (-> npc
                                     (assoc-in [:pos :x] player-x)
                                     (assoc-in [:pos :y] player-y))
                                 npc))))
      (and (not (rw/collide-in-water? state target-x target-y))
           (rw/player-mounted-on-raft? state))
        (as-> state state
          (rw/dec-cell-item-count state :raft)
          (rw/conj-cell-items state target-x target-y (ig/id->item :raft))
          (if (not (rv/xy-in-safe-zone? state target-x target-y))
            (move-outside-safe-zone state direction)
            state)
          (assoc-in state [:world :player :pos :x] target-x)
          (assoc-in state [:world :player :pos :y] target-y)
          ;; rafting = more hunger
          (update-in state [:world :player :hunger] (partial + 0.05 ))
          ;;
          (let [cell  (rw/get-cell state target-x target-y)
                items (get cell :items)]
            (if (seq items)
              (rdesc/search state)
              state)))
      ;; Hack down destroyable cells.
      (rw/type->destroyable? (get target-cell :type))
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
    (log/debug "open-door")
    (let [target-cell (rw/get-cell state target-x target-y)]
      (log/debug "target-cell" target-cell)
      (if (and (not (nil? target-cell)) (= (target-cell :type) :close-door))
        (do
          (log/debug "opening door")
          (log/debug (rw/get-cell state target-y target-x))
          (-> state
            (rc/append-log "The door creaks open")
            (rw/assoc-cell state target-y target-x :type :open-door)))
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
    (log/debug "close-door")
    (let [target-cell (rw/get-cell state target-x target-y)]
      (log/debug "target-cell" target-cell)
      (if (and (not (nil? target-cell)) (= (target-cell :type) :open-door))
        (-> state
          (rc/append-log "The door closes")
          (rw/assoc-cell state target-y target-x :type :close-door))
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
  {:pre  [(not (nil? state))]
   :post [(not (nil? %))]}
    ;; find all the items in the current cell
    ;; divide them into selected and not-selected piles using the selected-hotkeys
    ;; add the selected pile to the player's inventory
    ;; return the non-selcted pile to the cell
    ;; remove selected-hotkeys from remaining-hotkeys
    ;; clear selected-hotkeys
    (let [[player-cell x y]  (rw/player-cellxy state)
          items              (vec (get player-cell :items))
          remaining-hotkeys  (-> state :world :remaining-hotkeys)
          divided-items      (group-by (fn [item] (if (contains? (-> state :world :selected-hotkeys) (item :hotkey))
                                                      :selected
                                                      :not-selected))
                                       (map #(assoc %1 :hotkey %2)
                                            items
                                            (rc/fill-missing not
                                                            (fn [_ hotkey] hotkey)
                                                            remaining-hotkeys
                                                            (map :hotkey items))))
          selected-items     (vec (divided-items :selected))
          not-selected-items (vec (divided-items :not-selected))
          remaining-hotkeys  (vec (remove #(some (partial = %) (map :hotkey selected-items)) remaining-hotkeys))]
      (log/debug "divided-items" divided-items)
      (log/debug "selected-items" selected-items)
      (log/debug "not-selected-items" not-selected-items)
      (if (seq selected-items)
        (let [new-state (-> state
                          (rc/append-log "You pick up:")
                          ;; dup the item into inventory with hotkey
                          (rp/add-to-inventory selected-items)
                          ;; remove the item from cell
                          (rw/assoc-current-cell-items not-selected-items)
                          ;;;; hotkey is no longer available
                          (assoc-in [:world :remaining-hotkeys]
                              remaining-hotkeys)
                          ;; reset selected-hotkeys
                          (assoc-in [:world :selected-hotkeys] #{}))]
          new-state)
        state)))

(defn pick-up-all
  [state]
    (let [[player-cell x y]  (rw/player-cellxy state)
          items              (vec (get player-cell :items))
          remaining-hotkeys  (-> state :world :remaining-hotkeys)
          selected-items     (map #(assoc %1 :hotkey %2)
                                  items
                                  (rc/fill-missing not
                                                  (fn [_ hotkey] hotkey)
                                                  remaining-hotkeys
                                                  (map :hotkey items)))
          remaining-hotkeys  (vec (remove #(some (partial = %) (map :hotkey selected-items)) remaining-hotkeys))]
      (log/debug "selected-items" selected-items)
      (if (seq selected-items)
        (let [new-state (-> state
                          (rc/append-log "You pick up:")
                          ;; dup the item into inventory with hotkey
                          (rp/add-to-inventory selected-items)
                          ;; remove the item from cell
                          (rw/assoc-current-cell-items [])
                          ;;;; hotkey is no longer available
                          (assoc-in [:world :remaining-hotkeys]
                              remaining-hotkeys))]
          new-state)
        state)))

(defn drop-item
  "Drop the item from the player's inventory whose hotkey matches `keyin`.
   Put the item in the player's current cell."
  [state keyin]
  {:pre  [(not (nil? state))]
   :post [(not (nil? %))]}
  (let [[player-cell x y] (rw/player-cellxy state)
        items (-> state :world :player :inventory)
        inventory-hotkeys (map #(% :hotkey) items)
        item-index (.indexOf inventory-hotkeys keyin)]
    (if (and (>= item-index 0) (< item-index (count items)))
      (let [item (nth items item-index)
            new-state (-> state
              (rc/append-log (format "You let the %s fall to the ground" (lower-case (get item :name))))
              ;; dup the item into cell
              (rw/conj-cell-items x y (dissoc item :wielded))
              ;; remove the item from inventory
              (assoc-in [:world :player :inventory]
               (vec (concat (subvec items 0 item-index)
                            (subvec items (inc item-index) (count items))))))]
              ;;;; hotkey is now  available
              ;(assoc-in [:world :remaining-hotkeys]
              ;  (vec (concat (subvec remaining-hotkeys 0 item-index)
              ;               (subvec remaining-hotkeys (inc item-index) (count remaining-hotkeys))))))]
        (log/debug "dropping at:" x y "item with index" item-index "item" item)
        (log/debug "new-state" new-state)
        new-state)
        state)))

(defn apply-bandage
  [state]
  {:pre  [(not (nil? state))]
   :post [(not (nil? %))]}
  (-> state
    (rp/dec-item-count :bandage)
    (assoc-in [:world :player :hp] (get-in state [:world :player :max-hp]))
    (assoc-in [:world :current-state] :normal)
    (rc/append-log "You apply the bandage.")))

(defn apply-flashlight
  [state]
  {:pre  [(not (nil? state))]
   :post [(not (nil? %))]}
  (let [item         (rp/inventory-id->item state :flashlight)
        item-state   (get item :state :off)]
    (case item-state
      :on
      (-> state
        (rp/update-inventory-item :flashlight (fn [item] (assoc item :state :off)))
        (rc/append-log "You turn the flashlight off.")
        (inc-time)
        (update-visibility)
        (assoc-in [:world :current-state] :normal))
      :off
      (if (pos? (get item :charge))
        (-> state
          (rp/update-inventory-item :flashlight (fn [item] (assoc item :state :on)))
          (rc/append-log "You turn the flashlight on.")
          (inc-time)
          (update-visibility)
          (assoc-in [:world :current-state] :normal))
        (-> state
          (inc-time)
          (rc/append-log state "You try turning the flashlight on, but nothing happens."))))))

(defn assoc-apply-item
  [state item]
  {:pre  [(not (nil? state))]
   :post [(not (nil? %))]}
  (assoc-in state [:world :apply-item] item))

(defn get-apply-item
  [state]
  {:pre  [(not (nil? state))]
   :post [(not (nil? %))]}
  (get-in state [:world :apply-item]))

(defn select-apply-item
  "Apply the item from the player's inventory whose hotkey matches `keyin`."
  [state keyin]
  {:pre  [(not (nil? state))]
   :post [(not (nil? %))]}
  (let [item (rp/inventory-hotkey->item state keyin)]
    (if item
      (let [id (get item :id)]
        (-> state
          (assoc-apply-item item)
          (as-> state
            (cond
              (= id :bandage)
                (apply-bandage state)
              (= id :flashlight)
                (apply-flashlight state)
              (= id :fishing-pole)
                (-> state
                  (rw/assoc-current-state :apply-item-normal)
                  (rc/ui-hint "Pick a direction to use the fishing pole."))
              (contains? #{:red-frog-corpse :orange-frog-corpse :yellow-frog-corpse
                           :green-frog-corpse :blue-frog-corpse :purple-frog-corpse} id)
                (-> state
                  (rw/assoc-current-state :apply-item-inventory)
                  (rc/ui-hint "Pick an item to apply the frog secretion to."))
              (= id :match)
                (-> state
                  (rw/assoc-current-state :apply-item-normal)
                  (rc/ui-hint "Pick a direction to use the match."))
              (= id :fire-plough)
                (-> state
                  (rw/assoc-current-state :apply-item-normal)
                  (rc/ui-hint "Pick a direction to use the fire plough"))
              (= id :hand-drill)
                (-> state
                  (rw/assoc-current-state :apply-item-normal)
                  (rc/ui-hint "Pick a direction to use the fire plough"))
              (= id :bow-drill)
                (-> state
                  (rw/assoc-current-state :apply-item-normal)
                  (rc/ui-hint "Pick a direction to use the fire plough"))
              (= id :plant-guide)
                (-> state
                  (rw/assoc-current-state :apply-item-inventory)
                  (rc/ui-hint "Pick an item to identify."))
              (= id :stick)
                (-> state
                  (rw/assoc-current-state :apply-item-normal)
                  (rc/ui-hint "Pick a direction to use the stick."))
              (= id :saw)
                (-> state
                  (rw/assoc-current-state :apply-item-normal)
                  (rc/ui-hint "Pick a direction to use the saw."))
              (= id :flint-axe)
                (-> state
                  (rw/assoc-current-state :apply-item-normal)
                  (rc/ui-hint "Pick a direction to use the axe."))
              (= id :obsidian-axe)
                (-> state
                  (rw/assoc-current-state :apply-item-normal)
                  (rc/ui-hint "Pick a direction to use the axe."))
              (= id :flint)
                (-> state
                  (rw/assoc-current-state :apply-item-inventory)
                  (rc/ui-hint "Pick an item on which to apply the flint."))
              (ig/is-sharp? item)
                (-> state
                  (rw/assoc-current-state :apply-item-inventory)
                  (rc/ui-hint "Pick an item to use the sharp item on."))
              (ig/is-fruit? item)
                (-> state
                  (rw/assoc-current-state :apply-item-body)
                  (rc/ui-hint "a-apply to skin, b-apply to tongue"))
              :else state))))
        state)))

(defn dig-hole
  "Dig in the ground, creating a hole."
  [state]
  (let [[x y] (rp/player-xy state)]
    (rw/assoc-cell state x y :type 
      (rr/rand-nth [:freshwater-hole :saltwater-hole :dry-hole]))))

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
        target-cell   (rw/get-cell state target-x target-y)
        new-state     (case direction
                        :left  :fishing-left
                        :right :fishing-right
                        :up    :fishing-up
                        :down  :fishing-down)]
    (if (rw/type->water? (get target-cell :type))
      (-> state
        (rc/append-log "You start fishing.")
        (rw/assoc-current-state new-state))
      (rc/append-log state "You can't fish here."))))

(defn do-fishing
  "Fish somewhere."
  [state]
  (let [p (rr/uniform-int 0 50)]
    ;; chance of catching a fish
    (cond
      (= p 0)
      ;; catch a fish
      (rp/add-to-inventory state [(ig/gen-corpse (mg/gen-random-monster 1 :water))])
      :else
      state)))

(defn start-fire
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
        target-cell   (rw/get-cell state target-x target-y)]
    (cond
      (rw/type->flammable? (get target-cell :type))
      (-> state
        (rc/append-log (format "You light the %s." (clojure.string/replace (name (get target-cell :type))
                                                                                            #"-"
                                                                                            " ")))
        (rw/assoc-cell target-x target-y :type :fire :fuel (if (= (get target-cell :type)
                                                               :campfire)
                                                          (rr/uniform-int 500 600)
                                                          (rr/uniform-int 100 300))))
      :else
      (rc/append-log state "You don't think that is flammable."))))

(defn apply-match
  "Light something on fire, creating chaos."
  [state direction]
  (-> state
    (rp/dec-item-count :match)
    (start-fire direction)))
    
(defn apply-fire-plough
  "Light something on fire, creating chaos."
  [state direction]
  (-> state
    (rp/player-update-thirst inc)
    (rp/dec-item-utility :fire-plough)
    (start-fire direction)))
    
(defn apply-hand-drill
  "Light something on fire, creating chaos."
  [state direction]
  (-> state
    (rp/player-update-hunger inc)
    (rp/dec-item-utility :hand-drill)
    (start-fire direction)))
    
(defn apply-bow-drill
  "Light something on fire, creating chaos."
  [state direction]
  (-> state
    (rp/dec-item-utility :bow-drill)
    (start-fire direction)))
    
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
        target-cell   (rw/get-cell state target-x target-y)]
    (log/info "saw dir" direction)
    (log/info "sawing at" target-x target-y)
    (if (contains? #{:tree :palm-tree :fruit-tree} (get target-cell :type))
      (-> state
        (rc/append-log "You saw the tree into logs.")
        ;; sawing = more hunger
        (update-in [:world :player :hunger] (partial + 10))
        ;; decrease item utility
        (rp/dec-item-utility :saw)
        (rw/update-cell target-x
                        target-y
                        (fn [cell] (-> cell
                                     (dissoc :harvestable)
                                     (assoc :type (rr/rand-nth [:dirt :gravel :tall-grass :short-grass]))
                                     (assoc :items (concat (get cell :items) 
                                                         (repeat (rr/uniform-int 1 2) (ig/gen-item :log))))))))
      state)))

(defn apply-plant-guide
  "Apply a plant-guide to the inventory item."
  [state item]
  (log/info "applying plant guide to" item)
  (log/info "identified"  (get-in state [:world :fruit :identified]))
  (if (ig/is-fruit? item)
    (-> state
      (rc/conj-in [:world :fruit :identified] (get item :id))
      (rc/append-log (format "Identified %s." (name (get item :id)))))
    (rc/append-log state (format "You're not able to identify the %s." (name (get item :id))))))


(defn apply-flint
  "Apply flint to the inventory item."
  [state item]
  (log/info "applying flint to" item)
  (if (ig/is-metal? item)
    (-> state
      (assoc-apply-item {:id :flint-and-steel})
      (rw/assoc-current-state :apply-item-normal)
      (rc/ui-hint "Pick a direction to start a fire."))
    (-> state
      (rc/append-log "You're not sure how to apply that")
      (rw/assoc-current-state :normal))))

(defn apply-sharp-item
  "Apply a sharp item to the inventory item."
  [state item]
  (log/info "applying sharp item to" item)
  (case (get item :id)
    :unhusked-coconut
    (-> state
      (rp/dec-item-count (get item :id))
      (rp/add-to-inventory [(ig/gen-item :coconut)]))
    :stick
    (-> state
      (rp/dec-item-count (get item :id))
      (rp/add-to-inventory [(ig/gen-item :sharpened-stick)]))
    state))


(defn apply-fruit-to-skin
  "Apply fruit to the skin. If it is poisonous, display a message in the future."
  [state item]
  (log/info "skin-identifiable" (get-in state [:world :fruit]))
  (as-> state state
    (rc/append-log state (format "You touch the %s to your skin." (get item :name)))
    (if (ig/skin-identifiable? state item)
      (assoc-in state [:world :player :skin-identify-activate-time]
                (apply min (remove nil?
                                   [(get-in state [:world :player :skin-identify-activate-time])
                                    (+ (rw/get-time state) (rr/uniform-int 5 10))])))
      state)))

(defn apply-fruit-to-tongue
  "Apply fruit to the tongue If it is poisonous, display a message in the future."
  [state item]
  (log/info "skin-identifiable" (get-in state [:world :fruit]))
  (as-> state state
    (rc/append-log state (format "You touch the %s to your tongue." (get item :name)))
    (if (ig/tongue-identifiable? state item)
      (assoc-in state [:world :player :tongue-identify-activate-time]
                (apply min (remove nil?
                                   [(get-in state [:world :player :tongue-identify-activate-time])
                                    (+ (rw/get-time state) (rr/uniform-int 5 10))])))
      state)))

(defn apply-frog-corpse
  "Apply the secretions of a frog corpse to an inventory item."
  [state frog-corpse target-item]
  (log/info "poisonous frogs" (get-in state [:world :frogs]))
  (as-> state state
    (rc/append-log state (format "You touch the %s to the %s." (get frog-corpse :name) (get target-item :name)))
    (if (= :arrow (get target-item :id))
      (-> state
        (rp/dec-item-count :arrow)
        (rp/dec-item-count (get frog-corpse :id))
        (rp/add-to-inventory (ig/id->item (-> (get target-item :name)
                                           (clojure.string/split #"-")
                                           first
                                           (str "-tipped-arrow")
                                           keyword))))
      (rc/append-log state "You rub it all over but nothing happens."))))

(defn apply-item
  "Applies the selected item."
  [state keyin]
  {:pre  [(not (nil? state))]
   :post [(not (nil? %))]}
  (let [item (get-apply-item state)
        trans->dir? (comp is-direction? translate-directions)]
    (log/info "apply-item" [item keyin])
    (log/info "is-direction?" ((comp is-direction? translate-directions) keyin))
    (rm/first-vec-match [(get item :id) keyin]
      [:fishing-pole    trans->dir?] (apply-fishing-pole state (translate-directions keyin))
      [:match           trans->dir?] (-> state
                                       (apply-match (translate-directions keyin))
                                       (rw/assoc-current-state :normal))
      [:fire-plough     trans->dir?] (-> state
                                       (apply-fire-plough (translate-directions keyin))
                                       (rw/assoc-current-state :normal))
      [:hand-drill      trans->dir?] (-> state
                                       (apply-hand-drill (translate-directions keyin))
                                       (rw/assoc-current-state :normal))
      [:bow-drill       trans->dir?] (-> state
                                       (apply-bow-drill (translate-directions keyin))
                                       (rw/assoc-current-state :normal))
      [:plant-guide     :*         ] (if-let [item (rp/inventory-hotkey->item state keyin)]
                                       (-> state
                                         (apply-plant-guide item)
                                         (rw/assoc-current-state  :normal))
                                       state)
      [:stick           \>         ] (-> state
                                       (dig-hole)
                                       (rw/assoc-current-state :normal))
      [:flint-axe       trans->dir?] (-> state
                                       (saw (translate-directions keyin) keyin)
                                       (rw/assoc-current-state :normal))
      [:obsidian-axe    trans->dir?] (-> state
                                       (saw (translate-directions keyin) keyin)
                                       (rw/assoc-current-state :normal))
      [:saw             trans->dir?] (-> state
                                       (saw (translate-directions keyin) keyin)
                                       (rw/assoc-current-state :normal))
      [:flint           :*         ] (if-let [item (rp/inventory-hotkey->item state keyin)]
                                       (apply-flint state item)
                                       state)
      [:flint-and-steel trans->dir?] (-> state
                                       (start-fire (translate-directions keyin))
                                       (rw/assoc-current-state :normal))
      [ig/id-is-sharp?  :*         ] (if-let [item (rp/inventory-hotkey->item state keyin)]
                                       (-> state
                                         (apply-sharp-item item)
                                         (rw/assoc-current-state :normal))
                                       state)
      ;; apply fruit to body
      [ig/id-is-fruit?  \a         ] (-> state
                                       (apply-fruit-to-skin item)
                                       (rw/assoc-current-state :normal))
      [ig/id-is-fruit?  \b         ] (-> state
                                       (apply-fruit-to-tongue item)
                                       (rw/assoc-current-state :normal))
      [#{:red-frog-corpse
         :orange-frog-corpse
         :yellow-frog-corpse
         :green-frog-corpse
         :blue-frog-corpse
         :purple-frog-corpse}
                      :*          ] (if-let [target-item (rp/inventory-hotkey->item state keyin)]
                                      (-> state
                                        (apply-frog-corpse (get item :id) target-item)
                                        (rw/assoc-current-state  :normal))
                                      state)
      [:*              :*         ] (-> state
                                      (rc/ui-hint "You're not sure how to apply it to that.")
                                      (rw/assoc-current-state :normal)))))

(defn action-select
  [state]
  (let [actions (as-> [] actions
                  (if (-> (rw/player-cellxy state) first :items count pos?)
                    (conj actions {:state-id :pickup
                                   :name "Pickup"})
                    actions))]
    (assoc-in state
              [:world :action-select]
              (map (fn [action hotkey]
                     (assoc action :hotkey hotkey))
                   actions
                   rc/hotkeys))))

(defn do-selected-action
  [state keyin]
  (if-let [action (first (filter (fn [{:keys [state-id hotkey]}]
                                   (= hotkey keyin))
                                 (get-in state [:world :action-select])))]
    (rw/assoc-current-state state (get action :state-id))
    state))

(defn quaff-only-adjacent-cell
  "Drink all adjacent cells."
  [state]
  (let [[state water] 
        (reduce (fn [[state water] [x y]]
                  (log/info "testing cell for water:" (rw/get-cell state x y))
                  (if (contains? (rw/get-cell state x y) :water)
                    [(rw/update-cell state x y (fn [cell] (assoc cell :water 0))) (+ water (get (rw/get-cell state x y) :water 0))]
                    [state water]))
                [state 0]
                (rw/player-adjacent-xys state))]
    (log/info "player-adj-xys" (rw/player-adjacent-xys state))
    (update-in state [:world :player :thirst] (fn [thirst] (min 0 (- thirst water))))))

(defn quaff-select
  "Select the next state depending on what quaffable items are available."
  [state]
  (let [num-adjacent-quaffable-cells  (count
                                        (filter (fn [cell] (and (not (nil? cell))
                                                                (contains? #{:freshwater-hole :saltwater-hole} (get cell :type))
                                                                (> (get cell :water) 10)))
                                                (rw/player-adjacent-cells state)))
        _ (log/info "player-adj-cells" (rw/player-adjacent-cells state))
        quaffable-inventory-item? (some (fn [item] (contains? item :thirst)) (rp/player-inventory state))]
    (cond
      (and (pos? num-adjacent-quaffable-cells)
           quaffable-inventory-item?)
        (rw/assoc-current-state state :quaff-adj-or-inv)
      (= num-adjacent-quaffable-cells 1)
        (quaff-only-adjacent-cell state)
      (> num-adjacent-quaffable-cells 1)
        (-> state
          (rw/assoc-current-state :quaff-adj)
          (rc/ui-hint "Pick a direction to drink."))
      quaffable-inventory-item?
        (rw/assoc-current-state state :quaff-inventory)
      :else
        (-> state
          (rc/ui-hint "There is nothing to drink.")
          (rw/assoc-current-state :normal)))))

(defn quaff-cell-at-pos
  [state {x :x y :y}]
  (let [water (get (rw/get-cell state x y) :water)]
    (-> state
      (rw/assoc-cell state x y :water 0)
      (update-in [:world :player :thirst] (fn [thirst] (min 0 (- thirst water)))))))

(defn quaff-up
  [state]
  (quaff-cell-at-pos state (rw/player-adjacent-pos state :up)))

(defn quaff-down
  [state]
  (quaff-cell-at-pos state (rw/player-adjacent-pos state :down)))

(defn quaff-left
  [state]
  (quaff-cell-at-pos state (rw/player-adjacent-pos state :left)))

(defn quaff-right
  [state]
  (quaff-cell-at-pos state (rw/player-adjacent-pos state :right)))

(defn quaff-inventory
  [state keyin]
  (if-let [item (rp/inventory-hotkey->item state keyin)]
    (-> state
      (rc/append-log (format "The %s tastes %s." (lower-case (get item :name))
                                              (rr/rand-nth ["great" "foul" "greasy" "delicious" "burnt" "sweet" "salty"])))
      (rp/update-eaten item)
      ;; reduce thirst
      (update-in [:world :player :thirst]
        (fn [thirst]
          (let [new-thirst (- thirst (item :thirst))]
            (max 0 new-thirst))))
      ;; remove the item from inventory
      (rp/remove-from-inventory (get item :id))
      (as-> state
        (case (get item :id)
          :coconut (rp/add-to-inventory state [(ig/gen-item :coconut-empty)])
          state)))
     state))
     
(defn do-rest
  "NOP action. Player's hp increases a little."
  [state]
  {:pre  [(not (nil? state))]
   :post [(not (nil? %))]}
  (as-> state state
    ;; rest better then in shelter
    (if (let [[cell _ _] (rw/player-cellxy state)]
          (rw/type->shelter? (get cell :type)))
      (update-in state [:world :player :will-to-live]
        (fn [will-to-live] (+ 0.05 will-to-live)))
      state)
    ;; rest better when near a fire
    (if (some #{:fire :campfire} (map :type (rw/player-adjacent-cells state)))
      (update-in state [:world :player :will-to-live]
        (fn [will-to-live] (+ 0.05 will-to-live)))
      state)
    (update-in state [:world :player]
               (fn [player] (if (< (int (get player :hp)) (get player :max-hp))
                              (assoc-in player [:hp] (+ (get player :hp) 0.05))
                              player)))))

(defn do-sleep
  "Sleep."
  [state keyin]
  (let [d (rlos/sight-distance state)]
    (if (> d 2)
      (rw/assoc-current-state state :normal)
      (do-rest state))))

(defn eat
  "Remove the item whose `:hotkey` equals `keyin` and subtract from the player's
   hunger the item's `:hunger` value."
  [state keyin]
  (log/info "poisoned fruit" (get-in state [:world :fruit :poisonous]))
  (if-let [item (rw/inventory-and-player-cell-hotkey->item state keyin)]
    (-> state
      (rc/append-log (format "The %s tastes %s." (lower-case (get item :name))
                                              (rr/rand-nth ["great" "foul" "greasy" "delicious" "burnt" "sweet" "salty"])))
      (rp/update-eaten item)
      ;; reduce hunger
      (update-in [:world :player :hunger]
        (fn [hunger]
          (let [new-hunger (- hunger (item :hunger))]
            (max 0 new-hunger))))
      (as-> state
        (if (contains? (set (map :hotkey (rp/player-inventory state))) keyin)
          ;; remove the item from inventory
          (rp/dec-item-count state (get item :id))
          ;; remove the item from the current-cell
          (rw/dec-cell-item-count state (get item :id)))
        (if (= (get item :id) :coconut-empty)
          (rp/add-to-inventory state [(ig/gen-item :coconut-shell)])
          state))
      ;; if the item was a poisonous fruit, set a poisoned timebomb
      (as-> state
        (if (and (ig/is-fruit? item)
                 (contains? (set (get-in state [:world :fruit :poisonous])) (get item :id)))
          (do
            (log/info "Ate poisoned fruit." item)
            (assoc-in state
                      [:world :player :poisoned-time]
                      (apply min (remove nil?
                                         [(get-in state [:world :player :poisoned-time])
                                          (+ (rw/get-time state) (rr/uniform-int 100 200))]))))
          state)))
    state))

(defn init-cursor
  "Initialize the selection cursor at the player's current location."
  [state]
  (let [player-xy    (rp/player-xy state)
        viewport-xy  (rv/viewport-xy state)
        cursor-pos   (apply rc/xy->pos (rv/-xy player-xy viewport-xy))]
      ; cursor pos = player pos in screen coordinates
      (assoc-in state [:world :cursor] cursor-pos)))

(defn harvest
  "Collect non-item resources from adjacent or current cell"
  [state direction]
  {:pre  [(contains? #{:left :right :up :down :up-left :up-right :down-left :down-right :center} direction)]}
  (let [player-x      (-> state :world :player :pos :x)
        player-y      (-> state :world :player :pos :y)
        distance      (rp/player-distance-from-starting-pos state)
        target-x      (+ player-x (case direction
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
        target-cell   (rw/get-cell state target-x target-y)
        harvestable   (get target-cell :harvestable false)
        harvest-items (if (not= target-cell nil)
                        (cond
                          (= (get target-cell :type) :tree)
                            (if (or harvestable
                                    (= 0 (rr/uniform-int 1000)))
                              [(rr/rand-nth [(ig/gen-item :stick) (ig/gen-item :plant-fiber)])]
                              [])
                          (= (get target-cell :type) :bamboo)
                              (if (or harvestable
                                      (= 0 (rr/uniform-int 1000)))
                                [(ig/gen-item :bamboo)]
                                [])
                          (= (get target-cell :type) :palm-tree)
                            (concat
                              (if (or harvestable
                                      (= 0 (rr/uniform-int 1000)))
                                [(rr/rand-nth [(ig/gen-item :unhusked-coconut) (ig/gen-item :plant-fiber)])]
                                []))
                          (and (= (get target-cell :type) :tall-grass)
                               (= direction :center))
                            (concat
                              (if (or harvestable
                                      (= 0 (rr/uniform-int 1000)))
                                [(rr/rand-nth [(ig/gen-item :grass) (ig/gen-item :plant-fiber)])]
                                []))
                          (and (= (get target-cell :type) :gravel)
                               (= direction :center))
                             (if (get target-cell :near-lava)
                               (concat
                                 (if (or harvestable
                                         (= 0 (rr/uniform-int 1000)))
                                   [(rr/rand-nth [(ig/gen-item :rock) (ig/gen-item :obsidian)])]
                                   []))
                               (concat
                                 (if (or harvestable
                                         ;; TODO: make large flint stone have a higher chance of dropping away
                                         ;; from the player's starting-pos.
                                         (= 0 (rr/uniform-int 1000)))
                                   [(rr/rand-nth [(ig/gen-item :rock) (if (< 300 (rr/uniform-double 100 (max 101 distance)))
                                                                        (ig/gen-item :large-flint)
                                                                        (ig/gen-item :flint))])]
                                   [])))
                          :else [])
                        [])]
    (log/info "harvested" harvest-items)
    (if (empty? harvest-items)
      (rc/append-log state "You don't find anything.")
      (-> state
        (rp/add-to-inventory harvest-items)
        (rw/update-cell target-x target-y (fn [cell] (dissoc cell :harvestable)))
        (as-> state (reduce rp/update-harvested state harvest-items))
        (rc/append-log (format "You gather %s." (clojure.string/join ", " (map #(if (> (get % :count 1) 1)
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

(defn harvest-up-left [state]
  (harvest state :up-left))

(defn harvest-up-right [state]
  (harvest state :up-right))

(defn harvest-down-left [state]
  (harvest state :down-left))

(defn harvest-down-right [state]
  (harvest state :down-right))

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
              (rc/append-log (format "You wield the %s." (lower-case (get selected-item :name))))
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
  [state & more]
  (rc/dissoc-in state [:world :cursor]))

(defn move-cursor
  [state direction]
  (let [{cursor-x :x
         cursor-y :y} (get-in state [:world :cursor])
        target-x (+ cursor-x (case direction
                               :left -1
                               :right 1
                               :up-left -1
                               :up-right 1
                               :down-left -1
                               :down-right 1
                               0))
        target-y (+ cursor-y (case direction
                               :up  -1
                               :down 1
                               :up-left -1
                               :up-right -1
                               :down-left 1
                               :down-right 1
                               0))
        cursor-pos (rc/xy->pos (rc/bound 0 target-x (dec (get-in state [:world :viewport :width])))
                               (rc/bound 0 target-y (dec (get-in state [:world :viewport :height]))))]
    (assoc-in state [:world :cursor] cursor-pos)))

(defn move-cursor-left
  "Move the cursor pos one space to the left keeping in mind the bounds of the current place."
  [state]
  (move-cursor state :left))

(defn move-cursor-right
  "Move the cursor pos one space to the right keeping in mind the bounds of the current place."
  [state]
  (move-cursor state :right))

(defn move-cursor-up
  "Move the cursor pos one space up keeping in mind the bounds of the current place."
  [state]
  (move-cursor state :up))

(defn move-cursor-down
  "Move the cursor pos one space down keeping in mind the bounds of the current place."
  [state]
  (move-cursor state :down))

(defn move-cursor-up-left
  "Move the cursor pos one space to the left keeping in mind the bounds of the current place."
  [state]
  (move-cursor state :up-left))

(defn move-cursor-up-right
  "Move the cursor pos one space to the right keeping in mind the bounds of the current place."
  [state]
  (move-cursor state :up-right))

(defn move-cursor-down-left
  "Move the cursor pos one space up keeping in mind the bounds of the current place."
  [state]
  (move-cursor state :down-left))

(defn move-cursor-down-right
  "Move the cursor pos one space down keeping in mind the bounds of the current place."
  [state]
  (move-cursor state :down-right))

(defn describe-at-cursor
  "Add to the log, a message describing the scene at the cell indicated by the
   cursor's position."
  [state]
  (let [cursor-pos (get-in state [:world :cursor])
        [x y]      (rv/viewport-xy state)]
    (-> state
      (rc/append-log (rdesc/describe-cell-at-xy state (+ x (cursor-pos :x)) (+ y (cursor-pos :y))))
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
    (log/debug "start-talking")
    (if-let [target-npc (rw/npc-at-xy state target-x target-y)]
      ;; store update the npc and world state with talking
      (let [state (-> state
                      (rnpc/update-npc-at-xy target-x target-y #(assoc % :talking true))
                      (rw/assoc-current-state :talking))
            fsm (get-in state [:dialog (target-npc :id)])
            ;; get the dialog input options for the npc
            valid-input (rdiag/get-valid-input fsm)]
        ;; if the first option is `nil` then advance the fsm one step.
        ;; this auto step can be used to have the npc speak first when approached.
        (if (nil? (first valid-input))
          (rdiag/step-fsm state fsm nil)
          state))
      (rw/assoc-current-state state :normal))))

(defn talk-left [state]
  (start-talking state :left))

(defn talk-right [state]
  (start-talking state :right))

(defn talk-up [state]
  (start-talking state :up))

(defn talk-down [state]
  (start-talking state :down))

(defn talk [state keyin]
  (let [npc (first (rnpc/talking-npcs state))
        fsm (get-in state [:dialog (get npc :id)])
        valid-input (rdiag/get-valid-input fsm)
        options (zipmap (take (count valid-input) [\a \b \c \d \e \f]) valid-input)
        input (get options keyin)
        _ (log/debug "Stepping fsm. valid-input:" valid-input)
        _ (log/debug "Stepping fsm. options:" options)
        _ (log/debug "Stepping fsm. input:" input)]
    (rdiag/step-fsm state fsm input)))

(defn stop-talking
  "Remove :talking key from all npcs."
  [state]
  (log/debug "stop-talking")
  (-> state
      (rc/map-in [:world :npcs] (fn [npc] (dissoc npc :talking)))
      (rw/assoc-current-state :normal)))

(defn describe-inventory
  "Add to the log, a message describing the item with the `:hotkey` value
   matching `keyin`."
  [state keyin]
  (let [items (-> state :world :player :inventory)
        inventory-hotkeys (map #(% :hotkey) items)
        item-index (.indexOf inventory-hotkeys keyin)]
    (if (and (>= item-index 0) (< item-index (count items)))
      (let [item (nth items item-index)
            new-state (rc/append-log state
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
        (rnpc/update-npc npc #(assoc % :shopping true))
        (rw/assoc-current-state :shopping)))
    
(defn shop
  "Start shopping. Allows the player to select \\a -buy or \\b - sell."
  [state keyin]
  (case keyin
    \a (rw/assoc-current-state state :buy)
    \b (rw/assoc-current-state state :sell)
    :else state))

(defn buy
  "Buy an item from an npc in exchange for money."
  [state keyin]
  (let [npc       (first (rnpc/talking-npcs state))
        options   (zipmap [\a \b \c \d \e \f]
                          (get npc :inventory []))
        item      (get options keyin)]
    (if (and item (< (item :price)
                     (get-in state [:world :player :$])))
      (-> state
          (update-in [:world :player :$] (fn [gold] (- gold (item :price))))
          (rnpc/transfer-items-from-npc-to-player (get npc :id) (partial = item)))
      state)))


(defn sell
  "Sell an item to an npc in exchange for money."
  [state keyin]
  (let [npc       (first (rnpc/talking-npcs state))
        buy-fn    (get-in state (get npc :buy-fn-path) (fn [_] nil))
        sellable-items (filter #(not (nil? (buy-fn %)))
                                (get-in state [:world :player :inventory]))
        options   (apply hash-map
                         (mapcat (fn [item] [(item :hotkey) item]) sellable-items))
        _ (log/debug "sellable items" sellable-items)
        _ (log/debug "Sell options" options)]
    (if (contains? options keyin)
      (let [item  (get options keyin)
            price (buy-fn item)
            _ (log/debug "current $" (get-in state [:world :player :$]))]
        (-> state
            (update-in [:world :player :$] (fn [gold] (+ gold price)))
            (rnpc/transfer-items-from-player-to-npc (get npc :id) (partial = item))))
        state)))

(defn craft-select-recipe
  "Selects a craft recipe."
  [state keyin]
  (let [recipe-type      (case (rw/current-state state)
                           :craft-weapon   :weapons
                           :craft-survival :survival
                           :craft-shelter :shelter
                           :craft-transportation :transportation)
        matching-recipes (filter (fn [recipe] (= (get recipe :hotkey) keyin))
                                 (get (rcraft/get-recipes state) recipe-type))]
    (log/info "selecting matching recipe" matching-recipes)
    (if (empty? matching-recipes)
      (rc/ui-hint state "Pick a valid recipe.")
      (let [recipe-path [recipe-type (get (first matching-recipes) :hotkey)]]
        (log/info "selecting recipe path" recipe-path)
        (assoc-in state [:world :craft-recipe-path] recipe-path)))))

(defn assoc-throw-item
  [state item]
  (assoc-in state [:world :throw-item] item))

(defn get-throw-item
  [state]
  (get-in state [:world :throw-item]))

(defn throw-select-inventory
  [state keyin]
  (if-let [item (rw/inventory-and-player-cell-hotkey->item state keyin)]
    (-> state
      (rc/ui-hint "Select direction.")
      (assoc-throw-item item))
    state))

(defn throw-selected-inventory
  [state direction]
  {:pre  [(contains? #{:left :right :up :down} direction)]}
  (let [item           (get-throw-item state)
        throw-distance 5
        obj            (rw/first-collidable-object state direction throw-distance)]
    (cond
      (contains? obj :npc)
        ;; do attack
        (-> state
          (rcombat/attack [:world :player] (rnpc/npc->keys state (get obj :npc)) item)
          (rw/conj-cell-items (get-in obj [:pos :x]) (get-in obj [:pos :y]) item)
          (rp/dec-item-count (get item :id)))
      (contains? obj :cell)
        ;; drop item into cell before hitting colliding cell
        (as-> state state
          (if (= (get-in obj [:cell :type]) :fire)
            (rw/update-cell state (get-in obj [:pos :x]) (get-in obj [:pos :y]) (fn [cell] (update-in cell [:fuel] (partial + (ig/id->fuel (get item :id))))))
            (rw/conj-cell-items state
                                (+ (get-in obj [:pos :x])
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
                                  (assoc item :rot-time (inc (rw/get-time state)))
                                  item)))
          (rp/dec-item-count state (get item :id)))
      :else
        ;; didn't hit anything, drop into cell at max-distance
        (let [[x y] (nth (rw/direction->xys state direction) throw-distance)]
          (-> state
            (rw/conj-cell-items x y item)
            (rp/dec-item-count (get item :id)))))))

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
  (craft-select-recipe (rw/assoc-current-state state :craft-weapon) \a))

(defn craft-survival
  [state]
  (craft-select-recipe (rw/assoc-current-state state :craft-survival) \a))

(defn craft-shelter
  [state]
  (craft-select-recipe (rw/assoc-current-state state :craft-shelter) \a))

(defn craft-transportation
  [state]
  (craft-select-recipe (rw/assoc-current-state state :craft-transportation) \a))


(defn craft
  "Craft the selected recipe."
  [state]
  (let [[recipe-type hotkey] (get-in state [:world :craft-recipe-path])
        recipes (get (rcraft/get-recipes state) recipe-type)
        recipe (first (filter #(= hotkey (get % :hotkey))
                              recipes))]
    (log/info "recipe-type" recipe-type "hotkey" hotkey "recipes" recipes "recipe" recipe)
    (if recipe
      (-> state
        (rcraft/craft-recipe recipe)
        (rw/assoc-current-state :normal))
      (rc/append-log state "Pick a valid recipe." :white))))


(defn scroll-log-up
  [state]
  (let [t        (rw/get-time state)
        log-idx  (get-in state [:world :log-idx])
        num-logs (count (filter #(= t (get % :time)) (get-in state [:world :log])))]
    (update-in state [:world :log-idx] (fn [idx] (min (dec num-logs) (inc log-idx))))))

(defn scroll-log-down
  [state]
  (let [t        (rw/get-time state)
        log-idx  (get-in state [:world :log-idx])]
    (update-in state [:world :log-idx] (fn [idx] (max 0 (dec log-idx))))))

(defn use-ability
  [state keyin]
  (if-let [ability (rp/hotkey->player-ability state keyin)]
    (as-> state state
      (case (get ability :id)
        :wtl->hp             (rp/wtl->hp state)
        :wtl->thirst         (rp/wtl->thirst state)
        :wtl->hunger         (rp/wtl->hunger state)
        :wtl->strength-buff  (rp/wtl->strength-buff state)
        :wtl->dexterity-buff (rp/wtl->dexterity-buff state)
        :wtl->speed-buff     (rp/wtl->speed-buff state)
        :wtl->toughness-buff (rp/wtl->toughness-buff state)
        (assert false (format "Ability [%s] not found." (str ability))))
      (rw/assoc-current-state state :normal))
    state))

(defn reducing-join
  "If coll is empty, it is returned.
   If coll has one element it is returned.
   If coll has two elements, the collection [(f (first coll) (second coll))]
   is returned."
  [f coll]
  (if (> (count coll) 1)
    (reduce (fn [acc v]
              (concat (butlast acc) (f (last acc) v)))
            [(first coll)]
            (rest coll))
    coll))

(defn coalesce-log-group
  [logs]
  (reducing-join (fn [loga logb]
                   (if (< (+ (-> loga :text count)
                             (-> logb :text count))
                          70)
                     ;; join short lines
                     [{:time (get loga :time)
                       :text (clojure.string/join " " [(get loga :text)
                                                       (get logb :text)])
                       :color (get loga :color)}]
                     ;; pass long lines through
                     [loga logb]))
                  logs))
       

;; Logs is a vector with elements of {:text :color :time}
(defn coalesce-logs
  [state]
  (let [now (rw/get-time state)]
    (-> state
      (assoc-in [:world :log-idx] 0)
      (update-in [:world :log]
        (fn [logs]
          (log/debug "updating :world :log. logs" logs)
          (vec
            (mapcat
              (fn [logs-with-same-time]
                (if (= (-> logs-with-same-time first :time) now)
                  (coalesce-log-group logs-with-same-time)
                   logs-with-same-time))
              (partition-by (juxt :time :color) logs))))))))

(defn get-hungrier-and-thirstier
  "Increase player's hunger."
  [state]
  {:pre [(not (nil? state))]
   :post [(not (nil? %))]}
  (let [hunger (get-in state [:world :player :hunger])
        thirst (get-in state [:world :player :thirst])]
    (as-> state state
      (if (= (rw/current-state state) :sleep)
        (-> state
          (update-in [:world :player :hunger] (partial + 0.01))
          (update-in [:world :player :thirst] (partial + 0.05)))
        (-> state
          (update-in [:world :player :hunger] (partial + 0.05 (* 0.02 (count (rp/player-inventory state)))))
          (update-in [:world :player :thirst] (partial + 0.1))))
      (if (> (rp/player-hunger state) (rp/player-max-hunger state))
        (do
          (log/info "Player died from hunger")
          (-> state
            (rc/conj-in [:world :player :status] :dead)
            (rp/update-player-died :hunger)))
        state)
      (if (> (rp/player-thirst state) (rp/player-max-thirst state))
        (-> state
          (rc/conj-in [:world :player :status] :dead)
          (rp/update-player-died :thirst))
        state)
      (let [new-hunger (get-in state [:world :player :hunger])]
        (log/info "hunger" hunger "new-hunger" new-hunger)
        (if (< hunger 80 new-hunger)
          (rc/append-log state "You need to eat now." :yellow)
          state))
      (let [new-thirst (get-in state [:world :player :thirst])]
        (log/info "thirst" thirst "new-thirst" new-thirst)
        (if (< thirst 80 new-thirst)
          (rc/append-log state "You need to drink something now." :blue)
          state)))))

(defn get-rescued
  [state]
  {:pre [(not (nil? state))]
   :post [(not (nil? %))]}
  (if (rw/player-mounted-on-raft? state)
    (let [{x :x y :y}         (rp/player-pos state)
          distance-from-start (rc/chebyshev-distance {:x 0 :y 0}
                                                     (rp/player-pos state))
          chance-of-rescue (max 0 (+ (/ -300.0 distance-from-start) 1.5))]
      (log/info "distance from 0,0" distance-from-start)
      (if (< (rr/uniform-double 1) chance-of-rescue)
        (assoc-in state [:world :current-state] :rescued)
        state))
    state))

(defn decrease-will-to-live
  "Decrease the player's will-to-live depending on circumstances."
  [state]
  (if (= (rw/current-state state) :sleep)
    (let [[cell _ _] (rw/player-cellxy state)
          bedroll?   (contains? (set (map :id (get cell :items []))) :bedroll)]
      (if bedroll?
        (rp/player-update-wtl state
          (fn [will-to-live] (+ 0.05 will-to-live)))
        state))
    (-> state
      (rp/player-update-wtl
        (fn [will-to-live]
          (let [dwtl       0.001 ;; you've been on the island. It sucks and you want to get off.
                ;; if it is night and the player is not within range of a fire, things are extra tough.
                dwtl       (if (and (rw/is-night? state)
                                    (not-any? #(= (get % :type) :fire)
                                              (rw/cells-in-range-of-player state 3)))
                             (+ dwtl 0.002)
                             dwtl)
                hp         (rp/player-hp state)
                max-hp     (rp/player-max-hp state)
                _          (log/info "hp" hp "max-hp" max-hp)
                dwtl       (+ dwtl (if (> 0.5 (/ hp max-hp))
                                     0.1
                                     0))
                hunger     (rp/player-hunger state)
                max-hunger (rp/player-max-hunger state)
                _          (log/info "hunger" hunger "max-hunger" max-hunger)
                dwtl       (+ dwtl (if (> (/ hunger max-hunger) 0.8)
                                     0.1
                                     0))
                thirst     (rp/player-thirst state)
                max-thirst (rp/player-max-thirst state)
                _          (log/info "thirst" thirst "max-thirst" max-thirst)
                dwtl       (+ dwtl (if (> (/ thirst max-thirst) 0.5)
                                     0.1
                                     0))
                wounded    (rp/player-wounded? state)
                _          (log/info "wounded" wounded)
                dwtl       (+ dwtl (if wounded
                                     0.21
                                     0))
                poisoned   (rp/player-poisoned? state)
                _          (log/info "poisoned" poisoned)
                dwtl       (+ dwtl (if poisoned
                                     0.3
                                     0))
                infected   (rp/player-infected? state)
                _          (log/info "infected" infected)
                dwtl       (+ dwtl (if infected
                                     0.41
                                     0))]
          (log/info "dwtl" dwtl "will-to-live" will-to-live)
          (- will-to-live dwtl))))
      (update-in [:world :player :status]
                 (fn [status]
                   (set (if (neg? (rp/player-wtl state))
                          (conj status :dead)
                          status))))
      (as-> state
        (if (rp/player-status-contains? state :dead)
          (rp/update-player-died state :zero-will-to-live)
          state)))))

(defn log-will-to-live-flavor
  "Log a flavor message when will-to-live increases or decreases by a lot."
  [state prev-will-to-live]
  {:pre [(not (nil? state))]
   :post [(not (nil? %))]}
  (let [dwtl (- prev-will-to-live (get-in state [:world :player :will-to-live]))]
    (if (> (Math/abs dwtl) 1.5)
      (if (neg? dwtl)
        (rc/append-log state (format "You feel %s." (rr/rand-nth ["happy" "happier" "glad" "elated" "great" "good"])))
        (rc/append-log state (format "You feel %s." (rr/rand-nth ["sad" "sadder" "down" "unhappy" "bummed" "bad"]))))
      state)))

(defn maybe-gain-level
  [state]
  (if (> (rp/player-xp->level state) (rp/player-level state))
    (-> state
      (rp/inc-player-level)
      (assoc-in [:world :ability-choices] (take 3 (rr/rnd-shuffle (rp/applicable-abilities state))))
      ;; TODO: pick three random player abilities
      (rw/assoc-current-state :gain-level))
    state))

(defn choose-ability
  [state keyin]
  (if-let [ability (first (filter (fn [ability] (= keyin (get ability :hotkey)))
                                  (get-in state [:world :ability-choices])))]
                                  
    (-> state
      (rp/player-gain-ability (get ability :id))
      (rw/assoc-current-state :normal))
    state))

(defn fruit-identify-activate
  "Display a message if the fruit being identified is poisonous."
  [state]
  {:pre [(not (nil? state))]
   :post [(not (nil? %))]}
  (log/info "time" (rw/get-time state))
  (log/info "skin-id-time" (get-in state [:world :player :skin-identify-activate-time]))
  (log/info "tongue-id-time" (get-in state [:world :player :tongue-identify-activate-time]))
  (as-> state state
    (if (let [activate-time (get-in state [:world :player :skin-identify-activate-time])]
          (and activate-time
               (< activate-time (rw/get-time state))))
      (-> state
        (rc/append-log (rr/rand-nth ["Your skin feels burning."
                                  "Your skin is numb."
                                  "Your skin is has a rash."
                                  "Your skin hurts."]))
        (rc/dissoc-in [:world :player :skin-identify-activate-time]))
      state)
    (if (let [activate-time (get-in state [:world :player :tongue-identify-activate-time])]
          (and activate-time
               (< activate-time (rw/get-time state))))
      (-> state
        (rc/append-log (rr/rand-nth ["Your tongue feels burning."
                                  "Your tongue is numb."
                                  "Your tongue hurts."]))
        (rc/dissoc-in [:world :player :tongue-identify-activate-time]))
      state)))

(defmacro if-player-attribute-elapsed
  [state player-key true-expr false-expr]
  `(if (let [time-value# (rp/get-player-attribute ~state ~player-key)]
         (and (some? time-value#)
              (< time-value# (rw/get-time ~state))))
     ~true-expr
     ~false-expr))

(defn if-poisoned-get-hurt
  "Decrease player's hp if they are poisoned."
  [state]
  {:pre [(not (nil? state))]
   :post [(not (nil? %))]}
  (as-> state state
    (if (let [poisoned-time (get-in state [:world :player :poisoned-time])]
          (and poisoned-time
               (< poisoned-time (rw/get-time state))))
        (-> state
          (rc/conj-in [:world :player :status] :poisoned)
          (rc/append-log "You vomit.")
          (rp/player-update-hunger (fn [hunger] (min (+ hunger 30)
                                                      (rp/player-max-hunger state))))
          (rp/player-update-thirst (fn [thirst] (min (+ thirst 30)
                                                     (rp/player-max-thirst state))))
          (rc/dissoc-in [:world :player :poisoned-time]))
        state)
    (rp/player-update-hp state (fn [hp]
                                 (if (rp/player-status-contains? state :poisoned)
                                   (- hp 0.01)
                                   (min (rp/player-max-hp state)
                                        (+ hp 0.02)))))))
(defn check-paralyzed
  "Set and upset paralyzation player attribute."
  [state]
  {:pre [(not (nil? state))]
   :post [(not (nil? %))]}
  (as-> state state
    ;; after delay, make player paralyzed
    (if-player-attribute-elapsed state :paralyed-start-time
      (-> state
        ;; make player paralyzed
        (rp/assoc-player-attribute :paralyzed-expire-time)
        ;; clean up delay-time attribute
        (rp/dissoc-player-attribute :paralyzed-start-time)
      state)
    ;; after paralyzation, un-paralyze player
    (if-player-attribute-elapsed state :paralyed-expire-time
      ;; make player not paralyzed
      (rp/dissoc-player-attribute state :paralyzed-expire-time)
      state))))

(defn if-wounded-get-infected
  [state]
  {:pre [(not (nil? state))]
   :post [(not (nil? %))]}
  (let [hp                  (rp/player-hp state)
        max-hp              (rp/player-max-hp state)
        chance-of-infection (inc (/ -1 (inc (/ hp (* max-hp 20)))))] 
    (if (and (not-empty (get-in state [:world :player :wounds]))
             (< (rr/uniform-double 1) chance-of-infection))
      (-> state
        (rp/update-player-status
          (fn [status]
              (conj status :infected)))
        (rc/append-log "Your wounds have become infected." :green))
      state)))

(defn if-infected-get-hurt
  [state]
  {:pre [(not (nil? state))]
   :post [(not (nil? %))]}
  (rp/player-update-hp state
    (fn [hp]
      (if (rp/player-status-contains? state :infected)
        (- hp 0.2)
        hp))))

(defn if-near-too-much-fire-get-hurt
  [state]
  {:pre [(not (nil? state))]
   :post [(not (nil? %))]}
  (rp/player-update-hp state
    (fn [hp]
      (if (>= (count (filter #(= (get % :type) :fire)
                             (rw/player-adjacent-cells-ext state)))
              3)
        (dec hp)
        hp))))

(defn heal
  [state]
  {:pre [(not (nil? state))]
   :post [(not (nil? %))]}
  (as-> state state
    ;; heal wounds
    (update-in state [:world :player :wounds]
      (fn [wounds]
        (reduce-kv (fn [wounds body-part wound]
          (if (< (get wound :dmg) 1)
            wounds
            (assoc wounds body-part {:dmg (- (get wound :dmg) 0.1)
                                     :time (get wound :time)})))
          {}
          wounds)))
    ;; chance of poison wearing off
    (if (and (contains? (get-in state [:world :player :status]) :poisoned)
            (< (rr/uniform-double 1) 0.1))
      (-> state
        (rp/update-player-status
          (fn [status]
              (disj status :poisoned)))
        (rc/append-log "The poison wore off." :green))
      state)
    ;; chance of infection clearing up
    (if (and (rp/player-status-contains? state :infected)
            (< (rr/uniform-double 1) 0.1))
      (-> state
        (rp/update-player-status
          (fn [status]
              (disj status :infected)))
        (rc/append-log "The infection has cleared up." :yellow))
      state)))

(defn decrease-flashlight-charge
  [state]
  {:pre [(not (nil? state))]
   :post [(not (nil? %))]}
  (if-let [flashlight (rp/inventory-id->item state :flashlight)]
    (rp/update-inventory-item state :flashlight (fn [flashlight] (as-> flashlight flashlight
                                                                   (if (= (get flashlight :state :off) :on)
                                                                       (update-in flashlight [:charge] dec)
                                                                       flashlight)
                                                                   (if (neg? (get flashlight :charge))
                                                                       (assoc flashlight :state :off)
                                                                       flashlight))))
    state))

;; forward declaration because update-state and repeat-cmmands are mutually recursive.
(declare update-state)

(defn repeat-commands
  [state]
  {:pre [(not (nil? state))]
   :post [(not (nil? %))]}
  (let [command-seq (get-in state [:world :command-seq] [])]
    (log/info "repeating commands" command-seq)
    (reduce update-state state command-seq)))

(defn toggle-mount
  "Mount or unmount at the current cell."
  [state]
  {:pre [(not (nil? state))]
   :post [(not (nil? %))]}
  (let [[{items :items} x y]  (rw/player-cellxy state)
        mounted (get-in state [:world :player :mounted] false)]
    (cond
      (and mounted
           (contains? (set (map :id items)) :raft))
        (-> state
          (rc/append-log "You dismount the raft.")
          (assoc-in [:world :player :mounted] false))
      (and (not mounted) (contains? (set (map :id items)) :raft))
        (-> state
          (rc/append-log "You mount the raft.")
          (assoc-in [:world :player :mounted] true))
      (not (contains? (set (map :id items)) :raft))
        (rc/append-log state "There is nothing to mount here."))))

      

(defn next-party-member
  "Switch (-> state :world :player) with the next npc where (-> npc :in-party?) is equal to true.
   Place the player at the end of (-> state :world :npcs)."
  [state]
  {:pre [(not (nil? state))]
   :post [(not (nil? %))]}
  (let [npc (first (filter #(contains? % :in-party?)
                            (get-in state [:world :npcs])))
    state (-> state
      (rc/remove-in [:world :npcs] (partial = npc))
      ;; make world npcs a vector so that conj adds the player to the end of the collection
      ;; rather than the beginning.
      (update-in [:world :npcs] vec)
      (rc/conj-in [:world :npcs] (get-in state [:world :player]))
      (assoc-in [:world :player] npc))]
    #?(:clj
       (log/debug "npcs" (with-out-str (clojure.pprint/pprint (-> state :world :npcs)))))
    state))

(defn
  move-to-target
  "Move `npc` one space closer to the target position if there is a path
   from the npc to the target. Returns the moved npc and not the updated state.
   `target` is a map with the keys `:x` and `:y`."
  [state npc target]
  {:pre [(not (nil? state))]
   :post [(not (nil? %))]}
    (let [npcs                   (rnpc/npcs-in-viewport state)
          ;_                      (log/debug "meta" (-> move-to-target var meta))
          ;_                      (log/debug "move-to-target npc" npc "target" target)
          npc-pos                (get npc :pos)
          npc-pos-vec            [(npc-pos :x) (npc-pos :y)]
          threshold              (get npc :range-threshold)
          npc-can-move-in-water  (mg/can-move-in-water? (get npc :race))
          npc-can-move-on-land   (mg/can-move-on-land? (get npc :race))
        
          player                 (-> state :world :player)
          player-pos-vec         [(-> player :pos :x) (-> player :pos :y)]
          width                  (get-in state [:world :width])
          height                 (get-in state [:world :height])
          bounds                 [(- width) (- height) width height]
          _ (log/info "pathfinding bounds" bounds)
          get-type               (memoize (fn [x y] (do
                                                      ;(log/debug "traversable?" x y "type" (get-in place [y x :type]))
                                                      (or (get (rw/get-cell state x y) :type) :unknown))))
          npc-xys                (set (map (fn [npc] (rc/pos->xy (get npc :pos)))
                                           (rnpc/npcs-in-viewport state)))
      
          water-traversable?     (fn water-traversable? [[x y]]
                                   (or (= [x y] npc-pos-vec)
                                       (and (not (rc/farther-than? npc-pos {:x x :y y} threshold))
                                            #_(not (contains? (disj npc-xys npc-pos-vec) [x y]))
                                            (contains? #{:water :surf} (get-type x y))
                                            #_(every? water-traversable? (rw/adjacent-xys-ext x y)))))
          land-traversable?      (fn land-traversable? [[x y]]
                                   (or (= [x y] npc-pos-vec)
                                       (and (not (rc/farther-than? npc-pos {:x x :y y} threshold))
                                            #_(not (contains? (disj npc-xys npc-pos-vec) [x y]))
                                            (contains? #{:floor
                                                         :open-door
                                                         :corridor
                                                         :sand
                                                         :dirt
                                                         :gravel
                                                         :tall-grass
                                                         :short-grass}
                                                       (get-type x y)))))
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
                                   (log/debug "a* params" bounds traversable? npc-pos-vec (rc/pos->xy target))
                                   (clj-tiny-astar.path/a* bounds traversable? npc-pos-vec (rc/pos->xy target))
                                   #?(:clj
                                      (catch Exception e
                                        (log/error "Caught exception during a* traversal." npc-pos-vec [(target :x) (target :y)] e)
                                        (st/print-cause-trace e)
                                        nil)
                                      :cljs
                                      (catch js/Error e
                                        (log/error "Caught exception during a* traversal." npc-pos-vec [(target :x) (target :y)] e)
                                        ;(st/print-cause-trace e)
                                        nil)))
          _                      (log/debug "path to target" (str (type path)) (str path))
          new-pos                (if (and (not (nil? path))
                                          (> (count path) 1)
                                          ;; don't collide with player
                                          (let [new-pos (second path)]
                                            (not= ((juxt first second) new-pos)
                                                  ((juxt first second) player-pos-vec))))
                                   (second path)
                                   npc-pos-vec)
          _                      (log/debug "new-pos" new-pos)
          new-npc                (-> npc
                                     (assoc-in [:pos :x] (first new-pos))
                                     (assoc-in [:pos :y] (second new-pos)))
          _                      (log/debug "new-npc" new-npc)
        ]
      [{:x (first new-pos) :y (second new-pos)} new-npc npc]))

(defn
  move-away-from-target
  "Move `npc` one space closer to the target position if there is a path
   from the npc to the target. Returns the moved npc and not the updated state.
   `target` is a map with the keys `:x` and `:y`."
  [state npc target]
  {:pre [(not (nil? state))]
   :post [(not (nil? %))]}
    (let [npcs                   (rnpc/npcs-in-viewport state)
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
                                                      #_(log/debug "traversable?" x y "type" (get-in place [y x :type]))
                                                      (or (get (rw/get-cell state x y) :type) :unknown))))
          water-traversable?     (fn water-traversable? [[x y]]
                                   (and (< 0 x width)
                                        (< 0 y height)
                                        (not (rc/farther-than? npc-pos {:x x :y y} threshold))
                                        (contains? #{:water :surf} (get-type x y))
                                        #_(every? water-traversable? (rw/adjacent-xys-ext x y))))
          land-traversable?      (fn [[x y]]

                                        (not (rc/farther-than? npc-pos {:x x :y y} threshold))
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
          ;_                      (log/debug "path to target" path)
          new-pos                (first
                                   (sort-by (fn [[x y]] (rc/distance (rc/xy->pos npc-x npc-y) (rc/xy->pos x y)))
                                            (filter traversable?
                                                    (rw/adjacent-xys-ext npc-x npc-y))))
          new-pos                (or new-pos
                                     [npc-x npc-y])

          ;_                      (log/debug "new-pos" new-pos)
          new-npc                (-> npc
                                     (assoc-in [:pos :x] (first new-pos))
                                     (assoc-in [:pos :y] (second new-pos)))
          ;_                      (log/debug "new-npc" new-npc)
        ]
      [{:x (first new-pos) :y (second new-pos)} new-npc npc]))

(defn move-random
  [state npc]
  {:pre [(not (nil? state))]
   :post [(not (nil? %))]}
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
                            :short-grass})
       adj-positions (rnpc/adjacent-navigable-pos state
                                                  npc-pos
                                                  navigable-types)]
    (if (> (count adj-positions) 1)
      ;; move randomly into an adjacent cell
      (let [target (rr/rand-nth
                     adj-positions)]
        ;(log/debug "distance > threshold, move randomly. target" target)
        [target
         (-> npc
           (assoc-in [:pos :x] (get target :x))
           (assoc-in [:pos :y] (get target :y)))
         npc])
      [(get npc :pos) npc npc])))

(defn move-to-target-in-range-or-random
  [state npc target]
  {:pre [(not (nil? state))]
   :post [(not (nil? %))]}
  (let [threshold (get npc :range-threshold)
        npc-pos  (get npc :pos)
        distance (rc/distance npc-pos target)
        navigable-types (if (mg/can-move-in-water? (get npc :race))
                          #{:water :surf}
                          #{:floor
                            :corridor
                            :open-door
                            :sand
                            :rocky-shore
                            :dune
                            :dirt
                            :gravel
                            :tall-grass
                            :short-grass})
       adj-positions (rnpc/adjacent-navigable-pos state
                                                  npc-pos
                                                  navigable-types)]
    (if (and (> distance threshold)
             (> (count adj-positions) 1))
      ;; outside of range, move randomly into an adjacent cell
      (let [target (rr/rand-nth
                     adj-positions)]
        ;(log/debug "distance > threshold, move randomly. target" target)
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
        distance (rc/distance npc-pos target)
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
      (let [target (rr/rand-nth
                     (rnpc/adjacent-navigable-pos state
                                                  npc-pos
                                                  navigable-types))]
        ;(log/debug "distance > threshold, move randomly. target" target)
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
  {:pre [(not (nil? state))
         (contains? #{:constant :entourage :follow-player :random :follow-player-in-range-or-random :hide-from-player-in-range-or-random} (get npc :movement-policy))]
   :post [(not (nil? state))
          (= (count %) 3)]}
  (let [policy      (get npc :movement-policy)
        temperament (get npc :temperament)
        pos         (-> state :world :player :pos)
        ;; modify movement policy as dictated by day or night
        policy      (cond
                      (or
                        (and (= temperament :hostile-during-day)
                             (rw/is-day? state))
                        (and (= temperament :hostile-at-day)
                             (rw/is-night? state)))
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
        ;_ (log/info "moving npc@" (get npc :pos) "with policy" policy)]
    (case policy
      :constant                            [nil nil npc]
      :entourage                           (move-to-target state
                                                           npc
                                                           (rr/rand-nth
                                                             (rnpc/adjacent-navigable-pos state
                                                                                          pos
                                                                                          navigable-types)))
      :follow-player                       (move-to-target state npc pos)
      :random                              (move-random state npc)
      :follow-player-in-range-or-random    (move-to-target-in-range-or-random state npc pos)
      :hide-from-player-in-range-or-random (move-away-from-target-in-range-or-random state npc pos)
      [nil nil npc])))


(defn add-monsters-debug
  "Add a monster randomly just inside the player's view."
  [state]
  (let [r          (rlos/sight-distance state)
        [player-x
         player-y] (rp/player-xy state)
        xys        (rlos/perimeter-xys player-x player-y (min 5 r))
        xys        (remove (fn [[x y]] (rw/collide? state x y)) xys)]
    (if (not (empty? xys))
      (let [[x y]  (rand-nth xys)
            monster (mg/gen-random-monster (rand 10) :floor)]
       (log/info "Adding monster" monster "@ [" x y "] r" r)
       (rnpc/add-npc state monster x y))
      state)))
 
(defn update-npcs
  "Move all npcs in the current place using `move-npc`."
  [state]
  {:pre  [(vector? (get-in state [:world :npcs]))]
   :post [(not (nil? %))
          (vector? (get-in % [:world :npcs]))]}
  ;; do npc->player attacks if adjacent
  (let [[{cell-type :type}] (rw/player-cellxy state)
        slow-player?        (contains? #{:dune :surf} cell-type)
        ;; increase all npcs energy by their speed value and have any adjacent npcs attack the player.
        state (reduce
                (fn [state npc]
                  ;; only update npcs that are in the current place and have an :energy value.
                  (if (contains? npc :energy)
                    (let [npc-keys (rnpc/npc->keys state npc)
                          ;; add speed value to energy.
                          ;_ (trace "adding speed to" (select-keys npc [:race :place :pos :speed :energy]))
;                         ;; if the player is slowed, add twice as much energy to the npcs
                          state (update-in state (conj npc-keys :energy) (partial + (* (if slow-player? 2 1)
                                                                                       (get npc :speed))))]
                      ;; adjacent hostile npcs attack player.
                      (if (and (contains? (get npc :status) :hostile)
                               (rw/adjacent-to-player? state (get npc :pos)))
                        (let [_ (log/info "npc attacker" npc)]
                          (rcombat/attack state npc-keys [:world :player]))
                        state))
                    state))
                state
                (rnpc/npcs-in-viewport state))]
    state
    (loop [state          state
           ;; find all npcs in the current place with enough energy to move (energy > 1).
           ;;; Since most npcs are moving toward the player, sort by distance
           ;;; to player. The npcs closest will be moved first, leaving a gap
           ;;; behind them allowing the next most distant npc to move forward
           ;;; to fill the gap.
           remaining-npcs (sort-by (fn [npc] (rw/distance-from-player state (get npc :pos)))
                                   (filter (fn [npc] (and (contains? npc :energy)
                                                          (> (get npc :energy) 1)))
                                     (rnpc/npcs-in-viewport state)))
           i 5]
      (log/info "looping over" (count remaining-npcs) "npcs i" i)
      (if (or (empty? remaining-npcs)
              (neg? i))
        ;; no more results to process, stop looping and return state
        (do
          #_(log/info "State" (dissoc (get state :world) :player :places))
          state)
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
        ;_ (log/info "map-result" (map (fn [npc]
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
                             (rnpc/update-npc state old-npc
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
                                         (not= new-pos (rp/player-pos state)))
                                     ;; use the new npc value
                                     new-npc
                                     ;; otherwise there is an npc at the destination, use the old npc
                                     old-npc)
                                   [:energy]
                                   (fn [energy] (max (dec energy) 0))))))
                           state
                           map-result)
              remaining-npcs (sort-by (fn [npc] (rw/distance-from-player state (get npc :pos)))
                               (filter (fn [npc] (and (contains? npc :energy)
                                                      (> (get npc :energy) 1)))
                                 (rnpc/npcs-in-viewport state)))]
              ;_ (trace "count remaining npcs" (count remaining-npcs))]
        (log/info "recurring over" (count remaining-npcs) "npcs i" (dec i))
        (recur state remaining-npcs (dec i)))))))

(defn update-cells
  "Fill holes with a small amount of water. Drop fruit. Drop harvest items."
  [state]
  {:pre  [(not (nil? state))
          (vector? (get-in state [:world :npcs]))]
   :post [(not (nil? %))
          (vector? (get-in % [:world :npcs]))]}
  (log/info "updating cells")
  (let [;xys      (viewport-xys state)
        ;get-cell-m (memoize (fn [x y] (get-cell state x y)))
        {{v-x :x v-y :y} :pos}
                       (get-in state [:world :viewport])
        harvestable-counts (atom (reduce-kv (fn [m k v]
                                              (assoc m k (get v :num-harvestable-cells 0)))
                                            {}
                                            (get-in state [:world :places])))
        ;viewport-cells (apply concat
        ;                 (map-indexed (fn [vy line]
        ;                                (map-indexed (fn [vx cell]
        ;                                               [cell (+ v-x vx) (+ v-y vy)])
        ;                                             line))
        ;                              (rv/cells-in-viewport state)))
        xy-fns
        ;fire-xys (set (filter (fn filter-fire-cells
        ;                        [[cell x y]]
        ;                        (= (get cell :type) :fire))
        ;                      viewport-cells))]
    (reduce 
      (fn cell-reduction-fn [xy-fns [cell sx sy wx wy]]
        (let [cell-type        (get cell :type)
              cell-items       (get cell :items)
              hole-types       #{:freshwater-hole :saltwater-hole}
              still-types      #{:solar-still}
              harvest-types    #{:gravel :tree :palm-tree :tall-grass}
              fruit-tree-types #{:fruit-tree}]
          #_(log/info "updating cell" cell "@" x y)
          #_(log/info "viewport" (get-in state [:world :viewport]))
          (-> 
            (cond
                ;; update holes
                (contains? hole-types cell-type)
                (conj xy-fns [[wx wy]
                  (fn increase-hole-water [cell] (assoc cell :water (min 20 (+ 0.1 (* (rr/uniform-double 1) 0.1) (get cell :water 0.0)))))])
                ;; update solar stills
                (contains? still-types (get cell :type))
                (conj xy-fns [[wx wy]
                  (fn increase-still-water [cell] (assoc cell :water (min 20 (+ 0.2 (* (rr/uniform-double 1) 0.1) (get cell :water 0.0)))))])
                ;; drop harvest items
                (contains? harvest-types cell-type)
                (let [place-id         (rv/xy->place-id state wx wy)
                      num-harvestable (get @harvestable-counts place-id)]
                  (if (< (rr/uniform-int 0 10000)
                         (/ 8 (inc num-harvestable)))
                    (do
                      (swap! harvestable-counts (fn [counts] (update counts place-id inc)))
                      (conj xy-fns [[wx wy]
                        (fn drop-harvest-items [cell]
                          (assoc cell :harvestable true))]))
                    xy-fns))
                ;; drop fruit
                (contains? fruit-tree-types cell-type)
                ;; chance of dropped a fruit
                (if (= (rr/uniform-int 0 300) 0)
                  ;; make the fruit item and find an adjacent free cell to drop it into
                  (let [item    (assoc (ig/id->item (get cell :fruit-type)) :rot-time (+ (rw/get-time state) (rr/uniform-int 25 35)))
                        adj-xys (remove (fn [[x y]] (or (not (rv/xy-in-viewport? state x y))
                                                         (rw/type->collide?
                                                           (get (rw/get-cell state x y) :type))))
                                        (rw/adjacent-xys wx wy))]
                    (log/info "dropping fruit" item "at [" sx sy "]" adj-xys)
                    (if (seq adj-xys)
                      ;; drop the fruit into the cell
                      (conj xy-fns [(rr/rand-nth adj-xys)
                        (fn drop-fruit [cell] (update-in cell [:items] conj  item))])
                      xy-fns))
                  xy-fns)
                ;; update fire
                (= cell-type :fire)
                ;(contains? fire-xys [x y])
                (as-> xy-fns xy-fns
                  (conj xy-fns [[wx wy] (fn [cell] (update-in cell [:fuel] dec))])
                  ;; chance of fire spreading
                  (if (= 0 (rr/uniform-int 0 10))
                    ;; make the fire spread and find an adjacent free cell to spread it into
                    (let [cell-at-xy-flammable? (fn [[x y]]
                                                  (rw/type->flammable?
                                                    (get (rw/get-cell state x y) :type)))
                          adj-xys (filter cell-at-xy-flammable?
                                         (rw/adjacent-xys-ext wx wy))]
                      (log/info "spreading fire at [" sx sy "]" adj-xys)
                      (if (seq adj-xys)
                        ;; spread fire into the cell
                        (conj xy-fns [(rr/rand-nth adj-xys)
                          (fn spread-fire [cell] (assoc cell :type :fire :fuel (rr/uniform-int 10 50)))])
                        xy-fns))
                     xy-fns)
                  (if (neg? (get cell :fuel 0))
                    ;; extinguish the fire
                    (conj xy-fns [[wx wy]
                      (fn extinguish-fire [cell]
                        (-> cell
                          (assoc :type :dirt)
                          (dissoc :fuel)))])
                    xy-fns))
                 :else
                 xy-fns)
            ;; rot fruit
            ;; TODO: only rot fruit if it is in the set of visible cells.
            (as-> xy-fns
              (if (some (fn [item] (< (get item :rot-time (rw/get-time state)) (rw/get-time state)))
                                        cell-items)
                (conj xy-fns [[wx wy]
                              (fn rot-cell-fruit [cell]
                                (update-in cell [:items] (fn [items]
                                  (remove (fn [item] (< (get item :rot-time (rw/get-time state))
                                                        (rw/get-time state)))
                                           items))))])
                xy-fns)))))
      []
      (vec (rv/cellsxy-in-viewport state)))]
    (as-> state state
      (rw/update-cells state xy-fns)
      (reduce-kv (fn [state place-id harvest-count]
                   (assoc-in state [:world :places place-id :num-harvestable-cells] harvest-count))
                 state
                 @harvestable-counts))))

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
                  ;(log/debug "exec quest" quest)
                  ;(log/debug "quest-id" (quest :id))
                  ;(log/debug "stage-id" stage-id)
                  ;(log/debug "exec stage" stage)
                  (if ((stage :pred) state)
                    (-> state
                      ((stage :update))
                      (assoc-in [:world :quests (quest :id) :stage]
                                ((stage :nextstagefn) stage)))
                    state)))))
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
  (let [table {:start     {:space      [identity               :enter-name      false]
                           :else       [pass-state             identity         false]}
               :enter-name
                          {:enter      [identity               :start-inventory false]
                           :backspace  [backspace-name         :enter-name      false]
                           :else       [append-name            :enter-name      false]}
               :start-inventory
                          {:enter      [identity               :loading         false]
                           :else       [select-starting-inventory
                                                               :start-inventory false]}
               :loading   {:advance    [reinit-world           :start-text      false]
                           :else       [pass-state             :loading         false]}
               :start-text
                          {\?          [identity               :help-controls   false]
                           :else       [(fn [state _]
                                          (do-rest state))     :normal          true]}
               :normal    {\i          [identity               :inventory       false]
                           \d          [identity               :drop            false]
                           \,          [identity               :pickup          false]
                           \e          [identity               :eat             false]
                           \o          [identity               :open            false]
                           \c          [identity               :close           false]
                           \.          [do-rest                :normal          true]
                           :numpad5    [do-rest                :normal          true]
                           :left       [move-left              :normal          true]
                           :down       [move-down              :normal          true]
                           :up         [move-up                :normal          true]
                           :right      [move-right             :normal          true]
                           :up-left    [move-up-left           :normal          true]
                           :up-right   [move-up-right          :normal          true]
                           :down-left  [move-down-left         :normal          true]
                           :down-right [move-down-right        :normal          true]
                           :space      [action-select          :action-select   false]
                           \q          [quaff-select           identity         false]
                           \w          [identity               :wield           false]
                           \x          [identity               :harvest         false]
                           \a          [identity               :apply           false]
                           \;          [init-cursor            :describe        false]
                           \R          [identity               :sleep           false]
                           \s          [rdesc/search           :normal          true]
                           \S          [rdesc/extended-search  :normal          true]
                           \Q          [identity               :quests          false]
                           \M          [toggle-mount           :normal          false]
                           \P          [next-party-member      :normal          false]
                           \z          [identity               :craft           true]
                           \Z          [identity               :magic           true]
                           \v          [identity               :abilities       false]
                           \g          [identity               :player-stats    false]
                           \t          [identity               :throw-inventory false]
                           \T          [identity               :talk            true]
                           \m          [identity               :log             false]
                           \?          [identity               :help-controls   false]
                           \/          [scroll-log-up          :normal          false]
                           \*          [scroll-log-down        :normal          false]
                           \r          [repeat-commands        identity         false]
                           #_#_\0          [(fn [state]
                                          (if (get-in state [:world :dev-mode])
                                            (-> state
                                              (rc/append-log "log1...........................")
                                              (rc/append-log "log2...........................")
                                              (rc/append-log "log3...........................")
                                              (rc/append-log "log4...........................")
                                              (rc/append-log "log5..........................."))
                                            state))
                                          :normal true]
                          #_#_\0           [(fn [state]
                                          (log/info "monster level" (rnpc/monster-level state))
                                          state)               :normal false]
                          \0           [(fn [state]
                                          (rp/player-update-xp state (partial + 100))) :normal false]
                          \1           [(fn [state]
                                          (log/info "showing world")
                                          (when (get-in state [:world :dev-mode])
                                            (robinson.devtools/show-world state))
                                          state)
                                                                       :normal false]
                          \2           [(fn [state]
                                          (if (get-in state [:world :dev-mode])
                                            (rw/assoc-current-state state :dead)
                                            state))
                                                                       identity true]
                          \3           [(fn [state]
                                          (if (get-in state [:world :dev-mode])
                                            (rp/add-to-inventory state [(ig/gen-item :flint)])
                                            state))            :normal          true]
                          \4           [(fn [state]
                                          (if (get-in state [:world :dev-mode])
                                            (add-monsters-debug state)
                                            state))            :normal          false]
                          \5           [(fn [state]
                                          (if (get-in state [:world :dev-mode])
                                            (do
                                              (clojure.inspector/inspect-tree (get state :world))
                                              state)
                                            state))            :normal          false]
                          \6           [(fn [state]
                                          (if (get-in state [:world :dev-mode])
                                            (assoc-in state [:world :npcs] [])
                                            state))
                                                               :normal          false]
                          \7           [(fn [state]
                                          (when (get-in state [:world :dev-mode])
                                            (let [level (get log/*config* :level)
                                                  new-level (case level
                                                              :debug :info
                                                              :info :warn
                                                              :warn :error
                                                              :error :debug)]
                                            (log/set-level! new-level)))
                                          state)               :normal          false]
                          \8           [(fn [state]
                                          (if (get-in state [:world :dev-mode])
                                            (update-in state [:world :time] (partial + 100))
                                            state))
                                                               :normal          false]
                          \9           [(fn [state]
                                          (if (get-in state [:world :dev-mode])
                                            (rp/add-to-inventory state [(ig/id->item :raft)])
                                            state))
                                                               :normal          false]
                         :f9           [(fn [state]
                                          (-> state
                                            (rc/append-log "Development mode: on")
                                            (assoc-in [:world :dev-mode] true)))
                                                               :normal          false]
                         :f12          [(fn [state]
                                          (rf/send-report state)
                                          state)
                                                               :normal          false]
                           :escape     [identity               :quit?           false]}
               :action-select
                          {:escape     [identity               :normal          false]
                           :else       [do-selected-action     identity         false]}
               :inventory {:escape     [identity               :normal          false]}
               :abilities {:escape     [identity               :normal          false]
                           :else       [use-ability            identity         true]}
               :player-stats
                          {:escape     [identity               :normal          false]
                           :else       [pass-state             :player-stats    false]}
               :describe  {\i          [free-cursor            :describe-inventory false]
                           :left       [move-cursor-left       :describe        false]
                           :down       [move-cursor-down       :describe        false]
                           :up         [move-cursor-up         :describe        false]
                           :right      [move-cursor-right      :describe        false]
                           :up-left    [move-cursor-up-left    :describe        false]
                           :up-right   [move-cursor-up-right   :describe        false]
                           :down-left  [move-cursor-down-left  :describe        false]
                           :down-right [move-cursor-down-right :describe        false]
                           :else       [free-cursor            :normal          false]}
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
                           :else       [apply-item             identity         true]}
               :apply-item-body
                          {:escape     [identity               :normal          false]
                           \a          [(fn [state]
                                        (apply-item state \a)) :normal          true]
                           \b          [(fn [state]
                                        (apply-item state \b))
                                                               :normal          true]}
               :pickup    {:escape     [identity               :normal          false]
                           :space      [pick-up-all            :normal          true]
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
                           :up-left    [harvest-up-left        :normal          true]
                           :up-right   [harvest-up-right       :normal          true]
                           :down-left  [harvest-down-left      :normal          true]
                           :down-right [harvest-down-right     :normal          true]
                           :numpad5    [harvest-center         :normal          true]
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
               :gain-level
                          {:escape     [identity               :normal          false]
                           :else       [choose-ability         identity         false]}
               :help-controls
                          {\n          [identity               :help-ui         false]
                           \p          [identity               :help-gameplay   false]
                           :escape     [identity               :normal          false]
                           :else       [pass-state             identity         false]}
               :help-ui
                          {\n          [identity               :help-gameplay   false]
                           \p          [identity               :help-controls   false]
                           :escape     [identity               :normal          false]
                           :else       [pass-state             identity         false]}
               :help-gameplay
                          {\n          [identity               :help-controls   false]
                           \p          [identity               :help-ui         false]
                           :escape     [identity               :normal          false]
                           :else       [pass-state             identity         false]}
               :close     {:left       [close-left             :normal          true]
                           :down       [close-down             :normal          true]
                           :up         [close-up               :normal          true]
                           :right      [close-right            :normal          true]}
               :log       {:else       [pass-state             :normal          false]}
               :rescued   {:space      [pass-state             :game-over-rescued false]
                           :else       [pass-state             identity         false]}
               :dead      {:space      [pass-state             :game-over-dead  false]
                           :else       [pass-state             identity         false]}
               :game-over-rescued
                          {\y          [identity               :start-inventory false]
                           \n          [(constantly nil)       :normal          false]}
               :game-over-dead
                          {\y          [identity               :start-inventory false]
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
    ;(log/debug "current-state" current-state)
    (if (or (contains? table keyin) (contains? table :else))
      (let [[transition-fn new-state advance-time] (if (contains? table keyin) (get table keyin) (get table :else))
            ;; if the table contains keyin, then pass through transition-fn assuming arity-1 [state]
            ;; else the transition-fn takes [state keyin]. Bind keying so that it becomes arity-1 [state]
            _ (log/debug "current-state" (get-in state [:world :current-state]))
            _ (log/debug "transition-fn" transition-fn)
            transition-fn             (if (contains? table keyin)
                                        transition-fn
                                        (fn [state]
                                          (transition-fn state keyin)))
            command-seq (get-in state [:world :command-seq] [])
            _ (log/debug "type of npcs" (type (get-in state [:world :npcs])))
            new-time  (inc (get-in state [:world :time]))
            state     (rc/clear-ui-hint state)
            state     (transition-fn
                        (if advance-time
                          (assoc-in state [:world :time] new-time)
                          state))
            _ (log/debug "current-state" (get-in state [:world :current-state]))
            _ (log/debug "new-state" new-state)
            _ (log/debug "type of npcs" (type (get-in state [:world :npcs])))
            new-state (if (keyword? new-state)
                        new-state
                        (new-state (get-in state [:world :current-state])))
            _ (assert (not (nil? new-state)))
            state     (if state
                        (if-not (= keyin \r)
                          (cond
                            (= current-state :normal)
                              (assoc-in state [:world  :command-seq] [keyin])
                            :else
                              (rc/conj-in state [:world :command-seq] keyin))
                          (assoc-in state [:world :command-seq] command-seq))
                        state)
            _ (log/info "command-seq" (get-in state [:world :command-seq] :none))
            _ (log/debug "player" (get-in state [:world :player]))
            _ (log/info "new-state" new-state)
            wtl       (get-in state [:world :player :will-to-live])]
        (some-> state
            (rw/assoc-current-state new-state)
            (as-> state
              (if advance-time
                (-> state
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
                  (check-paralyzed)
                  (if-wounded-get-infected)
                  (if-infected-get-hurt)
                  (if-near-too-much-fire-get-hurt)
                  (update-npcs)
                  (decrease-will-to-live)
                  (update-cells)
                  (as-> state
                    (do
                      (log/info "Done updating cells")
                      state))
                  ;; TODO: Add appropriate level
                  (rnpc/add-npcs-random)
                  ;; update visibility
                  (update-visibility)
                  ;; add will-to-live flavor message
                  (log-will-to-live-flavor wtl)
                  ;; maybe gain level
                  (maybe-gain-level)
                  (coalesce-logs))
                state))
            (update-quests)
            (as-> state
              (if (contains? (-> state :world :player :status) :dead)
                (do
                  #?(:clj
                     ;; if UPLOADVERSION file is present, upload save/world.edn as json to aaron-santos.com:8888/upload
                     (when (get state :feedback-participant)
                       (let [version (get state :version)
                             userid  (get state :user-id)
                             url     (format "https://aaron-santos.com/saves/%s" userid)
                             body    (as-> (get state :world) world
                                           (assoc world :version version)
                                           (clojure.walk/postwalk (fn [v] (if #?(:clj
                                                                                 (char? v)
                                                                                 :cljs
                                                                                 (string? v))
                                                                            (str v)
                                                                            v))
                                                                   world)
                                           (json/write-str world))]
                         (log/info "Uploading save file version" version "userid" userid "to" url)
                         (async/thread 
                           (try
                             (log/info "Starting upload")
                             (System/setProperty "https.protocols" "TLSv1.2")
                             (http/post url
                               {:insecure? true
                                :body body
                                :content-type :json})
                             (log/info "Done uploading save file")
                             (catch Exception e
                               (log/error "Caught exception while uploading save" e))))))
                     ;; delete the save game on player death
                     (delete-save-game))
                  (-> state
                    (update-in [:world :player :status]
                     (fn [status]
                       (disj status :dead)))
                    (rw/assoc-current-state :dead)))
                state))))
      state)))

