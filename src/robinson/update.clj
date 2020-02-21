;; Functions that manipulate state to do what the user commands.
(ns robinson.update
  (:require 
            [robinson.common :as rc]
            [robinson.random :as rr]
            [robinson.scores :as rs]
            [robinson.world :as rw]
            [robinson.viewport :as rv]
            [robinson.font :as rfont]
            [robinson.log :as rlog]
            [zaffre.terminal :as zat]
            [robinson.describe :as rdesc]
            [robinson.dreams :as rdreams]
            [robinson.traps :as rt]
            [robinson.dialog :as rdiag]
            [robinson.actors :as ractors]
            [robinson.player :as rp]
            [robinson.inventory :as ri]
            [robinson.math :as rmath]
            [robinson.itemgen  :as ig]
            [robinson.monstergen :as mg]
            [robinson.apply-item :as rai]
            [robinson.startgame :as sg]
            [robinson.popover :as rpop]
            [robinson.fs :as rfs]
            [clojure.string :refer [lower-case]]
            ;[robinson.dialog :refer []]
            [robinson.npc :as rnpc]
            [robinson.combat :as rcombat]
            [robinson.crafting :as rcrafting]
            [robinson.crafting.weapon-gen :as rc-weapon-gen]
            [robinson.crafting.boat-gen :as rc-boat-gen]
            [robinson.worldgen :as rworldgen]
            [robinson.lineofsight :as rlos]
            [robinson.color :as rcolor]
            [robinson.renderutil :as rutil]
            [robinson.fx :as rfx]
            [robinson.fx.rain :as rfx-rain]
            [robinson.feedback :as rf]
            robinson.macros
            [robinson.macros :as rm]
            clojure.pprint
            clojure.edn
            [clojure.core.async :as async]
            [clojure.data.json :as json]
            [clojure.java.io :as io]
            [taoensso.timbre :as log]
            [dk.salza.liq.editor :as le]
            [dk.salza.liq.adapters.ghostadapter :as lag]
            clj-tiny-astar.path
            [clj-http.client :as http]
            [clojure.stacktrace :as st]
            clojure.inspector
            clojure.string)
  (:import zaffre.terminal.Terminal))

(defn format [s & args]
  (apply clojure.core/format s args))

(defn- pass-state
  [state & more]
  state)

(defn delete-save-game []
  ;; delete the save game on player death
  (doseq [file (filter (fn [^java.io.File file] (re-matches #".*edn" (.getName file))) (file-seq (io/file "save")))]
    (log/info "Deleting" file)
    (.delete file)))

(defn translate-directions
  [keyin]
  (log/info "translate-directions" keyin)
  (case keyin
    (\h \H :numpad4) :left
    (\j \J :numpad2) :down
    (\k \K :numpad8) :up
    (\l \L :numpad6) :right
    (\y \Y :numpad7) :up-left
    (\u \U :numpad9) :up-right
    (\b \B :numpad1) :down-left
    (\n \N :numpad3) :down-right
    keyin))

(defn next-font
  [state]
  (let [fonts           (sort-by :name (get state :fonts))
        current-font    (get state :new-font (get (rc/get-settings state) :font))
        remaining-fonts (rest (second (split-with (fn [[k v]] (not= k current-font))
                                                  fonts)))]
   (assoc state :new-font (if (seq remaining-fonts)
                            (ffirst remaining-fonts)
                            (ffirst fonts)))))

(defn previous-font
  [state]
  (let [fonts          (sort-by :name (get state :fonts))
        current-font   (get state :new-font (get (rc/get-settings state) :font))
        previous-fonts (first (split-with (fn [[k v]] (not= k current-font))
                                         fonts))]
   (assoc state :new-font (if (seq previous-fonts)
                            (-> previous-fonts last first)
                            (-> fonts last first)))))

(defn save-and-apply-font
  [state]
  (let [fonts        (sort-by :name (get state :fonts))
        new-font     (get state :new-font (get (rc/get-settings state) :font))
        font         (get-in state [:fonts new-font])
        screen ^robinson.terminal.Terminal (get state :screen)]
    (log/info (keys state))
    (log/info fonts)
    (when-not (= new-font (get (rc/get-settings state) :font))
      (log/info font)
      ;; send new settings to screen
      (zat/alter-group-font! screen :app
        (partial rfont/make-font-fn font))
      ;; persist
      (rc/reset-settings! state (assoc (rc/get-settings state) :font new-font)))
    ;; cleanup temp new-font value
    (dissoc state :new-font)))

(defn create-new-font
  [state]
  (assoc state
    :create-font-name ""
    :create-font-size 1
    :create-font-path ""
    :create-font-error "No path. Drag tileset."))

(defn backspace-create-font-name
  [state]
  (update-in
    state
    [:create-font-name]
    (fn [font-name]
      (clojure.string/join (butlast font-name)))))

(defn inc-create-font-size
  [state]
  (update state :create-font-size inc))

(defn dec-create-font-size
  [state]
  (update state :create-font-size (comp (partial max 1) dec)))

(defn accept-drop-create-font
  [state key-in]
  (log/info key-in)
  (let [{:keys [drag-and-drop]} key-in]
    (if (= (count drag-and-drop) 1)
      (-> state
        (assoc :create-font-path (first drag-and-drop))
        (dissoc :create-font-error))
      (assoc state :create-font-error "Multiple files dragged"))))

(defn append-create-font-name
  [state key-in]
  (let [key-in (cond
                 (= :space key-in)
                   \ 
                 (char? key-in)
                   key-in
                 :else nil)]
    (if key-in
      (-> state
        (dissoc :create-font-error)
        (update
          :create-font-name
          (fn [font-name]
            (if (and (< (count font-name) 20)
                     (or (<= (int \A) (int key-in) (int \Z))
                     (<= (int \a) (int key-in) (int \z))
                     (contains? #{\- \ } key-in)))
               (str font-name key-in)
               font-name))))
      state)))

(defn accept-input-new-font
  [state key-in]
  (if (map? key-in)
    (accept-drop-create-font state key-in)
    (append-create-font-name state key-in)))

(defn save-new-font
  [state]
  (let [{:keys [create-font-name
                create-font-size
                create-font-path
                create-font-error]} state]
    (if (= (count create-font-name) 0)
      (assoc state :create-font-error "Empty font name")
      (let [font {:name create-font-name
                  :type :cp437
                  :url create-font-path
                  :scale create-font-size
                  :alpha-channel :green
                  :transparent true}]
        (spit (str "config/fonts/" create-font-name ".edn") font)
        (-> state
          (assoc :fonts (rfont/read-font-configs))
          (rw/assoc-current-state :configure-font))))))
           

(defn backspace-name
  [state]
  (update-in
    state
    [:world :player :name]
    (fn [player-name]
      (clojure.string/join (butlast player-name)))))

(defn append-name
  [state key-in]
  (let [key-in (cond
                 (= :space key-in)
                   \ 
                 (char? key-in)
                   key-in
                 :else nil)]
    (if key-in
      (update-in
        state
        [:world :player :name]
        (fn [player-name]
          (if (and (< (count player-name) 20)
                   (or (<= (int \A) (int key-in) (int \Z))
                   (<= (int \a) (int key-in) (int \z))
                   (contains? #{\- \ } key-in)))
             (str player-name key-in)
             player-name)))
      state)))

(defn toggle-hotkey
  "Toggle mark `keyin` as a selected hotkey, or not if it already is."
  [state keyin]
  (log/debug "toggle-hotkey" keyin)
  (update-in state [:world :selected-hotkeys]
             (fn [hotkeys] (if (contains? hotkeys keyin)
                             (disj (set hotkeys) keyin)
                             (conj (set hotkeys) keyin)))))

;; update visibility
(defn update-visibility
  [state]
  {:pre [(not (nil? state))]
   :post [(not (nil? %))]}
  (let [{px :x
         py :y :as pos} (rp/player-pos state)
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
                                                                 blocking?)))]
  (log/info "visible-cells" visible-cells)
  (as-> state state
    (rw/assoc-cells state (zipmap visible-cells (repeat {:discovered new-time}))))))

(defn add-starting-inventory
  [state selected-hotkeys]
  {:pre  [(not (nil? state))]
   :post [(not (nil? %))]}
  (let [_                  (log/info "selected-hotkeys" selected-hotkeys)
        start-inventory    (filter #(contains? (set selected-hotkeys) (get % :hotkey)) (sg/start-inventory))
        _                  (log/info "Adding to starting inventory:" start-inventory)
        state              (ri/add-to-inventory state start-inventory)]
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
                                                 (rworldgen/init-world (rc/system-time-millis))
                                                 (catch Throwable e
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
  (if (and (char? keyin)
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
  (log/info (rw/get-cell-type state x y))
  (let [wielded-item (-> state
                       rp/get-player 
                       rp/wielded-item)
        is-saw? (= (get wielded-item :item/id) :saw)
        destroy? (rr/rand-bool (cond
                                 is-saw?
                                    0.01
                                 (ig/is-sharp? rp/wielded-item)
                                    0.2
                                 :else
                                    0.4))
        cell (rw/get-cell state x y)
        cell-type (get cell :type)
        cell-type-name (rdesc/describe-cell-type cell)
        drop-item? (or (get cell :harvestable)
                       (rr/rand-bool (case cell-type
                                       :tall-grass 0.95
                                       0.70)))
        drop-item (ig/id->item
                    (case cell-type
                      :tree :log
                      :bamboo :bamboo
                      :palm-tree :log
                      :tall-grass :plant-fiber
                      :close-door :door
                      :open-door :door
                      nil))]
    (if destroy?
      (-> state
        (rw/assoc-cell x y :type (get cell :prev-type :dirt))
        (rw/update-cell x y (fn [cell] (dissoc cell :harvestable)))
        (rp/player-update-thirst + (if is-saw? 1 3))
        (rp/player-update-hunger + (if is-saw? 1 3))
        (rc/append-log (str "You destory the " cell-type-name))
        (cond->
          (and drop-item? drop-item)
          (rw/conj-cell-items x y drop-item))
        (cond->
          (and drop-item? drop-item)
          (rfx/conj-effect :blip
            (rc/xy->pos x y)
            (rutil/item->char drop-item)
            (rcolor/color->rgb (rutil/item->bg drop-item))
            (rcolor/color->rgb (rutil/item->fg drop-item))
            2)))
      (-> state
        (rc/append-log (str "You fail to destory the " cell-type-name))
        (rfx/conj-effect :blip
                    (rc/xy->pos x y)
                    \/
                    (rcolor/color->rgb :blue)
                    (rcolor/color->rgb :transparent)
                    2)))))

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
  (let [[player-cell x y]    (rw/player-cellxy state)
        [target-x
         target-y]           (rw/player-adjacent-xy state direction)
        target-place         (rv/xy->place-id state target-x target-y)
        ;rv/visible-place-ids (rv/visible-place-ids state target-x target-y)
        _ (log/info "target-x" target-x "target-y" target-y)
        _ (log/info "target-place" target-place)
       vp-pos                (apply rc/xy->pos
                               (rv/+xy (rc/pos->xy (get-in state [:world :viewport :pos]))
                                    (rv/-xy [target-x target-y] [x y])))
       {width :width
        height :height}  (get-in state [:world :viewport])]
      (log/debug "viewport-pos" vp-pos)
      (-> state
        (assoc-in [:world :viewport :pos] vp-pos)
        (rworldgen/load-unload-places)
        (as-> state
          ;; discovered encounter
          (if (let [place     (rv/player-place state)
                    place-id  (rv/player-place-id state)
                    [place-x
                     place-y] (rv/place-id->center-xy state place-id)]
              (log/info "place" (select-keys place [:discovered :discovered-message])
                        "place-id" place-id
                        "place-xy" place-x place-y
                        "player-xy" x y)
              (and (not (get place :discovered))
                   (get place :discovered-message)
                   (not (rc/farther-than? place-x place-y x y 80))))
            ;; show popover
            (let [place (rv/player-place state)
                  place-id  (rv/player-place-id state)
                  message (get place :discovered-message)]
              (-> state
                (rw/update-place place-id (fn [place] (assoc place :discovered true)))
                (rp/player-update-wtl
                  (fn [will-to-live] (rp/player-max-wtl state)))
                (rpop/show-popover
                  (rdesc/describe-encounter message))))
            (rw/assoc-current-state state :normal))))))
             
(defn sense-traps
  [state difficulty]
  (reduce (fn [state [x y]]
            (let [trap-difficulty (get (rw/get-cell state x y) :difficulty 1)]
              (if (< trap-difficulty difficulty)
                (-> state
                  (rw/assoc-cell x y :trap-found true)
                  (rc/append-log state "You sense a trap."))
                state)))
          state
          (apply rw/adjacent-xys (rp/player-xy state))))

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
        [target-x
         target-y] (rw/player-adjacent-xy state direction)
        target-cell (rw/get-cell state target-x target-y)]
    (log/info "moving to" target-x target-y "type:" (get target-cell :type))
    (log/info "inside-safe-zone?" (rv/xy-in-safe-zone? state target-x target-y) target-x target-y)
    (log/info "not collide?" (not (rw/collide? state target-x target-y {:include-npcs? false})))
    (log/info "not mounted-on-raft?" (not (rw/player-mounted-on-raft? state)))
    (cond
      ;; Don't move paralyzed player
      (rp/player-paralyzed? state)
        (-> state
          (rc/append-log "You can not move (paralyzed).")
          rc/inc-time)
      ;; Open space (may include npc) and not on raft?
      (and (not (rw/collide? state target-x target-y {:include-npcs? false}))
           (not (rw/player-mounted-on-raft? state)))
        (as-> state state
          (rc/inc-time state)
          (if (rw/npc-at-xy state target-x target-y)
            ;; collided with npc. Engage in combat.
            (let [npc (rw/npc-at-xy state target-x target-y)]
              (log/info "npc" npc)
              (-> state
                (rcombat/attack [:world :player] (rnpc/npc->keys state npc))
                (rw/assoc-current-state :normal)))
            ;; regular move
            (as-> state state
              (if (and (not (rv/xy-in-safe-zone? state target-x target-y))
                       (not= :fixed (get-in state [:world :places (rw/current-place-id state) :movement])))
                (move-outside-safe-zone state direction)
                (rw/assoc-current-state state :normal))
              (assoc-in state [:world :player :pos :x] target-x)
              (assoc-in state [:world :player :pos :y] target-y)
              ; clear bloody status when moving into water
              (if (rw/type->water? (get target-cell :type))
                (rp/assoc-bloodied state false)
                state)
              (if (rp/player-status-contains? state :trap-sense)
                (sense-traps state 0.5)
                (sense-traps state 0.1))
              (let [cell  (rw/get-cell state target-x target-y)
                    items (get cell :items)]
                (if (seq items)
                  (rdesc/search state)
                  state)))))
      ;; Swap places with party members
      (= (get (rw/npc-at-xy state target-x target-y) :in-party?) true)
        (-> state
          (assoc-in [:world :player :pos :x] target-x)
          (assoc-in [:world :player :pos :y] target-y)
          pick-up-gold
          (rw/assoc-current-state :normal)
          rc/inc-time
          (rc/map-in [:world :npcs]
                     (fn [npc] (if (and (= (-> npc :pos :x) target-x)
                                        (= (-> npc :pos :y) target-y))
                                 (-> npc
                                     (assoc-in [:pos :x] player-x)
                                     (assoc-in [:pos :y] player-y))
                                 npc))))
      ;; Passable water terrain and on raft?
      (and (not (rw/collide-in-water? state target-x target-y))
           (rw/player-mounted-on-raft? state))
        (as-> state state
          (rw/dec-cell-item-count state player-x player-y :raft)
          (rw/conj-cell-items state target-x target-y (ig/id->item :raft))
          (if (not (rv/xy-in-safe-zone? state target-x target-y))
            (move-outside-safe-zone state direction)
            state)
          (rp/assoc-player-pos state (rc/xy->pos target-x target-y))
          (rw/assoc-current-state state :normal)
          (rc/inc-time state)
          ;; rafting = more hunger
          (update-in state [:world :player :hunger] (partial + 0.05 ))
          ;;
          (let [cell  (rw/get-cell state target-x target-y)
                items (get cell :items)]
            (if (seq items)
              (rdesc/search state)
              state)))
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

(defn action
  "Perform action adjacent to the player provided her/she is able. Else do combat. Else swap positions
   with party member. Else hack something down."
  [state direction]
  {:pre  [(contains? #{:left :right :up :down :up-left :up-right :down-left :down-right} direction)
          (not (nil? state))
          (vector? (get-in state [:world :npcs]))]
   :post [(vector? (get-in % [:world :npcs]))]}
  (let [player-x (-> state :world :player :pos :x)
        player-y (-> state :world :player :pos :y)
        [target-x
         target-y] (rw/player-adjacent-xy state direction)
        target-cell (rw/get-cell state target-x target-y)]
     ;; Hack down destroyable cells.
     (if (rw/type->destroyable? (get target-cell :type))
       (-> state
         (destroy-cell target-x target-y)
         (rw/assoc-current-state :normal)
         rc/inc-time)
        state)))

(defn action-left
  "performs actions to the player one space to the left provided he/she is able."
  [state]
  (action state :left))

(defn action-right
  "performs actions to the player one space to the right provided he/she is able."
  [state]
  (action state :right))

(defn action-up
  "performs actions to the player one space up provided he/she is able."
  [state]
  (action state :up))

(defn action-down
  "performs actions to the player one space down provided he/she is able."
  [state]
  (action state :down))

(defn action-up-left
  "performs actions to the player one space to the left and up provided he/she is able."
  [state]
  (action state :up-left))

(defn action-up-right
  "performs actions to the player one space to the right and up provided he/she is able."
  [state]
  (action state :up-right))

(defn action-down-left
  "performs actions to the player one space down and left provided he/she is able."
  [state]
  (action state :down-left))

(defn action-down-right
  "performs actions to the player one space down and right provided he/she is able."
  [state]
  (action state :down-right))

(defn use-stairs-island->generated
  [state dest-place-id dest-x dest-y]
  {:post [(not (nil? %))]}
    (-> state
      ;; unload old places
      (rworldgen/unload-all-places)
      ;; load the generated place
      (assoc-in [:world :places dest-place-id] (rworldgen/load-place state dest-place-id))
      ;; move the player to dest-x dest-y
      (rp/assoc-player-pos (rc/xy->pos dest-x dest-y))
      ;; set the dest place id
      (rw/assoc-current-place-id dest-place-id)))

(defn use-stairs-island->ungenerated
  [state src-x src-y dest-type gen-args]
  {:post [(not (nil? %))]}
  (log/info "use-stairs-island->ungenerated" src-x src-y dest-type gen-args)
  (let [; keep keys of island places to remove later
        island-place-ids (keys (get-in state [:world :places]))
        ; insert the new place into the world
        [state
         dest-place-id] (rworldgen/load-place state nil dest-type gen-args)
        _ (log/info "loaded-place" dest-place-id)
        ;; find the down-stairs location
        [_ ;{src-place-id :src-place-id}
          dest-x
          dest-y] (first (rw/filter-cellxys state
                                            (fn [[{cell-type :type} _ _]]
                                              (= cell-type :up-stairs))
                                             dest-place-id))]
        (log/info "dest-place-id" dest-place-id)
        (-> state
          ;; connect the island's down-stairs to the now-generated place
          (rw/update-cell src-x src-y (fn [cell] (assoc cell :dest-place-id dest-place-id
                                                             :dest-pos      (rc/xy->pos dest-x dest-y))))
                                                     
          ;; point destination stairs back to island
          (rw/update-cell dest-place-id
                          dest-x
                          dest-y
                          (fn [cell] (assoc cell :dest-place-id nil
                                                 :dest-type     nil
                                                 :dest-pos      (rc/xy->pos src-x src-y))))
          ;; move the player to [dest-place-id dest-x dest-y]
          (rp/assoc-player-pos (rc/xy->pos dest-x dest-y))
          (rw/assoc-current-place-id dest-place-id)
          ;; unload old places
          (rworldgen/unload-places island-place-ids))))

(defn use-stairs-generated->island
  [state dest-x dest-y]
  {:post [(not (nil? %))]}
  (-> state
    ;; move the player to [dest-place-id dest-x dest-y]
    (rp/assoc-player-pos (rc/xy->pos dest-x dest-y))
    (rw/assoc-current-place-id nil)
    (rworldgen/unload-all-places)
    ;; load island places
    (rworldgen/load-unload-places)
    (update-visibility)))
    
(defn use-stairs-generated->generated
  [state dest-place-id dest-x dest-y]
  {:post [(not (nil? %))]}
  (log/info "use-stairs-generated->generated" dest-place-id dest-x dest-y)
  (-> state
    (rworldgen/unload-all-places)
    ;; load the generated place
    (as-> state
      (first (rworldgen/load-place state dest-place-id nil nil)))
    ;; move the player to [dest-place-id dest-x dest-y]
    (rp/assoc-player-pos (rc/xy->pos dest-x dest-y))
    (rw/assoc-current-place-id dest-place-id)
    (update-visibility)))

(defn use-stairs-generated->ungenerated
  [state src-place-id src-x src-y dest-type gen-args]
  {:post [(not (nil? %))]}
  (let [; insert the new place into the world
        [state
         dest-place-id] (rworldgen/load-place state nil dest-type gen-args)
        _ (log/info "loaded-place" dest-place-id)
        ;; find the down-stairs location
        [_ ;{src-place-id :src-place-id}
          dest-x
          dest-y] (first (rw/filter-cellxys state
                                            (fn [[{cell-type :type} _ _]]
                                              (= cell-type :up-stairs))
                                             dest-place-id))]
        (log/info "dest-place-id" dest-place-id)
        (-> state
          ;; connect the island's down-stairs to the now-generated place
          (rw/update-cell src-x src-y (fn [cell] (assoc cell :dest-place-id dest-place-id
                                                             :dest-pos      (rc/xy->pos dest-x dest-y))))
                                                     
          ;; point destination stairs back to srd
          (rw/update-cell dest-place-id
                          dest-x
                          dest-y
                          (fn [cell] (assoc cell :dest-place-id src-place-id
                                                 :dest-pos      (rc/xy->pos src-x src-y))))
          ;; move the player to [dest-place-id dest-x dest-y]
          (rp/assoc-player-pos (rc/xy->pos dest-x dest-y))
          (rw/assoc-current-place-id dest-place-id)
          ;; unload old places
          (rworldgen/unload-place src-place-id))))

(defn use-stairs
  [state]
  {:post [(not (nil? %))]}
  (let [[player-cell x y] (rw/player-cellxy state)
        src-place-id     (rw/current-place-id state)]
    (if (rw/player-on-stairs? state)
      (let [dest-place-id  (get player-cell :dest-place-id)
            dest-type      (get player-cell :dest-type)
            dest-pos       (get player-cell :dest-pos)
            gen-args       (get player-cell :gen-args)
            src-kind       (if (nil? src-place-id)
                             :island
                             :generated)
            dest-kind      (cond
                             (and (nil? dest-place-id)
                                  (nil? dest-type))
                               :island
                             (and (nil? dest-place-id)
                                  (not (nil? dest-type)))
                               :ungenerated
                             :else
                               :generated)]
        (log/info "dest-place-id" (or dest-place-id "nil")
                  "dest-type" (or dest-type "nil")
                  "dest-pos" (or dest-pos "nil")
                  "gen-args" (or gen-args "nil"))
        (log/info "src-kind" src-kind "dest-kind" dest-kind)
        (let [state (cond
                      ;; island->generated
                      (and (= src-kind :island)
                           (= dest-kind :generated))
                        (use-stairs-island->generated state dest-place-id (get dest-pos :x) (get dest-pos :y))
                      ;; island->ungenerated
                      (and (= src-kind :island)
                           (= dest-kind :ungenerated))
                        (use-stairs-island->ungenerated state x y dest-type gen-args)
                      ;; generated->island
                      (and (= src-kind :generated)
                           (= dest-kind :island))
                        (use-stairs-generated->island state (get dest-pos :x) (get dest-pos :y))
                      ;; generated->generated
                      (and (= src-kind :generated)
                           (= dest-kind :generated))
                        (use-stairs-generated->generated state dest-place-id (get dest-pos :x) (get dest-pos :y))
                      ;; generated->ungenerated
                      (and (= src-kind :generated)
                           (= dest-kind :ungenerated))
                        (use-stairs-generated->ungenerated state src-place-id x y dest-type gen-args)
                      :else
                        (assert "Invalid combination of values"))]
          (assoc state :advance-time true)))
      (rc/ui-hint state "No stairs here."))))

(defn open-door
  "Open the door one space in the direction relative to the player's position."
  [state direction]
  (let [[target-x
         target-y] (rw/player-adjacent-xy state direction)]
    (log/debug "open-door")
    (log/info "target-xy" target-x target-y)
    (let [target-cell (rw/get-cell state target-x target-y)]
      (log/info "target-cell" target-cell)
      (if (and (not (nil? target-cell)) (= (target-cell :type) :close-door))
        (do
          (log/debug "opening door")
          (log/debug (rw/get-cell state target-x target-y))
          (-> state
            (rc/append-log "The door creaks open")
            (rw/assoc-cell target-x target-y :type :open-door)
            (rw/assoc-current-state :normal)))
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
  "Close the door one space in the direction relative to the player's position."
  [state direction]
  (let [[target-x
         target-y] (rw/player-adjacent-xy state direction)]
    (log/debug "close-door")
    (log/info "target-xy" target-x target-y)
    (let [target-cell (rw/get-cell state target-x target-y)]
      (log/debug "target-cell" target-cell)
      (if (and (not (nil? target-cell)) (= (target-cell :type) :open-door))
        (as-> state state
          (cond
            (seq (get target-cell :items []))
              (rc/append-log state "Cannot close door. There are items in the way.")
            (rw/npc-at-xy state target-x target-y)
              (rc/append-log state "Cannot close door. There is a creature in the way.")
            (= (rp/player-xy state) [target-x target-y])
              (rc/append-log state "Cannot close door. You're in the way.")
            :else
            (-> state
              (rc/append-log "The door closes")
              (rw/assoc-cell target-x target-y :type :close-door)))
          (rw/assoc-current-state state :normal))
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

(defn fixup-shelter
  [items]
  (map (fn [item]
    (log/info "fixing up" item)
    (if (contains? #{:tarp-hung :sail-hung} (item :item/id))
      ; merge so that id, name, name-plural are all copied
      (merge item (ig/id->item (case (item :item/id)
                                 :tarp-hung :tarp
                                 :sail-hung :sail)))
      item))
    items))

(defn remove-sheltermates
  [state shelter-item]
  (reduce (fn [state [x y]]
    (rw/dec-cell-item-count state x y (get shelter-item :item/id)))
    state
    (shelter-item :sibling-xys)))

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
    (let [direction          (get-in state [:world :pickup-direction])
          {x :x y :y}      (rw/player-adjacent-pos state direction)
          cell               (rw/get-cell state x y)
          items              (vec (get cell :items))
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
          selected-items     (if (= (count items) 1)
                               ;; if just one item, then auto select it and pick it up
                               items
                               (vec (divided-items :selected)))
          not-selected-items (if (= (count items) 1)
                               ;; if just one item, then auto select it and not-selected is empty
                               []
                               (vec (divided-items :not-selected)))
          selected-shelter   (first (filter (fn [item] (contains? #{:tarp-hung :sail-hung} (item :item/id)))
                                     selected-items))
          remaining-hotkeys  (vec (remove #(some (partial = %) (map :hotkey selected-items)) remaining-hotkeys))]
      (log/info "divided-items" divided-items)
      (log/info "selected-items" selected-items)
      (log/info "not-selected-items" not-selected-items)
      (if (seq selected-items)
        (let [new-state (-> state
                          (rc/append-log "You pick up:")
                          ;; dup the item into inventory with hotkey
                          (ri/add-to-inventory (fixup-shelter selected-items))
                          ;; remove the item from cell
                          (rw/assoc-cell-items x y not-selected-items)
                          (cond-> selected-shelter
                            (remove-sheltermates selected-shelter))
                          ;; reset selected-hotkeys
                          (assoc-in [:world :selected-hotkeys] #{}))]
          (if (= (count items) 1)
            (rw/assoc-current-state new-state :normal)
            new-state))
        state)))

(defn pickup-directions
  [state]
  (map second
       (filter (fn [[cell direction]]
                 (-> (get cell :items [])
                     count
                     pos?))
               (map (fn [direction]
                      [(rw/player-adjacent-cell state direction) direction])
                    rc/directions-ext))))

(defn assoc-pickup-target
  [state direction]
  {:pre  [(contains? rc/directions-ext direction)]}
  (let [directions (pickup-directions state)]
    (if (contains? (set directions) direction)
      (-> state
        (assoc-in [:world :pickup-direction] direction)
        (rw/assoc-current-state :pickup-selection))
      (-> state
          (rc/append-log "Nothing to pickup.")
          (rw/assoc-current-state :normal)))))
  
(defn pickup-left [state]
  (assoc-pickup-target state :left))

(defn pickup-right [state]
  (assoc-pickup-target state :right))

(defn pickup-up [state]
  (assoc-pickup-target state :up))

(defn pickup-down [state]
  (assoc-pickup-target state :down))

(defn pickup-up-left [state]
  (assoc-pickup-target state :up-left))

(defn pickup-up-right [state]
  (assoc-pickup-target state :up-right))

(defn pickup-down-left [state]
  (assoc-pickup-target state :down-left))

(defn pickup-down-right [state]
  (assoc-pickup-target state :down-right))

(defn pickup-center [state]
  (assoc-pickup-target state :center))


(defn pick-up-all
  [state]
    (let [direction (get-in state [:world :pickup-direction])
          {x :x y :y}        (rw/player-adjacent-pos state direction)
          cell               (rw/get-cell state x y)
          items              (vec (get cell :items))
          remaining-hotkeys  (-> state :world :remaining-hotkeys)
          selected-items     (map #(assoc %1 :hotkey %2)
                                  items
                                  (rc/fill-missing not
                                                  (fn [_ hotkey] hotkey)
                                                  remaining-hotkeys
                                                  (map :hotkey items)))
          selected-shelter   (first (filter  (fn [item] (contains? #{:tarp-hung :sail-hung} (item :item/id)))
                                     selected-items))
          remaining-hotkeys  (vec (remove #(some (partial = %) (map :hotkey selected-items)) remaining-hotkeys))]
      (log/debug "selected-items" selected-items)
      (if (seq selected-items)
        (let [new-state (-> state
                          (rc/append-log "You pick up:")
                          ;; dup the item into inventory with hotkey
                          (ri/add-to-inventory (fixup-shelter selected-items))
                          ;; remove the item from cell
                          (rw/assoc-cell-items x y [])
                          (cond-> selected-shelter
                            (remove-sheltermates selected-shelter))
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
      (let [item (nth items item-index)]
        (if (and (= (get player-cell :type) :campfire)
                 (ig/flammable? item))
          ; on campfire and item is flammable - burn it!
          (-> state
            ;; remove the item from inventory
            (assoc-in [:world :player :inventory]
             (vec (concat (subvec items 0 item-index)
                          (subvec items (inc item-index) (count items)))))
            (rw/update-player-cell update :fuel + (get item :fuel 0)))
          ; regular drop - put item on ground
          (let [new-state (-> state
                  (rc/append-log (format "You let the %s fall to the ground" (lower-case (get item :name "unknown"))))
                  ;; dup the item into cell
                  (rw/conj-cell-items x y (dissoc item :wielded :wielded-ranged))
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
            new-state)))
        state)))

(defn apply-bandage
  [state hotkey]
  {:pre  [(not (nil? state))]
   :post [(not (nil? %))]}
  (-> state
    (ri/dec-item-count hotkey)
    (assoc-in [:world :player :hp] (get-in state [:world :player :max-hp]))
    (assoc-in [:world :current-state] :normal)
    (rc/append-log "You apply the bandage.")))

(defn apply-lantern
  [state]
  {:pre  [(not (nil? state))]
   :post [(not (nil? %))]}
  (let [item         (ri/inventory-id->item state :lantern)
        item-state   (get item :state :off)]
    (case item-state
      :on
      (-> state
        (ri/update-inventory-item-by-id :lantern (fn [item] (assoc item :state :off)))
        (rc/append-log "You turn the lantern off.")
        (rc/inc-time)
        ;TODO: remove?
        (update-visibility)
        (assoc-in [:world :current-state] :normal))
      :off
      (if (pos? (get item :charge))
        (-> state
          (ri/update-inventory-item-by-id :lantern (fn [item] (assoc item :state :on)))
          (rc/append-log "You turn the lantern on.")
          (rc/inc-time)
          ;TODO: remove?
          (update-visibility)
          (assoc-in [:world :current-state] :normal))
        (-> state
          ;TODO: remove?
          (rc/inc-time)
          (rc/append-log state "You try turning the lantern on, but nothing happens."))))))

(defn wear-clothes
  [state selected-item]
  (-> state
    (rc/append-log (format "You wear the %s." (lower-case (get selected-item :name))))
    ;; remove :worn from all items
    (update-in [:world :player :inventory]
      (fn [items] (mapv (fn [item] (dissoc item :worn)) items)))
    (update-in [:world :player :inventory]
      (fn [items] (mapv (fn [item] (if (= item selected-item)
                                     (assoc item :worn true)
                                     item)) items)))))

(defn select-apply-item
  "Apply the item from the player's inventory whose hotkey matches `keyin`."
  [state keyin]
  {:pre  [(not (nil? state))]
   :post [(not (nil? %))]}
  (let [item (ri/inventory-hotkey->item state keyin)]
    (if item
      (let [id (get item :item/id)]
        (-> state
          (rai/assoc-apply-item item)
          (as-> state
            (cond
              (= id :bandage)
                (apply-bandage state keyin)
              (= id :lantern)
                (apply-lantern state)
              (= id :fishing-line-and-hook)
                (-> state
                  (rw/assoc-current-state :apply-item-inventory)
                  (rc/ui-hint "Pick an item to combine with."))
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
              (= id :door)
                (-> state
                  (rw/assoc-current-state :apply-item-normal)
                  (rc/ui-hint "Pick a direction to place the door."))
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
              (ig/is-shelter? item)
                (-> state
                  (rai/apply-shelter item)
                  (rw/assoc-current-state :apply-item-normal))
              (= id :log)
                (-> state
                  (rai/apply-log item)
                  (rw/assoc-current-state :normal))
              (= id :bamboo)
                (-> state
                  (rai/apply-log item)
                  (rw/assoc-current-state :normal))
              (= id :bedroll)
                (-> state
                  (rai/apply-bedroll item)
                  (rw/assoc-current-state :normal))
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
              (= id :rock)
                (-> state
                  (rw/assoc-current-state :apply-item-inventory)
                  (rc/ui-hint "Pick an item to bash with the rock."))
              (= id :plant-fiber)
                (-> state
                  (rai/apply-plant-fiber item)
                  (rw/assoc-current-state :normal))
              (ig/is-sharp? item)
                (-> state
                  (rw/assoc-current-state :apply-item-inventory)
                  (rc/ui-hint "Pick an item to use the sharp item on."))
              (ig/is-fruit? item)
                (-> state
                  (rw/assoc-current-state :apply-item-body)
                  (rc/ui-hint "a-apply to skin, b-apply to tongue"))
              (ig/is-clothes? item)
                (wear-clothes state item)
              (contains?  #{:stone-tablet :codex} id)
                (as-> state state
                  (if (and (= 0 (rr/uniform-int 10))
                           (= (get (rv/player-place state) :type) :temple))
                    ;; sometimes books act as magic-mapping
                    (let [[width height] (rv/viewport-wh state)]
                      (-> state
                        (rw/assoc-cells (zipmap (for [x (range width)
                                                      y (range height)]
                                                  [x y])
                                                (repeat {:discovered (rw/get-time state)})))
                        (rc/append-log "You learn the temple's secrets.")
                        rc/inc-time))
                    ;; otherwise choose between identifying fruit and flavor text
                    (let [text-id (ig/gen-text-id state)
                          item    (ri/inventory-id->item state id)]
                      (if (rp/player-status-contains? state text-id)
                        (-> state
                          (rc/append-log "You've already read that story.")
                          rc/inc-time)
                        (as-> state state
                          (if (ig/is-fruit-text-id? text-id)
                            ;; identified fruit
                            (-> state
                              (rc/conj-in [:world :fruit :identified] (ig/fruit-text-id->fruit-id text-id))
                              (rp/conj-player-status (get item :text-id)))
                            state)
                          (-> state
                           (rc/append-log (format "You peice together a story about %s." (rdesc/gen-temple-text text-id)))
                           rc/inc-time)))))
                  (ri/dec-item-count state keyin)
                  (rw/assoc-current-state state :normal))
              ;; pirate items
              (= id :dice)
                (rc/append-log state (format "You roll a %d and a %d" (rr/uniform-int 1 7) (rr/uniform-int 1 7)))
              :else state))))
        state)))

(defn harvest
  "Collect non-item resources from adjacent or current cell"
  [state direction]
  {:pre  [(contains? rc/directions-ext direction)]}
  (let [player-x      (-> state :world :player :pos :x)
        player-y      (-> state :world :player :pos :y)
        distance      (rp/player-distance-from-starting-pos state)
        [target-x
         target-y] (rw/player-adjacent-xy state direction)
        target-cell   (rw/get-cell state target-x target-y)
        harvestable   (get target-cell :harvestable false)
        harvest-items (if (not= target-cell nil)
                        (cond
                          (= (get target-cell :type) :tree)
                            (if (or harvestable
                                    (= 0 (rr/uniform-int 1000)))
                              [(rr/rand-nth [(ig/gen-item :stick)
                                             (ig/gen-item :stick)
                                             (ig/gen-item :stick)
                                             (ig/gen-item :branch)
                                             (ig/gen-item :branch)
                                             (ig/gen-item :plant-fiber)])]
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
                                [(rr/rand-nth [(ig/gen-item :unhusked-coconut) (ig/gen-item :unhusked-coconut) (ig/gen-item :unhusked-coconut) (ig/gen-item :plant-fiber)])]
                                []))
                          (= (get target-cell :type) :tall-grass)
                            (concat
                              (if (or harvestable
                                      (= 0 (rr/uniform-int 1000)))
                                [(rr/rand-nth [(ig/gen-item :grass) (ig/gen-item :plant-fiber)])]
                                []))
                          (= (get target-cell :type) :gravel)
                            (if (get target-cell :near-lava)
                              (concat
                                (if (or harvestable
                                        (= 0 (rr/uniform-int 1000)))
                                  [(rr/rand-nth [(ig/gen-item :rock) (ig/gen-item :obsidian)])]
                                  []))
                              (concat
                                (if (or harvestable
                                        ;; Make large flint stone have a higher chance of dropping away
                                        ;; from the player's starting-pos.
                                        (= 0 (rr/uniform-int 1000)))
                                  (concat
                                    (repeat (rr/uniform-int 4 6) (ig/gen-item :rock))
                                    [(rr/rand-nth [(ig/gen-item :rock) (if (< 300 (rr/uniform-double 100 (max 101 distance)))
                                                                         (ig/gen-item :large-flint)
                                                                         (ig/gen-item :flint))])])
                                  [])))
                          :else [])
                        [])]
    (log/info "harvested" harvest-items)
    (if (empty? harvest-items)
      (rc/append-log state "You don't find anything.")
      (-> state
        (ri/add-to-inventory harvest-items)
        (rw/update-cell target-x target-y (fn [cell] (dissoc cell :harvestable)))
        (as-> state (reduce rp/update-harvested state harvest-items))
        (rc/append-log (format "You gather %s." (clojure.string/join ", " (map #(if (> (get % :count 1) 1)
                                                                                  (format "%d %s" (get % :count) (get % :name-plural))
                                                                                  (format "%s %s" (if (contains? #{\a \e \i \o} (first (get % :name)))
                                                                                                     "an"
                                                                                                     "a")
                                                                                                  (get % :name)))
                                                                              harvest-items))))
        (rw/assoc-current-state :normal)))))

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

(defn smart-pickup
  [state]
  ;; If there is just one adjacent thing to pickup, just pick it up. Otherwise enter a pickup mode.
  (let [directions (pickup-directions state)]
    (log/info "smart-pickup directions" (vec directions))
    (cond
      (empty? directions)
        (-> state
          (rc/append-log "Nothing to pick up.")
          (rw/assoc-current-state :normal))
      (= (count directions) 1)
        (-> state
          (assoc-pickup-target (first directions))
          pick-up)
      :else
        (rw/assoc-current-state state :pickup))))

(defn harvestable-directions
  [state]
  (map second
       (filter (fn [[cell direction]]
                 (get cell :harvestable))
               (map (fn [direction]
                      [(rw/player-adjacent-cell state direction) direction])
                    rc/directions-ext))))

(defn smart-harvest
  [state]
  ;; If there is just one adjacent thing to harvest, just harvest it. Otherwise enter a harvest mode.
  (let [directions (harvestable-directions state)]
    (cond
      (empty? directions)
        (-> state
          (rc/append-log "Nothing to harvest.")
          (rw/assoc-current-state :normal))
      (= (count directions) 1)
        (-> state
          (harvest (first directions))
          (rw/assoc-current-state :normal))
      :else
        (rw/assoc-current-state state :harvest))))

(defn openable-directions
  [state]
  (map second
       (filter (fn [[cell direction]]
                 (contains? #{:close-door} (get cell :type)))
               (map (fn [direction]
                      [(rw/player-adjacent-cell state direction) direction])
                    rc/directions-ext))))
(defn smart-open
  [state]
  ;; If there is just one adjacent thing to open, just open it. Otherwise enter open mode.
  (let [directions (openable-directions state)]
    (cond
      (empty? directions)
        (-> state
          (rc/append-log "Nothing to open")
          (rw/assoc-current-state :normal))
      (= (count directions) 1)
        (-> state
          ;; TODO: open chests too
          (open-door (first directions))
          (rw/assoc-current-state :normal))
      :else
        (rw/assoc-current-state state :open))))

(defn closeable-directions
  [state]
  (map second
       (filter (fn [[cell direction]]
                 (contains? #{:open-door} (get cell :type)))
               (map (fn [direction]
                      [(rw/player-adjacent-cell state direction) direction])
                    rc/directions-ext))))

(defn quaff-directions
  [state]
  (->> rc/directions-ext
     (map (fn [direction]
            [(rw/player-adjacent-cell state direction) direction]))
     (filter (fn [[cell direction]]
               (rw/type->water? (get cell :type))))
    (map second)))

(defn smart-close
  [state]
  ;; If there is just one adjacent thing to close, just close it. Otherwise enter close mode.
  (let [directions (closeable-directions state)]
    (cond
      (empty? directions)
        (-> state
          (rc/append-log "Nothing to close")
          (rw/assoc-current-state :normal))
      (= (count directions) 1)
        (-> state
            (close-door (first directions))
          (rw/assoc-current-state :normal))
      :else
        (rw/assoc-current-state state :close))))

(defn directions-to-actions
  [state]
  (let [pickup-directions (pickup-directions state)
        _ (log/info "pickup directions" (vec pickup-directions))
        harvest-directions (harvestable-directions state)
        _ (log/info "harvest-directions" (vec harvest-directions))
        open-directions (openable-directions state)
        _ (log/info "open-directions" (vec open-directions))
        close-directions (closeable-directions state)
        _ (log/info "close-directions" (vec close-directions))
        quaff-directions (quaff-directions state)
        _ (log/info "quaff-directions" (vec quaff-directions))
        stairs-directions (if (rw/player-on-stairs? state) [:center] [])
        _ (log/info "player-on-stairs?" stairs-directions)
        actions (as-> [] actions
                  (if (seq pickup-directions)
                    (conj actions {:state-id :pickup
                                   :name "Pickup"})
                    actions)
                  (if (seq harvest-directions)
                    (conj actions {:state-id :harvest
                                   :name "Harvest"})
                    actions)
                  (if (seq open-directions)
                    (conj actions {:state-id :open
                                   :name "Open"})
                    actions)
                  (if (seq close-directions)
                    (conj actions {:state-id :close
                                   :name "Close"})
                    actions)
                  (if (seq quaff-directions)
                    (conj actions {:state-id :quaff
                                   :name "Quaff"}
                    actions))
                  (if stairs-directions
                    (conj actions {:state-id :stairs
                                   :name "Stairs"})
                    actions))
        ;; direction->#{set of actions}
        directions (reduce-kv (fn [directions action action-directions]
                             (merge-with clojure.set/union directions (zipmap action-directions (repeat #{action}))))
                           {}
                           {:pickup  pickup-directions
                            :harvest harvest-directions
                            :open    open-directions
                            :close   close-directions
                            :quaff   quaff-directions
                            :stairs  stairs-directions})]
      (log/info "num pickup-directions" (count pickup-directions)
                "num harvest-directions" (count harvest-directions)
                "num open-directions" (count open-directions)
                "num close-directions" (count close-directions))
    directions))

(defn direction->unicode-char
  [direction]
  (case direction
    :center     \·
    :up         \↑ 
    :down       \↓
    :left       \←
    :right      \→
    :up-left    \↖
    :up-right   \↗
    :down-left  \↙
    :down-right \↘))

(def quaff-popover-message
  "This water is not potable. Are you sure?")

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
                (rw/player-adjacent-xys-ext state))]
    (log/info "player-adj-xys" (rw/player-adjacent-xys state))
    (-> state
      (update-in [:world :player :thirst] (fn [thirst] (min 0 (- thirst water))))
      (rc/append-log "You drink deeply from the water."))))
  
(defn quaff-cell-at-pos
  ([state]
  (-> state
    (update-in [:world :player :thirst] (fn [thirst] (min 0 (- thirst 10))))
    (rp/player-update-hp (fn [hp] (max (- hp 1) 0)))
    (rw/assoc-current-state :normal)))
  ([state {x :x y :y}]
  (if-let [water (get (rw/get-cell state x y) :water)]
    (do (log/info "Drinking potable water")
    (-> state
      (rw/assoc-cell state x y :water 0)
      (update-in [:world :player :thirst] (fn [thirst] (min 0 (- thirst water))))
      (rw/assoc-current-state :normal)))
    (do (log/info "Drinking nonpotable water")
    (-> state
      (assoc-in [:world :popover-message] quaff-popover-message)
      (assoc-in [:world :quaff-pos] (rc/xy->pos x y))
      (rw/assoc-current-state :quaff-popover))))))

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
  (log/info "remaining-hotkeys" (get-in state [:world :remaining-hotkeys]))

  (if-let [item (ri/inventory-hotkey->item state keyin)]
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
      (ri/remove-from-inventory keyin)
      (as-> state
        (case (get item :item/id)
          :coconut (ri/add-to-inventory state [(ig/gen-item :coconut-empty)])
          state)))
     state))
(defn quaff-select
  "Select the next state depending on what quaffable items are available."
  [state]
  (let [num-adjacent-quaffable-cells (count
                                       (filter (fn [cell] (and (not (nil? cell))
                                                               (contains? #{:freshwater-hole :saltwater-hole :spring} (get cell :type))
                                                               (> (get cell :water 100) 10)))
                                               (rw/player-adjacent-cells-ext state :include-center)))
        num-adjacent-dangerous-quaffable-cells
                                     (count
                                       (filter (fn [cell] (and (not (nil? cell))
                                                               (contains? #{:water
                                                                            :surf
                                                                            :ocean
                                                                            :shallow-water
                                                                            :swamp}
                                                                          (get cell :type))))
                                               (rw/player-adjacent-cells-ext state :include-center)))
        _ (log/info "player-adj-cells" (rw/player-adjacent-cells state))
        quaffable-inventory-item? (some (fn [item] (contains? item :thirst)) (ri/player-inventory state))]
    (cond
      (and (or (pos? num-adjacent-quaffable-cells)
               (pos? num-adjacent-dangerous-quaffable-cells))
           quaffable-inventory-item?)
        (rw/assoc-current-state state :quaff-adj-or-inv)
      (and (= num-adjacent-quaffable-cells 1)
           (zero? num-adjacent-dangerous-quaffable-cells))
        (quaff-only-adjacent-cell state)
      (and (= num-adjacent-dangerous-quaffable-cells 1)
           (zero? num-adjacent-quaffable-cells))
        (-> state
          (assoc-in [:world :popover-message] quaff-popover-message)
          (rw/assoc-current-state :quaff-popover))
      (or (> num-adjacent-quaffable-cells 1)
          (> num-adjacent-dangerous-quaffable-cells 1))
        (-> state
          (rw/assoc-current-state :quaff-adj)
          (rc/ui-hint "Pick a direction to drink."))
      quaffable-inventory-item?
        (rw/assoc-current-state state :quaff-inventory)
      :else
        (-> state
          (rc/ui-hint "There is nothing to drink.")
          (rw/assoc-current-state :normal)))))

(defn action-select
  [state]
  (let [directions (directions-to-actions state)]
    (log/info directions)
    (cond
      (empty? directions)
        (-> state
          (rc/append-log "No available actions.")
          (rw/assoc-current-state :normal))
      ;; only one direction and one action?
      (and (= (count directions) 1)
           (= (count (-> directions first second)) 1))
        ;; then just do the action
        (case (-> directions first second first)
          :pickup
            (smart-pickup state)
          :harvest
            (smart-harvest state)
          :open
            (smart-open state)
          :close
            (smart-close state)
          :quaff
            (quaff-select state)
          :stairs
            (use-stairs state))
      ;; only one direction but multiple actions?
      (= (count directions) 1)
        ;; let the player choose the action
        (-> state
          (assoc-in [:world :action-select]
                    (map (fn [action hotkey]
                           {:name (name action) :hotkey hotkey :direction (ffirst directions) :action action})
                         (-> directions first second)
                         rc/hotkeys))
          (rw/assoc-current-state :action-select))
      ;; multiple directions
      :else
        ;; let the player choose the direction
        (-> state
          (rw/assoc-current-state :direction-select)
          (rc/ui-hint (format "Pick a direction. (<%s>)" (apply str (map direction->unicode-char (keys directions)))))))))

(def ed-chan (async/chan))
(let [ctrl (atom false)]
  (async/go-loop [input (async/<! ed-chan)]
    (if (= input :lcontrol)
      (reset! ctrl true)
      (let [i (cond
                  @ctrl
                    (str "C-" input)
                  (char? input)
                    (str input)
                  (keyword? input) 
                    (get {:escape "esc"
                          :enter "\n"
                          :space " "} input (name input))
                  :else
                    (get {\: "colon"} input input))]
        (log/info i)
        (try
          (lag/send-input i)
          (catch Throwable t
            (log/error t)))
        (reset! ctrl false)))
    (recur (async/<! ed-chan))))
  
(defn debug-input
  [state input]
  (async/put! ed-chan input)
  state)

(defn do-selected-direction
  [state keyin]
  (let [direction  keyin
        directions (directions-to-actions state)]
    (log/info "keyin" keyin "directions" directions)
    (if (contains? directions direction)
      ;; If there is just one action in that direction
      (if (= (count (get directions keyin)) 1)
        ;; Just do the action
        (let [action (-> directions direction first)]
          (log/info "doing action" action)
          (case action
            :pickup
              (assoc-pickup-target state direction)
            :harvest
              (harvest state direction)
            :open
              (open-door state direction)
            :close
              (close-door state direction)
            (rw/assoc-current-state state :normal)))
        ;; let the player choose the action
        (-> state
          (assoc-in [:world :action-select]
                    (map (fn [action hotkey]
                           {:name (name action) :hotkey hotkey :direction direction :action action})
                         (get directions direction)
                         rc/hotkeys))
          (rw/assoc-current-state :action-select)))
      ;; player chose an invalid direction, reset
      (-> state
        (rc/ui-hint "You choose not to.")
        (rw/assoc-current-state :normal)))))
        
(defn do-selected-action
  [state keyin]
  (log/info "action-select" (vec (get-in state [:world :action-select])))
  (if-let [action (first (filter (fn [{:keys [state-id hotkey]}]
                              (= hotkey keyin))
                            (get-in state [:world :action-select])))]
    (do (log/info "action" action)
    (case (get action :action)
      :pickup
        (if (contains? action :direction)
          (assoc-pickup-target state (get action :direction))
          (smart-pickup state))
      :harvest
        (if (contains? action :direction)
          (harvest state (get action :direction))
          (smart-harvest state))
      :open
        (if (contains? action :direction)
          (open-door state (get action :direction))
          (smart-open state))
      :close
        (if (contains? action :direction)
          (close-door state (get action :direction))
          (smart-close state))
      :quaff
        (if (contains? action :direction)
          (quaff-only-adjacent-cell state)
          (quaff-only-adjacent-cell state))
      :stairs
        (use-stairs state)))
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

(defn start-sleep
  [state]
  (let [d (rlos/sight-distance state)]
    (if (> d 4)
      (rc/ui-hint state "You can't sleep yet. Try again when it is dark.")
      (-> state
        (assoc-in [:world :weather] (rr/rand-nth [:clear :clear :rain]))
        (assoc-in [:world :sleep-start-time] (rw/get-time state))
        (rw/assoc-current-state :sleep)))))

(defn do-sleep
  "Sleep."
  [state keyin]
  (let [d (rlos/sight-distance state)
        item-ids (->> state rw/player-cellxy first :items (map :item/id) set)
        ; FIXME set sleep-start time when crafting
        sleep-start-time (get-in state [:world :sleep-start-time] 0)
        {:keys [hours mins min-rem]} (rc/turns-to-time (- (rw/get-time state) sleep-start-time))
        player-under-shelter? (some (fn [shelter-id] (contains? item-ids shelter-id))
                                    #{:tarp-hung :sail-hung})
        player-on-bedroll? (contains? item-ids :bedroll)]
    (if (> d 4)
      (-> state
        (cond->
          (or player-on-bedroll?
              player-under-shelter?)
          (rp/player-update-wtl
            (fn [will-to-live] (rp/player-max-wtl state))))
        (update-in [:world :dream]
          assoc
          :hours hours
          :player-on-bedroll? player-on-bedroll?
          :player-under-shelter? player-under-shelter?
          :description (rdreams/description))
        (rw/assoc-current-state :dream))
      (do-rest state))))

(defn eat
  "Remove the item whose `:hotkey` equals `keyin` and subtract from the player's
   hunger the item's `:hunger` value."
  [state keyin]
  (log/info "poisoned fruit" (get-in state [:world :fruit :poisonous]))
  (log/info (rw/inventory-and-player-cell-hotkey->item state keyin))
  (if-let [item (rw/inventory-and-player-cell-hotkey->item state keyin)]
    (let [[x y] (rp/player-xy state)
          unknown-fruit (and (ig/is-fruit? item)
                             (not (contains? (get-in state [:world :fruit :identified]) (get item :item/id))))]
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
          (if (contains? (set (map :hotkey (ri/player-inventory state))) keyin)
            ;; remove the item from inventory
            (ri/dec-item-count state keyin)
            ;; remove the item from the current-cell
            (rw/dec-cell-item-count state (get item :item/id)))
          (if (= (get item :item/id) :coconut-empty)
            (ri/add-to-inventory state [(ig/gen-item :coconut-shell)])
            state))
        (cond-> unknown-fruit
            (rpop/show-popover "You decide to eat the unknown fruit. Good luck."))
        ;; if the item was a poisonous fruit, set a poisoned timebomb
        (as-> state
          (if (and (ig/is-fruit? item)
                   (contains? (set (get-in state [:world :fruit :poisonous])) (get item :item/id)))
            (do
              (log/info "Ate poisoned fruit." item)
              (assoc-in state
                        [:world :player :poisoned-time]
                        (apply min (remove nil?
                                           [(get-in state [:world :player :poisoned-time])
                                            (+ (rw/get-time state) (rr/uniform-int 100 200))]))))
            state))
        (cond-> (not unknown-fruit)
          (rw/assoc-current-state :normal))))
    state))

(defn init-cursor
  "Initialize the selection cursor at the player's current location."
  [state]
  (let [player-xy    (rp/player-xy state)
        viewport-xy  (rv/viewport-xy state)
        cursor-pos   (apply rc/xy->pos (rv/-xy player-xy viewport-xy))]
      ; cursor pos = player pos in screen coordinates
      (rv/assoc-cursor-pos state cursor-pos)))

(defn find-trap
  [state x y]
  (if (and (rw/is-trap-type? (rw/get-cell-type state x y))
           (< 1 (rr/uniform-int 10)))
    (rw/assoc-cell state  x y :trap-found true)
    state))

(defn find-traps
  [state]
  (let [[x y] (rp/player-xy state)]
    (reduce (fn [state [x y]]
              (find-trap state x y))
            state
            (rw/adjacent-xys x y))))

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

(defn wield-ranged
  "Wield the ranged weapon item from the player's inventory whose hotkey matches `keyin`."
  [state keyin]
  (let [items (-> state :world :player :inventory)
        inventory-hotkeys (map #(% :hotkey) items)
        item-index (.indexOf inventory-hotkeys keyin)]
    (if (and (>= item-index 0) (< item-index (count items)))
      (let [selected-item (nth items item-index)
            new-state (-> state
              (rc/append-log (format "You wield the %s." (lower-case (get selected-item :name))))
              ;; remove :wielded-ranged from all items
              (update-in [:world :player :inventory]
                (fn [items] (mapv (fn [item] (dissoc item :wielded-ranged)) items)))
              (update-in [:world :player :inventory]
                (fn [items] (mapv (fn [item] (if (= item selected-item)
                                               (assoc item :wielded-ranged true)
                                               item)) items))))]
        new-state)
        state)))

(defn init-select-ranged-target
  [state]
  (let [ranged-weapon-item  (first (filter (fn [item] (get item :wielded-ranged))
                                           (ri/player-inventory state)))]
    (log/info "ranged-weapon-item" ranged-weapon-item)
    ;; check if the player has a ranged wielded weapon
    (if ranged-weapon-item
      (if (or (not (ig/requires-reload? ranged-weapon-item))
              (get ranged-weapon-item :loaded))
        ;; save some values so we don't have to recalc them each time
        (let [npcs-in-range    (rnpc/visible-npcs state)
              npcs-by-distance (sort-by (fn [npc]
                                          (rp/player-distance-from-pos state (get npc :pos)))
                                        npcs-in-range)]
          ;; any npcs in range?
          (if (seq npcs-by-distance)
            (-> state
              (assoc-in [:world :target-ranged-pos-coll] (map :pos npcs-by-distance))
              (assoc-in [:world :target-ranged-index] 0)
              (rw/assoc-current-state :select-ranged-target)
              (rc/ui-hint "<f>-fire. <Tab>/<n>-next target. <p>-previous target"))
            (rc/ui-hint state "No targets in range")))
          (rc/ui-hint state "Must reload (<r>) weapon first."))
        (rc/ui-hint state "Must wield ranged weapon first (<W>)"))))

(defn reload-ranged-weapon
  [state]
  (let [ranged-weapon-item  (first (filter (fn [item] (get item :wielded-ranged))
                                           (ri/player-inventory state)))]
    ;; player is weilding ranged weapon?
    (if ranged-weapon-item
      ;; ranged weapon is not already loaded?
      (if (not (get ranged-weapon-item :loaded))
        ;; the weapon requires reloading?
        (if (ig/requires-reload? ranged-weapon-item)
          ;; the player has ammo for the weapon?
          (if (pos? (ri/inventory-id->count state (ig/item->ranged-combat-ammunition-item-id ranged-weapon-item)))
            (-> state
              ;; set weapon as :loaded
              (ri/update-inventory-item-by-id
                (get ranged-weapon-item :item/id)
                (fn [item] (assoc item :loaded true)))
              ;; dec ranged weapon ammunition
              (ri/dec-item-count (-> ranged-weapon-item
                                   ig/item->ranged-combat-ammunition-item-id
                                   (as-> id (ri/inventory-id->item state id))
                                   :hotkey))
              ;; successful reloading takes a turn
              (rc/inc-time))
            (rc/ui-hint state "You do not have the required ammunition."))
          (rc/ui-hint state "You do not need to reload this weapon."))
        (rc/ui-hint state "The weapon is already loaded."))
      (rc/ui-hint state "Must weild ranged weapon first (<W>)."))))

(defn select-next-ranged-target
  [state]
  (-> state
    (update-in 
      [:world :target-ranged-index]
      (fn [idx] (mod (inc idx)
                     (count (get-in state [:world :target-ranged-pos-coll])))))
    (rc/ui-hint "<f>-fire. <Tab>/<n>-next target. <p>-previous target")))

(defn select-previous-ranged-target
  [state]
  (-> state
    (update-in
      [:world :target-ranged-index]
      (fn [idx] (mod (dec idx)
                     (count (get-in state [:world :target-ranged-pos-coll])))))
    (rc/ui-hint "<f>-fire. <Tab>/<n>-next target. <p>-previous target")))

(defn fire-wielded-ranged-weapon
  [state]
  (let [target-ranged-index (get-in state [:world :target-ranged-index])
        target-pos          (nth (get-in state [:world :target-ranged-pos-coll])
                                 target-ranged-index)
        npc                 (rnpc/npc-at-pos state target-pos)
        ranged-weapon-item  (first (filter (fn [item] (get item :wielded-ranged))
                                                (ri/player-inventory state)))
        [target-x
         target-y]          (rc/pos->xy target-pos)
        path                (rlos/line-segment (rp/player-xy state) [target-x target-y])
        do-fire             (fn [state]
                              (rfx/conj-effect
                                state
                                (get ranged-weapon-item :ranged-attack :airborn-item)
                                (if-let [ammunition-item-id (ig/item->ranged-combat-ammunition-item-id ranged-weapon-item)]
                                  ;; weapon shoots ammo
                                  (-> ammunition-item-id
                                    ig/id->item
                                    (assoc :effects (get ranged-weapon-item :effects))
                                    (assoc :attacker (rp/get-player state)))
                                  ;; weapon "throws" itself
                                  (assoc ranged-weapon-item :attacker (rp/get-player state)))
                                true ;persist-item
                                (rest path)
                                (count path)
                                (get ranged-weapon-item :ch-cycle)))]

    (log/info "player-xy" (rp/player-xy state))
    (log/info "target-ranged-index" target-ranged-index)
    (log/info "target-ranged-pos-coll" (get-in state [:world :target-ranged-pos-coll]))
    (log/info "npc" npc)
    (log/info "ranged-weapon-item" ranged-weapon-item)
    (log/info "path" path)
    ;; the weapon requires reloading?
    (if (ig/requires-reload? ranged-weapon-item)
      (if (< 0 (-> ranged-weapon-item
                 ig/item->ranged-combat-ammunition-item-id
                 (as-> id
                   (ri/inventory-id->count state id))))
          ;; dec ranged weapon ammunition
          (-> state
            (ri/dec-item-count
              (-> ranged-weapon-item
                ig/item->ranged-combat-ammunition-item-id
                (as-> id
                  (ri/inventory-id->item state id))
                :hotkey))
            do-fire)
          (rc/ui-hint state "You need to reload first."))
      ; Add weapon actor and effect
      (-> state
        (cond-> (= (ranged-weapon-item :ranged-attack) :airborn-item)
          (ri/dec-item-count (get ranged-weapon-item :hotkey)))
        do-fire))))

(defn free-cursor
  "Dissassociate the cursor from the world."
  [state & more]
  (rc/dissoc-in state [:world :cursor]))

(defn move-cursor
  [state direction]
  (let [;; player cusror and target in screenspace
        [player-x
         player-y]    (rv/world-xy->screen-xy state (rp/player-xy state))
        {cursor-x :x
         cursor-y :y} (rv/get-cursor-pos state)
        target-x      (+ cursor-x (case direction
                                    :left -1
                                    :right 1
                                    :up-left -1
                                    :up-right 1
                                    :down-left -1
                                    :down-right 1
                                    0))
        target-y      (+ cursor-y (case direction
                                    :up  -1
                                    :down 1
                                    :up-left -1
                                    :up-right -1
                                    :down-left 1
                                    :down-right 1
                                    0))
        cursor-pos    (rc/xy->pos (rc/bound 0 target-x (dec (get-in state [:world :viewport :width])))
                                  (rc/bound 0 target-y (dec (get-in state [:world :viewport :height]))))]
    (if (and (= (rw/current-state state) :select-throw-target)
             (rc/farther-than? player-x player-y target-x target-y (* 5 (rp/player-dexterity state))))
      (rc/ui-hint state "You can't throw that far.")
      (rv/assoc-cursor-pos state cursor-pos))))

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
  (let [[x y]      (rv/get-cursor-world-xy state)]
    (-> state
      (rc/append-log (rdesc/describe-cell-at-xy state x y))
      (free-cursor))))

(defn start-talking
  "Open the door one space in the direction relative to the player's position.

   Valid directions are `:left` `:right` `:up` `:down`."
  [state direction]
  (let [[target-x
         target-y] (rw/player-adjacent-xy state direction)]
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

(defn transition-recipes
  [state]
  (let [npcs-in-range    (rnpc/visible-npcs state)]
    (if (not-empty npcs-in-range)
      (rc/ui-hint state "Cannot craft when creatures are nearby.")
      (rw/assoc-current-state state :recipes))))
  

(defn select-recipe [keyin state]
  (assoc-in state [:world :selected-recipe-hotkey] keyin))

(defn transition-make-recipe [state]
  (let [recipe (rcrafting/selected-recipe state)]
    (log/info "transition-make-recipe" recipe)
    (if (get recipe :empty)
      state
      (rw/assoc-current-state state :craft))))

(defn transition-select-recipe-type [state]
  (if (contains? #{\a \b \c} (get-in state [:world :selected-recipe-hotkey]))
    (let [current-recipe-in-progress (-> state rcrafting/current-recipe rcrafting/in-progress?)]
      (log/info "current-recipe-in-progress" current-recipe-in-progress (-> state rcrafting/current-recipe :done))
      (log/info "current-recipe" (rcrafting/current-recipe state))
      (if current-recipe-in-progress
        ; current-recipe is in progress, continue
        (rw/assoc-current-state state :in-progress-recipe)
        ; if current-recipe is complete, replace. Transition to select-recipe-type
        (rw/assoc-current-state state :select-recipe-type)))
    state))

(defn new-or-continue-recipe [state]
  (let [selected-recipe-hotkey (get-in state [:world :selected-recipe-hotkey])
        recipe (rcrafting/current-recipe state)]
    (if (and (contains? #{\a \b \c} selected-recipe-hotkey)
             ; new recipe
             (or (nil? recipe)
                 ; or continue in-progress recipe
                 (rcrafting/in-progress? recipe)))
        (transition-select-recipe-type state)
        state)))


(defn replace-recipe [state]
  (let [selected-recipe-hotkey (get-in state [:world :selected-recipe-hotkey])]
    (if (and (contains? #{\a \b \c} selected-recipe-hotkey)
             (-> state rcrafting/current-recipe rcrafting/complete?))
      (-> state
        (update-in [:world :recipes]
          dissoc selected-recipe-hotkey)
        transition-select-recipe-type)
      state)))

(defn craft-new-recipe
  "Start creating a new a crafting recipe."
  [state recipe-type]
  (let [current-recipe (rcrafting/current-recipe state)]
    (if (and false current-recipe)
      state
      (let [_ (log/info "Generating recipe")
            recipe-ns (case recipe-type
                        :weapon 'robinson.crafting.weapon-gen
                        :boat 'robinson.crafting.boat-gen
                        #_#_:trap rc-trap-gen
                        #_#_:food rc-food-gen)
            _ (log/info "recipe-ns" recipe-ns)
            _ (assert (some? recipe-ns))
            recipe {:type recipe-type}
            new-state (-> state
                        (assoc-in [:world :recipes (get-in state [:world :selected-recipe-hotkey])] recipe)
                        (rcrafting/init recipe-ns recipe))]
        (log/info "done with in-progress-recipes")
        new-state))))

(defn select-recipe-type-weapon
  [state]
  (craft-new-recipe state :weapon))
  
(defn select-recipe-type-boat
  [state]
  (craft-new-recipe state :boat))
  
(defn select-recipe-type-trap
  [state]
  (craft-new-recipe state :trap))
  
(defn select-recipe-type-food
  [state]
  (craft-new-recipe state :food))
  
(defn select-recipe-type-signal
  [state]
  (craft-new-recipe state :signal))
  
(defn select-recipe-type-survival
  [state]
  (craft-new-recipe state :survival))

(defn craft-in-progress-recipe
  "Take a step in crafting a recipe."
  [state keyin]
  (let [recipe (rcrafting/current-recipe state)
        recipe-type (get recipe :type)
        _ (log/info "Recipe type" recipe-type)
        #_ (log/info "Recipe"  recipe)
        recipe-ns (case recipe-type
                        :weapon 'robinson.crafting.weapon-gen
                        :boat 'robinson.crafting.boat-gen
                    #_#_:trap rc-trap-gen
                    #_#_:food rc-food-gen)]
    (case keyin
      :up (rcrafting/update-current-stage state :start-index (fn [i] (max (dec (or i 0)) 0)))
      :down (let [num-choices (-> state rcrafting/current-stage :event/choices count)]
                    (rcrafting/update-current-stage
                      state
                      :start-index
                      (fn [i]
                        (min (inc (or i 0)) (- num-choices 8)))))
      (rcrafting/update state recipe-ns keyin))))

(defn craft
  "Use a recipe to make an item."
  [state]
  (let [selected-recipe-hotkey (get-in state [:world :selected-recipe-hotkey])]
    (if (contains? #{\a \b \c} selected-recipe-hotkey)
      (let [recipe (get-in state [:world :recipes selected-recipe-hotkey])]
        (rcrafting/craft-recipe state recipe))
      state)))

(defn craft-select
  "Select an inventory item to be used in a recipe."
  [state keyin]
  (let [selected-recipe-hotkey (get-in state [:world :selected-recipe-hotkey])
        recipe (get-in state [:world :recipes selected-recipe-hotkey])
        selected-slot (get-in state [:world :selected-slot])]
    (cond
      ; in slot-selected mode, escape clears slot
      (and selected-slot (= keyin :escape))
        (-> state
          (assoc-in [:world :selected-slot] nil)
          (assoc-in [:world :recipes selected-recipe-hotkey :slots selected-slot] nil))
      ; regular escape, change state to recipes
      (= keyin :escape)
        (rw/assoc-current-state state :recipes)
      ; number? select slot
      (contains? (set (map char (range (int \0) (int \9)))) keyin)
        (do 
            (log/info [:world :selected-slot] keyin)
            (assoc-in state [:world :selected-slot] (Integer/parseInt (str keyin))))
      ; make the item
      (= keyin :space)
        (let [satisfied (rcrafting/requirements-satisfied? (rcrafting/recipe-requirements recipe))]
          (-> state
            (rcrafting/craft-recipe recipe)
            (rw/assoc-current-state :normal)))
      ; else, connect selected slot to inventory item by hotkey
      :default
        (let [item-inventory-count (ri/inventory-id->count state (ri/inventory-hotkey->item-id state keyin))
              slots (keys (get-in state [:world :recipes selected-recipe-hotkey :slots]))
              hotkey-count (count (mapcat (fn [slot]
                                            (let [slot-hotkey (get-in state [:world :recipes selected-recipe-hotkey :slots slot])]
                                              (if (= slot-hotkey keyin)
                                                [slot-hotkey]
                                                [])))
                                          slots))]
          (log/info "item-inventory-count" item-inventory-count)
          (log/info "hotkey-count" hotkey-count)
          ; skip if there are not enough items of that type in inventory
          (if (< hotkey-count item-inventory-count)
            (-> state
              (assoc-in [:world :selected-slot] nil)
              (assoc-in [:world :recipes selected-recipe-hotkey :slots selected-slot] keyin))
            state)))))

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
      (rc/ui-hint "Select target.")
      (assoc-throw-item item)
      (init-cursor))
    state))

(defn throw-selected-inventory
  [state]
  {:pre  [(rv/get-cursor-pos state)]}
  (let [item           (-> state
                         get-throw-item
                         (assoc :attacker (rp/get-player state)))
        [target-x
         target-y]     (rv/get-cursor-world-xy state)
        path           (rest (rlos/line-segment (rp/player-xy state) [target-x target-y]))]
    (-> state
      (free-cursor)
      ; remove the item from inventory
      (ri/dec-item-count (get item :hotkey))
      ; Add airborn item effect
      (rfx/conj-effect :airborn-item item true path 5))))

(defn scroll-log-up
  [state]
  (let [t        (rw/get-time state)
        log-idx  (get-in state [:world :log-idx])
        num-logs (count (rlog/current-logs state))]
    (log/info "current-logs" (rlog/current-logs state))
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
          (update-in [:world :player :hunger] (partial + 0.03 (* 0.025 (count (ri/player-inventory state)))))
          (update-in [:world :player :thirst] (partial + 0.12))))
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
          (-> state
            (rc/append-log "You need to eat now." :yellow)
            (rpop/show-popover "You need to eat now."))
          state))
      (let [new-thirst (get-in state [:world :player :thirst])]
        (log/info "thirst" thirst "new-thirst" new-thirst)
        (if (< thirst 80 new-thirst)
          (-> state
            (rc/append-log "You need to drink something now." :blue)
            (rpop/show-popover "You need to drink something now."))
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
  (let [[cell _ _] (rw/player-cellxy state)
        bedroll?   (contains? (set (map :item/id (get cell :items []))) :bedroll)
        in-water?  (contains? #{:ocean :surf :shallow-water} (get cell :type))
        rain-rate  (if (and (= (get-in state [:world :weather] :celar) :rain)
                            (not (rw/in-dungeon? state)))
                     0 (rfx-rain/rain-rate (rw/get-time state)))]
    (if (= (rw/current-state state) :sleep)
      (if bedroll?
        (rp/player-update-wtl state
          (fn [will-to-live] (+ 0.05 will-to-live)))
        state)
      (-> state
        (rp/player-update-wtl
          (fn [will-to-live]
            (let [dwtl       0.0 ;; you've been on the island. It sucks and you want to get off.
                  ;; if it is night and the player is not within range of a fire, things are extra tough.
                  dwtl       (if (and (rw/is-night? state)
                                      (not-any? #(contains? #{:campfire :fire} (get % :type) :fire)
                                                (rw/cells-in-range-of-player state 3)))
                               (+ dwtl 0.3)
                               dwtl)
                  ;; if it is night and the player is not wearing clothes, things are extra tough.
                  dwtl       (if (and (rw/is-night? state)
                                      (nil? (rp/worn-item state)))
                               (+ dwtl 0.5)
                               dwtl)
                  ;; in water, cold and wet - less will to live.
                  dwtl       (+ dwtl
                                (if in-water?
                                  0.1
                                  0))
                  ; raining?
                  dwtl       (+ dwtl
                                ; raining in overworld and not under shelter?
                                (if (and (not (rw/in-dungeon? state))
                                         (not (some #{:tarp-hung
                                                      :sail-hung}
                                                (->> state
                                                rw/current-cell-items
                                                (map :item/id)
                                                set)))
                                         (= (get-in state [:world :weather]) :rain))
                                  (* 2 (rfx-rain/rain-rate (rw/get-time state)))
                                  0))
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
            state))))))

(defn log-will-to-live-flavor
  "Log a flavor message when will-to-live increases or decreases by a lot."
  [state prev-will-to-live]
  {:pre [(not (nil? state))]
   :post [(not (nil? %))]}
  (let [dwtl (- prev-will-to-live (get-in state [:world :player :will-to-live]))]
    (if (> (Math/abs (float dwtl)) 1.5)
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
                             (rw/player-adjacent-cells-ext state :include-center)))
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

(defn decrease-lantern-charge
  [state]
  {:pre [(not (nil? state))]
   :post [(not (nil? %))]}
  (if-let [lantern (ri/inventory-id->item state :lantern)]
    (ri/update-inventory-item-by-id state :lantern (fn [lantern] (as-> lantern lantern
                                                                    (if (= (get lantern :state :off) :on)
                                                                        (update-in lantern [:charge] dec)
                                                                        lantern)
                                                                    (if (neg? (get lantern :charge))
                                                                        (assoc lantern :state :off)
                                                                        lantern))))
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
           (contains? (set (map :item/id items)) :raft))
        (-> state
          (rc/append-log "You dismount the raft.")
          (assoc-in [:world :player :mounted] false))
      (and (not mounted) (contains? (set (map :item/id items)) :raft))
        (-> state
          (rc/append-log "You mount the raft.")
          (assoc-in [:world :player :mounted] true))
      (not (contains? (set (map :item/id items)) :raft))
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
    (log/debug "npcs" (with-out-str (clojure.pprint/pprint (-> state :world :npcs))))
    state))

(defn save-score
  [state]
  (rs/persist-state-score! state)
  (assoc state :points (rs/state->points state)))


(defn transition-privacy
  [state]
  (let [privacy-file (rfs/cwd-file "config/.privacyoptin")]
    (if (.exists privacy-file)
      :connecting
      :privacy)))
    
(defn privary-opt-in
  [state]
  (spit (rfs/cwd-path "config/.privacyoptin") "")
  state)

(defn privacy-scroll-up
  [state]
  (update state :privary-scroll (comp (partial max 0) dec)))
  
(defn privacy-scroll-down
  [state]
  (update state :privary-scroll (comp (partial min 100) inc)))

(defn move-to-target
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
          _ (log/info "npc" npc npc-pos-vec)
          threshold              (get npc :range-threshold)
          npc-can-move-in-water  (mg/can-move-in-water? (get npc :race))
          npc-can-move-on-land   (mg/can-move-on-land? (get npc :race))
          player                 (-> state :world :player)
          player-pos-vec         [(-> player :pos :x) (-> player :pos :y)]
          _ (log/info "player" player-pos-vec)
          width                  (get-in state [:world :width])
          height                 (get-in state [:world :height])
          bounds                 (vec (mapcat (fn [t] (map (partial + t) npc-pos-vec)) [(- threshold) threshold]))
          _ (log/info "pathfinding bounds" bounds)
          get-type               (memoize (fn [x y] (do
                                                      ;(log/debug "traversable?" x y "type" (get-in place [y x :type]))
                                                      (or (get (rw/get-cell state x y) :type) :unknown))))
          npc-xys                (set (map (fn [npc] (rc/pos->xy (get npc :pos)))
                                           (rnpc/npcs-in-viewport state)))

          cells                  (rv/cells-in-viewport state)
          campfire-xys           (rw/campfire-xys state cells)
          lantern-on             (when-let [lantern (ri/inventory-id->item state :lantern)]
                                    (= (get lantern :state :off) :on))
          light-traversable?     (fn light-traversable? [[x y]]
                                   (let [near-campfire          (rc/xy-in-range? x y 2.9 campfire-xys)]
                                     (or
                                       ; day is always traversable
                                       (rw/is-day? state)
                                       (not (or
                                         ; not in range of campfire is traversable
                                         near-campfire
                                         ; not in range of player if lantern is on
                                         lantern-on)))))
          water-traversable?     (fn water-traversable? [[x y]]
                                   (or (= [x y] npc-pos-vec)
                                       (and (not (rc/farther-than? npc-pos {:x x :y y} threshold))
                                            #_(not (contains? (disj npc-xys npc-pos-vec) [x y]))
                                            (contains? #{:water :surf :shallow-water} (get-type x y))
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
                                                         :short-grass
                                                         ;; pirate ship
                                                         :deck
                                                         :shallow-water
                                                         ;; ruined temple
                                                         :moss-corridor
                                                         :white-corridor
                                                         :crushing-wall-trigger
                                                         :wall-darts-trigger}
                                                       (get-type x y)))))
          traversable?           (cond
                                   (and npc-can-move-in-water
                                        npc-can-move-on-land)
                                   (fn [xy]
                                     (and
                                       (light-traversable? xy)
                                       (or (water-traversable? xy)
                                           (land-traversable? xy))))
                                   npc-can-move-in-water
                                     (fn [xy]
                                       (and
                                         (light-traversable? xy)
                                         (water-traversable? xy)))
                                   npc-can-move-on-land
                                     (fn [xy]
                                       (and
                                         (light-traversable? xy)
                                         (land-traversable? xy))))
          path                   (try
                                   (log/debug "a* params" bounds traversable? npc-pos-vec (rc/pos->xy target))
                                   (clj-tiny-astar.path/a* bounds traversable? npc-pos-vec (rc/pos->xy target))
                                   (catch Exception e
                                     (log/error "Caught exception during a* traversal." npc-pos-vec [(target :x) (target :y)] e)
                                     (st/print-cause-trace e)
                                     nil))
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
                                        (contains? #{:water :surf :shallow-water} (get-type x y))
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
                                                     :short-grass
                                                     ;; pirate ship
                                                     :deck}
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
                          #{:water :surf :shallow-water}
                          #{:floor
                            :corridor
                            :open-door
                            :sand
                            :dirt
                            :gravel
                            :tall-grass
                            :short-grass
                            ;; pirate ship
                            :deck})
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
                          #{:water :surf :shallow-water}
                          #{:floor
                            :corridor
                            :open-door
                            :sand
                            :rocky-shore
                            :dune
                            :dirt
                            :gravel
                            :tall-grass
                            :short-grass
                            :shallow-water
                            ;; pirate ship
                            :deck})
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
                          #{:water :surf :shallow-water}
                          #{:floor
                            :corridor
                            :open-door
                            :sand
                            :dirt
                            :gravel
                            :tall-grass
                            :short-grass
                            :shallow-water
                            ;; pirate ship
                            :deck})]
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
        lantern-on             (when-let [lantern (ri/inventory-id->item state :lantern)]
                                  (= (get lantern :state :off) :on))
        lantern-active (and
                         (rw/is-night? state)
                         lantern-on)

        ;; modify movement policy as dictated by day or night
        policy      (cond
                      lantern-active
                        :hide-from-player-in-range-or-random 
                      (or
                        (and (= temperament :hostile-during-day)
                             (rw/is-day? state))
                        (and (= temperament :hostile-at-night)
                             (rw/is-night? state)))
                      :follow-player-in-range-or-random
               
                      :else
                      policy)
        navigable-types (if (mg/can-move-in-water? (get npc :race))
                          #{:water :surf :shallow-water}
                          #{:floor
                            :corridor
                            :open-door
                            :sand
                            :dirt
                            :gravel
                            :tall-grass
                            :short-grass
                            :shallow-water
                            ;; pirate ship
                            :deck})]
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
  (let [r          3 ;3 ;3 ;(rlos/sight-distance state)
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
 
; Define fire attacker as a trap
(def fire-attacker
  (rt/->Trap
	; type
	:fire
	; race
	:trap
	; name
	"fire"
	; name-plural
	"fires"
	; speed
	50
	; size
	50
	; strength
	50
	; dexterity
	10
	; toughness
	50
	; attacks
	#{:fire}))

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
                  (if (and (contains? npc :energy)
                           (not (rnpc/has-status? npc :stunned))
                           (not (rnpc/has-status? npc :entangled)))
                    (let [npc-keys (rnpc/npc->keys state npc)
                          ;; add speed value to energy.
                          ;_ (trace "adding speed to" (select-keys npc [:race :place :pos :speed :energy]))
                          ;; if the player is slowed, add twice as much energy to the npcs
                          state (update-in state (conj npc-keys :energy) (partial + (* (if slow-player? 2 1)
                                                                                       (get npc :speed))))]
                      (-> state
                        (cond->
                          ;; adjacent hostile npcs attack player.
                          (and (contains? (get npc :status) :hostile)
                               (rw/adjacent-to-player? state (get npc :pos)))
                            (rcombat/attack npc-keys [:world :player]))
                        (cond->
                          (< 2 (->> npc
                                 :pos
                                 rw/adjacent-xys
                                 (map (fn [[x y]]
                                   (get (rw/get-cell state x y) :type)))
                                 (filter rw/type->on-fire?)
                                 count))
                            (rcombat/attack fire-attacker npc-keys))))
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
        world-time     (rw/get-time state)
        fruit-rotted?  (atom false)
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
              harvest-types    #{:bamboo :gravel :tree :palm-tree :tall-grass}
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
                      num-harvestable (get @harvestable-counts place-id 0)]
                  (if (< (rr/uniform-int 0 100000)
                         (/ (case cell-type
                              :bamboo 4
                              :palm-tree 10
                              :tree 25
                              :tall-grass 2
                              :gravel 20
                              10)
                            (inc num-harvestable)))
                    (do
                      (swap! harvestable-counts (fn [counts] (update counts place-id inc)))
                      (conj xy-fns [[wx wy]
                        (fn drop-harvest-items [cell]
                          (assoc cell :harvestable true))]))
                    xy-fns))
                ;; drop fruit
                (contains? fruit-tree-types cell-type)
                ;; chance of dropped a fruit
                (if (= (rr/uniform-int 0 3000) 0)
                  ;; make the fruit item and find an adjacent free cell to drop it into
                  (let [item    (assoc (ig/id->item (get cell :fruit-type)) :rot-time (+ world-time (rr/uniform-int 25 35)))
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
              (if (some (fn [item] (< (get item :rot-time world-time) world-time))
                                        cell-items)
                (do
                  ;; display a message when fruit rots in the players view. Since update-cells
                  ;; is called before update-visiblity, we'll decrement the time by one.
                  (when (= (get cell :discovered) (dec world-time))
                    (reset! fruit-rotted? true))
                  (conj xy-fns [[wx wy]
                                (fn rot-cell-fruit [cell]
                                  (update-in cell [:items] (fn [items]
                                    (remove (fn [item] (< (get item :rot-time world-time)
                                                          world-time))
                                             items))))]))
                xy-fns)))))
      []
      (vec (rv/cellsxy-in-viewport state)))]
    (as-> state state
      (if @fruit-rotted?
        (rc/append-log state "The fruit rots away.")
        state)
      (rw/update-cells state xy-fns)
      (reduce-kv (fn [state place-id harvest-count]
                   (log/info "updating num-harvestable-cells in" (or place-id "nil"))
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

(defn share-score-and-get-scores
  [state]
  ;; upload save/world.edn as json to aaron-santos.com:8888/upload
  (let [version (get state :version)
        userid  (get state :user-id)
        url     (format "https://aaron-santos.com/saves/%s" userid)
        cur-state      (rw/current-state state)
        points         (get state :points)
        turns-survived  (rw/get-time state)
        turns-per-day   (count (get-in state [:data :atmo]))
        days-survived   (int (/ turns-survived turns-per-day))
        
        world   (assoc (get state :world)
                       :days-survived days-survived
                       :turns-survived turns-survived
                       :points points
                       :version version)
        body    (as-> world world
                      (clojure.walk/postwalk (fn [v] (if (char? v)
                                                       (str v)
                                                       v))
                                              world)
                      (json/write-str world))]
    (log/info "Uploading save file version" version "userid" userid "to" url)
    (try
      (log/info "Starting upload")
      (System/setProperty "https.protocols" "TLSv1.2")
      (http/post url
        {:insecure? true
         :body body
         :content-type :json})
      (log/info "Done uploading save file")
      (log/info "Downloading top scores")
      (let [response (http/get "https://aaron-santos.com/scores")
            _ (log/info "Got response" response)
            body (json/read-str (get response :body))
            top-scores (get body "scores")
            point-data (get body "points")
            time-data      (get body "time")
            kills-data     (get body "kills")
            harvested-data (get body "items-harvested")
            crafted-data   (get body "items-crafted")]
        
        (-> state
          (assoc :top-scores     top-scores
                 :points         points
                 :point-data     point-data
                 :time-data      time-data
                 :kills-data     kills-data
                 :harvested-data harvested-data
                 :crafted-data   crafted-data)
          (rw/assoc-current-state :share-score)))
      (catch Exception e
        (log/error "Caught exception while swapping scores" e)
        (rw/assoc-current-state state :connection-failed)))))

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
                           \c          [identity               :configure       false]
                           \q          [identity               :quit            false]
                           :else       [pass-state             rw/current-state false]}
               :configure
                          {\f          [identity               :configure-font  false]
                           :escape     [identity               :start           false]
                           :else       [pass-state             rw/current-state false]}
               :configure-font
                          {\n          [next-font              :configure-font  false]
                           \p          [previous-font          :configure-font  false]
                           \s          [save-and-apply-font    :configure-font  false]
                           \c          [create-new-font        :create-font     false]
                           :escape     [identity               :configure       false]
                           :else       [pass-state             rw/current-state false]}
               :create-font
                          {:escape     [identity               :configure-font  false]
                           :enter      [save-new-font          rw/current-state false]
                           \+          [inc-create-font-size   rw/current-state false]
                           \-          [dec-create-font-size   rw/current-state false]
                           :backspace  [backspace-create-font-name :create-font false]
                           :else       [accept-input-new-font  :create-font     false]}
               :create-font-name
                          {:escape     [identity                :create-font     false]
                           :enter      [identity                :create-font     false]
                           :else       [append-create-font-name :create-font-name false]}
               :enter-name
                          {:enter      [identity               :start-inventory false]
                           :backspace  [backspace-name         :enter-name      false]
                           :escape     [identity               :start           false]
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
                           \,          [smart-pickup           rw/current-state false]
                           \e          [identity               :eat             false]
                           \o          [identity               :open            false]
                           \c          [identity               :close           false]
                           \.          [do-rest                :normal          true]
                           :numpad5    [do-rest                :normal          true]
                           :left       [move-left              rw/current-state false]
                           :down       [move-down              rw/current-state false]
                           :up         [move-up                rw/current-state false]
                           :right      [move-right             rw/current-state false]
                           :up-left    [move-up-left           rw/current-state false]
                           :up-right   [move-up-right          rw/current-state false]
                           :down-left  [move-down-left         rw/current-state false]
                           :down-right [move-down-right        rw/current-state false]
                           :shift+left       [action-left       rw/current-state false]
                           :shift+down       [action-down       rw/current-state false]
                           :shift+up         [action-up         rw/current-state false]
                           :shift+right      [action-right      rw/current-state false]
                           :shift+up-left    [action-up-left    rw/current-state false]
                           :shift+up-right   [action-up-right   rw/current-state false]
                           :shift+down-left  [action-down-left  rw/current-state false]
                           :shift+down-right [action-down-right rw/current-state false]
                           \>          [use-stairs             :normal          true]
                           \<          [use-stairs             :normal          true]
                           :space      [action-select          rw/current-state false]
                           \q          [quaff-select           rw/current-state false]
                           \w          [identity               :wield           false]
                           \W          [identity               :wield-ranged    false]
                           \f          [init-select-ranged-target
                                                               rw/current-state false]
                           \r          [reload-ranged-weapon   :normal          false]
                           \x          [smart-harvest          rw/current-state false]
                           \a          [identity               :apply           false]
                           \;          [init-cursor            :describe        false]
                           \S          [start-sleep            rw/current-state false]
                           \s          [(comp find-traps
                                              rdesc/search)    :normal          true]
                           ;\S          [rdesc/extended-search  :normal          true]
                           \Q          [identity               :quests          false]
                           \M          [toggle-mount           :normal          false]
                           \P          [next-party-member      :normal          false]
                           \C          [transition-recipes     rw/current-state false]
                           \v          [identity               :abilities       false]
                           \@          [identity               :player-stats    false]
                           \t          [identity               :throw-inventory false]
                           \T          [identity               :talk            true]
                           \m          [identity               :log             false]
                           \?          [identity               :help-controls   false]
                           \/          [scroll-log-up          :normal          false]
                           \*          [scroll-log-down        :normal          false]
                           \R          [repeat-commands        rw/current-state false]
                          :f9          [identity :debug-eval false]
                          :f12         [(fn [state]
                                          (rf/send-report state)
                                          state)
                                                               :normal          false]
                           :escape     [identity               :quit?           false]}
               :debug-eval {:f4        [identity               :normal          false]
                            :else [debug-input rw/current-state false]}
               :direction-select
                          {:escape     [identity               :normal          false]
                           :else       [do-selected-direction  rw/current-state false]}
               :action-select
                          {:escape     [identity               :normal          false]
                           :else       [do-selected-action     rw/current-state false]}
               :inventory {:escape     [identity               :normal          false]}
               :abilities {:escape     [identity               :normal          false]
                           :else       [use-ability            rw/current-state true]}
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
                           :else       [pass-state             rw/current-state false]
                           :escape     [free-cursor            :normal          false]}
               :quests    {:escape     [identity               :normal          false]}
               :describe-inventory
                          {:escape     [identity               :normal          false]
                           :else       [describe-inventory     :normal          false]}
               :drop      {:escape     [identity               :normal          false]
                           :else       [drop-item              :normal          true]}
               :apply     {:escape     [identity               :normal          false]
                           :else       [select-apply-item      rw/current-state false]}
               :apply-item-normal
                          {:escape     [identity               :normal          false]
                           :else       [(fn [state
                                             keyin]
                                          (rai/apply-item state translate-directions keyin))
                                                               rw/current-state true]}
               :apply-item-inventory
                          {:escape     [identity               :normal          false]
                           :else       [(fn [state
                                             keyin]
                                          (rai/apply-item state translate-directions keyin))
                                                               rw/current-state true]}
               :apply-item-body
                          {:escape     [identity               :normal          false]
                           \a          [(fn [state]
                                        (rai/apply-item state translate-directions \a))
                                                               :normal          true]
                           \b          [(fn [state]
                                        (rai/apply-item state translate-directions \b))
                                                               :normal          true]}
               :apply-item-message
                          {:escape     [identity               :normal          false]
                           :enter      [identity               :apply-item-message false]
                           :else       [(fn [state
                                             keyin]
                                          (rai/apply-item-message state keyin))
                                                               rw/current-state true]}
               :pickup    {:left       [pickup-left            rw/current-state false]
                           :down       [pickup-down            rw/current-state false]
                           :up         [pickup-up              rw/current-state false]
                           :right      [pickup-right           rw/current-state false]
                           :up-left    [pickup-up-left         rw/current-state false]
                           :up-right   [pickup-up-right        rw/current-state false]
                           :down-left  [pickup-down-left       rw/current-state false]
                           :down-right [pickup-down-right      rw/current-state false]
                           :numpad5    [pickup-center          rw/current-state false]
                           \>          [pickup-center          rw/current-state false]
                           :escape     [identity               :normal          false]}
               :pickup-selection
                          {:escape     [identity               :normal          false]
                           :space      [pick-up-all            :normal          true]
                           :else       [toggle-hotkey          :pickup-selection
                                                                                false]
                           :enter      [pick-up                :normal          true]}
               :eat       {:escape     [identity               :normal          false]
                           :else       [eat                    rw/current-state true]}
               :quaff-adj-or-inv
                          {\i          [identity               :quaff-inventory false]
                           :left       [quaff-left             rw/current-state true]
                           :down       [quaff-down             rw/current-state true]
                           :up         [quaff-up               rw/current-state true]
                           :right      [quaff-right            rw/current-state true]}
               :quaff-adj {:escape     [identity               :normal          false]
                           :left       [quaff-left             rw/current-state true]
                           :down       [quaff-down             rw/current-state true]
                           :up         [quaff-up               rw/current-state true]
                           :right      [quaff-right            rw/current-state true]}
               :quaff-popover
                          {\y          [quaff-cell-at-pos      :normal          true]
                           \n          [identity               :normal          false]}
               :quaff-inventory
                          {:escape     [identity               :normal          false]
                           :else       [quaff-inventory        :normal          true]}
               :open      {:left       [open-left              :normal          true]
                           :down       [open-down              :normal          true]
                           :up         [open-up                :normal          true]
                           :right      [open-right             :normal          true]}
               :talk      {:left       [talk-left              rw/current-state false]
                           :down       [talk-down              rw/current-state false]
                           :up         [talk-up                rw/current-state false]
                           :right      [talk-right             rw/current-state false]}
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
               :wield-ranged
                          {:escape     [identity               :normal          false]
                           :else       [wield-ranged           :normal          true]}
               :select-ranged-target
                          {:escape     [identity               :normal          false]
                           :tab        [select-next-ranged-target
                                                               rw/current-state false]
                           \n          [select-next-ranged-target
                                                               rw/current-state false]
                           \p          [select-previous-ranged-target
                                                               rw/current-state false]
                           \f          [fire-wielded-ranged-weapon
                                                               :normal          true]}
               :throw-inventory
                          {:escape     [identity               :normal          false]
                           :else       [throw-select-inventory :select-throw-target
                                                                                false]}
               :select-throw-target
                          {:escape     [free-cursor            :normal          false]
                           :left       [move-cursor-left       rw/current-state false]
                           :down       [move-cursor-down       rw/current-state false]
                           :up         [move-cursor-up         rw/current-state false]
                           :right      [move-cursor-right      rw/current-state false]
                           :up-left    [move-cursor-up-left    rw/current-state false]
                           :up-right   [move-cursor-up-right   rw/current-state false]
                           :down-left  [move-cursor-down-left  rw/current-state false]
                           :down-right [move-cursor-down-right rw/current-state false]
                           \t          [throw-selected-inventory
                                                               :normal          false]
                           :enter      [throw-selected-inventory
                                                               :normal          false]
                           :space      [throw-selected-inventory
                                                               :normal          false]
                           :else       [free-cursor            :normal          false]}
               :talking   {:escape     [stop-talking           :normal          false]
                           :else       [talk                   rw/current-state true]}
               :shopping  {\a          [identity               :buy             true]
                           \b          [identity               :sell            true]
                           :escape     [identity               :normal          false]}
               :buy       {:escape     [identity               :normal          false]
                           :else       [buy                    :buy             true]}
               :sell      {:escape     [identity               :normal          false]
                           :else       [sell                   :sell            true]}
               :recipes   {\a          [(partial select-recipe \a) rw/current-state false]
                           \b          [(partial select-recipe \b) rw/current-state false]
                           \c          [(partial select-recipe \c) rw/current-state false]
                           \m          [transition-make-recipe rw/current-state false]
                           \n          [new-or-continue-recipe rw/current-state false]
                           \r          [replace-recipe         rw/current-state false]
                           :escape     [identity               :normal          false]}
               :select-recipe-type
                          {\w          [select-recipe-type-weapon :in-progress-recipe false]
                           \b          [select-recipe-type-boat   :in-progress-recipe  false]
                           ;\t          [select-recipe-type-trap   :in-progress-recipe  false]
                           ;\f          [select-recipe-type-food   :in-progress-recipe false]
                           ;\s          [select-recipe-type-signal :in-progress-recipe false]
                           ;\v          [select-recipe-type-survival :in-progress-recipe false]
                           :escape     [identity               :recipes         false]}
               :in-progress-recipe
                          {:escape     [identity               :recipes           false]
                           :else       [craft-in-progress-recipe rw/current-state 5]}
               :craft     {:else       [craft-select           rw/current-state false]}
               :fishing-left
                          {\.          [rai/do-fishing         rw/current-state true]
                           :else       [pass-state             :normal          false]}
               :fishing-right
                          {\.          [rai/do-fishing         rw/current-state true]
                           :else       [pass-state             :normal          false]}
               :fishing-up
                          {\.          [rai/do-fishing         rw/current-state true]
                           :else       [pass-state             :normal          false]}
               :fishing-down
                          {\.          [rai/do-fishing         rw/current-state true]
                           :else       [pass-state             :normal          false]}
               :sleep     {:else       [do-sleep               rw/current-state true]}
               :dream     {:space      [identity               :normal false]}
               :gain-level
                          {:escape     [identity               :normal          false]
                           :else       [choose-ability         rw/current-state false]}
               :help-controls
                          {\n          [identity               :help-ui         false]
                           :up-right    [identity               :help-ui         false]
                           \9          [identity               :help-ui         false]
                           \p          [identity               :help-gameplay   false]
                           :up-left    [identity               :help-gameplay   false]
                           \7          [identity               :help-gameplay   false]
                           :escape     [identity               :normal          false]
                           :else       [pass-state             rw/current-state false]}
               :help-ui
                          {\n          [identity               :help-gameplay   false]
                           :up-right    [identity               :help-gameplay   false]
                           \9          [identity               :help-gameplay   false]
                           \p          [identity               :help-controls   false]
                           :up-left    [identity               :help-controls   false]
                           \7          [identity               :help-controls   false]
                           :escape     [identity               :normal          false]
                           :else       [pass-state             rw/current-state false]}
               :help-gameplay
                          {\n          [identity               :help-controls   false]
                           :up-right    [identity               :help-controls   false]
                           \9          [identity               :help-controls   false]
                           \p          [identity               :help-ui         false]
                           :up-left    [identity               :help-ui         false]
                           \7          [identity               :help-ui         false]
                           :escape     [identity               :normal          false]
                           :else       [pass-state             rw/current-state false]}
               :close     {:left       [close-left             :normal          true]
                           :down       [close-down             :normal          true]
                           :up         [close-up               :normal          true]
                           :right      [close-right            :normal          true]}
               :log       {:else       [pass-state             :normal          false]}
               :popover   {:space      [rpop/clear-popover     rpop/get-popover-next-state
                                                                                false]
                           :else       [pass-state             rw/current-state false]}
                                                                 
               :rescued   {:space      [save-score             :game-over-rescued false]
                           :else       [pass-state             rw/current-state false]}
               :dead      {:space      [save-score             :game-over-dead  false]
                           :else       [pass-state             rw/current-state false]}
               :game-over-rescued
                          {\y          [identity               :start-inventory false]
                           \n          [(constantly nil)       :normal          false]
                           :space      [identity               transition-privacy false]}
               :game-over-dead
                          {\y          [identity               :start-inventory false]
                           \n          [identity               :start           false]
                           :space      [identity               transition-privacy false]}

               :privacy
                          {\y          [privary-opt-in         :connecting      false]
                           \n          [identity               :start           false]
                           :up         [privacy-scroll-up      rw/current-state false]
                           :down       [privacy-scroll-down    rw/current-state false]}
               :connecting
                          {:advance    [share-score-and-get-scores
                                                               rw/current-state  false]
                           :else       [pass-state             :connecting       false]}
               :connection-failed
                          {\y          [identity               :start-inventory false]
                           \n          [(constantly nil)       :normal          false]}
               :share-score
                          {\y          [identity               :start-inventory false]
                           \n          [identity               :start           false]}
               :quit?     {\y          [identity               :start           false]
                           :else       [pass-state             :normal          false]}}
        expander-fn (fn [table] table)]
    (expander-fn table)))

(def translate-direction-states
  #{:normal
    :describe
    :select-throw-target
    :quaff-adj-or-inv
    :quaff-adj
    :open
    :talk
    :pickup
    :harvest
    :direction-select
    :close})

(def modifier-keys #{:lshift :lcontrol :rshift :rcontrol})

(defn update-advance-time
  [state]
  (if (get state :advance-time)
    (let [wtl (get-in state [:world :player :will-to-live])]
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
        (decrease-lantern-charge)
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
        (as-> state
          (if (nil? (rw/current-place-id state))
            (rnpc/add-npcs-random state)
            state))
        ;; update visibility
        (update-visibility)
        ;; update traps
        (rt/update-traps)
        ;; add will-to-live flavor message
        (log-will-to-live-flavor wtl)
        ;; maybe gain level
        (maybe-gain-level)
        (coalesce-logs)
        (assoc :advance-time false)))
    state))

(defn add-mods
  [keyin keymods]
  (log/info "keymods" keymods)
  (if (char? keyin)
    keyin
    (let [mod-names (->> keymods
                      (filter second)
                      (mapv (comp name first)))]
      (->> (conj mod-names (name keyin))
        (interpose "+")
        (apply str)
        keyword))))


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
  [state keyin keymods]
  (let [current-state (get-in state [:world :current-state])
        table         (get state-transition-table current-state)
        keyin         (if (contains? translate-direction-states current-state)
                        (-> keyin
                          translate-directions
                          (add-mods keymods))
                        keyin)
        keyin (get {\space :space} keyin keyin)]
    (log/info "current-state" current-state)
    (log/info "keyin" keyin)
    (log/info "keyin" (type keyin))
    (log/info "keyin" (contains? #{\space} keyin))
    (log/info "keyin" (get {\space :space} keyin))
    ;(log/debug "current-state" current-state)
    (if (and (or (contains? table keyin) (contains? table :else))
             (not (contains? modifier-keys keyin)))
      (let [[transition-fn new-state advance-time] (if (contains? table keyin) (get table keyin) (get table :else))
            ;; if the table contains keyin, then pass through transition-fn assuming arity-1 [state]
            ;; else the transition-fn takes [state keyin]. Bind keying so that it becomes arity-1 [state]
            _ (log/info "current-state" (get-in state [:world :current-state]))
            _ (log/info "transition-fn" transition-fn)
            transition-fn             (if (contains? table keyin)
                                        transition-fn
                                        (fn [state]
                                          (transition-fn state keyin)))
            command-seq (get-in state [:world :command-seq] [])
            _ (log/debug "type of npcs" (type (get-in state [:world :npcs])))
            new-time  (+ (if (number? advance-time) advance-time 1)
                         (get-in state [:world :time]))
            state     (rc/clear-ui-hint state)
            state     (transition-fn state)
            ;; some states conditionally advance time by calling (advance-time state)
            ;; check to see if this has occurred if advance-time was not set in the state transition entry
            advance-time (if-not advance-time
                           (rc/inc-time? state)
                           ;; if advance time, only really advance time if no actors
                           (ractors/empty-actors? state))
            state        (if advance-time
                           (assoc-in state [:world :time] new-time)
                           state)
            _ (log/info "advance-time" advance-time)
            ;; clear inc-time flag if set
            state        (if (rc/inc-time? state)
                           (rc/clear-inc-time state)
                           state)
            _ (log/debug "current-state" (get-in state [:world :current-state]))
            _ (log/debug "new-state" new-state)
            _ (log/debug "type of npcs" (type (get-in state [:world :npcs])))
            new-state (if (keyword? new-state)
                        new-state
                        (new-state state))
            _ (assert (not (nil? new-state)))
            state     (if state
                        (if-not (= keyin \r)
                          (if (= current-state :normal)
                              (assoc-in state [:world  :command-seq] [keyin])
                              (rc/conj-in state [:world :command-seq] keyin))
                          (assoc-in state [:world :command-seq] command-seq))
                        state)
            _ (log/info "command-seq" (get-in state [:world :command-seq] :none))
            _ (log/debug "player" (get-in state [:world :player]))
            _ (log/info "new-state" new-state)]
        (some-> state
            (rw/assoc-current-state new-state)
            ; add :advance-time key
            ; this gets used in revents/stream in order to toggle calling
            ; update/update-advance-time
            (assoc :advance-time advance-time)
            ;; update visibility
            (cond-> advance-time update-visibility)
            coalesce-logs
            update-quests
            (as-> state
              (if (contains? (-> state :world :player :status) :dead)
                (do
                  ;; delete the save game on player death
                  (delete-save-game)
                  (-> state
                    (update-in [:world :player :status]
                     (fn [status]
                       (disj status :dead)))
                    (rw/assoc-current-state :dead)))
                state))))
      state)))

