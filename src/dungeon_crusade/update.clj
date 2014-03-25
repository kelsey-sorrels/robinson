(ns dungeon-crusade.update
  (use     dungeon-crusade.common))


(defn do-combat [x y state]
  (let [npc (npc-at-xy x y state)
        player (-> state :world :player)
        current-place-id (-> state :world :current-place)
        npc-idx (.indexOf (-> state :world :npcs current-place-id) npc)
        player-hp (player :hp)
        npc-hp    (npc :hp)
        npc-dmg 1
        player-dmg 1]
    (println "fighting npc" npc "with idx" npc-idx)
    (cond
      ;; npc and player still alive?
      (and (pos? (- player-hp player-dmg))
           (pos? (- npc-hp npc-dmg)))
        (-> state
          ;; modify player hp
          (update-in [:world :player :hp]
            (fn [hp] (- hp player-dmg)))
          ;; modify npc hp
          (update-in [:world :npcs current-place-id npc-idx :hp]
          (fn [hp] (- hp npc-dmg))))
      ;; npc dead?
      (not (pos? (- npc-hp npc-dmg)))
        (-> state
          ;; modify player hp
          (update-in [:world :player :hp]
            (fn [hp] (- hp player-dmg)))
          ;; remove npc
          (update-in [:world :npcs current-place-id]
            (fn [npcs]
              (vec (concat (subvec npcs 0 npc-idx)
                           (subvec npcs (inc npc-idx) (count npcs))))))
          ;; TODO add corpse
          )
      ;; player dead?
      (not (pos? (- player-hp player-dmg)))
        ;; add dead status
        (-> state
          (update-in [:world :player :status]
            (fn [status] (conj status :dead)))))))

(defn move-left [state]
  (let [x (-> state :world :player :pos :x)
        y (-> state :world :player :pos :y)]
    (if-not (collide? (- x 1) y state)
      (-> state
        (assoc-in [:world :player :pos :x] (- x 1))
        (assoc-in [:world :last-command] :move-left))
      (if (npc-at-xy (- x 1) y state)
        ;; collided with npc. Engage in combat.
        (do-combat (- x 1) y state)
        ;; collided with a wall or door, nothing to be done.
        state))))
  
(defn move-right [state]
  (let [x (-> state :world :player :pos :x)
        y (-> state :world :player :pos :y)]
    (if-not (collide? (+ x 1) y state)
      (-> state
        (assoc-in [:world :player :pos :x] (+ x 1))
        (assoc-in [:world :last-command] :move-right))
      (if (npc-at-xy (+ x 1) y state)
        ;; collided with npc. Engage in combat.
        (do-combat (+ x 1) y state)
        ;; collided with a wall or door, nothing to be done.
        state))))
  
(defn move-up [state]
  (let [x (-> state :world :player :pos :x)
        y (-> state :world :player :pos :y)]
    (if-not (collide? x (- y 1) state)
      ;; no collision. move up
      (-> state
        (assoc-in [:world :player :pos :y] (- y 1))
        (assoc-in [:world :last-command] :move-up))
      (if (npc-at-xy x (- y 1) state)
        ;; collided with npc. Engage in combat.
        (do-combat x (- y 1) state)
        ;; collided with a wall or door, nothing to be done.
        state))))
  
(defn move-down [state]
  (let [x (-> state :world :player :pos :x)
        y (-> state :world :player :pos :y)]
    (println "move-down")
    (if-not (collide? x (+ y 1) state)
      (-> state
        (assoc-in [:world :player :pos :y] (+ y 1))
        (assoc-in [:world :last-command] :move-down))
      (if (npc-at-xy x (+ y 1) state)
        ;; collided with npc. Engage in combat.
        (do-combat x (+ y 1) state)
        ;; collided with a wall or door, nothing to be done.
        state))))

(defn open-door [state direction]
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
    (println "open-door")
    (let [target-cellxy (get-xy target-x target-y (current-place state))
          target-cell   (first target-cellxy)]
      (println "target-cellxy" target-cellxy)
      (println "target-cell" target-cell)
      (if (and (not (nil? target-cell)) (= (target-cell :type) :door-closed))
        (let [place (-> state :world :current-place)]
          (println "opening door")
          (assoc-in state [:world :places place target-y target-x :type] :door-open))
        state))))

(defn close-door [state direction]
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
    (println "close-door")
    (let [target-cellxy (get-xy target-x target-y (current-place state))
          target-cell   (first target-cellxy)]
      (println "target-cellxy" target-cellxy)
      (println "target-cell" target-cell)
      (if (and (not (nil? target-cell)) (= (target-cell :type) :door-open))
        (let [place (-> state :world :current-place)]
          (println "opening door")
          (assoc-in state [:world :places place target-y target-x :type] :door-closed))
        state))))

(defn pick-up [state keyin]
  (let [place (-> state :world :current-place)
        [player-cell x y] (player-cellxy state)
        items (into [] (player-cell :items))
        remaining-hotkeys (-> state :world :remaining-hotkeys)
        item-index (.indexOf (fill-missing #(not %)
                                           (fn [_ hotkey] hotkey)
                                           remaining-hotkeys
                                           (map :hotkey items))
                              keyin)]
    (println "item-index" item-index)
    (if (and (>= item-index 0) (< item-index (count items)))
      (let [item (nth items item-index)
            item-with-hotkey (if (item :hotkey) item (assoc item :hotkey keyin))
            hotkey (item :hotkey)
            new-state (-> state
        ;; dup the item into inventory with hotkey
        (update-in [:world :player :inventory]
          (fn [prev-inventory]
            (conj prev-inventory item-with-hotkey)))
        ;; remove the item from cell
        (assoc-in [:world :places place y x :items]
         (vec (concat (subvec items 0 item-index)
                      (subvec items (inc item-index) (count items)))))
        ;;;; hotkey is no longer available
        (assoc-in [:world :remaining-hotkeys]
          (if (some #(= keyin %) remaining-hotkeys)
            (vec (concat (subvec remaining-hotkeys 0 item-index)
                         (subvec remaining-hotkeys (inc item-index) (count remaining-hotkeys))))
            remaining-hotkeys)))]
    (println "picking up at:" x y "item with index" item-index "item" item)
    (println "new-state" new-state)
    (println "cell-items (-> state :world :places" place y x ":items)")
    new-state)
    state)))

(defn drop-item [state keyin]
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
        (println "dropping at:" x y "item with index" item-index "item" item)
        (println "new-state" new-state)
        (println "cell-items (-> state :world :places" place y x ":items)")
        new-state)
        state)))

(defn toggle-inventory [state]
  (println "toggle-inventory" (not (-> state :world :show-inventory?)))
  (assoc-in state [:world :show-inventory?] (not (-> state :world :show-inventory?))))

(defn toggle-pick-up [state]
  (println "toggle-pick-up" (not (-> state :world :show-pick-up?)))
  (assoc-in state [:world :show-pick-up?] (not (-> state :world :show-pick-up?))))
  
(defn toggle-drop [state]
  (println "toggle-drop" (not (-> state :world :show-drop?)))
  (assoc-in state [:world :show-drop?] (not (-> state :world :show-drop?))))
  
  
(defn update-state [state keyin]
  (cond
    ;; handle game over
    (player-dead? state)
      (case keyin
        \y (-> state (assoc :world (init-world))
                     (assoc :time 0))
        \n nil ;TODO: exit game
        state
    )
    ;; handle pickup
    (-> state :world :show-pick-up?)
      (let [state-with-command(set-last-command state nil)]
        (println "picking up")
        (case keyin
          \, (toggle-pick-up state-with-command)
          (pick-up state-with-command keyin)))
    ;; handle drop
    (-> state :world :show-drop?)
      (let [state-with-command(set-last-command state nil)]
        (println "dropping")
        (case keyin
          \d (toggle-drop state-with-command)
          (-> state-with-command
              (drop-item keyin)
              (assoc-in [:world :show-drop?] false))))
    ;; handle main game commands
    :else
      (do
        (println "last-command " (get-last-command state))
        (case (get-last-command state)
          :open (let [state-with-command (set-last-command state nil)]
                  (open-door state-with-command (case keyin
                                                  \h :left
                                                  \j :down
                                                  \k :up
                                                  \l :right)))
          :close (let [state-with-command (set-last-command state nil)]
                   (close-door state-with-command (case keyin
                                                    \h :left
                                                    \j :down
                                                    \k :up
                                                    \l :right)))
          (case keyin
            \h (move-left state)
            \j (move-down state)
            \k (move-up state)
            \l (move-right state)
            \i (toggle-inventory state)
            \, (toggle-pick-up state)
            \d (toggle-drop state)
            \o (set-last-command state :open)
            \c (set-last-command state :close)
            (do (println "command not found") state))))))

