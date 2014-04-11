(ns dungeon-crusade.update
  (:require clojure.pprint
            clj-tiny-astar.path)
  (:use     
    dungeon-crusade.common
    dungeon-crusade.lineofsight))


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
          ;; maybe add corpse
          (update-in [:world :places current-place-id y x :items]
                     (fn [items]
                       (if (= (rand-int 3) 0)
                         (conj items {:type :food :name (format "%s corpse" (name (npc :type))) :hunger 10})
                         items))))
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
        (assoc-in [:world :player :pos :x] (- x 1)))
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
        (assoc-in [:world :player :pos :x] (+ x 1)))
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
        (assoc-in [:world :player :pos :y] (- y 1)))
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
        (assoc-in [:world :player :pos :y] (+ y 1)))
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
      (if (and (not (nil? target-cell)) (= (target-cell :type) :close-door))
        (let [place (-> state :world :current-place)]
          (println "opening door")
          (println (get-in state [:world :places place target-y target-x]))
          (assoc-in state [:world :places place target-y target-x :type] :open-door))
        state))))

(defn open-left [state]
  (open-door state :left))

(defn open-right [state]
  (open-door state :right))

(defn open-up [state]
  (open-door state :up))

(defn open-down [state]
  (open-door state :down))

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
      (if (and (not (nil? target-cell)) (= (target-cell :type) :open-door))
        (let [place (-> state :world :current-place)]
          (println "opening door")
          (assoc-in state [:world :places place target-y target-x :type] :close-door))
        state))))

(defn close-left [state]
  (close-door state :left))

(defn close-right [state]
  (close-door state :right))

(defn close-up [state]
  (close-door state :up))

(defn close-down [state]
  (close-door state :down))

(defn pick-up [state]
    ;; find all the items in the current cell
    ;; divide them into selected and not-selected piles using the selected-hotkeys
    ;; add the selected pile to the player's inventory
    ;; return the non-selcted pile to the cell
    ;; remove selected-hotkeys from remaining-hotkeys
    ;; clear selected-hotkeys
    (let [place              (-> state :world :current-place)
          [player-cell x y]  (player-cellxy state)
          items              (into [] (player-cell :items))
          remaining-hotkeys  (-> state :world :remaining-hotkeys)
          divided-items      (group-by (fn [item] (if (contains? (-> state :world :selected-hotkeys) (item :hotkey))
                                                      :selected
                                                      :not-selected))
                                       (map #(assoc %1 :hotkey %2)
                                            items
                                            (fill-missing #(not %)
                                                     (fn [_ hotkey] hotkey)
                                                     remaining-hotkeys
                                                     (map :hotkey items))))
          selected-items     (vec (divided-items :selected))
          not-selected-items (vec (divided-items :not-selected))
          remaining-hotkeys  (vec (remove #(some (partial = %) (map :hotkey selected-items)) remaining-hotkeys))]
      (println "divided-items" divided-items)
      (println "selected-items" selected-items)
      (println "not-selected-items" not-selected-items)
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
      (println "cell-items (-> state :world :places" place y x ":items)")
      new-state)))

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


(defn do-rest [state]
  (-> state
      (update-in [:world :player]
                 (fn [player] (if (< (int (player :hp)) (player :max-hp))
                                (assoc-in player [:hp] (+ (player :hp) 0.1))
                                player)))))

(defn toggle-hotkey [state keyin]
  (println "toggle-hotkey" keyin)
  (update-in state [:world :selected-hotkeys]
             (fn [hotkeys] (if (contains? hotkeys keyin)
                             (disj hotkeys keyin)
                             (conj hotkeys keyin)))))

(defn reinit-world [state]
  (assoc state :world (init-world)))

(defn eat [state keyin]
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
        (println "new-state" new-state)
        new-state)
        state)))

(defn use-stairs [state]
  (let [[player-cell x y] (player-cellxy state)
        orig-place-id     (-> state :world :current-place)]
    (if (contains? #{:down-stairs :up-stairs} (player-cell :type))
      (let [dest-place-id     (player-cell :dest-place)
            ;; manipulate state so that if there isn't a destination place, create one
            _ (println "dest-place-id" dest-place-id)
            _ (println "name" (name dest-place-id))
            _ (println "int" (read-string (name dest-place-id)))
            state             (if (contains? (-> state :world :places) dest-place-id)
                                state
                                (assoc-in state [:world :places dest-place-id] (init-random-n (read-string (name dest-place-id)))))
 
            dest-place        (-> state :world :places dest-place-id)

            ;; find the location in the destination place that points to the original place
            dest-cellxy       (first (filter #(and (not (nil? (first %)))
                                                 (contains? #{:up-stairs :down-stairs} ((first %) :type))
                                                 (= ((first %) :dest-place) orig-place-id))
                                    (with-xy dest-place)))
            _ (println "dest-cellxy" dest-cellxy)
            dest-x             (second dest-cellxy)
            dest-y             (last dest-cellxy)]
        (println "dest-x" dest-x "dest-y" dest-y)
        (-> state
          ;; change the place
          (assoc-in [:world :current-place] (player-cell :dest-place))
          ;; move player to cell where stairs go back to original place
          (assoc-in [:world :player :pos] {:x dest-x :y dest-y})))
      state)))

(defn move-npc [state result npc]
  (let [npcs (-> state :world :npcs ((-> state :world :current-place)))
        _ (println "npc" npc)
        _ (println "result" result)
        npc-pos [(-> npc :pos :x) (-> npc :pos :y)]
        player  (-> state :world :player)
        player-pos [(-> player :pos :x) (-> player :pos :y)]
        traversable? (fn [[x y]]
                       (let [place (current-place state)]
                         (and (< 0 x (count (first place)))
                              (< 0 y (count place))
                              (not-any? (fn [npc] (and (= (-> npc :pos :x) x)
                                                       (= (-> npc :pos :y) y)))
                                        npcs)
                              (contains? #{:floor
                                           :open-door
                                           :corridor}
                                         (get-in place [y x :type])))))
        path (try
               (clj-tiny-astar.path/a* traversable? npc-pos player-pos)
               (catch Exception e
                 nil))
                 ;(println "(clj-tiny-astar.path/a* traversable?" npc-pos player-pos ")")
                 ;(println (map (fn [y] (map (fn [x] (traversable? [x y])) (range (count (first (current-place state)))))) (range (count (current-place state)))))))
        _ (println "path to player" path)
        new-pos (if (and (not (nil? path))
                         (> (count path) 1)
                         (not (= (first (second path)) (first player-pos)))
                         (not (= (second (second path)) (second player-pos))))
                  (second path)
                  npc-pos)
        new-npc (-> npc
                    (assoc-in [:pos :x] (first new-pos))
                    (assoc-in [:pos :y] (second new-pos)))
        _ (println "new-npc" new-npc)]
    (conj result new-npc)))
 
(defn move-npcs [state]
  (update-in state [:world :npcs (-> state :world :current-place)]  (fn [npcs] (reduce (partial move-npc state) [] npcs))))

(defn add-npcs [state]
  (if (< (rand-int 10) 4)
    (let [[_ x y] (first (shuffle (filter (fn [[cell _ _]] (and (not (nil? cell))
                                                                (= (cell :type) :floor)))
                                          (with-xy (current-place state)))))]
      (update-in state [:world :npcs (-> state :world :current-place)]
                 (fn [npcs]
                   (conj npcs {:pos {:x x :y y} :type :rat :hp 9 :attacks #{:bite :claw}}))))
    state))

(def state-transition-table
  ;;         starting      transition transition       new
  ;;         state         symbol     fn               state
  (let [table {:normal    {\i        [identity         :inventory]
                           \d        [identity         :drop]
                           \,        [identity         :pickup]
                           \e        [identity         :eat]
                           \o        [identity         :open]
                           \c        [identity         :close]
                           \.        [do-rest          :normal]
                           \h        [move-left        :normal]
                           \j        [move-down        :normal]
                           \k        [move-up          :normal]
                           \l        [move-right       :normal]
                           \>        [use-stairs       :normal]
                           \<        [use-stairs       :normal]
                           :escape   [identity         :quit?]}
               :inventory {:escape   [identity         :normal]}
               :drop      {:escape   [identity         :normal]
                           :else     [drop-item        :normal]}
               :pickup    {:escape   [identity         :normal]
                           :else     [toggle-hotkey    :pickup]
                           :enter    [pick-up          :normal]}
               :eat       {:escape   [identity         :normal]
                           :else     [eat              :normal]}
               :open      {\h        [open-left        :normal]
                           \j        [open-down        :normal]
                           \k        [open-up          :normal]
                           \l        [open-right       :normal]}
               :close     {\h        [close-left       :normal]
                           \j        [close-down       :normal]
                           \k        [close-up         :normal]
                           \l        [close-right      :normal]}
               :dead      {\y        [reinit-world     :normal]
                           \n        [(constantly nil) :normal]}
               :quit?     {\y        [(constantly nil) :normal]
                           :else     [(fn [s _] s)     :normal]}}
        expander-fn (fn [table] table)]
    (expander-fn table)))


(defn update-state [state keyin]
  (let [current-state (-> state :world :current-state)
        table         (-> state-transition-table current-state)]
    (println "current-state" current-state)
    (if (or (contains? table keyin) (contains? table :else))
      (let [[transition-fn new-state] (if (contains? table keyin) (table keyin) (table :else))
            ;; if the table contains keyin, then pass through transition-fn assuming arity-1 [state]
            ;; else the transition-fn takes [state keyin]. Bind keying so that it becomes arity-1 [state]
            transition-fn             (if (contains? table keyin)
                                        transition-fn
                                        (fn [state]
                                          (transition-fn state keyin)))]
        (some-> state
            transition-fn
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
            ((fn [state] (update-in state [:world :current-state]
                                    (fn [current-state]
                                      (if (contains? (-> state :world :player :status) :dead)
                                        ;; delete the save game on player death
                                        (do (.delete (clojure.java.io/file "world.save"))
                                            :dead)
                                        current-state)))))
            ((fn [state] (update-in state [:world :places (-> state :world :current-place)]
                                    (fn [place]
                                     (let [visibility (map-visibility (let [pos (-> state :world :player :pos)]
                                                                             [(pos :x) (pos :y)])
                                                                           cell-blocking?
                                                                           place)]
                                       ;(println "visibility")
                                       ;(clojure.pprint/pprint visibility)
                                       ;(println "place")
                                       ;(clojure.pprint/pprint place)
                                       (vec (map (fn [place-line visibility-line]
                                                       (vec (map (fn [cell visible?]
                                                              (if (and (not (nil? cell)) visible?)
                                                                (assoc cell :discovered :true)
                                                                cell))
                                                            place-line visibility-line)))
                                                     place visibility)))))))
            ((fn [state] (update-in state [:world :time] inc)))))
      state)))

