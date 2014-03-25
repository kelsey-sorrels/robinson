(ns dungeon-crusade.main
  (require [lanterna.screen :as s]))

(defmacro defsource
  "Similar to clojure.core/defn, but saves the function's definition in the var's
   :source meta-data."
  {:arglists (:arglists (meta (var defn)))}
  [fn-name & defn-stuff]
  `(do (defn ~fn-name ~@defn-stuff)
       (alter-meta! (var ~fn-name) assoc :source (quote ~&form))
       (var ~fn-name)))

(defn fill-missing [pred f vcoll coll]
  "For each item in coll that (pred item) returns true, replace that element with the result
   of (f item vcoll-item) where vcoll-item starts with (first vcoll) and proceeds to the next
   element each time (pred item) is true.
   Ex: user=> (fill-missing #(not (contains? % :val))
                            #(assoc %1 :val %2)
                            [1 2 3]
                            [{:val \\a} {} {:val \\b} {:val \\c} {} {} {}])
   ({:val \\a} {:val 1} {:val \\b} {:val \\c} {:val 2} {:val 3} {:val nil})"
  (if (empty? coll)
    coll
    (let [x  (first coll)
          xs (rest coll)
          y  (first vcoll)
          ys (rest vcoll)]
      (if (pred x)
        (cons (f x y) (if (empty? xs) [] (fill-missing pred f ys xs)))
        (cons x (if (empty? xs) [] (fill-missing pred f vcoll xs)))))))

(defn ascii-to-place [ascii extras]
  "ascii: an ascii representatin of the place
   extra: a list of [[k & ks] v] where the first element is a list of keys
          to assoc-in the place, and the second is the value to associate"
  (let [
    char-to-cell (fn [c]
      (case c
        \| {:type :vertical-wall}
        \- {:type :horizontal-wall}
        \. {:type :floor}
        \+ {:type :door-closed}
        \# {:type :corridor}
        nil))
    ;; convert ascii to place
    setting (vec (map (fn [line] (vec (map char-to-cell line))) ascii))
    ;; add in extras like items, monsters, etc.
    ;; create a list of functions that can be applied to assoc extras, then create a composition of
    ;; so that setting can pass through each fn in turn.
    place ((apply comp (map (fn [e] (partial (fn [e s] (apply assoc-in s e)) e)) extras)) setting)]
    (println "setting" setting)
    (println "place" place)
    place))

(defn init-place-0 []
  (ascii-to-place
    ["----   ----"
     "|..+## |..|"
     "|..| ##+..|"
     "----   ----"]
    [[[1 9 :items] [{:type :ring   :name "Ring of Power"}]]
     [[1 1 :items] [{:type :scroll :name "Scroll of Power"}]]]))

(defn init-world []
  {:places {:0 (init-place-0)}
   :current-place :0
   :show-inventory? false
   :show-pick-up? false
   :show-drop? false
   :last-command nil
   :remaining-hotkeys (vec (seq "abcdefghijklmnopqrstuvwxyzABCdEFGHIJKLMNOPQRSTUVWQYZ"))
   :player {:hp 10
            :max-hp 10
            :$ 0
            :xp 0
            :level 0
            :sym "@"
            :pos {:x 2 :y 1}
            :inventory []
            :status #{}}
    :npcs {:0 [{:x 8 :y 1 :type :rat :hp 9 :attacks #{:bite :claw}}
               {:x 9 :y 1 :type :rat :hp 9 :attacks #{:bite :claw}}]}})

(defn current-place [state]
  (let [current-place (-> state :world :current-place)]
    (-> state :world :places current-place)))

(defn set-last-command [state command]
  (println "setting last-command " command)
  (assoc-in state [:last-command] command))

(defn get-last-command [state]
  (state :last-command))

(defn with-xy [place]
  (mapcat concat (map-indexed (fn [y line] (map-indexed (fn [x cell] [cell x y]) line)) place)))

(defn map-with-xy [f place]
  (doall 
    (map (fn [e] (apply f e)) (with-xy place))))

(defn get-xy [x y place]
  (when-first [cell (filter (fn [[cell cx cy]] (and (= x cx) (= y cy))) (with-xy place))]
    cell))

(defn npc-at-xy [x y state]
  (let [current-place-id (-> state :world :current-place)
        npcs             (-> state :world :npcs current-place-id)]
    (some (fn [npc] (when (and (= x (npc :x)) (= y (npc :y))) npc)) npcs)))

(defn collide? [x y state]
  (let [cellxy (get-xy x y (current-place state))]
    (println "collide? " cellxy)
    (let [cell (first cellxy)]
      ;; check the cell to see if it is a wall or closed door
      (or
        (-> cell nil?)
        (some (fn [collision-type] (= (cell :type) collision-type)) [:vertical-wall
                                                                     :horizontal-wall
                                                                     :door-closed])
        ;; not a wall or closed door, check for npcs
        (npc-at-xy x y state)))))

(defn player-cellxy [state]
  (let [x (-> state :world :player :pos :x)
        y (-> state :world :player :pos :y)]
    (get-xy x y (current-place state))))

(defn player-dead? [state]
  (contains? (-> state :world :player :status) :dead))
 
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
          (drop-item state-with-command keyin)))
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

(defn render-pick-up [state]
  ;; maybe draw pick up menu
  (when (-> state :world :show-pick-up?)
    (println "render-pick-up")
    (let [player-x   (-> state :world :player :pos :x)
          player-y   (-> state :world :player :pos :y)
          cell       (first (get-xy player-x player-y (current-place state)))
          cell-items (or (cell :items) [])
          hotkeys    (-> state :world :remaining-hotkeys)
          contents   (take 22
                       (concat (map #(format "%c %-38s" (% :hotkey) (% :name))
                                    (fill-missing #(not (contains? % :hotkey))
                                                  #(assoc %1 :hotkey %2)
                                                  hotkeys
                                                  cell-items))
                               (repeat (apply str (repeat 40 " ")))))]
    (println "player-x" player-x "player-y" player-y)
    (println "cell" cell)
    (println "cell-items" cell-items)
    (println "type-contents" (type contents))
    (println "contents" contents)
    (doall (map-indexed (fn [y line]
      (do
        (s/put-string (state :screen) 40 (inc y) line {:fg :black :bg :white :styles #{:bold}}))) contents))
    (println "header")
    (s/put-string (state :screen) 40 0 "  Pick up                               " {:fg :black :bg :white :styles #{:underline :bold}}))))

(defn render-inventory [state]
  (when (-> state :world :show-inventory?)
    (let [contents (take 22
                     (concat (map #(format "%-40s"  (% :name)) (-> state :world :player :inventory))
                             (repeat (apply str (repeat 40 " ")))))]
      (println "render-inventory")
      (println "contents" contents)
      (dorun (map-indexed (fn [y line] (s/put-string (state :screen) 40 (inc y) line {:fg :black :bg :white :styles #{:bold}})) contents))
      (s/put-string (state :screen) 40 0 "  Inventory                             " {:fg :black :bg :white :styles #{:underline}}))))

(defn render-drop [state]
  (when (-> state :world :show-drop?)
    (let [contents (take 22
                     (concat (map #(format "%c %-40s"  (% :hotkey) (% :name))
                                  (-> state :world :player :inventory))
                             (repeat (apply str (repeat 40 " ")))))]
      (println "render-inventory")
      (println "contents" contents)
      (dorun (map-indexed (fn [y line] (s/put-string (state :screen) 40 (inc y) line {:fg :black :bg :white :styles #{:bold}})) contents))
      (s/put-string (state :screen) 40 0 "  Drop Inventory                        " {:fg :black :bg :white :styles #{:underline}}))))


(defn render-map [state]
  (do
    (println "begin-render")
    (s/clear (state :screen))
    ;; draw map
    (map-with-xy
      (fn [cell x y]
        ;;(println cell x y)
        (when (not (nil? cell))
          (let [cell-items (cell :items)
                out-char (if (and cell-items (not (empty? cell-items)))
                           (case :ring 
                             :ring           ["="]
                             :food           ["%"]
                             :bow            [")"]
                             :sword          [")"]
                             :armor          ["["]
                             :shoes          ["!"]
                             :wand           ["/"]
                             :spellbook      ["+"]
                             :scroll         ["?"]
                             :coins          ["$" {:fg :yellow :styles #{:bold}}]
                             :amulet         ["\"" {:fg :blue :styles #{:bold}}]
                             ["?"])
                           (case (cell :type)
                            :vertical-wall   ["|"]
                            :horizontal-wall ["-"]
                            :floor           ["."]
                            :door-open       ["+" {:fg :red :styles #{:bold}}]
                            :door-closed     ["+" {:fg :black :bg :red :styles #{:bold}}]
                            :corridor        ["#"]
                            ["?"]))]
            (apply s/put-string (state :screen) x y out-char))))
      (current-place state))
    ;; draw character
    (println (-> state :world :player))
    (s/put-string
      (state :screen)
      (-> state :world :player :pos :x)
      (-> state :world :player :pos :y)
      (-> state :world :player :sym)
      {:fg :green})
    ;; draw npcs
    (let [current-place-id (-> state :world :current-place)
          place-npcs       (-> state :world :npcs current-place-id)]
      (doall (map (fn [npc]
                    (s/put-string (state :screen)
                                  (npc :x)
                                  (npc :y)
                                  (case (npc :type)
                                    :rat "r"
                                    "?")))
                   place-npcs)))
    ;; maybe draw pick up menu
    (render-pick-up state)
    ;; maybe draw inventory
    (render-inventory state)
    ;; maybe draw drop menu
    (render-drop state)
    ;; draw status bar
    (s/put-string (state :screen) 0  23
      (format " %s $%d HP:%d(%d) Pw:%d(%d) Amr:%d XP:%d/%d T%d                         "
        "location-detail"
        (-> state :world :player :$)
        (-> state :world :player :hp)
        (-> state :world :player :max-hp)
        0 0 10
        (-> state :world :player :xp)
        100
        (-> state :time))
        {:fg :black :bg :white})
    (s/redraw (state :screen))
    (println "end-render")))

(defn render-game-over [state]
  (let [points 0]
    (s/clear (state :screen))
    ;; Title
    (s/put-string (state :screen) 10 1 "You died")
    (s/put-string (state :screen) 10 3 "Inventory:")
    (doall (map-indexed
      (fn [idx item] (s/put-string (state :screen) 10 (+ idx 5) (item :name)))
      (-> state :world :player :inventory)))
    (s/put-string (state :screen) 10 22 "Play again? [yn]")
    (s/redraw (state :screen))))

(defn render [state]
  (cond
    ;; Is player dead?
    (player-dead? state)
      (render-game-over state)
      ;; Render game over
    :else (render-map state)))

;; Example setup and tick fns
(defsource setup []
  (let [screen (s/get-screen :swing)]
    (s/start screen)
    {:world (init-world) :screen screen :time 0}))


(def render-count (atom 0))
(defn tick [state]
  (do
    (println "before-render")
    (swap! render-count inc)
    (when (> @render-count 1) (throw nil))
    (render state)
    (println "after-render")
    (swap! render-count dec)
    (let [keyin  (s/get-key-blocking (state :screen))]
      (println "got " keyin " type " (type keyin))
      (let [newstate (update-state state keyin)]
        (if-not (nil? newstate)
          (update-in newstate [:time] (fn [t] (inc t)))
          nil)))))

