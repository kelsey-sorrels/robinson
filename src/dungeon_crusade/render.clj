(ns dungeon-crusade.render
  (use     dungeon-crusade.common)
  (require [lanterna.screen :as s]))


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


