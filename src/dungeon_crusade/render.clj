;; Functions for rendering state to screen
(ns dungeon-crusade.render
  (:use     dungeon-crusade.common
            dungeon-crusade.lineofsight
            [clojure.pprint :only [print-table]])
  (:require [lanterna.screen :as s]
            [lanterna.terminal :as t]
            [lanterna.constants :as c]
            [clojure.reflect :as r]))

;; RBG color definitions. 
;; It's easier to use names than numbers.
(def rgb-color {
 :brown [139 69 19]
 :black [0 0 0]})

(defn render-multi-select
  "Render a menu on the right side of the screen. It has a title, and selected items
   can be identitifed by hotkeys. If elements in `items` have a key whose value
   equals an element in `hotkeys` then the item will be displayed with a `+` rather
   than a `-`. Each element in `items` must contains a key `:name`, the value of which
   will be printed in the menu.
  
       (render-multi-select
         screen
         \"Select\"
         [:a :c]
         [{:name \"Item 1\"
           :hotkey :a}
          {:name \"Item 2\"
           :hotkey :b}
          {:name \"Item 3\"
           :hotkey :c})
  "
  [screen title selected-hotkeys items]
  (let [contents (take 22
                       (concat (map #(format "%c%c%-38s"
                                             (or (% :hotkey)
                                                 \ )
                                             (if (contains? selected-hotkeys (% :hotkey))
                                               \+
                                               \-)
                                             (% :name))
                                    items)
                              (repeat (apply str (repeat 40 " ")))))] 
    (println "contents" contents)
    (doall (map-indexed (fn [y line]
      (do
        (s/put-string screen 40 (inc y) line {:fg :black :bg :white :styles #{:bold}}))) contents))
    (println "header")
    (s/put-string screen 40 0 (format "  %s                               " title) {:fg :black :bg :white :styles #{:underline :bold}})))

(defn render-pick-up
  "Render the pickup item menu if the world state is `:pickup`."
  [state]
  ;; maybe draw pick up menu
  (when (= (-> state :world :current-state) :pickup)
    (let [player-x         (-> state :world :player :pos :x)
          player-y         (-> state :world :player :pos :y)
          cell             (first (get-xy player-x player-y (current-place state)))
          cell-items       (or (cell :items) [])
          hotkeys          (-> state :world :remaining-hotkeys)
          selected-hotkeys (-> state :world :selected-hotkeys)
          items            (fill-missing #(not (contains? % :hotkey))
                                              #(assoc %1 :hotkey %2)
                                              hotkeys
                                              cell-items)]
    (println "player-x" player-x "player-y" player-y)
    ;(println "cell" cell)
    ;(println "cell-items" cell-items)
    (render-multi-select (state :screen) "Pick up" selected-hotkeys items))))

(defn render-inventory
  "Render the pickup item menu if the world state is `:pickup`."
  [state]
  (when (= (-> state :world :current-state) :inventory)
    (render-multi-select (state :screen) "Inventory" [] (-> state :world :player :inventory))))

(defn render-drop
  "Render the pickup item menu if the world state is `:pickup`."
  [state]
  (when (= (-> state :world :current-state) :drop)
    (render-multi-select (state :screen) "Drop Inventory" [] (-> state :world :player :inventory))))

(defn render-describe-inventory
  "Render the pickup item menu if the world state is `:pickup`."
  [state]
  (when (= (-> state :world :current-state) :describe-inventory)
    (render-multi-select (state :screen) "Describe" [] (-> state :world :player :inventory))))

(defn render-eat
  "Render the pickup item menu if the world state is `:pickup`."
  [state]
  (when (= (-> state :world :current-state) :eat)
    (render-multi-select (state :screen) "Eat Inventory" [] (filter #(= (% :type) :food) (-> state :world :player :inventory)))))

(defn render-quests
  "Render the pickup item menu if the world state is `:pickup`."
  [state]
  (when (= (-> state :world :current-state) :quests)
    (render-multi-select (state :screen) "Quests" [] (-> state :quests))))

(defn render-quit?
  "Render the pickup item menu if the world state is `:pickup`."
  [state]
  (when (= (-> state :world :current-state) :quit?)
    (s/put-string (state :screen) 1 0 "quit? [yn]")))

(defn render-map
  "The big render function used during the normal game.
   This renders everything - the map, the menus, the log,
   the status bar. Everything."
  [state]
  (do
    (println "begin-render")
    (s/clear (state :screen))
    ;(println "rendering place" (current-place state))
    ;; draw map
    (map-with-xy
      (fn [cell x y]
        ;(println "render-cell" cell x y)
        (when (and (not (nil? cell))
                   (cell :discovered))
          (let [cell-items (cell :items)
                out-char (if (and cell-items (not (empty? cell-items)))
                           (case (-> cell-items first :type)
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
                            :open-door       ["-" {:fg (rgb-color :brown) :bg (rgb-color :black) :styles #{:bold}}]
                            :close-door     ["+" {:fg (rgb-color :brown) :bg (rgb-color :black) :styles #{:bold}}]
                            :corridor        ["#"]
                            :down-stairs     [">"]
                            :up-stairs       ["<"]
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
          place-npcs       (-> state :world :npcs current-place-id)
          visibility (map-visibility (let [pos (-> state :world :player :pos)]
                                          [(pos :x) (pos :y)])
                                     cell-blocking?
                                     (current-place state))]
      (doall (map (fn [npc]
                    (let [x       (-> npc :pos :x)
                          y       (-> npc :pos :y)
                          visible (get-in visibility [y x])]
                      (when visible
                        (s/put-string (state :screen)
                                      (-> npc :pos :x)
                                      (-> npc :pos :y)
                                      (case (npc :type)
                                        :rat "r"
                                        "?")))))
                   place-npcs)))
    ;; maybe draw pick up menu
    (render-pick-up state)
    ;; maybe draw inventory
    (render-inventory state)
    ;; maybe draw drop menu
    (render-drop state)
    ;; maybe draw describe menu
    (render-describe-inventory state)
    ;; maybe draw eat menu
    (render-eat state)
    ;; maybe draw quests menu
    (render-quests state)
    ;; draw status bar
    (s/put-string (state :screen) 0  23
      (format " %s $%d HP:%d(%d) Pw:%d(%d) Amr:%d XP:%d/%d T%d %s                      "
        "location-detail"
        (-> state :world :player :$)
        (int (-> state :world :player :hp))
        (-> state :world :player :max-hp)
        0 0 10
        (-> state :world :player :xp)
        100
        (-> state :world :time)
        (apply str (interpose " " (-> state :world :player :status))))
        {:fg :black :bg :white})
    ;; draw log
    (when-let [message (-> state :world :log last)]
      (println "message" message)
      (when (< (- (-> state :world :time) (message :time)) 5)
        (s/put-string (state :screen) 0 0 (message :text))))
    ;; draw cursor
    (if-let [cursor-pos (-> state :world :cursor)]
      (s/move-cursor (state :screen) (cursor-pos :x) (cursor-pos :y))
      (s/move-cursor (state :screen) 0 0))
    ;; draw quit prompt
    (render-quit? state)
    (s/redraw (state :screen))
    (println "end-render")))

(defn render-game-over
  "Render the game over screen."
  [state]
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

(defn render
  "Pick between the normal render function and the
   game over render function based on the dead state
   of the player."
  [state]
  (cond
    ;; Is player dead?
    (player-dead? state)
      (render-game-over state)
      ;; Render game over
    :else (render-map state)))


