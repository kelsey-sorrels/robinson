(ns dungeon-crusade.render
  (:use     dungeon-crusade.common
            [clojure.pprint :only [print-table]])
  (:require [lanterna.screen :as s]
            [lanterna.terminal :as t]
            [lanterna.constants :as c]
            [clojure.reflect :as r]))

(def rgb-color {
 :brown [139 69 19]
 :black [0 0 0]})

(set! *warn-on-reflection* true)

(defn render-multi-select [screen title selected-hotkeys items]
  (let [contents (take 22
                       (concat (map #(format "%c%c%-38s"
                                             (% :hotkey)
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

(defn render-pick-up [state]
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
    (println "cell" cell)
    (println "cell-items" cell-items)
    (render-multi-select (state :screen) "Pick up" selected-hotkeys items))))

(defn render-inventory [state]
  (when (= (-> state :world :current-state) :inventory)
    (render-multi-select (state :screen) "Inventory" [] (-> state :world :player :inventory))))

(defn render-drop [state]
  (when (= (-> state :world :current-state) :drop)
    (render-multi-select (state :screen) "Drop Inventory" [] (-> state :world :player :inventory))))

(defn render-eat [state]
  (when (= (-> state :world :current-state) :eat)
    (render-multi-select (state :screen) "Eat Inventory" [] (filter #(= (% :type) :food) (-> state :world :player :inventory)))))

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
                            :door-open       ["-" {:fg (rgb-color :brown) :bg (rgb-color :black) :styles #{:bold}}]
                            :door-closed     ["+" {:fg (rgb-color :brown) :bg (rgb-color :black) :styles #{:bold}}]
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
    ;; maybe draw eat menu
    (render-eat state)
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


