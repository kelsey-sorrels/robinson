;; Functions for rendering state to screen
(ns dungeon-crusade.render
  (:import  (java.awt Color)
            (java.awt.image BufferedImage)
            (javax.swing ImageIcon))
  (:use     dungeon-crusade.common
            dungeon-crusade.lineofsight
            [dungeon-crusade.dialog :exclude [-main]]
            dungeon-crusade.npc
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
  ([screen title selected-hotkeys items]
   (render-multi-select screen title selected-hotkeys items 40 0 40 22))
  ([screen title selected-hotkeys items x y width height]
   (let [contents (take height
                        (concat (map #(format (clojure.string/join ["%c%c%-" (- width 2)  "s"])
                                              (or (% :hotkey)
                                                  \ )
                                              (if (contains? selected-hotkeys (% :hotkey))
                                                \+
                                                \-)
                                              (% :name))
                                     items)
                               (repeat (clojure.string/join (repeat width " ")))))] 
     (println "contents" contents)
     (doall (map-indexed (fn [idx line]
       (do
         (s/put-string screen x (+ y idx 1) line {:fg :black :bg :white :styles #{:bold}}))) contents))
     (println "header")
     (s/put-string screen x y (apply str (repeat width " ")) {:fg :black :bg :white})
     (s/put-string screen (+ x 2) y title {:fg :black :bg :white :styles #{:underline :bold}}))))

(defn render-img
  "Render an image using block element U+2584."
  [state path x y]
  (let [image          (-> (ImageIcon. path) .getImage)
        width          (.getWidth image)
        height         (.getHeight image)
        buffered-image (BufferedImage. width height BufferedImage/TYPE_INT_RGB)
        gfx2d          (doto (.createGraphics buffered-image)
                         (.drawImage image 0 0 width height nil)
                         (.dispose))]
   (doall
     (for [py (filter even? (range height))
           px (range width)]
       (let [color1 (Color. (.getRGB buffered-image px py))
             color2 (Color. (.getRGB buffered-image px (inc py)))
             rgb1 ((juxt #(.getRed %) #(.getGreen %) #(.getBlue %)) color1)
             rgb2 ((juxt #(.getRed %) #(.getGreen %) #(.getBlue %)) color2)]
         (s/put-string (state :screen)
                       (+ x px)
                       (int (+ y (/ py 2)))
                       "\u2584"
                       {:fg rgb2 :bg rgb1 :styles #{:underline}}))))))


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
    (render-multi-select (state :screen)
                         "Eat Inventory"
                         []
                         (filter #(= (% :type) :food)
                                 (-> state :world :player :inventory)))))

(defn render-quests
  "Render the pickup item menu if the world state is `:pickup`."
  [state]
  (when (= (-> state :world :current-state) :quests)
    (render-multi-select (state :screen)
                         "Quests"
                         []
                         (filter (fn [quest]
                                   (not (nil? (get-in state [:world  :quests (quest :id) :stage] nil))))
                                 (-> state :quests)))))

(defn render-quit?
  "Render the pickup item menu if the world state is `:pickup`."
  [state]
  (when (= (-> state :world :current-state) :quit?)
    (s/put-string (state :screen) 1 0 "quit? [yn]")))

(defn render-dialog
  "Render the dialog menu if the world state is `:talking`."
  [state]
  (when (= (get-in state [:world :current-state]) :talking)
    (let [npc           (first (talking-npcs state))
          ;_ (println "world state" (get-in state [:world :current-state]))
          ;_ (println "state :dialog" (state :dialog))
          ;_ (println "npcid" (npc :id))
          fsm           (get-in state [:dialog (npc :id)])
          ;_ (println "fsm" fsm)
          valid-input   (get-valid-input fsm)
          ;_ (println "render: valid-input:" valid-input)
          ;_ (println "render: current-state:" (fsm-current-state fsm))
          options       (take (count valid-input)
                              (map (fn [k v]
                                     {:hotkey k
                                      :name v})
                                   [\a \b \c \d \e \f]
                               valid-input))
          last-response ((or (last (get-in state [:world :dialog-log])) {:text ""}) :text)
          _ (println "last-response" last-response)
          response-wrapped (wrap-line (- 30 17) last-response)
          _ (println "response-wrapped" response-wrapped)
          style {:fg :black :bg :white :styles #{:bold}}]
      (s/put-string (state :screen) 0 16 (format "Talking to %-69s" (npc :name)) style)
      (doall (map (fn [y] (s/put-string (state :screen) 12 y "                    " style))
                  (range 17 (+ 17 6))))
      (doall (map-indexed (fn [idx line] (s/put-string (state :screen) 13 (+ 17 idx) line style))
                          response-wrapped))
      (render-multi-select (state :screen) "Respond:" [] options 32 17 68 5)
      (render-img state (npc :image-path) 0 17))))

(defn render-shopping
  "Render the shopping menu if the world state is `:shopping`."
  [state]
  (when (= (get-in state [:world :current-state]) :shopping)
    (let [npc           (first (talking-npcs state))
          options       [{:hotkey \a
                          :name "Buy"}
                         {:hotkey \b
                          :name "Sell"}]
          last-response ((or (last (get-in state [:world :dialog-log])) {:text ""}) :text)
          response-wrapped (wrap-line (- 30 17) last-response)
          style {:fg :black :bg :white :styles #{:bold}}]
      (s/put-string (state :screen) 0 16 (format "Doing business with %-69s" (npc :name)) style)
      (doall (map (fn [y] (s/put-string (state :screen) 12 y "                    " style))
                  (range 17 (+ 17 6))))
      (doall (map-indexed (fn [idx line] (s/put-string (state :screen) 13 (+ 17 idx) line style))
                          response-wrapped))
      (render-multi-select (state :screen) "Option:" [] options 32 17 68 5)
      (render-img state (npc :image-path) 0 17))))

(defn render-buy
  "Render the dialog menu if the world state is `:buy`."
  [state]
  (when (= (get-in state [:world :current-state]) :buy)
    (let [npc           (first (talking-npcs state))
          valid-input   (map (fn [item] (format "%s-$%d"  (item :name) (item :price)))
                             (filter (fn [item] (contains? item :price))
                                     (get npc :inventory [])))
          options       (take (count valid-input)
                              (map (fn [k v]
                                     {:hotkey k
                                      :name v})
                                   [\a \b \c \d \e \f]
                               valid-input))
          last-response ((or (last (get-in state [:world :dialog-log])) {:text ""}) :text)
          _ (println "last-response" last-response)
          response-wrapped (wrap-line (- 30 17) last-response)
          _ (println "response-wrapped" response-wrapped)
          style {:fg :black :bg :white :styles #{:bold}}]
      (s/put-string (state :screen) 0 16 (format "Doing business with %-69s" (npc :name)) style)
      (doall (map (fn [y] (s/put-string (state :screen) 12 y "                    " style))
                  (range 17 (+ 17 6))))
      (doall (map-indexed (fn [idx line] (s/put-string (state :screen) 13 (+ 17 idx) line style))
                          response-wrapped))
      (render-multi-select (state :screen) "Buy:" [] options 32 17 68 5)
      (render-img state (npc :image-path) 0 17))))

(defn render-sell
  "Render the dialog menu if the world state is `:sell`."
  [state]
  (when (= (get-in state [:world :current-state]) :sell)
    (let [npc           (first (talking-npcs state))
          buy-fn        (get-in state (npc :buy-fn-path) (fn [_] nil))
          _ (println "render-sell (npc :buy-fn-path)" (npc :buy-fn-path))
          _ (println "render-sell buy-fn" buy-fn)
          options       (filter #(not (nil? (buy-fn %)))
                                 (get-in state [:world :player :inventory]))
          _ (println "options" options)
          last-response ((or (last (get-in state [:world :dialog-log])) {:text ""}) :text)
          _ (println "last-response" last-response)
          response-wrapped (wrap-line (- 30 17) last-response)
          _ (println "response-wrapped" response-wrapped)
          style {:fg :black :bg :white :styles #{:bold}}]
      (s/put-string (state :screen) 0 16 (format "Doing business with %-69s" (npc :name)) style)
      (doall (map (fn [y] (s/put-string (state :screen) 12 y "                    " style))
                  (range 17 (+ 17 6))))
      (doall (map-indexed (fn [idx line] (s/put-string (state :screen) 13 (+ 17 idx) line style))
                          response-wrapped))
      (render-multi-select (state :screen) "Sell:" [] options 32 17 68 5)
      (render-img state (npc :image-path) 0 17))))

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
    (let [place-npcs (npcs-at-current-place state)
          _ (println "place-npcs" place-npcs)
          visibility (map-visibility (let [pos (-> state :world :player :pos)]
                                          [(pos :x) (pos :y)])
                                     cell-blocking?
                                     (current-place state))]
      ;(println "visibility" visibility)
      (doall (map (fn [npc]
                    (let [x       (-> npc :pos :x)
                          y       (-> npc :pos :y)
                          visible (get-in visibility [y x])]
                      (println "npc@" x y "visible?" visible)
                      (when visible
                        (s/put-string (state :screen)
                                      (-> npc :pos :x)
                                      (-> npc :pos :y)
                                      (case (npc :type)
                                        :rat "r"
                                        "@")))))
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
      (format "Dgnlvl %s $%d HP:%d(%d) Pw:%d(%d) Amr:%d XP:%d/%d T%d %s                             "
        (name (-> state :world :current-place))
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
        (s/put-string (state :screen) 0 0 (or (message :text) ""))))
    ;; draw quit prompt
    (render-quit? state)
    ;; draw dialog menu
    (render-dialog state)
    ;; draw shopping menu
    (render-shopping state)
    ;; draw buying menu
    (render-buy state)
    ;; draw selling menu
    (render-sell state)
    ;; draw cursor
    (if-let [cursor-pos (-> state :world :cursor)]
      (s/move-cursor (state :screen) (cursor-pos :x) (cursor-pos :y))
      (let [[x y] (s/get-size (state :screen))]
        (s/move-cursor (state :screen) (dec x) (dec y))))
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

(defn render-help
  "Render the help screen."
  [state]
  (let [help-contents (read-string (slurp "data/help"))]
    (s/clear (state :screen))
    (doall (map-indexed (fn [idx line]
                          (s/put-string (state :screen) 0 idx line))
                        help-contents))
    (s/redraw (state :screen))))

(defn render
  "Pick between the normal render function and the
   game over render function based on the dead state
   of the player."
  [state]
  (cond
    ;; Is player dead?
    (player-dead? state)
      ;; Render game over
      (render-game-over state)
    (= (get-in state [:world :current-state]) :help)
      (render-help state)
    :else (render-map state)))


