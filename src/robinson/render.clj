;; Functions for rendering state to screen
(ns robinson.render
  (:import  (java.awt Color)
            (java.awt.image BufferedImage)
            (javax.swing ImageIcon))
  (:use     robinson.common
            robinson.player
            robinson.magic
            robinson.lineofsight
            [robinson.dialog :exclude [-main]]
            robinson.npc
            tinter.core
            [clojure.pprint :only [print-table]])
  (:require ;[lanterna.screen :as s]
            ;[lanterna.terminal :as t]
            ;[lanterna.constants :as c]
            [clojure.reflect :as r]
            [taoensso.timbre :as timbre]
            [robinson.swingterminal :as swingterminal])
  (:refer   clojure.set)
  (:import robinson.swingterminal.ATerminal))


(timbre/refer-timbre)
(set! *warn-on-reflection* true)

;; RBG color definitions. 
;; It's easier to use names than numbers.
(def color-to-rgb-map
  {:brown       [139 69 19]
   :black       [0 0 0]
   :white       [255 255 255]
   :gray        [128 128 128]
   :light-gray  [64 64 64]
   :dark-gray   [192 192 192]
   :red         (vec (hex-str-to-dec "D31C00"))
   :orange      (vec (hex-str-to-dec "D36C00"))
   :yellow      (vec (hex-str-to-dec "D3B100"))
   :green       (vec (hex-str-to-dec "81D300"))
   :dark-green  (vec (hex-str-to-dec "406900"))
   :blue        (vec (hex-str-to-dec "00ACD3"))
   :light-blue  (vec (hex-str-to-dec "19B4D7"))
   :dark-blue   (vec (hex-str-to-dec "009ABD"))
   :purple      (vec (hex-str-to-dec "8500D3"))
   :fushia      (vec (hex-str-to-dec "D30094"))
   :beige       (vec (hex-str-to-dec "C8B464"))})

(defn darken-rgb [rgb]
  (vec (map #(/ % 3) rgb)))

(defn color->rgb
  [color]
  (get color-to-rgb-map color color))

(defn move-cursor
  ([screen x y]
  nil)
  ([screen o]
  nil))

(defn fill-put-string-color-style-defaults
  ([string]
    (fill-put-string-color-style-defaults string :white :black #{}))
  ([string fg bg]
    (fill-put-string-color-style-defaults string fg bg #{}))
  ([string fg bg styles]
   {:pre [(clojure.set/superset? #{:underline :bold} styles)]}
   (let [fg (color->rgb fg)
         bg (color->rgb bg)]
     [string fg bg styles])))

(defn put-string
  ([^robinson.swingterminal.ATerminal screen x y string]
     (put-string screen x y string :white :black #{}))
  ([^robinson.swingterminal.ATerminal screen x y string fg bg]
     (put-string screen x y string fg bg #{}))
  ([^robinson.swingterminal.ATerminal screen x y string fg bg styles]
   {:pre [(clojure.set/superset? #{:underline :bold} styles)]}
   (let [fg        (color->rgb fg)
         bg        (color->rgb bg)]
     (.put-string screen
                  x
                  y
                  string
                  fg
                  bg
                  styles))))
      
(defn get-size
  [^robinson.swingterminal.ATerminal screen]
  (.get-size screen))

(defn refresh
  [^robinson.swingterminal.ATerminal screen]
  (.refresh screen))

(defn clear
  [^robinson.swingterminal.ATerminal screen]
  (.clear screen))

(defn class->rgb
  "Convert a class to a color characters of that type should be drawn."
  [pc-class]
  ;(debug "class->color" pc-class)
  (color->rgb
    (case pc-class
      :cleric    :white
      :barbarian :red
      :bard      :fushia
      :druid     :yellow
      :fighter   :orange
      :ranger    :green
      :rogue     :gray
      :wizard    :purple)))

(defn is-menu-state? [state]
  (contains? #{:inventory :describe-inventory :pickup :drop :eat} (get-in state [:world :current-state])))

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
                        (concat (map #(do (info %) (format (clojure.string/join ["%c%c%s%-" (- width 2)  "s"])
                                              (or (% :hotkey)
                                                  \ )
                                              (if (contains? selected-hotkeys (% :hotkey))
                                                \+
                                                \-)
                                              (if (contains? % :count)
                                                (format "%dx " (int (get % :count)))
                                                "")
                                              (get % :name)))
                                     items)
                               (repeat (clojure.string/join (repeat width " ")))))] 
     (doseq [i (range (count contents))]
       (let [line (nth contents i)]
         (put-string screen x (+ y i 1) line :black :white #{:bold})))
     (put-string screen x y (apply str (repeat width " ")) :black :white)
     (put-string screen (+ x 2) y title :black :white #{:underline :bold}))))

(defn render-img
  "Render an image using block element U+2584."
  [state ^String path x y]
  (let [image ^java.awt.Image (-> (ImageIcon. path) .getImage)
        width                 (.getWidth image)
        height                (.getHeight image)
        buffered-image        (BufferedImage. width height BufferedImage/TYPE_INT_RGB)
        gfx2d                 (doto (.createGraphics buffered-image)
                                (.drawImage image 0 0 width height nil)
                                (.dispose))]
   (doall
     (for [py (filter even? (range height))
           px (range width)]
       (let [color1 (Color. (.getRGB buffered-image px py))
             color2 (Color. (.getRGB buffered-image px (inc py)))
             rgb1 ((juxt #(.getRed %) #(.getGreen %) #(.getBlue %)) color1)
             rgb2 ((juxt #(.getRed %) #(.getGreen %) #(.getBlue %)) color2)]
         (put-string (state :screen)
                       (+ x px)
                       (int (+ y (/ py 2)))
                       "\u2584"
                       rgb2
                       rgb1
                       #{:underline}))))))


(defn render-pick-up
  "Render the pickup item menu if the world state is `:pickup`."
  [state]
  ;; maybe draw pick up menu
  (when (= (-> state :world :current-state) :pickup)
    (let [player-x         (-> state :world :player :pos :x)
          player-y         (-> state :world :player :pos :y)
          cell             (get-cell (current-place state) player-x player-y)
          cell-items       (or (cell :items) [])
          hotkeys          (-> state :world :remaining-hotkeys)
          selected-hotkeys (-> state :world :selected-hotkeys)
          items            (fill-missing #(not (contains? % :hotkey))
                                              #(assoc %1 :hotkey %2)
                                              hotkeys
                                              cell-items)]
    (debug "player-x" player-x "player-y" player-y)
    (trace "cell" cell)
    (trace "cell-items" cell-items)
    (render-multi-select (state :screen) "Pick up" selected-hotkeys items))))

(defn render-inventory
  "Render the pickup item menu if the world state is `:pickup`."
  [state]
  (when (= (-> state :world :current-state) :inventory)
    (render-multi-select (state :screen) "Inventory" [] (-> state :world :player :inventory))))

(defn render-magic
  "Render the pickup item menu if the world state is `:magic`."
  [state]
  (when (= (-> state :world :current-state) :magic)
    (render-multi-select (state :screen) "Magic" [] (get-magical-abilities (-> state :world :player)))))

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
    (put-string (state :screen) 1 0 "quit? [yn]")))

(defn render-dialog
  "Render the dialog menu if the world state is `:talking`."
  [state]
  (when (= (get-in state [:world :current-state]) :talking)
    (let [npc           (first (talking-npcs state))
          _ (trace "world state" (get-in state [:world :current-state]))
          _ (trace "state :dialog" (state :dialog))
          _ (trace "npcid" (get npc :id))
          fsm           (get-in state [:dialog (get npc :id)])
          _ (trace "fsm" fsm)
          valid-input   (get-valid-input fsm)
          _ (trace "render: valid-input:" valid-input)
          _ (trace "render: current-state:" (fsm-current-state fsm))
          options       (take (count valid-input)
                              (map (fn [k v]
                                     {:hotkey k
                                      :name v})
                                   [\a \b \c \d \e \f]
                               valid-input))
          last-response ((or (last (get-in state [:world :dialog-log])) {:text ""}) :text)
          _ (debug "last-response" last-response)
          response-wrapped (wrap-line (- 30 17) last-response)
          _ (debug "response-wrapped" response-wrapped)]
      (put-string (state :screen) 0 16 (format "Talking to %-69s" (get npc :name)) :black :white #{:bold})
      (doall (map (fn [y] (put-string (state :screen) 12 y "                    " :black :white #{:bold}))
                  (range 17 (+ 17 6))))
      (doall (map-indexed (fn [idx line] (put-string (state :screen) 13 (+ 17 idx) line :black :white #{:bold}))
                          response-wrapped))
      (render-multi-select (state :screen) "Respond:" [] options 32 17 68 5)
      (render-img state (get npc :image-path) 0 17))))

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
      (put-string (state :screen) 0 16 (format "Doing business with %-69s" (get npc :name)) :black :white #{:bold})
      (doall (map (fn [y] (put-string (state :screen) 12 y "                    " :black :white #{:bold}))
                  (range 17 (+ 17 6))))
      (doall (map-indexed (fn [idx line] (put-string (state :screen) 13 (+ 17 idx) line :black :white #{:bold}))
                          response-wrapped))
      (render-multi-select (state :screen) "Option:" [] options 32 17 68 5)
      (render-img state (get npc :image-path) 0 17))))

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
          _ (debug "last-response" last-response)
          response-wrapped (wrap-line (- 30 17) last-response)
          _ (debug "response-wrapped" response-wrapped)
          style {:fg :black :bg :white :styles #{:bold}}]
      (put-string (state :screen) 0 16 (format "Doing business with %-69s" (get npc :name)) :black :white #{:bold})
      (doall (map (fn [y] (put-string (state :screen) 12 y "                    " :black :white #{:bold}))
                  (range 17 (+ 17 6))))
      (doall (map-indexed (fn [idx line] (put-string (state :screen) 13 (+ 17 idx) line :black :white #{:bold}))
                          response-wrapped))
      (render-multi-select (state :screen) "Buy:" [] options 32 17 68 5)
      (render-img state (get npc :image-path) 0 17))))

(defn render-sell
  "Render the dialog menu if the world state is `:sell`."
  [state]
  (when (= (get-in state [:world :current-state]) :sell)
    (let [npc           (first (talking-npcs state))
          buy-fn        (get-in state (get npc :buy-fn-path) (fn [_] nil))
          _ (debug "render-sell (npc :buy-fn-path)" (get npc :buy-fn-path))
          _ (debug "render-sell buy-fn" buy-fn)
          options       (filter #(not (nil? (buy-fn %)))
                                 (get-in state [:world :player :inventory]))
          _ (debug "options" options)
          last-response ((or (last (get-in state [:world :dialog-log])) {:text ""}) :text)
          _ (debug "last-response" last-response)
          response-wrapped (wrap-line (- 30 17) last-response)
          _ (debug "response-wrapped" response-wrapped)
          style {:fg :black :bg :white :styles #{:bold}}]
      (put-string (state :screen) 0 16 (format "Doing business with %-69s" (get npc :name)) :black :white #{:bold})
      (doall (map (fn [y] (put-string (state :screen) 12 y "                    " :black :white #{:bold}))
                  (range 17 (+ 17 6))))
      (doall (map-indexed (fn [idx line] (put-string (state :screen) 13 (+ 17 idx) line :black :white #{:bold}))
                          response-wrapped))
      (render-multi-select (state :screen) "Sell:" [] options 32 17 68 5)
      (render-img state (get npc :image-path) 0 17))))

(defn render-map
  "The big render function used during the normal game.
   This renders everything - the map, the menus, the log,
   the status bar. Everything."
  [state]
  (let [[columns rows] (get-size (state :screen))
         place         (current-place state)
         current-time  (get-in state [:world :time])]
    ;(debug "begin-render")
    ;(clear (state :screen))
    (trace "rendering place" (current-place state))
    ;; draw map
    (doseq [y (range rows)
            x (range
                (if (is-menu-state? state)
                  (- columns 40)
                  columns))]
      (let [screen (get state :screen)
            cell (get-cell place x y)]
        ;(trace "render-cell" cell x y)
        (if (or (nil? cell)
                (not (cell :discovered)))
          (put-string screen x y " ")
          (let [cell-items (cell :items)
                out-char (apply fill-put-string-color-style-defaults
                           (if (and cell-items (not (empty? cell-items)))
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
                               :$              ["$"  :yellow :black #{:bold}]
                               :amulet         ["\"" :blue   :black #{:bold}]
                               ["?"])
                             (case (cell :type)
                              :vertical-wall   ["|"]
                              :horizontal-wall ["-"]
                              :floor           ["."]
                              :open-door       ["-"  :brown  :black #{:bold}]
                              :close-door      ["+"  :brown  :black #{:bold}]
                              :corridor        ["#"] 
                              :down-stairs     [">"] 
                              :up-stairs       ["<"] 
                              :water           ["~"  (rand-nth [:blue :light-blue :dark-blue]) :black]
                              :sand            ["_"  :beige      :black]
                              :dirt            [","  :brown      :black]
                              :gravel          ["`"  :gray       :black]
                              :short-grass     ["'"  :green      :black]
                              :tall-grass      ["\"" :dark-green :black]
                              :tree            ["T"  :dark-green :black]
                              ["?"])))
                shaded-out-char (if (= (cell :discovered) current-time)
                                  out-char
                                  (update-in out-char [1] darken-rgb))]
              (apply put-string screen x y shaded-out-char)))))
    ;; draw character
    ;(debug (-> state :world :player))
    (put-string
      (state :screen)
      (-> state :world :player :pos :x)
      (-> state :world :player :pos :y)
      "@"
      (class->rgb (-> state :world :player :class))
      :black)
    ;; draw npcs
    (let [place-npcs (npcs-at-current-place state)
          ;_ (debug "place-npcs" place-npcs)
          pos (-> state :world :player :pos)
          get-cell (memoize (fn [x y] (get-in (current-place state) [y x])))]
      (doall (map (fn [npc]
                    (let [x       (-> npc :pos :x)
                          y       (-> npc :pos :y)
                          visible 
                                  (and (not (farther-than?
                                              pos
                                              {:x x :y y}
                                              3))
                                       (visible? get-cell
                                                 cell-blocking?
                                                 (pos :x)
                                                 (pos :y)
                                                 x
                                                 y))]
                      ;(debug "npc@" x y "visible?" visible)
                      (when visible
                        (apply put-string (state :screen)
                                            (-> npc :pos :x)
                                            (-> npc :pos :y)
                                            (case (get npc :race)
                                              :rat ["r"]
                                              :spider ["S"]
                                              :scorpion ["\u03C2"] ;;ς
                                              :snake ["\u00A7"] ;;§
                                              :bat ["B"]
                                              :boar ["b"]
                                              :gecko ["g"]
                                              :monkey ["y"]
                                              :bird ["a"]
                                              :centipede ["c"]
                                              :turtle ["t"]
                                              :frog ["\u03B1"] ;;α
                                              :parrot ["p"]
                                              :shark ["\u039B"] ;;Λ
                                              :fish ["f"]
                                              :octopus ["#" :orange :black]
                                              :sea-snake ["\u00A7"]
                                              :clam ["c"]
                                              :urchin ["u"]
                                              :squid ["q" :orange :black]
                                              :human ["@" (class->rgb (get npc :class)) :black]
                                              ["@"])))))
                   place-npcs)))
    ;; maybe draw pick up menu
    (render-pick-up state)
    ;; maybe draw inventory
    (render-inventory state)
    ;; maybe draw magic menu
    (render-magic state)
    ;; maybe draw drop menu
    (render-drop state)
    ;; maybe draw describe menu
    (render-describe-inventory state)
    ;; maybe draw eat menu
    (render-eat state)
    ;; maybe draw quests menu
    (render-quests state)
    ;; draw status bar
    (put-string (state :screen) 0  23
      (format "Dgnlvl %s $%d HP:%d(%d) Pw:%d(%d) Amr:%d XP:%d/%d T%d %s %s                            "
        (name (-> state :world :current-place))
        (-> state :world :player :$)
        (int (-> state :world :player :hp))
        (-> state :world :player :max-hp)
        0 0 10
        (-> state :world :player :xp)
        100
        (-> state :world :time)
        (apply str (interpose " " (-> state :world :player :status)))
        (-> state :world :player :name))
        :black :white)
    (doall (map #(put-string (state :screen) (+ 73 %1) 23 " " :white (color->rgb %2))
                (range)
                [:dark-gray :gray :light-gray]))
    (put-string (state :screen) 76 23 "@" (class->rgb (-> state :world :player :class)) :black)
    (put-string (state :screen) 77 23 "    " :white :black)
    ;; draw log
    (let [logs-viewed (get-in state [:world :logs-viewed])
          current-time (get-in state [:world :time])
          cur-state (current-state state)]
      (debug "current-state" cur-state)
      (if (= cur-state :more-log)
        (let [logs (vec (filter #(= (get % :time) current-time) (get-in state [:world :log])))
              _ (info "logs-viewed" logs-viewed "current-time" current-time "logs" logs)
              message (get logs (dec logs-viewed))]
          ;(debug "message" message)
          (put-string (state :screen) 0 0 (format "%s --More--" (message :text))))
        (let [message (last (get-in state [:world :log]))]
          (when (and message
                     (< (- current-time (message :time)) 5))
            (put-string (state :screen) 0 0 (get message :text))))))
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
      (move-cursor (state :screen) (cursor-pos :x) (cursor-pos :y))
      (let [[x y] (get-size (state :screen))]
        (move-cursor (state :screen) (dec x) (dec y))))
    (move-cursor (state :screen) nil)
    (refresh (state :screen))))
    ;;(debug "end-render")))

(defn render-game-over
  "Render the game over screen."
  [state]
  (let [points 0]
    (clear (state :screen))
    ;; Title
    (put-string (state :screen) 10 1 "You died")
    (put-string (state :screen) 10 3 "Inventory:")
    (doall (map-indexed
      (fn [idx item] (put-string (state :screen) 10 (+ idx 5) (item :name)))
      (-> state :world :player :inventory)))
    (put-string (state :screen) 10 22 "Play again? [yn]")
    (refresh (state :screen))))

(defn render-help
  "Render the help screen."
  [state]
  (let [help-contents (read-string (slurp "data/help"))]
    (clear (state :screen))
    (doall (map-indexed (fn [idx line]
                          (put-string (state :screen) 0 idx line))
                        help-contents))
    (refresh (state :screen))))

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


