;; Functions for rendering state to screen
(ns robinson.render
  (:import  (java.awt Color Image)
            (java.awt.image BufferedImage)
            (javax.swing ImageIcon))
  (:use     robinson.common
            robinson.player
            robinson.endgame
            robinson.magic
            robinson.crafting
            [robinson.itemgen :only [can-be-wielded?
                                     id->name]]
            [robinson.lineofsight :exclude [-main]]
            [robinson.dialog :exclude [-main]]
            robinson.npc
            tinter.core
            [clojure.pprint :only [print-table]])
  (:require 
            [clojure.reflect :as r]
            [taoensso.timbre :as timbre]
            [robinson.startgame :as sg]
            [robinson.itemgen :as ig]
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

(defn render-line
  [screen x y width line fg bg {:keys [underline center invert]
                                :or   {:underline false :center false :invert false}}]
  (let [width    (if (= width :auto)
                   (count line)
                   width)
        ;; pad to width
        s        (if center
                   ;; center justify
                   (clojure.pprint/cl-format nil (format "~%d<~;~A~;~>" width) line)
                   ;; left justify
                   (clojure.pprint/cl-format nil (format "~%d<~A~;~>" width) line))
        [fg bg]  (if invert
                   [bg fg]
                   [fg bg])
        style    (if underline
                   #{:underline}
                   #{})]
    #_(info "put-string" (format "\"%s\"" line) "width" width)
    (put-string screen x y s fg bg style)))


(defn render-rect-border
  [screen x y width height fg bg]
  ;; render top and bottom
  (doseq [dx (range (dec width))]
    (put-string screen (+ x dx 1) y "\u2500" fg bg #{})
    (put-string screen (+ x dx 1) (+ y height) "\u2500" fg bg #{}))
  ;; render left and right
  (doseq [dy (range (dec height))]
    (put-string screen x (+ y dy 1) "\u2502" fg bg #{})
    (put-string screen (+ x width) (+ y dy 1) "\u2502" fg bg #{}))
  ;; render tl, tr, bl, br
  (put-string screen x y "\u250C" fg bg #{})
  (put-string screen (+ x width) y "\u2510" fg bg #{})
  (put-string screen x (+ y height) "\u2514" fg bg #{})
  (put-string screen (+ x width) (+ y height) "\u2518" fg bg #{}))

(defn render-vertical-border
  [screen x y height fg bg]
  (doseq [dy (range (dec height))]
    (put-string screen x (+ y dy) "\u2502" fg bg #{})))

(defn render-list
  "Render a sequence of lines padding to height if necessary.
   Lines are maps containing the keys `:s :fg :bg :style`."
  [screen x y width height items]
  (doseq [i (range height)]
    (if (< i (count items))
      (let [item (nth items i)]
        (render-line screen x (+ y i) width (get item :s) (get item :fg) (get item :bg) (get item :style)))
      (render-line screen x (+ y i) width "" :white :white #{}))))

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
   (render-multi-select screen title selected-hotkeys items x y width height {}))
  ([screen title selected-hotkeys items x y width height {:keys [use-applicable center border center-title]
                                                          :or {:use-applicable false :border false :center false :center-title false}}]
   ;; items is list of {:s "string" :fg :black :bg :white}
   (let [items    (map (fn [item] {:s (format "%c%c%s%s"
                                              (or (item :hotkey)
                                                  \ )
                                              (if (contains? selected-hotkeys (item :hotkey))
                                                \+
                                                \-)
                                              (if (contains? item :count)
                                                (format "%dx " (int (get item :count)))
                                                "")
                                              (get item :name))
                                   :fg (if (or (not use-applicable)
                                               (get item :applicable))
                                         :black
                                         :gray)
                                   :bg :white
                                   :style #{}})
                       items)
         ;; if width is :auto calc width by using max item length
         width    (if (= width :auto)
                    (reduce max (map (fn [item] (count (get item :s))) items))
                    width)
         height   (if (= height :auto)
                    (count items)
                    height)
         title    (if (and title center-title)
                    (clojure.pprint/cl-format nil (format "~%d<~;~A~;~>" width) title)
                    (when title
                      (format "  %s" title)))
         items    (if title
                     (concat [{:s title :fg :black :bg :white :style #{:underline :bold}}]
                              items)
                     items)]
     (render-list screen x y width height items))))

(defn render-atmo
  [state x y]
  (let [screen (get state :screen)
        atmo   (get-in state [:data :atmo])
        frames (count atmo)
        t      (mod (get-in state [:world :time]) frames)
        frame  (nth atmo t)
        indexed-colors (map vector (partition 3 frame) (range))]
    (doseq [[column i] indexed-colors]
      #_(info x i y column)
      (put-string screen (+ x i) y       "\u2584" (if (contains? #{0 6} i)
                                                    :black
                                                    (nth column 0))
                                                  :black #{:underline})
      (put-string screen (+ x i) (inc y) "\u2584" (nth column 2) (nth column 1) #{:underline}))))

(defn render-hud
  [state]
    ;; render atmo
    (render-atmo state 37 21)
    ;; render statuses
    (let [screen (state :screen)]
      (put-string screen 37 23 " "      :black :gray)
      (put-string screen 38 23 "\u2665" :black :gray)
      (put-string screen 39 23 " "      :black :gray)
      (put-string screen 40 23 "\u2665" :black :gray)
      (put-string screen 41 23 " "      :black :gray)
      (put-string screen 42 23 "\u2665" :black :gray)
      (put-string screen 43 23 " "      :black :gray)
      ;; render will to live and hp
      (let [wtl        (get-in state [:world :player :will-to-live])
            max-wtl    (get-in state [:world :player :max-will-to-live])
            hp         (get-in state [:world :player :hp])
            max-hp     (get-in state [:world :player :max-hp])
            hunger     (get-in state [:world :player :hunger])
            max-hunger (get-in state [:world :player :max-hunger])
            thirst     (get-in state [:world :player :thirst])
            max-thirst (get-in state [:world :player :max-thirst])]
        (doseq [x (range 37)]
          (put-string screen x 23 "\u2584" (if (> (/ (- 37 x) 37)
                                                  (/ wtl max-wtl))
                                             :black
                                             :green)
                                           (if (> (/ (- 37 x) 37)
                                                  (/ hp max-hp))
                                             :black
                                             :red)
                                           #{:underline}))
        (doseq [x (range (- 80 43))]
          (put-string screen (+ 44 x) 23 "\u2584"
                                           (if (> (/ x (- 80 44))
                                                  (/ hunger max-hunger))
                                             :black
                                             :yellow)
                                           (if (> (/ x (- 80 44))
                                                  (/ thirst max-thirst))
                                             :black
                                             :blue)
                                           #{:underline})))))
    ;    (int (-> state :world :player :hp))
    ;    (-> state :world :player :max-hp)
    ;    (apply str (interpose " " (-> state :world :player :status)))

(defn render-img
  "Render an image using block element U+2584."
  [state ^String path x y]
  (let [image ^Image (-> (ImageIcon. path) .getImage)
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
             rgb1 ((juxt (fn [^Color c] (.getRed c)) 
                         (fn [^Color c] (.getGreen c))
                         (fn [^Color c] (.getBlue c)))
                   color1)           
             rgb2 ((juxt (fn [^Color c] (.getRed c))
                         (fn [^Color c] (.getGreen c))
                         (fn [^Color c] (.getBlue c)))
                   color2)]
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
  (render-multi-select (state :screen) "Pick up" selected-hotkeys items)))

(defn render-inventory
  "Render the pickup item menu if the world state is `:pickup`."
  [state]
  (render-multi-select (state :screen) "Inventory" [] (-> state :world :player :inventory)))

(defn render-apply
  "Render the inventory menu with `Apply` as the title."
  [state]
  (render-multi-select (state :screen) "Apply Inventory" [] (-> state :world :player :inventory)))

(defn render-apply-to
  "Render the inventory menu with `Apply To` as the title."
  [state]
  (render-multi-select (state :screen) "Apply To" [] (-> state :world :player :inventory)))

(defn render-quaff-inventory
  "Render the inventory menu with `Quaff` as the title."
  [state]
  (render-multi-select (state :screen) "Quaff To" [] (filter ig/is-quaffable?
                                                             (-> state :world :player :inventory))))

(defn render-magic
  "Render the pickup item menu if the world state is `:magic`."
  [state]
  (render-multi-select (state :screen) "Magic" [] (get-magical-abilities (-> state :world :player))))

(defn render-drop
  "Render the pickup item menu if the world state is `:pickup`."
  [state]
  (render-multi-select (state :screen) "Drop Inventory" [] (-> state :world :player :inventory)))

(defn render-describe-inventory
  "Render the pickup item menu if the world state is `:pickup`."
  [state]
  (render-multi-select (state :screen) "Describe" [] (-> state :world :player :inventory)))

(defn render-eat
  "Render the pickup item menu if the world state is `:pickup`."
  [state]
  (render-multi-select (state :screen)
                       "Eat Inventory"
                       []
                       (filter #(contains? % :hunger)
                               (-> state :world :player :inventory))))

(defn render-quests
  "Render the pickup item menu if the world state is `:pickup`."
  [state]
  (render-multi-select (state :screen)
                       "Quests"
                       []
                       (filter (fn [quest]
                                 (not (nil? (get-in state [:world  :quests (quest :id) :stage] nil))))
                               (-> state :quests))))

(defn render-quit?
  "Render the pickup item menu if the world state is `:pickup`."
  [state]
  (put-string (state :screen) 1 0 "quit? [yn]"))

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
    (render-img state (get npc :image-path) 0 17)))

(defn render-buy
  "Render the dialog menu if the world state is `:buy`."
  [state]
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
    (render-img state (get npc :image-path) 0 17)))

(defn render-sell
  "Render the dialog menu if the world state is `:sell`."
  [state]
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
    (render-img state (get npc :image-path) 0 17)))

(defn render-craft
  "Render the craft menu if the world state is `:craft`."
  [state]
  (let [screen (state :screen)]
    (render-multi-select screen nil [] [{:name "Weapons" :hotkey \w}
                                            {:name "Survival" :hotkey \s}]
                                            30 6 20 5)
    (render-rect-border screen 29 5 20 5 :black :white)
    (put-string screen 37 5 "Craft" :black :white)))

(defn render-craft-submenu
  "Render the craft submenu"
  [state recipe-type]
  (let [screen               (state :screen)
        selected-recipe-path (get-in state [:world :craft-recipe-path])
        hotkey               (when selected-recipe-path
                               (last selected-recipe-path))
        recipes              (get (get-recipes state) recipe-type)]
  (info "recipe-type" recipe-type)
  (info "recipes" (get-recipes state))
  (info "selected recipes" recipes)
  ;; render recipes
  (render-list screen 11 6 29 15
    (concat
      [{:s (str recipe-type) :fg :black :bg :white :style #{:underline}}]
       (map (fn [recipe]
              {:s (format "%c-%s"
                    (get recipe :hotkey)
                    (get recipe :name))
               :fg (if (contains? recipe :applicable)
                     :black
                     :gray)
               :bg :white
               :style (if (= (get recipe :hotkey) hotkey)
                         #{:invert}
                         #{})})
            recipes)))
  ;; render recipe-info
  (if hotkey
    (let [matching-recipes (filter (fn [recipe] (= (get recipe :hotkey) hotkey))
                                   recipes)
          recipe           (get (first matching-recipes) :recipe)
          exhaust          (get recipe :exhaust [])
          have             (get recipe :have-or [])]
      (info "exhaust" exhaust "have" have)
      (render-list screen 41 6 29 15
      (concat
        [{:s "" :fg :black :bg :white :style #{}}
         {:s "Consumes" :fg :black :bg :white :style #{}}]
        (if (empty? exhaust)
          [{:s "N/A" :fg :black :bg :white :style #{}}]
          (map (fn [id] {:s (id->name id) :fg :black :bg :white :style #{}}) exhaust))
        [{:s "" :fg :black :bg :white :style #{}}
         {:s "Required tools" :fg :black :bg :white :style #{}}]
        (if (empty? have)
          [{:s "N/A" :fg :black :bg :white :style #{}}]
          (map (fn [id] {:s (id->name id) :fg :black :bg :white :style #{}}) have)))))
    (render-list screen 41 6 29 15
        [{:s "Select a recipe" :fg :black :bg :white :style #{}}]))
  (render-rect-border screen 10 5 60 15 :black :white)
  (render-vertical-border screen 40 6 15 :black :white)
  (put-string screen 40 20 "\u2534" :black :white)
  (put-string screen 37 5 "Craft" :black :white)))
          
(defn render-craft-weapon
  "Render the craft weapon menu if the world state is `:craft-weapon`."
  [state]
  (render-craft-submenu state :weapons))

(defn render-craft-survival
  "Render the craft menu if the world state is `:craft-survival`."
  [state]
  (render-craft-submenu state :survival))

(defn render-wield
  "Render the wield item menu if the world state is `:wield`."
  [state]
  (render-multi-select (state :screen) "Wield" [] (filter can-be-wielded? (-> state :world :player :inventory))))

(defn render-harvest
  "Render the harvest prompt if the world state is `:harvest`."
  [state]
  (put-string (state :screen) 0 0 "Pick a direction to harvest."))

(defn render-map
  "The big render function used during the normal game.
   This renders everything - the map, the menus, the log,
   the status bar. Everything."
  [state]
  (let [screen (state :screen)
        [columns rows] (get-size screen)
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
      (let [cell (get-cell place x y)]
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
                              :sand            ["."  :beige      :black]
                              :dirt            ["."  :brown      :black]
                              :gravel          ["."  :gray       :black]
                              :short-grass     ["."  :green      :black]
                              :tall-grass      ["\"" :dark-green :black]
                              :tree            ["T"  :dark-green :black]
                              :palm-tree       ["7"  :dark-green :black]
                              :fruit-tree      ["\u2648"  :dark-green :black] ;; ♈
                              :freshwater-hole (if (< 10 (get cell :water 0))
                                                 ["~" (rand-nth [:blue :light-blue :dark-blue]) :black]
                                                 ["O"])
                              :saltwater-hole  (if (< 10 (get cell :water 0))
                                                 ["~" (rand-nth [:blue :light-blue :dark-blue]) :black]
                                                 ["O"])
                              :dry-hole        ["O"]
                              ["?"])))
                shaded-out-char (if (= (cell :discovered) current-time)
                                  out-char
                                  (update-in out-char [1] darken-rgb))]
              (apply put-string screen x y shaded-out-char)))))
    ;; draw character
    ;(debug (-> state :world :player))
    (put-string
      screen
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
                        (apply put-string screen
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
    (render-hud state)
    (info "current-state" (current-state state))
    (case (current-state state)
      :pickup             (render-pick-up state)
      :inventory          (render-inventory state)
      :apply              (render-apply state)
      :apply-item-inventory
                          (render-apply-to state)
      :quaff-inventory
                          (render-quaff-inventory state)
      :magic              (render-magic state)
      :drop               (render-drop state)
      :describe-inventory (render-describe-inventory state)
      :eat                (render-eat state)
      :quests             (render-quests state)
      :craft              (render-craft state)
      :craft-weapon       (render-craft-weapon state)
      :craft-survival     (render-craft-survival state)
      :wield              (render-wield state)
      nil)
    (if-not (nil? (get-in state [:world :ui-hint]))
      (put-string screen 0 0 (get-in state [:world :ui-hint]) :white :black)
      ;; draw log
      (when (= (current-state state) :normal)
        (let [logs-viewed (get-in state [:world :logs-viewed])
              current-time (get-in state [:world :time])
              cur-state (current-state state)]
          (debug "current-state" cur-state)
          (if (= cur-state :more-log)
            (let [logs (vec (filter #(= (get % :time) current-time) (get-in state [:world :log])))
                  _ (info "logs-viewed" logs-viewed "current-time" current-time "logs" logs)
                  message (get logs (dec logs-viewed))]
              ;(debug "message" message)
              (put-string screen 0 0 (format "%s --More--" (message :text)) (get message :color) :black))
            (let [message (last (get-in state [:world :log]))]
              (info "message" message)
              (when (and message
                         (< (- current-time (message :time)) 5))
                (put-string screen 0 0 (get message :text) (get message :color) :black)))))))
    (case (current-state state)
      :quit               (render-quit? state)
      :harvest            (render-harvest state)
      :dialog             (render-dialog state)
      :shopping           (render-shopping state)
      :buy                (render-buy state)
      :sell               (render-sell state)
      nil)
    ;; draw cursor
    (if-let [cursor-pos (-> state :world :cursor)]
      (move-cursor screen (cursor-pos :x) (cursor-pos :y))
      (let [[x y] (get-size screen)]
        (move-cursor screen (dec x) (dec y))))
    (move-cursor screen nil)
    (refresh screen)))
    ;;(debug "end-render")))

(defn render-start [state]
  (let [screen (state :screen)
        player-name (get-in state [:world :player :name])]
    (clear (state :screen))
    (put-string screen 30 5 "Robinson")
    (put-string screen 20 7 "Name:___________________")
    (put-string screen 25 7 (str player-name "\u2592"))
    (refresh screen)))

(defn render-start-inventory [state]
  (let [screen           (state :screen)
        player-name      (get-in state [:world :player :name])
        selected-hotkeys (get-in state [:world :selected-hotkeys])
        start-inventory  (sg/start-inventory)]
    (clear (state :screen))
    (put-string screen 20 5 "Pick up to three things to take:")
    (render-list screen 20 7 60 (count start-inventory)
                                (map (fn [item] {:s (format "%c%c%s" (get item :hotkey)
                                                                       (if (contains? selected-hotkeys (get item :hotkey))
                                                                         \+
                                                                         \-)
                                                                       (get item :name))
                                                    :fg :white
                                                    :bg :black
                                                    :style #{}})
                                        start-inventory))
    (refresh screen)))


(defn render-start-text [state]
  (let [screen     (state :screen)
        start-text (sg/start-text)]
    (clear (state :screen))
    (doall (map-indexed
      (fn [idx line] (put-string screen 5 (+ idx 5) line))
      (clojure.string/split-lines start-text)))
    (refresh screen)))

(defn render-game-over
  "Render the game over screen."
  [state]
  (let [points 0
        player-name    (get-in state [:world :player :name])
        death-madlib   (gen-death-madlib state)
        hp             (get-in state [:world :player :hp])
        hunger         (get-in state [:world :player :hunger])
        max-hunger     (get-in state [:world :player :max-hunger])
        thirst         (get-in state [:world :player :thirst])
        max-thirst     (get-in state [:world :player :max-thirst])
        will-to-live   (get-in state [:world :player :will-to-live])
        cause-of-death (cond
                         (<= hp 0)             "massive injuries"
                         (> hunger max-hunger) "literall starving to death"
                         (> thirst max-thirst) "not drinking enough water"
                         (<= will-to-live)     "just giving up on life"
                         :else                 "mysterious causes")]
    (clear (state :screen))
    ;; Title
    (put-string (state :screen) 10 1 (format "%s: %s, died from %s." player-name death-madlib cause-of-death))
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

(defn render-full-log
  "Render the log as a full screen."
  [state]
  (let [log (get-in state [:world :log])]
    (clear (state :screen))
    (doall (map-indexed (fn [idx message]
                          (put-string (state :screen) 0 idx (get message :text) (get message :color) :black))
                        log))
    (refresh (state :screen))))

(defn render
  "Pick between the normal render function and the
   game over render function based on the dead state
   of the player."
  [state]
  (info "render current-state" (current-state state))
  (cond
    (= (current-state state) :start)
      (render-start state)
    (= (current-state state) :start-inventory)
      (render-start-inventory state)
    (= (current-state state) :start-text)
      (render-start-text state)
    ;; Is player dead?
    (= (current-state state) :dead)
      ;; Render game over
      (render-game-over state)
    (= (get-in state [:world :current-state]) :help)
      (render-help state)
    (= (get-in state [:world :current-state]) :log)
      (render-full-log state)
    :else (render-map state)))


