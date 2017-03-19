;; Functions for rendering state to screen
(ns robinson.render
  (:require 
            [taoensso.timbre :as log]
            [robinson.random :as rr]
            [robinson.math :as rmath]
            [robinson.color :as rcolor]
            [robinson.renderutil :as rutil]
            [robinson.image :as rimage]
            [robinson.scores :as rs]
            [robinson.startgame :as sg]
            [robinson.popover :as rpop]
            [robinson.itemgen :as ig]
            [robinson.common :as rc :refer [farther-than?
                                            wrap-line
                                            fill-missing
                                            xy->pos]]
                                            
            [robinson.world :as rw :refer [current-state
                                           get-time
                                           get-cell
                                           player-cellxy
                                           distance-from-player
                                           inventory-and-player-cell-items]]
            [robinson.viewport :as rv :refer [cells-in-viewport]]
            [robinson.player :as rp :refer [player-xy
                                            player-wounded?
                                            player-poisoned?
                                            player-infected?]]
            [robinson.describe :as rdesc]
            [robinson.endgame :as rendgame :refer [gen-end-madlib]]
            [robinson.crafting :as rcrafting :refer [get-recipes]]
            [robinson.itemgen :refer [can-be-wielded?
                                      can-be-wielded-for-ranged-combat?
                                      id->name]]
            [robinson.lineofsight :as rlos :refer [visible?
                                                   cell-blocking?]]
            [robinson.dialog :as rdiag :refer [get-valid-input
                                               fsm-current-state]]
            [robinson.npc :as rnpc :refer [talking-npcs
                                           npcs-in-viewport]]
            [tinter.core :as tinter]
            [rockpick.core :as rockpick]
            [puget.printer :as pprinter]
            clojure.set
            [clojure.test :as t :refer [is]]
            [clojure.xml :as xml]
            [clojure.zip :as zip]
            #?(:clj
               [clojure.pprint :as pprint]
               clojure.string
               :cljs
               [cljs.pprint :as pprint]
               [goog.string :as gstring]
               [goog.string.format])))

#?(:clj
(set! *warn-on-reflection* true))

;;;
;;; Render to a datastructure
;;; {
;;;   :layers {
;;;     :map      [...characters]
;;;     :features [...characters]
;;;     :fx       [...characters]
;;;     :ui       [...characters]}
;;;   :fov #{[x y] [x y]...} Fov in screen space
;;;   :lantern [r g b]
;;;   :night   [r g b]
;;;   :events (lazy-seq [dt {:layer-id :layer :characters [...characters]})
;;; }
;;;
;;; The datastructure is easy to feed into a terminal and animate      

(defn format [s & args]
  #?(:clj
     (apply clojure.core/format s args)
     :cljs
     (apply gstring/format s args)))

(defn character? [c]
  (and (is (char? (get c :c)))
       (is (integer? (get c :x)))
       (is (integer? (get c :y)))
       (is (vector? (get c :fg [])))
       (is (vector? (get c :bg [])))))
(defn characters? [xs]
  (and (is (sequential? xs))
       (is (every? character? xs))))

(defn renderstate? [x]
  (is (every? characters? (vals (get x :layers {})))))

;;; {
;;;   :layers {
;;;     :map      [...characters]
;;;     :features [...characters]
;;;     :fx       [...characters]
;;;     :ui       [...characters]}
;;;   :fov #{[x y] [x y]...} Fov in screen space
;;;   :lantern [r g b]
;;;   :night   [r g b]
;;;   :events (lazy-seq [dt {:layer-id :layer :characters [...characters]})
;;; }

(defn log-render [rstate state path file-name]
  #_(letfn [(to-layer [characters] (if (empty? characters)
                                   [[]]
                                   (let [[max-x max-y] ((juxt :x :y) (sort-by (juxt :y :x) characters))
                                          empty-character {:ch (char 0)
                                                           :fg {:r 0 :g 0 :b 0}
                                                           :bg {:r 0 :g 0 :b 0}}
                                          empty-grid   (vec (repeat 24 (vec (repeat 80 empty-character))))]
                                     (->> characters
                                          (map (fn [character]
                                                 (-> character
                                                   (clojure.set/rename-keys {:c :ch})
                                                   (update :fg (fn [[r g b]] {:r r :g g :b b}))
                                                   (update :bg (fn [[r g b]] {:r r :g g :b b})))))
                                          (filter (fn [{:keys [x y]}]
                                                    (and (< 0 x 80)
                                                         (< 0 y 23))))
                                          (reduce (fn [grid character]
                                                    (assoc-in grid [(get character :y) (get character :x)] character))
                                                  empty-grid)))))]
    (let [layers (map to-layer ((juxt :map :features :fx :ui) (get rstate :layers)))
          location (str path "/" (rw/get-time state) "-" file-name)]
      #_(doseq [layer layers]
        (println "layer" layer))
      (rockpick/write-xp (clojure.java.io/output-stream location) layers)))
  rstate)

(def cell-type-palette
  {:fire                   [:red :orange]
   :water                  [:blue :dark-blue]
   :surf                   [:white :sea-foam :blue-green]
   :shallow-water          [:white :sea-foam :blue-green]
   :swamp                  [:white :sea-foam :blue-green]
   :lava                   [:red :orange :yellow]
   :bamboo-water-collector [:blue :light-blue :dark-blue]
   :solar-still            [:blue :light-blue :dark-blue]
   :freshwater-hole        [:blue :light-blue :dark-blue]
   :saltwater-hole         [:blue :light-blue :dark-blue]})

(defn cell-type->color
  [cell-type]
  (rand-nth (get cell-type-palette cell-type)))

(defn cell-type->rbg
  [cell-type]
  (-> cell-type
    cell-type->color
    rcolor/color->rgb))

(defn has-palette?
  [cell-type]
  (contains? #{:fire :water 
               :surf :shallow-water 
               :swamp :lava 
               :bamboo-water-collector :solar-still 
               :freshwater-hole :saltwater-hole} cell-type))

(defn move-cursor
  [rstate x y]
  (log/info "moving cursor to" x y)
  ;; TODO implement
  rstate)

(defn fill-put-string-color-style-defaults
  ([string]
    (fill-put-string-color-style-defaults string :white :black #{}))
  ([string fg]
    (fill-put-string-color-style-defaults string fg :black #{}))
  ([string fg bg]
    (fill-put-string-color-style-defaults string fg bg #{}))
  ([string fg bg styles]
   {:pre [(clojure.set/superset? #{:underline :bold} styles)]}
   (let [new-fg (rcolor/color->rgb (if (has-palette? fg)
                                     (cell-type->color fg)
                                     fg))
         bg     (rcolor/color->rgb bg)]
     (if (has-palette? fg)
       [string new-fg bg styles {:palette fg}]
       [string new-fg bg styles {}]))))

(defn zip-str [s]
  (zip/xml-zip 
      (xml/parse (java.io.ByteArrayInputStream. (.getBytes s)))))

(defn str->chars
  ([s]
   (str->chars s :white :black))
  ([s fg bg]
   (str->chars s fg bg #{}))
  ([s fg bg style]
   (map (fn [c] {:c c :fg fg :bg bg :style style})
        s)))

(defn markup->chars
  ([x y s]
   (markup->chars x y s :white :black))
  ([x y s fg bg]
   (markup->chars x y s fg bg #{}))
  ([x y s fg bg style]
   (if (re-find #"^\s*$" s)
     (map (fn [idx] {:c     \space
                     :x     (+ x idx)
                     :y     y
                     :fg    (rcolor/color->rgb fg)
                     :bg    (rcolor/color->rgb bg)
                     :style style})
          (range (count s)))
     (let [body    (str "<body>" s "</body>")
           z       (zip-str body)
           content (-> z zip/node :content)
           characters
       (map-indexed (fn [idx c] (assoc c :c     (get c :c)
                                         :x     (+ x idx)
                                         :y     y
                                         :fg    (rcolor/color->rgb (get c :fg))
                                         :bg    (rcolor/color->rgb (get c :bg))
                                         :style style))
                    (reduce (fn [characters v]
                              (cond
                                (string? v)
                                  (concat characters
                                          (str->chars v fg bg style))
                                (map? v)
                                  (concat characters
                                          (str->chars (-> v :content first)
                                                      (if (= (get v :tag) :color)
                                                        (keyword (get-in v [:attrs :fg] fg))
                                                        fg)
                                                      (if (= (get v :tag) :color)
                                                        (keyword (get-in v [:attrs :bg] bg))
                                                        bg)))))
                            []
                            content))]
      characters))))

(defn markup->length
  [s]
  (count (markup->chars 0 0 s)))

(defn put-chars
  [rstate layer-id characters]
  {:pre [characters?
         (is (get-in rstate [:layers layer-id]) (format (str [:layers layer-id]) "not in" (str rstate)))]}
  (rc/concat-in rstate
                [:layers layer-id]
                (filter (fn [character]
                          (and (< -1 (get character :x) 80)
                               (< -1 (get character :y) 23)))
                          characters)))
      
(defn put-string
  ([rstate layer-id x y string]
     (put-string rstate layer-id (int (rmath/ceil x)) (int (rmath/ceil y)) string :white :black #{}))
  ([rstate layer-id x y string fg bg]
     (put-string rstate layer-id (int (rmath/ceil x)) (int (rmath/ceil y)) string fg bg #{}))
  ([rstate layer-id x y string fg bg styles]
     (put-string rstate layer-id (int (rmath/ceil x)) (int (rmath/ceil y)) string fg bg #{} {}))
  ([rstate layer-id x y string fg bg styles mask-opts]
   {:pre [(clojure.set/superset? #{:underline :bold} styles)
          (integer? x)
          (integer? y)
          (string? string)]}
   (let [fg   (rcolor/color->rgb fg)
         bg   (rcolor/color->rgb bg)
         {:keys [mask unmask] :or {mask #{} unmask #{}}} mask-opts
         characters (map-indexed (fn [i c] {:c  c
                                            :fg fg
                                            :bg bg
                                            :x  (+ x i)
                                            :y  y
                                            :opts {}})
                                 string)]
     (put-chars rstate layer-id characters))))
      
(defn set-bg!
  [rstate layer-id x y bg]
  ;; TODO Implement
  nil)

(defn class->rgb
  "Convert a class to a color characters of that type should be drawn."
  [pc-class]
  ;(log/debug "class->color" pc-class)
  (rcolor/color->rgb
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
  (contains? #{:inventory :describe-inventory :pickup-selection :drop :eat} (get-in state [:world :current-state])))

(defn markup-length [s]
 (let [body    (str "<body>" s "</body>")
       z       (zip-str body)
       content (-> z zip/node :content)]
   (reduce (fn [n v]
             (cond
               (string? v)
                 (+ n
                    (count v))
               (map? v)
                 (+ n
                    (-> v :content first count))))
           0
           content)))

(defn center-text [s width]
  (let [n-ws-left (int (/ (- width (count s)) 2))
        n-ws-right (int (+ 0.5 (/ (- width (count s)) 2)))]
   (apply str (concat (repeat n-ws-left " ") s (repeat n-ws-right " ")))))

(defn left-justify-text [s width]
  (let [n-ws (- width (count s))]
   (apply str (concat s (repeat n-ws " ")))))

(defn center-markup [s width]
  (try
    (let [n-ws-left (int (/ (- width (markup-length s)) 2))
          n-ws-right (int (+ 0.5 (/ (- width (markup-length s)) 2)))]
      (apply str (concat (repeat n-ws-left " ") s (repeat n-ws-right " "))))
    (catch Exception e
      (log/error "Error in markup" s e)
      "")))

(defn left-justify-markup [s width]
  (try
    (let [n-ws (- width (markup-length s))]
      (apply str (concat s (repeat n-ws " "))))
    (catch Exception e
      (println "Error in markup" s e)
      "")))

(defn render-line
  ([rstate layer-id x y width line fg bg]
    (render-line rstate layer-id x y width line fg bg #{}))
  ([rstate layer-id x y width line fg bg style]
  (let [{:keys [underline center invert]}
               (merge {:underline false :center false :invert false}
                      (into {} (map (fn [k] [k true]) style)))
        width    (if (= width :auto)
                   (count line)
                   width)
        ;; pad to width
        s        (if center
                   ;; center justify
                   (center-markup line width)
                   ;; left justify
                   (left-justify-markup line width))
        [fg bg]  (if invert
                   [bg fg]
                   [fg bg])
        style    (if underline
                   #{:underline}
                   #{})]
    (let [characters (markup->chars (int (rmath/ceil x)) (int (rmath/ceil y)) s fg bg style)]
      (put-chars rstate layer-id characters)))))

(def single-border
  {:horizontal   \u2500
   :vertical     \u2502
   :top-left     \u250C
   :top-right    \u2510
   :bottom-left  \u2514
   :bottom-right \u2518})

(def double-border
  {:horizontal   \u2550
   :vertical     \u2551
   :top-left     \u2554
   :top-right    \u2557
   :bottom-left  \u255A
   :bottom-right \u255D})

(defn render-rect-border
  [rstate x y width height fg bg characters]
  {:pre [(integer? x)
         (integer? y)]}
  (let [{:keys [horizontal
                vertical
                top-left
                top-right
                bottom-left
                bottom-right]} characters
        fg (rcolor/color->rgb fg)
        bg (rcolor/color->rgb bg)]
    ;; render top and bottom
    (put-chars rstate :ui
      (mapcat identity
          (concat
            (for [dx (range (dec width))]
              [{:x (+ x dx 1) :y y :c horizontal :fg fg :bg bg}
               {:x (+ x dx 1) :y (+ y height) :c horizontal :fg fg :bg bg}])
            ;; render left and right
            (for [dy (range (dec height))]
              [{:x x :y (+ y dy 1) :c vertical :fg fg :bg bg}
               {:x (+ x width) :y (+ y dy 1) :c vertical :fg fg :bg bg}])
            ;; render tl, tr, bl, br
            [[{:x x :y y :c top-left :fg fg :bg bg}
              {:x (+ x width) :y y :c top-right :fg fg :bg bg}
              {:x x :y (+ y height) :c bottom-left :fg fg :bg bg}
              {:x (+ x width) :y (+ y height) :c bottom-right :fg fg :bg bg}]])))))

(defn render-rect-single-border
  [rstate x y width height fg bg]
  (render-rect-border rstate x y width height fg bg single-border))

(defn render-rect-double-border
  [rstate x y width height fg bg]
  (render-rect-border rstate x y width height fg bg double-border))

(defn render-vertical-border
  [rstate x y height fg bg]
  (put-chars rstate :ui
    (for [dy (range (dec height))]
      {:x x :y (+ y dy) :c \u2502 :fg fg :bg bg})))

(defn render-list
  "Render a sequence of lines padding to height if necessary.
   Lines are maps containing the keys `:s :fg :bg :style`."
  [rstate layer-id x y width height items]
  {:pre [(not (nil? rstate))]}
  (reduce (fn [rstate i] [i (range height)]
            (if (< i (count items))
              (let [item (nth items i)]
                (render-line rstate layer-id x (+ y i) width (get item :s) (get item :fg) (get item :bg)))
              (render-line rstate layer-id x (+ y i) (dec width) " " :white :white)))
    rstate
    (range height)))

(defn wrap-chars [x y width height characters fg bg style]
  (let [words  (take-nth 2 (partition-by (fn [ch] (= " " (get ch :c))) characters))
        ox        x
        oy        y
        line      (atom y)
        nchars (atom 0)]
    (loop [characters []
           words      words]
      (if-not (seq words)
        ;; no more words, pad to width and height, and return characters
        (do
          (println "characters" (count characters) "area" (* width height))
          (concat characters
                  (for [idx    (range (dec (count characters)) (* width height))
                        :let [x (mod idx width)
                              y (int (/ (- idx x) width))]]
                    {:c " "
                     :x (+ x ox)
                     :y (+ y oy)
                     :fg (rcolor/color->rgb fg)
                     :bg (rcolor/color->rgb bg)})))
        (let [[word & words] words
                             ;; would putting this word exceed the width?
              characters     (if (> (+ @nchars (count word) 1) width)
                               ;; break to next line
                               ;; pad spaces up to width
                               ;; deref y into y-val otherwise the for evaluation will be done
                               ;; lazily and after the swap! has occurred.
                               (let [y-val      @line
                                     nchars-val @nchars
                                     characters (vec (concat characters
                                                        (for [x (range (+ x nchars-val -1)
                                                                       (+ x width))]
                                                          (do
                                                          (println "padding @" x y-val)
                                                          {:c " "
                                                           :x x
                                                           :y y-val
                                                           :fg (rcolor/color->rgb fg)
                                                           :bg (rcolor/color->rgb bg)}))))]
                                 (println "line-break padding from" (+ x nchars-val -1) "to" (+ x width))
                                 (swap! line inc)
                                 (reset! nchars 0)
                                 characters)
                               characters)
              ;; reposition the characters in the word
              word           (map-indexed
                               (fn [idx ch]
                                 (assoc ch
                                        :x (+ idx @nchars x)
                                        :y @line))
                               word)
              characters     (concat characters
                                     word
                                     [{:c " "
                                       :x (+ @nchars (count word) x)
                                       :y @line
                                       :fg (rcolor/color->rgb fg)
                                       :bg (rcolor/color->rgb bg)
                                       :style style}])
             ;; update counters
             _               (swap! nchars (partial + (count word) 1))]
          (println (format "word %s" (mapv (fn [ch] (select-keys ch #{:c :x :y})) word)))
          (recur characters
                 words))))))
           
(defn render-text
  [rstate position title text]
  (let [x        (case position
                   :left 0
                   :right 50)
        y        0
        width    30
        height   23
        lines    (rc/wrap-line 28 text)
        characters (markup->chars x (inc y) text :black :white #{})
        characters (wrap-chars x (inc y) width height characters :black :white #{})]
     (put-chars rstate (wrap-chars x y  width 1 (markup->chars x y title :black :white #{}) :black :white #{}))
     (put-chars rstate characters)))
     ;;(render-list rstate x y width height items)))

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
  ([rstate title selected-hotkeys items]
   (render-multi-select rstate title selected-hotkeys items 40 0 40 22))
  ([rstate title selected-hotkeys items x y width height]
   (render-multi-select rstate title selected-hotkeys items x y width height #{}))
  ([rstate title selected-hotkeys items x y width height style]
   ;; items is list of {:s "string" :fg :black :bg :white}
   (let [{:keys [use-applicable
                 center
                 border
                 center-title]} (merge
                                  {:use-applicable false :border false :center false :center-title false}
                                  (map (fn [k] [k true]) style))
         items    (map (fn [item] {:s (format #?(:clj  "<color fg=\"highlight\">%c</color>%c%s%s %s %s"
                                                 :cljs "<color fg=\"highlight\">%s</color>%s%s%s %s %s")
                                              (or (item :hotkey)
                                                  \ )
                                              (if (contains? selected-hotkeys (item :hotkey))
                                                \+
                                                \-)
                                              (if (contains? item :count)
                                                (format "%dx " (int (get item :count)))
                                                "")
                                              (get item :name)
                                              (if (contains? item :utility)
                                                (format "(%d%%)" (int (get item :utility)))
                                                "")
                                              (cond
                                                (contains? item :wielded)
                                                  "(wielded)"
                                                (contains? item :wielded-ranged)
                                                  "(wielded ranged)"
                                                (contains? item :worn)
                                                  "(worn)"
                                                :else
                                                  ""))
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
                    (center-text title width)
                    (when title
                      (format "  %s" title)))
         items    (if title
                     (concat [{:s title :fg :black :bg :white :style #{:underline :bold}}]
                              items)
                     items)]
     (render-list rstate :ui x y width height items))))

(defn render-atmo
  [rstate state x y]
  (let [atmo   (get-in state [:data :atmo])
        frames (count atmo)
        t      (mod (get-in state [:world :time]) frames)
        frame  (nth atmo t)
        indexed-colors (map vector (partition 3 frame) (range))
        black-rgb (rcolor/color->rgb :black)
        characters (mapcat identity
                     (map (fn [[column i]]
                            #_(log/info x i y column)
                            [{:x (+ x i) :y y :c \u2584 :fg (if (contains? #{0 6} i)
                                                                       black-rgb
                                                                       (nth column 0))
                                                        :bg black-rgb}
                             {:x (+ x i) :y (inc y) :c \u2584 :fg (nth column 2) :bg (nth column 1)}])
                          indexed-colors))]
    (println "atmo chars" (vec characters))
    (put-chars rstate :ui characters)))

(defn render-hud
  [rstate state]
  {:pre [(not (nil? rstate))
         (not (nil? state))]}
    (let [black-rgb   (rcolor/color->rgb :black)
          gray-rgb    (rcolor/color->rgb :gray)
          red-rgb     (rcolor/color->rgb :red)
          yellow-rgb  (rcolor/color->rgb :yellow)
          green-rgb   (rcolor/color->rgb :green)]
      (-> rstate
        ;; render atmo
        (render-atmo state 37 21)
        ;; render statuses
        (put-chars :ui [{:x 37 :y 23 :c \      :fg gray-rgb :bg gray-rgb}
                        {:x 38 :y 23 :c \u2665 :fg (if (player-wounded? state) red-rgb black-rgb) :bg gray-rgb}
                        {:x 39 :y 23 :c \      :fg gray-rgb :bg gray-rgb}
                        {:x 40 :y 23 :c \u2665 :fg (if (player-poisoned? state) green-rgb black-rgb) :bg gray-rgb}
                        {:x 41 :y 23 :c \      :fg gray-rgb :bg gray-rgb}
                        {:x 42 :y 23 :c \u2665 :fg (if (player-infected? state) yellow-rgb black-rgb) :bg gray-rgb}
                        {:x 43 :y 23 :c \      :fg gray-rgb :bg gray-rgb}])
        ((fn [rstate]
          (if (= (current-state state) :sleep)
            (put-string rstate 38 20 :ui (format "Zzz%s" (apply str (repeat (mod (get-time state) 3) "." ))) #{} {:mask #{:rain :transform}}))
            rstate))
        ((fn [rstate]
          ;; render will to live and hp
          (let [wtl        (get-in state [:world :player :will-to-live])
                max-wtl    (get-in state [:world :player :max-will-to-live])
                hp         (get-in state [:world :player :hp])
                max-hp     (get-in state [:world :player :max-hp])
                hunger     (get-in state [:world :player :hunger])
                max-hunger (get-in state [:world :player :max-hunger])
                thirst     (get-in state [:world :player :thirst])
                max-thirst (get-in state [:world :player :max-thirst])
                red-rgb    (rcolor/color->rgb :red)
                green-rgb  (rcolor/color->rgb :green)
                yellow-rgb (rcolor/color->rgb :yellow)
                blue-rgb   (rcolor/color->rgb :blue)
                black-rgb  (rcolor/color->rgb :black)]
            (put-chars rstate :ui
              (concat
                (for [x (range 37)]
                  {:x x :y 23 :c \u2584
                   :fg (if (> (/ (- 37 x) 37)
                              (/ wtl max-wtl))
                         black-rgb
                         green-rgb)
                   :bg (if (> (/ (- 37 x) 37)
                            (/ hp max-hp))
                         black-rgb
                         red-rgb)})
                (for [x (range (- 80 44))]
                  {:x (+ 44 x) :y 23 :c \u2584
                   :fg (if (> (/ x (- 80 44))
                          (/ hunger max-hunger))
                         black-rgb
                         yellow-rgb)
                   :bg (if (> (/ x (- 80 44))
                          (/ thirst max-thirst))
                         black-rgb
                         blue-rgb)})))))))))
      ;    (int (-> state :world :player :hp))
      ;    (-> state :world :player :max-hp)
      ;    (apply str (interpose " " (-> state :world :player :status)))

(defn render-crushing-wall
  [rstate trap]
  (let [ch (case (get trap :direction)
             :up    \╧
             :down  \╤
             :left  \╢
             :right \╟)]
    (put-chars rstate :features (for [[x y] (first (get trap :locations))]
                                  {:x x :y y :c ch :fg [90 90 90] :bg [0 0 0]}))))

(defn render-poisonous-gas
  [rstate state trap]
  (let [current-time (rw/get-time state)]
    ; FIXME
    (doseq [[x y] (keys (get trap :locations))]
      (when (= (get (rw/get-cell state x y) :discovered) current-time)
        (let [bg (rcolor/color->rgb (rand-nth [:beige :temple-beige :light-brown]))]
          ;; TODO implement
          #_(set-bg! screen :map x y bg))))))

(defn render-traps
  [rstate state]
  (if-let [traps (rw/current-place-traps state)]
    ;; FIXME
    (reduce (fn [rstate trap]
      (case (get trap :type)
        :crushing-wall
          (render-crushing-wall rstate trap)
        :poisonous-gas
          (render-poisonous-gas rstate state trap)
        rstate))
      rstate
      traps)
    rstate))

#?(:clj
(defn render-img
  "Render an image using block element U+2584."
  [^String path x y]
  {:post [characters?]}
  (let [data   (rimage/image-values path)
        width  (reduce max (map :x data))
        height (reduce max (map :y data))
        xy->rgb (into {} (map (fn [{:keys [x y fg]}] [[x y] fg]) data))]
        ;; take lines two at a time and use one for fg an the other for bg
    (for [py (filter even? (range height))
          px (range width)
          :let [color1 (xy->rgb [px py])
                color2 (xy->rgb [px (inc py)])]]
        {:c  \u2584
         :x  (+ x px)        
         :y  (int (+ y (/ py 2)))        
         :fg color2        
         :bg color1})))

:cljs
(defn render-img
  "Render an image using block element U+2584."
  [state path x y]
  ;TOOD: implement
  nil))

(defn translate-identified-items
  [state items]
  (let [identified (get-in state [:world :fruit :identified])
        poisonous  (get-in state [:world :fruit :poisonous])]
    (map (fn [item] (if (contains? identified (get item :id))
                      (if (contains? poisonous (get item :id))
                        (assoc item :name (format "%s (poisonous)" (get item :name))
                                    :name-plural (format "%s (poisonous)" (get item :name-plural)))
                        (assoc item :name (format "%s (safe)" (get item :name))
                                    :name-plural (format "%s (safe)" (get item :name-plural))))
                      item))
         items)))

(defn render-pick-up-selection
  "Render the pickup item menu if the world state is `:pickup-selection`."
  [rstate state]
  (let [direction        (get-in state [:world :pickup-direction])
        {x :x y :y}      (rw/player-adjacent-pos state direction)
        cell             (get-cell state x y)
        cell-items       (or (cell :items) [])
        hotkeys          (-> state :world :remaining-hotkeys)
        selected-hotkeys (-> state :world :selected-hotkeys)
        items            (fill-missing (fn missing [item] (not (contains? item :hotkey)))
                                       (fn apply-hotkey [item hotkey] (assoc item :hotkey hotkey))
                                       hotkeys
                                       cell-items)]
  (log/debug "x" x "y" y)
  (log/debug "cell" cell)
  (log/debug "cell-items" cell-items)
  (render-multi-select rstate "Pick up" selected-hotkeys (translate-identified-items state items))
  (put-chars rstate :ui (markup->chars 41 20 "<color fg=\"highlight\">space</color>-All" :black :white #{}))))

(defn render-inventory
  "Render the pickup item menu if the world state is `:inventory`."
  [rstate state]
  (render-multi-select rstate "Inventory" [] (translate-identified-items state (-> state :world :player :inventory))))

(defn render-abilities
  "Render the player abilities menu if the world state is `:abilities`."
  [rstate state]
  (let [abilities (rp/player-abilities state)
        height (if (seq abilities)
                 (+ 3 (* 3 (count abilities)))
                 4)]  
    (-> rstate
      (render-list :ui 17 4 43 height
        (if (seq abilities)
          (concat
            (mapcat
              (fn [ability]
                [{:s (format "<color fg=\"highlight\">%s</color> - %s" (get ability :hotkey) (get ability :name))
                  :fg :black
                  :bg :white
                  :style #{}}
                 {:s (format "    %s" (get ability :description)) :fg :black :bg :white :style #{}}
                 {:s "" :fg :black :bg :white :style #{}}])
              abilities)
            [{:s "" :fg :black :bg :white :style #{}}
             {:s "Select hotkey or press <color fg=\"highlight\">Esc</color> to exit." :fg :black :bg :white :style #{}}])
          [{:s "No abilities." :fg :black :bg :white :style #{}}
           {:s "" :fg :black :bg :white :style #{}}
           {:s "Press <color fg=\"highlight\">Esc</color> to exit." :fg :black :bg :white :style #{}}]))
          
      (render-rect-double-border 16 3 43 height :black :white)
      (put-string :ui 33 3 "Abilities" :black :white))))

(defn render-ability-choices 
  "Render the player ability choice menu if the world state is `:gain-level`."
  [rstate state]
  (let [abilities (get-in state [:world :ability-choices])
        height (+ 3 (* 3 (count abilities)))]
    (-> rstate
      (render-list :ui 17 4 43 height
        (concat
          (mapcat
            (fn [ability]
              [{:s (format "<color fg=\"highlight\">%s</color> - %s" (get ability :hotkey) (get ability :name))
                :fg :black
                :bg :white
                :style #{}}
               {:s (format "    %s" (get ability :description)) :fg :black :bg :white :style #{}}
               {:s "" :fg :black :bg :white :style #{}}])
            abilities)
          [{:s "" :fg :black :bg :white :style #{}}
           {:s "Select hotkey." :fg :black :bg :white :style #{}}]))
      (render-rect-double-border 16 3 43 height :black :white)
      (put-string :ui 29 3 "Choose New Ability" :black :white))))

(defn render-action-choices
  "Render the player action choices menu if the world state is `:action-select`."
  [rstate state]
  (let [abilities (get-in state [:world :action-select])
        height (if (seq abilities)
                 (+ 3 (* 3 (count abilities)))
                 4)]
    (-> rstate
      (render-list :ui 17 4 43 height
        (if (seq abilities)
          (concat
            (mapcat
              (fn [ability]
                [{:s (format "<color fg=\"highlight\">%s</color> - %s" (get ability :hotkey) (get ability :name))
                  :fg :black
                  :bg :white
                  :style #{}}
                 {:s "" :fg :black :bg :white :style #{}}])
              abilities)
            [{:s "" :fg :black :bg :white :style #{}}
             {:s "Select hotkey or press <color fg=\"highlight\">Esc</color> to exit." :fg :black :bg :white :style #{}}])
          [{:s "Nothing to do." :fg :black :bg :white :style #{}}
           {:s "" :fg :black :bg :white :style #{}}
           {:s "Press <color fg=\"highlight\">Esc</color> to exit." :fg :black :bg :white :style #{}}]))
          
      (render-rect-double-border 16 3 43 height :black :white)
      (put-string :ui 30 3 "Choose Action" :black :white))))


(defn render-player-stats
  "Render the player character stats  menu if the world state is `:player-stats`."
  [rstate state]
  (let [x             18
        y             5
        height        11
        player-name   (rp/get-player-attribute state :name)
        max-hp        (int (rp/player-max-hp state))
        level         (rp/player-level state)
        xp            (or (rp/xp-acc-for-next-level state) -9)
        xp-next-level (or (rp/xp-for-next-level state) -99)
        strength      (int (rp/get-player-attribute state :strength))
        dexterity     (rp/get-player-attribute state :dexterity)
        toughness     (rp/get-player-attribute state :toughness)]
    (-> rstate
      (render-list :ui (inc x) (inc y) 43 height
          [{:s (format "Name:      %s" player-name) :fg :black :bg :white :style #{}}
           {:s (format "Level:     %d (%d/%d)" level xp xp-next-level) :fg :black :bg :white :style #{}}
           {:s "" :fg :black :bg :white :style #{}}
           {:s (format "Max HP:    %d" max-hp ) :fg :black :bg :white :style #{}}
           {:s "" :fg :black :bg :white :style #{}}
           {:s (format "Strength:  %d" strength ) :fg :black :bg :white :style #{}}
           {:s (format "Dexterity: %d" dexterity ) :fg :black :bg :white :style #{}}
           {:s (format "Toughness: %d" toughness ) :fg :black :bg :white :style #{}}
           {:s "" :fg :black :bg :white :style #{}}
           {:s "Press <color fg=\"highlight\">Esc</color> to exit." :fg :black :bg :white :style #{}}])
      (render-rect-double-border x y 43 height :black :white)
      (put-string :ui (+ x 16) y "Player Info" :black :white))))

(defn render-describe
  "Render the describe info pane if the world state is `:describe`."
  [rstate state]
  (let [{cursor-x :x
         cursor-y :y} (get-in state [:world :cursor])
        [x y]         (rv/viewport-xy state)
        [w _]         (rv/viewport-wh state)
        _             (println "cursor-x" cursor-x "w" w)
        position      (if (< cursor-x (/ w 2))
                        :right
                        :left)
        description   (rdesc/describe-cell-at-xy state (+ x cursor-x) (+ y cursor-y))]
  (render-text rstate position "Look" (if (get-in state [:world :dev-mode])
                                                 (str description "[" (+ x cursor-x) " " (+ y cursor-y) "]"
                                                      (get-cell state (+ x cursor-x) (+ y cursor-y)))
                                                 description))))

(defn render-apply
  "Render the inventory menu with `Apply` as the title."
  [rstate state]
  (render-multi-select rstate "Apply Inventory" [] (translate-identified-items state (-> state :world :player :inventory))))

(defn render-apply-to
  "Render the inventory menu with `Apply To` as the title."
  [rstate state]
  (render-multi-select rstate "Apply To" [] (translate-identified-items state (-> state :world :player :inventory))))

(defn render-quaff-inventory
  "Render the inventory menu with `Quaff` as the title."
  [rstate state]
  (render-multi-select rstate "Quaff To" [] (translate-identified-items state (filter ig/is-quaffable?
                                                             (-> state :world :player :inventory)))))

(defn render-drop
  "Render the pickup item menu if the world state is `:pickup`."
  [rstate state]
  (render-multi-select rstate "Drop Inventory" [] (translate-identified-items state (-> state :world :player :inventory))))

(defn render-describe-inventory
  "Render the pickup item menu if the world state is `:pickup`."
  [rstate state]
  (render-multi-select rstate "Describe" [] (translate-identified-items state (-> state :world :player :inventory))))

(defn render-throw-inventory
  "Render the throw item menu if the world state is `:throw-inventory`."
  [rstate state]
  (render-multi-select rstate "Throw" [] (translate-identified-items state (-> state :world :player :inventory))))

(defn render-eat
  "Render the eat item menu if the world state is `:pickup`."
  [rstate state]
  (render-multi-select rstate
                       "Eat Inventory"
                       []
                       (translate-identified-items state
                         (filter #(contains? % :hunger)
                               (inventory-and-player-cell-items state)))))

(defn render-quests
  "Render the pickup item menu if the world state is `:pickup`."
  [rstate state]
  (render-multi-select rstate
                       "Quests"
                       []
                       (filter (fn [quest]
                                 (not (nil? (get-in state [:world  :quests (quest :id) :stage] nil))))
                               (:quests state))))

(defn render-quit?
  "Render the pickup item menu if the world state is `:pickup`."
  [rstate state]
  (put-string rstate :ui 1 0 "quit? [yn]"))

(defn render-dialog
  "Render the dialog menu if the world state is `:talking`."
  [rstate state]
  (when (= (get-in state [:world :current-state]) :talking)
    (let [npc           (first (talking-npcs state))
          _ (log/debug "world state" (get-in state [:world :current-state]))
          _ (log/debug "state :dialog" (state :dialog))
          _ (log/debug "npcid" (get npc :id))
          fsm           (get-in state [:dialog (get npc :id)])
          _ (log/debug "fsm" fsm)
          valid-input   (get-valid-input fsm)
          _ (log/debug "render: valid-input:" valid-input)
          _ (log/debug "render: current-state:" (fsm-current-state fsm))
          options       (take (count valid-input)
                              (map (fn [k v]
                                     {:hotkey k
                                      :name v})
                                   [\a \b \c \d \e \f]
                               valid-input))
          last-response ((or (last (get-in state [:world :dialog*log])) {:text ""}) :text)
          _ (log/debug "last-response" last-response)
          response-wrapped (wrap-line (- 30 17) last-response)
          _ (log/debug "response-wrapped" response-wrapped)]
      (put-string rstate :ui 0 16 (format "Talking to %-69s" (get npc :name)) :black :white #{:bold})
      (doall (map (fn [y] (put-string rstate :ui 12 y "                    " :black :white #{:bold}))
                  (range 17 (+ 17 6))))
      (doall (map-indexed (fn [idx line] (put-string rstate :ui 13 (+ 17 idx) line :black :white #{:bold}))
                          response-wrapped))
      (render-multi-select rstate "Respond:" [] options 32 17 68 5)
      (render-img state (get npc :image-path) 0 17))))

(defn render-shopping
  "Render the shopping menu if the world state is `:shopping`."
  [rstate state]
  (let [npc           (first (talking-npcs state))
        options       [{:hotkey \a
                        :name "Buy"}
                       {:hotkey \b
                        :name "Sell"}]
        last-response ((or (last (get-in state [:world :dialog-log])) {:text ""}) :text)
        response-wrapped (wrap-line (- 30 17) last-response)
        style {:fg :black :bg :white :styles #{:bold}}]
    (put-string rstate :ui 0 16 (format "Doing business with %-69s" (get npc :name)) :black :white #{:bold})
    (doall (map (fn [y] (put-string rstate :ui 12 y "                    " :black :white #{:bold}))
                (range 17 (+ 17 6))))
    (doall (map-indexed (fn [idx line] (put-string rstate :ui 13 (+ 17 idx) line :black :white #{:bold}))
                        response-wrapped))
    (render-multi-select rstate "Option:" [] options 32 17 68 5)
    (render-img state (get npc :image-path) 0 17)))

(defn render-buy
  "Render the dialog menu if the world state is `:buy`."
  [rstate state]
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
        _ (log/debug "last-response" last-response)
        response-wrapped (wrap-line (- 30 17) last-response)
        _ (log/debug "response-wrapped" response-wrapped)
        style {:fg :black :bg :white :styles #{:bold}}]
    (put-string rstate :ui 0 16 (format "Doing business with %-69s" (get npc :name)) :black :white #{:bold})
    (doall (map (fn [y] (put-string rstate :ui 12 y "                    " :black :white #{:bold}))
                (range 17 (+ 17 6))))
    (doall (map-indexed (fn [idx line] (put-string rstate :ui 13 (+ 17 idx) line :black :white #{:bold}))
                        response-wrapped))
    (render-multi-select rstate "Buy:" [] options 32 17 68 5)
    (render-img state (get npc :image-path) 0 17)))

(defn render-sell
  "Render the dialog menu if the world state is `:sell`."
  [rstate state]
  (let [npc           (first (talking-npcs state))
        buy-fn        (get-in state (get npc :buy-fn-path) (fn [_] nil))
        _ (log/debug "render-sell (npc :buy-fn-path)" (get npc :buy-fn-path))
        _ (log/debug "render-sell buy-fn" buy-fn)
        options       (filter #(not (nil? (buy-fn %)))
                               (get-in state [:world :player :inventory]))
        _ (log/debug "options" options)
        last-response ((or (last (get-in state [:world :dialog-log])) {:text ""}) :text)
        _ (log/debug "last-response" last-response)
        response-wrapped (wrap-line (- 30 17) last-response)
        _ (log/debug "response-wrapped" response-wrapped)
        style {:fg :black :bg :white :styles #{:bold}}]
    (put-string rstate :ui 0 16 (format "Doing business with %-69s" (get npc :name)) :black :white #{:bold})
    (doall (map (fn [y] (put-string rstate :ui 12 y "                    " :black :white #{:bold}))
                (range 17 (+ 17 6))))
    (doall (map-indexed (fn [idx line] (put-string rstate :ui 13 (+ 17 idx) line :black :white #{:bold}))
                        response-wrapped))
    (render-multi-select rstate "Sell:" [] options 32 17 68 5)
    (render-img state (get npc :image-path) 0 17)))

(defn render-craft
  "Render the craft menu if the world state is `:craft`."
  [rstate state]
  (-> rstate
    (render-multi-select nil [] [{:name "Weapons" :hotkey \w}
                                        {:name "Survival" :hotkey \s}
                                        {:name "Shelter" :hotkey \c}
                                        {:name "Transportation" :hotkey \t}]
                                        30 6 20 5)
    (render-rect-single-border 29 5 20 5 :black :white)
    (put-string :ui 37 5 "Craft" :black :white)))

(defn render-craft-submenu
  "Render the craft submenu"
  [rstate state recipe-type]
  (let [selected-recipe-path (get-in state [:world :craft-recipe-path])
        hotkey               (when selected-recipe-path
                               (last selected-recipe-path))
        recipes              (get (get-recipes state) recipe-type)]
  (log/info "recipe-type" recipe-type)
  (log/info "recipes" (get-recipes state))
  (log/info "selected recipes" recipes)
  ;; render recipes
  (render-list rstate :ui 11 6 29 15
    (concat
      [{:s (name recipe-type) :fg :black :bg :white :style #{:underline}}]
       (map (fn [recipe]
              {:s (format #?(:clj  "%c-%s"
                             :cljs "%s-%s")
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
    (let [matching-recipes   (filter (fn [recipe] (= (get recipe :hotkey) hotkey))
                                     recipes)
          recipe             (get (first matching-recipes) :recipe)
          exhaust            (get recipe :exhaust [])
          have               (get recipe :have-or [])
          inventory-id-freqs (rp/inventory-id-freqs state)]
      (log/info "exhaust" exhaust "have" have)
      (render-list rstate :ui 41 6 29 15
      (concat
        [{:s "" :fg :black :bg :white :style #{}}
         {:s "Consumes" :fg :black :bg :white :style #{}}]
        (if (empty? exhaust)
          [{:s "N/A" :fg :black :bg :white :style #{}}]
          (let [idx-ids (mapcat (fn [ids]
                           (map-indexed vector ids))
                         (partition-by identity (sort exhaust)))]
            (println "idx-ids" idx-ids)
            (reduce (fn [lines [idx id]]
                      (conj lines
                            (if (< idx (get inventory-id-freqs id 0))
                              {:s (id->name id) :fg :black :bg :white :style #{}}
                              {:s (id->name id) :fg :gray :bg :white :style #{}})))
                   []
                   idx-ids)))
        [{:s "" :fg :black :bg :white :style #{}}
         {:s "Required tools" :fg :black :bg :white :style #{}}]
        (if (empty? have)
          [{:s "N/A" :fg :black :bg :white :style #{}}]
          (map (fn [id] {:s (id->name id) :fg :black :bg :white :style #{}}) have)))))
    (render-list rstate :ui 41 6 29 15
        [{:s "Select a recipe" :fg :black :bg :white :style #{}}]))
  (render-rect-single-border rstate 10 5 60 15 :black :white)
  (render-vertical-border rstate 40 6 15 :black :white)
  (put-string rstate :ui 40 20 "\u2534" :black :white)
  (put-string rstate :ui 37 5 "Craft" :black :white)))
          
(defn render-craft-weapon
  "Render the craft weapon menu if the world state is `:craft-weapon`."
  [state]
  (render-craft-submenu state :weapons))

(defn render-craft-survival
  "Render the craft menu if the world state is `:craft-survival`."
  [state]
  (render-craft-submenu state :survival))

(defn render-craft-shelter
  "Render the craft menu if the world state is `:craft-shelter`."
  [state]
  (render-craft-submenu state :shelter))

(defn render-craft-transportation
  "Render the craft menu if the world state is `:craft-transportation`."
  [state]
  (render-craft-submenu state :transportation))

(defn render-wield
  "Render the wield item menu if the world state is `:wield`."
  [rstate state]
  (render-multi-select rstate "Wield" [] (filter can-be-wielded? (-> state :world :player :inventory))))

(defn render-wield-ranged
  "Render the wield ranged  item menu if the world state is `:wield-ranged`."
  [rstate state]
  (render-multi-select rstate "Wield Ranged" [] (filter can-be-wielded-for-ranged-combat? (-> state :world :player :inventory))))

(defn render-start-text [rstate state]
  {:pre [(not (nil? rstate))]}
  (let [start-text (sg/start-text state)
        width      (reduce max (map markup->length (clojure.string/split-lines start-text)))]
    (-> rstate
      (render-list :ui 16 4 width 6
        (concat
          [{:s "" :fg :black :bg :white :style #{}}]
          (map
            (fn [line] {:s line :fg :black :bg :white :style #{:center}})
            (remove empty? (clojure.string/split-lines start-text)))
          [{:s "" :fg :black :bg :white}
           {:s "  Press <color fg=\"highlight\">any key</color> to continue and <color fg=\"highlight\">?</color> to view help."
            :fg :black :bg :white}]))
      (render-rect-double-border 15 4 (inc width) 6 :black :white))))

(defn render-raw-popover [rstate state]
  (let [message    (rpop/get-popover-message state)
        lines      (clojure.string/split-lines message)
        width      (reduce max 27 (map markup->length lines))
        height     (count lines)
        [v-width
         v-height] (rv/viewport-wh state)
        popover-x  (int (- (/ v-width 2) (/ width 2)))
        popover-y  (int (- (/ v-height 2) (/ height 2)))]
    (render-list rstate :ui popover-x popover-y width (inc height)
      (concat
        [{:s "" :fg :black :bg :white :style #{}}]
        (map
          (fn [line] {:s line :fg :black :bg :white :style #{:center}})
          lines)))
    (render-rect-double-border rstate (dec popover-x) popover-y (inc width) (inc height) :black :white)))

(defn render-popover [rstate state]
  (let [message    (rpop/get-popover-message state)
        lines      (clojure.string/split-lines message)
        width      (reduce max 27 (map markup->length lines))
        height     (count lines)
        [v-width
         v-height] (rv/viewport-wh state)
        popover-x  (int (- (/ v-width 2) (/ width 2)))
        popover-y  (int (- (/ v-height 2) (/ height 2)))]
    (-> rstate
      (render-list :ui popover-x popover-y width (+ 4 height)
        (concat
          [{:s "" :fg :black :bg :white :style #{}}]
          (map
            (fn [line] {:s line :fg :black :bg :white :style #{:center}})
            (remove empty? lines))
          [{:s "" :fg :black :bg :white :style #{}}
           {:s "  Press <color fg=\"highlight\">space</color> to continue." :fg :black :bg :white :style #{:center}}]))
      (render-rect-double-border (dec popover-x) popover-y (inc width) (+ 4 height) :black :white))))


(defn render-dead-text [rstate state]
  (let [hp             (get-in state [:world :player :hp])
        hunger         (get-in state [:world :player :hunger])
        max-hunger     (get-in state [:world :player :max-hunger])
        thirst         (get-in state [:world :player :thirst])
        max-thirst     (get-in state [:world :player :max-thirst])
        will-to-live   (get-in state [:world :player :will-to-live])
        cause-of-death (or
                         (get-in state [:world :cause-of-death])
                         (format "%s"
                           (cond
                             (<= hp 0)             "massive injuries"
                             (> hunger max-hunger) "literally starving to death"
                             (> thirst max-thirst) "not drinking enough water"
                             (<= will-to-live 0)   "just giving up on life"
                             :else                 "mysterious causes")))
        width          (max 25 (+ 7 (markup->length cause-of-death)))]
    (-> rstate
      (render-list :ui 27 4 width 6
        (concat
          [{:s "" :fg :black :bg :white :style #{}}]
          (map
            (fn [line] {:s line :fg :black :bg :white :style #{:center}})
            ["You died."
             (format "From %s" cause-of-death)
             ""
             "Press <color fg=\"highlight\">space</color> to continue."])))
      (render-rect-double-border 26 4 width 6 :black :white))))

(defn render-rescued-text [rstate state]
  (let [rescue-mode (rendgame/rescue-mode state)]
    (-> rstate
      (render-list :ui 16 4 53 6
        (concat
          [{:s "" :fg :black :bg :white :style #{}}]
          (map
            (fn [line] {:s line :fg :black :bg :white :style #{:center}})
            ["Rescue!"
             (format "A passing %s spots you." rescue-mode)
             "Press <color fg=\"highlight\">space</color> to continue."])))
      (render-rect-double-border 16 4 53 6 :black :white))))


(defn render-harvest
  "Render the harvest prompt if the world state is `:harvest`."
  [rstate state]
  (put-string rstate :ui 0 0 "Pick a direction to harvest."))

(defn npc->char-fg-bg [current-time d targeted? npc]
  (rcolor/target-tint-npc
    (rcolor/night-tint-npc
      (rcolor/color-bloodied-char 
        (< current-time (get npc :bloodied 0))
        (case (get npc :race)
          :rat             [\r]
          :spider          [\S]
          :scorpion        [\u03C2] ;;ς
          :snake           [\u00A7] ;;§
          :bat             [\B]
          :boar            [\b :brown :black]
          :gecko           [\g :green :black]
          :monkey          [\y :orange :black]
          :bird            [\a :red :black]
          :centipede       [\c :red :black]
          :turtle          [\t :green :black]
          :red-frog        [\u03B1 :red :black] ;;α
          :orange-frog     [\u03B1 :orange :black] ;;α
          :yellow-frog     [\u03B1 :yellow :black] ;;α
          :green-frog      [\u03B1 :green :black] ;;α
          :blue-frog       [\u03B1 :blue :black] ;;α
          :purple-frog     [\u03B1 :purple :black] ;;α
          :parrot          [\p :red :black]
          :shark           [\u039B] ;;Λ
          :fish            [\f]
          :octopus         [\# :orange :black]
          :sea-snake       [\u00A7]
          :clam            [\c]
          :urchin          [\u :purple :black]
          :squid           [\q :orange :black]
          :crocodile       [\l :green :black]
          :mosquito        [\m]
          :mongoose        [\r :brown :black]
          :tarantula       [\s :brown :black]
          :monitor-lizard  [\l :gray :black]
          :komodo-dragon   [\l :dark-green :black]
          :cobra           [\u00A7] ;;§
          :puffer-fish     [\f :yellow :black]
          :crab            [\c :orange :black]
          :hermit-crab     [\c :yellow :black]
          :electric-eel    [\e :brown :black]
          :jellyfish       [\j]
          ;; pirate ship npc
          :giant-rat       [\R]
          :eel             [\e]
          :giant-lizard    [\L]
          ;; ruined temple ncs
          :giant-centipedge [\C]
          :gorilla         [\M]
          :giant-snake     [\u00A7 :green :black] ;;§
          :human           [\@ (class->rgb (get npc :class)) :black]
          [\@])) d) targeted?))

(defn draw-npcs [rstate state player-pos current-time vx vy d]
  {:pre [(not (nil? rstate))]
   :post [(comp nil? not)]}
  ;; draw npcs
  (let [place-npcs (npcs-in-viewport state)
        ;_ (log/debug "place-npcs" place-npcs)
        get-cell (memoize (fn [x y] (get-cell state x y)))
        place-id (rw/current-place-id state)]
    (put-chars rstate :features
      (reduce (fn [characters npc]
                (let [x         (-> npc :pos :x)
                      y         (-> npc :pos :y)
                      targeted? (when (= (current-state state) :select-ranged-target)
                                  (let [target-ranged-index (get-in state [:world :target-ranged-index])
                                        target-ranged-pos-coll (get-in state [:world :target-ranged-pos-coll])
                                        target-pos             (nth target-ranged-pos-coll target-ranged-index)]
                                    (= target-pos (get npc :pos))))
                      visible (and (not (farther-than?
                                          player-pos
                                          {:x x :y y}
                                          8))
                                   (= (get (rw/get-cell state x y) :discovered) current-time))]
                  ;(log/debug "npc@" x y "visible?" visible)
                  (if visible
                    (let [[c fg bg] (npc->char-fg-bg current-time d targeted? npc)]
                      (cons {:c c :x (- x vx) :y (- y vy) :fg fg :bg bg} characters))
                    characters)))
              []
              place-npcs))))

(defn draw-log [rstate state]
  (if-not (nil? (get-in state [:world :ui-hint]))
    ;; ui-hint
    (put-chars rstate :ui (markup->chars 0 0 (get-in state [:world :ui-hint])))
    ;; draw log
    (let [current-time     (dec (rw/get-time state))
          log-idx          (get-in state [:world :log-idx] 0)
          num-logs         (count (filter #(= current-time (get % :time)) (get-in state [:world :log])))
          up-arrow-char    \u2191
          down-arrow-char  \u2193
          msg-above?       (< log-idx (dec num-logs))
          msg-below?       (pos? log-idx)
          up-arrow-color   (when msg-above?
                             (get (nth (reverse (get-in state [:world :log])) (inc log-idx)) :color))
          down-arrow-color (when msg-below?
                             (get (nth (reverse (get-in state [:world :log])) (dec log-idx)) :color))
          message          (if (zero? num-logs)
                             {:message "" :time 0 :color :black} 
                             (nth (reverse (get-in state [:world :log])) log-idx))
          darken-factor    (inc  (* (/ -1 5) (- current-time (message :time))))
          log-color        (rcolor/darken-rgb (rcolor/color->rgb (get message :color)) darken-factor)
          characters       (markup->chars 0 0 (format "%s%s %s" (if msg-above?
                                                                  (str "<color fg=\"highlight\">/</color>-<color fg=\"" (name up-arrow-color) "\">" up-arrow-char "</color>")
                                                                  "   ")
                                                                (if msg-below?
                                                                  (str "<color fg=\"highlight\">*</color>-<color fg=\"" (name down-arrow-color) "\">" down-arrow-char "</color>")
                                                                  "   ")
                                                                (if (get message :text)
                                                                  (str "<color fg=\"" (name (get message :color)) "\">" (get message :text) "</color>")
                                                                  "")))]
      (log/info "current-time" current-time)
      (log/info "num-log-msgs" num-logs)
      (log/info "message" message)
      (put-chars rstate :ui characters))))

(defn render-menus [rstate state]
  {:pre [(not (nil? rstate))]}
  (case (current-state state)
    :pickup-selection     (render-pick-up-selection rstate state)
    :inventory            (render-inventory rstate state)
    :abilities            (render-abilities rstate state)
    :player-stats         (render-player-stats rstate state)
    :gain-level           (render-ability-choices rstate state)
    :action-select        (render-action-choices rstate state)
    :describe             (render-describe rstate state)
    :apply                (render-apply rstate state)
    :apply-item-inventory
                          (render-apply-to rstate state)
    :quaff-inventory
                          (render-quaff-inventory rstate state)
    :drop                 (render-drop rstate state)
    :describe-inventory   (render-describe-inventory rstate state)
    :throw-inventory      (render-throw-inventory rstate state)
    :eat                  (render-eat rstate state)
    :quests               (render-quests rstate state)
    :craft                (render-craft rstate state)
    :craft-weapon         (render-craft-weapon rstate state)
    :craft-survival       (render-craft-survival rstate state)
    :craft-shelter        (render-craft-shelter rstate state)
    :craft-transportation (render-craft-transportation rstate state)
    :wield                (render-wield rstate state)
    :wield-ranged         (render-wield-ranged rstate state)
    :start-text           (render-start-text rstate state)
    :popover              (render-popover rstate state)
    :quaff-popover        (render-raw-popover rstate state)
    :dead                 (render-dead-text rstate state)
    :rescued              (render-rescued-text rstate state)
    :quit               (render-quit? rstate state)
    :harvest            (render-harvest rstate state)
    :dialog             (render-dialog rstate state)
    :shopping           (render-shopping rstate state)
    :buy                (render-buy rstate state)
    :sell               (render-sell rstate state)
    rstate))

(defn draw-player [rstate state current-time vx vy player player-x player-y]
  (-> rstate
    (put-string
      :features
      (- player-x vx)
      (- player-y vy)
      "@"
      (if (< current-time (get player :bloodied 0))
        :dark-red
        :white)
      (if (contains? (set (map :id (get (first (player-cellxy state)) :items))) :raft)
        :brown
        :black))
    ((fn [rstate]
      ;; if character is fishing, draw pole
      (condp = (current-state state)
        :fishing-left  (put-string rstate :ui (dec (-> state :world :player :pos :x))
                                          (-> state :world :player :pos :y)
                                          "\\"
                                          :white :black)
        :fishing-right (put-string rstate :ui (inc (-> state :world :player :pos :x))
                                          (-> state :world :player :pos :y)
                                          "/"
                                          :white :black)
        :fishing-up    (put-string rstate :ui (-> state :world :player :pos :x)
                                          (dec (-> state :world :player :pos :y))
                                          "/"
                                          :white :black)
        :fishing-down  (put-string rstate :ui (-> state :world :player :pos :x)
                                          (inc (-> state :world :player :pos :y))
                                          "\\"
                                          :white :black)
        rstate)))))

(defmulti cell->ch-fg-bg (fn [{:keys [type]}  _] type))
(defmethod cell->ch-fg-bg :floor [cell _] [\·])
(defmethod cell->ch-fg-bg :open-door [cell _] [\-  :brown  :black #{:bold}])
(defmethod cell->ch-fg-bg :close-door [cell _] [\+  :brown  :black #{:bold}])
(defmethod cell->ch-fg-bg :corridor [cell _] [\#] )
(defmethod cell->ch-fg-bg :down-stairs [cell _] [\>] )
(defmethod cell->ch-fg-bg :up-stairs [cell _] [\<] )
(defmethod cell->ch-fg-bg :fire [cell current-time]
  [\u2240 (if (= (cell :discovered) current-time)
            :fire
            :red) :black]) ;; ≀ 
(defmethod cell->ch-fg-bg :water [cell current-time]
  [\u2248 (if (= (cell :discovered) current-time)
            :water
            :blue) :black]) ;; ≈ 
(defmethod cell->ch-fg-bg :surf [cell current-time]
  [\~ (if (= (cell :discovered) current-time)
        :surf
        :light-blue) :black])
(defmethod cell->ch-fg-bg :shallow-water [cell current-time]
  [\~ (if (= (cell :discovered) current-time)
        :shallow-water
        :light-blue) :black])
(defmethod cell->ch-fg-bg :swamp [cell current-time]
  [\~ (if (= (cell :discovered) current-time)
        :swamp
        :light-blue) :black])
(defmethod cell->ch-fg-bg :lava [cell current-time]
  [\~ (if (= (cell :discovered) current-time)
       :lava
       :light-blue) :black])
(defmethod cell->ch-fg-bg :mountain [cell _] [\u2206 :gray :black]) ;; ∆
(defmethod cell->ch-fg-bg :sand [cell _] [\·  :beige      :black])
(defmethod cell->ch-fg-bg :dirt [cell _] [\·  :brown      :black])
(defmethod cell->ch-fg-bg :dune [cell _] [\u1d16  :light-brown :black]) ;; ᴖ
(defmethod cell->ch-fg-bg :rocky-shore [cell _] [\u1d16  :dark-gray  :black]) ;; ᴖ
(defmethod cell->ch-fg-bg :gravel [cell _] [\·  :gray       :black])
(defmethod cell->ch-fg-bg :short-grass [cell _] [\·  :green      :black])
(defmethod cell->ch-fg-bg :tall-grass [cell _] [\" :dark-green :black])
(defmethod cell->ch-fg-bg :tree [cell _] [\T  :dark-green :black])
(defmethod cell->ch-fg-bg :bamboo [cell _] [\u01c1 :light-green :black]) ;; ∥ 
(defmethod cell->ch-fg-bg :palisade [cell _] [\# :brown :black])
(defmethod cell->ch-fg-bg :ramada [cell _] [\# :beige :black])
(defmethod cell->ch-fg-bg :tarp-shelter [cell _] [\# :blue  :black])
(defmethod cell->ch-fg-bg :lean-to [cell _] [\# :light-green :black])
(defmethod cell->ch-fg-bg :campfire [cell _] [\^ :brown :black])
(defmethod cell->ch-fg-bg :bamboo-water-collector [cell _]
                 (if (< 10 (get cell :water 0))
                   [\O :bamboo-water-collector :black]
                   [\O]))
(defmethod cell->ch-fg-bg :solar-still [cell _]
                 (if (< 10 (get cell :water 0))
                   [\O :solar-still :black]
                   [\O]))
(defmethod cell->ch-fg-bg :palm-tree [cell _] [\7  :dark-green :black])
(defmethod cell->ch-fg-bg :fruit-tree [cell _] [\u2648  :light-green :black]) ;; ♈
(defmethod cell->ch-fg-bg :freshwater-hole [cell _] (if (< 10 (get cell :water 0))
                   [\~ :freshwater-hole :black]
                   [\O]))
(defmethod cell->ch-fg-bg :saltwater-hole [cell _] (if (< 10 (get cell :water 0))
                   [\~ :saltwater-hole :black]
                   [\O]))
(defmethod cell->ch-fg-bg :dry-hole [cell _] [\O])
;; pirate ship cell types
(defmethod cell->ch-fg-bg :bulkhead [cell _] [\◘ :brown :black])
(defmethod cell->ch-fg-bg :wheel [cell _] [\○ :dark-brown :black])
(defmethod cell->ch-fg-bg :bulkhead2 [cell _] [\◘ :brown :black])
(defmethod cell->ch-fg-bg :wooden-wall [cell _] [\# :ship-brown :black])
(defmethod cell->ch-fg-bg :railing [cell _] [\# :ship-brown :black])
(defmethod cell->ch-fg-bg :hammock-v [cell _] [\) :brown :black])
(defmethod cell->ch-fg-bg :hammock-h [cell _] [\- :brown :black])
(defmethod cell->ch-fg-bg :deck [cell _] [\· :dark-brown :black])
(defmethod cell->ch-fg-bg :canon-breach [cell _] [\║ :gray :dark-brown])
(defmethod cell->ch-fg-bg :tackle [cell _] [\º :brown :black])
(defmethod cell->ch-fg-bg :canon [cell _] [\║ :gray :black])
(defmethod cell->ch-fg-bg :grate [cell _] [\╬ :dark-beige :black])
(defmethod cell->ch-fg-bg :table [cell _] [\╤ :ship-light-brown :black])
(defmethod cell->ch-fg-bg :chair [cell _] [\╥ :ship-light-brown :black])
(defmethod cell->ch-fg-bg :mast [cell _] [\╨ :ship-light-brown :black])
(defmethod cell->ch-fg-bg :beam [cell _] [\═ :brown :black])
(defmethod cell->ch-fg-bg :canon-truck-1 [cell _] [\▄ :dark-brown :black])
(defmethod cell->ch-fg-bg :locker [cell _] [\▌ :brown :black])
(defmethod cell->ch-fg-bg :locker2 [cell _] [\▐ :brown :black])
(defmethod cell->ch-fg-bg :canon-truck-2 [cell _] [\▀ :dark-brown :black])
(defmethod cell->ch-fg-bg :ships-wheel [cell _] [\Φ :brown :black])
(defmethod cell->ch-fg-bg :ladder [cell _] [\≡ :dark-beige :black])
(defmethod cell->ch-fg-bg :porthole [cell _] [\° :brown :black])
(defmethod cell->ch-fg-bg :chest [cell _] [\■ :ship-dark-brown :black])
(defmethod cell->ch-fg-bg :artifact-chest [cell _] [\■ :dark-beige :black])
;; ruined temple cell types
(defmethod cell->ch-fg-bg :vertical-wall [cell _] [\║ :temple-beige :black])
(defmethod cell->ch-fg-bg :horizontal-wall [cell _] [\═ :temple-beige :black])
(defmethod cell->ch-fg-bg :vertical-wall-alt [cell _] [\° :white :black])
(defmethod cell->ch-fg-bg :horizontal-wall-alt [cell _] [\° :white :black])
(defmethod cell->ch-fg-bg :upper-left-1 [cell _] [\╔ :temple-beige :black])
(defmethod cell->ch-fg-bg :upper-right-1 [cell _] [\╗ :temple-beige :black])
(defmethod cell->ch-fg-bg :bottom-left-1 [cell _] [\╚ :temple-beige :black])
(defmethod cell->ch-fg-bg :bottom-right-1 [cell _] [\╝ :temple-beige :black])
(defmethod cell->ch-fg-bg :upper-left-2 [cell _] [\◙ :temple-beige :black])
(defmethod cell->ch-fg-bg :upper-right-2 [cell _] [\◙ :temple-beige :black])
(defmethod cell->ch-fg-bg :bottom-left-2 [cell _] [\◙ :temple-beige :black])
(defmethod cell->ch-fg-bg :bottom-right-2 [cell _] [\◙ :temple-beige :black])
(defmethod cell->ch-fg-bg :altar [cell _] [\┬ :white :black])
(defmethod cell->ch-fg-bg :vine [cell _] [\⌠ :moss-green :black])
(defmethod cell->ch-fg-bg :moss-corridor [cell _] [\# :moss-green :black])
(defmethod cell->ch-fg-bg :moss-vertical-wall [cell _] [\║ :moss-green :black])
(defmethod cell->ch-fg-bg :moss-horizontal-wall [cell _] [\═ :moss-green :black])
(defmethod cell->ch-fg-bg :moss-vertical-wall-alt [cell _] [\° :white :black])
(defmethod cell->ch-fg-bg :moss-horizontal-wall-alt [cell _] [\° :white :black])
(defmethod cell->ch-fg-bg :moss-upper-left-1 [cell _] [\╔ :moss-green :black])
(defmethod cell->ch-fg-bg :moss-upper-right-1 [cell _] [\╗ :moss-green :black])
(defmethod cell->ch-fg-bg :moss-bottom-left-1 [cell _] [\╚ :moss-green :black])
(defmethod cell->ch-fg-bg :moss-bottom-right-1 [cell _] [\╝ :moss-green :black])
(defmethod cell->ch-fg-bg :moss-upper-left-2 [cell _] [\◙ :moss-green :black])
(defmethod cell->ch-fg-bg :moss-upper-right-2 [cell _] [\◙ :moss-green :black])
(defmethod cell->ch-fg-bg :moss-bottom-left-2 [cell _] [\◙ :moss-green :black])
(defmethod cell->ch-fg-bg :moss-bottom-right-2 [cell _] [\◙ :moss-green :black])
(defmethod cell->ch-fg-bg :white-corridor [cell _] [\# :white :black])
(defmethod cell->ch-fg-bg :white-vertical-wall [cell _] [\║ :white :black])
(defmethod cell->ch-fg-bg :white-horizontal-wall [cell _] [\═ :white :black])
(defmethod cell->ch-fg-bg :white-vertical-wall-alt [cell _] [\° :white :black])
(defmethod cell->ch-fg-bg :white-horizontal-wall-alt [cell _] [\° :white :black])
(defmethod cell->ch-fg-bg :white-upper-left-1 [cell _] [\╔ :white :black])
(defmethod cell->ch-fg-bg :white-upper-right-1 [cell _] [\╗ :white :black])
(defmethod cell->ch-fg-bg :white-bottom-left-1 [cell _] [\╚ :white :black])
(defmethod cell->ch-fg-bg :white-bottom-right-1 [cell _] [\╝ :white :black])
(defmethod cell->ch-fg-bg :white-upper-left-2 [cell _] [\◙ :white :black])
(defmethod cell->ch-fg-bg :white-upper-right-2 [cell _] [\◙ :white :black])
(defmethod cell->ch-fg-bg :white-bottom-left-2 [cell _] [\◙ :white :black])
(defmethod cell->ch-fg-bg :white-bottom-right-2 [cell _] [\◙ :white :black])
(defmethod cell->ch-fg-bg :empty [cell _] [\space :black :black])
(defmethod cell->ch-fg-bg :crushing-wall-trigger [cell _]
                  (if (get cell :trap-found)
                    [\^]
                    [\·]))
(defmethod cell->ch-fg-bg :wall-darts-trigger [cell _]
                  (if (get cell :trap-found)
                    [\^]
                    [\·]))
(defmethod cell->ch-fg-bg :poisonous-gas-trigger [cell _]
                  (if (get cell :trap-found)
                    [\^]
                    [\·]))
(defmethod cell->ch-fg-bg :spike-pit [cell _]
                  (if (get cell :trap-found)
                    [\^]
                    [\·]))
(defmethod cell->ch-fg-bg :snakes-trigger [cell _]
                  (if (get cell :trap-found)
                    [\^]
                    [\·]))
(defmethod cell->ch-fg-bg :default [cell _]
  (log/info (format "unknown type: %s %s" (str (get cell :type)) (str cell)))
  [\?])

;; Add cells to rstate.palette-cells
(defn concat-palette-cells [rstate cells-vxy-wxy]
  (rc/concat-in
    rstate
    [:palette-cells]
    (filter (comp has-palette? :type)
      (map (fn [[cell vx vy _ _]]
             (assoc cell :x vx :y vy))
           cells-vxy-wxy))))

(defn cell->char [current-time sight-distance lantern-on [cell vx vy wx wy]]
   #_(when (= (get cell :discovered 0) current-time)
     (println "render-cell" (select-keys cell [:type :discovered]) vx vy wx wy))
     (let [cell-items (cell :items)
           ;_ (println "render-cell" (str cell) vx vy wx wy)
           out-char (apply fill-put-string-color-style-defaults
                      (rcolor/color-bloodied-char 
                        (< current-time (get cell :bloodied 0))
                        (if (and cell-items
                                 (seq cell-items)
                                 (= (cell :discovered) current-time))
                          (if (contains? #{:chest :artifact-chest} (get cell :type))
                            [\■ :dark-beige :black]
                            [(rutil/item->char (first cell-items))
                             (rutil/item->fg   (first cell-items))
                             :black])
                          (cell->ch-fg-bg cell current-time))))
           ;; shade character based on visibility, harvestableness, raft
           shaded-out-char (cond
                             (not= (cell :discovered) current-time)
                               (-> out-char
                                 (update-in [1] (fn [c] (rcolor/rgb->mono (rcolor/darken-rgb c 0.18))))
                                 (update-in [2] (fn [c] (rcolor/rgb->mono (rcolor/darken-rgb c 0.15)))))
                             (contains? cell :harvestable)
                               (let [[chr fg bg] out-char]
                                 [chr bg (rcolor/night-tint (rcolor/color->rgb fg) sight-distance)])
                             (contains? (set (map :id cell-items)) :raft)
                               (let [[chr fg bg] out-char]
                                 (log/info "raft-cell" out-char cell-items)
                                 (if (> (count cell-items) 1)
                                   [chr fg (rcolor/color->rgb :brown)]
                                   [\u01c1 (rcolor/color->rgb :black) (rcolor/color->rgb :brown)]))
                             :else
                               out-char)
           ;; add character opts to indicate distance from player
           #_#_shaded-out-char (if (and (= (get cell :discovered) current-time)
                                    (not (contains? #{:fire :lava} (get cell :type)))
                                    (get shaded-out-char 4))
                             (update shaded-out-char
                                     4 ;; opts index
                                     (fn [opts]
                                       (assoc opts
                                              :visible (= (cell :discovered) current-time)
                                              :lantern-flicker (and lantern-on (= (cell :discovered) current-time))
                                              :distance-from-player
                                                (distance-from-player state (xy->pos wx wy))
                                              :night-tint
                                                d)))
                             shaded-out-char)]
         {:x    vx
          :y    vy
          :c    (get shaded-out-char 0)
          :fg   (get shaded-out-char 1)
          :bg   (get shaded-out-char 2)}))

(defn draw-ranged-attack-line [rstate state player player-x player-y]
  ;; draw ranged-attack line
  (if (contains? #{:select-ranged-target :select-throw-target} (current-state state))
    (let [[target-sx
           target-sy] (case (current-state state)
                        :select-ranged-target
                          (let [target-ranged-index (get-in state [:world :target-ranged-index])
                                target-ranged-pos-coll (get-in state [:world :target-ranged-pos-coll])
                                target-pos             (nth target-ranged-pos-coll target-ranged-index)]
                            (log/debug "target-ranged-index" target-ranged-index)
                            (log/debug "target-ranged-pos-coll" (get-in state [:world :target-ranged-pos-coll]))
                            (log/debug "target-pos" target-pos)
                            (rv/world-xy->screen-xy state (rc/pos->xy target-pos)))
                        :select-throw-target
                          (rc/pos->xy (rv/get-cursor-pos state)))]
      (log/debug "target-sx" target-sx "target-y" target-sy)
      ; FIXME
      (doseq [[sx sy] (rlos/line-segment-fast-without-endpoints (rv/world-xy->screen-xy state [player-x player-y])
                                                                [target-sx target-sy])]
          (put-string rstate :ui sx sy "\u25CF" :green :black)))
    rstate))
      
(defn render-map
  "The big render function used during the normal game.
   This renders everything - the map, the menus, the log,
   the status bar. Everything."
  [rstate state]
  (let [{:keys [columns rows]}  (get state :screen)
        current-time            (get-in state [:world :time])
        {{player-x :x
          player-y :y
          :as player-pos} :pos
         :as player}            (rp/get-player state)
        [vx vy]                 (rv/viewport-xy state)
        sight-distance          (rlos/sight-distance state)
        cells                   (rv/cellsxy-in-viewport state)
        lantern-on              (when-let [lantern (rp/inventory-id->item state :lantern)]
                                  (get lantern state :off))
        ;_ (println cells)
        ;_ (log/info "cells" (str cells)
        ;; separate cells into those inside fov and those outside
        {fov-cells true
         non-fov-cells false} (group-by (fn [[cell _ _ _ _]]
                                          (= (get cell :discovered 0) current-time))
                                        cells)
         characters           (map (partial cell->char current-time sight-distance lantern-on) (concat fov-cells non-fov-cells))]
    ;; set rain mask to all true
    ; TODO: remove and use groups instead
    ;(ranimation/reset-rain-mask! screen true)
    ;; set palette
    ;(ranimation/set-palette! screen cell-type-palette)
    #_(log/info "putting chars" characters)
    (-> rstate
      (put-chars :map characters)
      (log-render state "debug" "0-map-chars.xp")
      (concat-palette-cells fov-cells)
    ;; draw character
    ;(log/debug (-> state :world :player))
      (draw-player state current-time vx vy player player-x player-y)
      (log-render state "debug" "1-draw-player.xp")
      (draw-ranged-attack-line state player player-x player-y)
      (log-render state "debug" "2-draw-ranged-attack-line.xp")
      (draw-npcs state player-pos current-time vx vy sight-distance)
      (log-render state "debug" "3-draw-npcs.xp")
      (render-hud state)
      (log-render state "debug" "4-render-hud.xp")
    #_(log/info "current-state" (current-state state))
    #_(println "ui-hint" (get-in state [:world :ui-hint]))
      (draw-log state)
      (log-render state "debug" "5-render-log.xp")
      (render-traps state)
      (log-render state "debug" "6-render-traps.xp")
      (render-menus state)
      (log-render state "debug" "7-render-menus.xp")
      ((fn [rstate]
         ;; draw cursor
         (if-let [cursor-pos (-> state :world :cursor)]
           (move-cursor rstate (cursor-pos :x) (cursor-pos :y))
           (move-cursor rstate -1 -1)))))))
 
(defn render-enter-name [rstate state]
  (let [player-name (get-in state [:world :player :name])]
    (-> rstate
      (put-string :ui 30 5 "Robinson")
      (put-string :ui 20 7 "Name:")
      (put-string :ui 25 7 (str player-name)))))

(defn render-start [rstate state]
  (let [player-name (get-in state [:world :player :name])]
    (-> rstate
      (put-chars :ui (render-img "images/robinson-mainmenu.jpg" 0 0))
      (put-chars :ui (markup->chars 30 20 "<color bg=\"background\">Press </color><color fg=\"highlight\" bg=\"background\">space</color><color bg=\"background\"> to play</color>"))
      (put-chars :ui (markup->chars 30 22 "<color fg=\"highlight\" bg=\"background\">c</color><color bg=\"background\">-configure </color>")))))

(defn render-configure [rstate state]
  (-> rstate
    (put-string :ui 34 5 "Configure")
    (put-chars :ui (markup->chars 34 7 "<color fg=\"highlight\" bg=\"background\">f</color><color bg=\"background\">-font </color>"))))

(defn render-configure-font [rstate state]
  (let [font (get-in state [:fonts (get state :new-font (get (rc/get-settings state) :font))])]
    (-> rstate
      (put-string :ui 31 5 "Configure Font")
      (put-string :ui 31 7 (format "Font: %s" (get font :name)))
      (put-chars :ui (markup->chars 31 12 "<color fg=\"highlight\" bg=\"background\">n</color><color bg=\"background\">-next font </color>"))
      (put-chars (markup->chars 31 13 "<color fg=\"highlight\" bg=\"background\">p</color><color bg=\"background\">-previous font </color>"))
      (put-chars (markup->chars 31 14 "<color fg=\"highlight\" bg=\"background\">s</color><color bg=\"background\">-save and apply</color>")))))

(defn render-start-inventory [rstate state]
  (let [player-name      (get-in state [:world :player :name])
        selected-hotkeys (get-in state [:world :selected-hotkeys])
        start-inventory  (sg/start-inventory)]
    
    (reduce (fn [rstate y]
              (let [item      (nth start-inventory y)
                    hotkey    (get item :hotkey)
                    item-name (get item :name)]
                (put-chars rstate :ui (markup->chars 20 (+ 7 y) (format #?(:clj  "<color fg=\"highlight\">%c</color>%c%s"
                                                                       :cljs "<color fg=\"highlight\">%s</color>%s%s")
                                                                    hotkey
                                                                    (if (contains? selected-hotkeys hotkey)
                                                                      \+
                                                                      \-)
                                                                    item-name)))))
            (put-string rstate :ui 20 5 "Choose up to three things to take with you:")
            (range (count start-inventory)))))

(def loading-index (atom 0))
(def loading-tidbits
  ["rat swarm"
   "poisonous frog"
   "coconut"
   "palm tree"
   "human blood"
   "monkey"
   "island"
   "terrain"
   "lava"
   "volcano"
   "abandoned hut"
   "lair"
   "pirate"
   "clam"
   "leaf"
   "ancient temple"
   "beaches"
   "crab legs. (8) check."
   "cannibal"
   "beeshive"
   "monkey ambush"
   "cave"
   "spider tunnel"
   "insane castaway"
   "watering hole"
   "other survivor"
   "goat"
   "hurricane"])
(def tidbit-freqs (atom (zipmap (range (count loading-tidbits)) (repeat 0))))

(defn render-loading [rstate state]
  (let [n (->> (sort-by val @tidbit-freqs)
               (partition-by val)
               first
               rand-nth
               first)]
    (swap! tidbit-freqs (fn [freqs] (update freqs n inc)))
    (-> rstate
      (put-string :ui 30 12 (format "Generating %s..." (nth loading-tidbits n)))
      (put-string :ui 40 18 (nth ["/" "-" "\\" "|"] (mod (swap! loading-index inc) 4))))))

(defn render-connection-failed [rstate state]
  (-> rstate
    (put-string 30 12 :ui "Connection failed")
    (put-chars :ui (markup->chars 30 22 "Play again? [<color fg=\"highlight\">y</color>/<color fg=\"highlight\">n</color>]"))))

(def connecting-index (atom 0))
(defn render-connecting [rstate state]
  (put-string rstate 30 12 :ui (format "Connecting%s" (apply str (repeat (mod (swap! connecting-index inc) 4) ".")))))

(defn render-game-over
  "Render the game over screen."
  [rstate state]
  (let [cur-state      (current-state state)
        points         (rs/state->points state)
        turns-survived  (get-time state)
        turns-per-day   (count (get-in state [:data :atmo]))
        days-survived   (int (/ turns-survived turns-per-day))
        player-name     (get-in state [:world :player :name])
        madlib          (gen-end-madlib state)]
    (case cur-state
      :game-over-dead
        (let [hp             (get-in state [:world :player :hp])
              hunger         (get-in state [:world :player :hunger])
              max-hunger     (get-in state [:world :player :max-hunger])
              thirst         (get-in state [:world :player :thirst])
              max-thirst     (get-in state [:world :player :max-thirst])
              will-to-live   (get-in state [:world :player :will-to-live])
              cause-of-death (or
                               (get-in state [:world :cause-of-death])
                               (cond
                                 (<= hp 0)             "massive injuries"
                                 (> hunger max-hunger) "literall starving to death"
                                 (> thirst max-thirst) "not drinking enough water"
                                 (<= will-to-live 0)   "just giving up on life"
                                 :else                 "mysterious causes"))]
          ;; Title
          (put-string rstate :ui 10 1 (format "%s: %s" player-name madlib))
          (put-string rstate :ui 10 3 (format "Survived for %d %s. (%d turns)" days-survived (if (> 1 days-survived) "days" "day") turns-survived))
          (put-string rstate :ui 10 4 (format "Died from %s" cause-of-death))
          (put-string rstate :ui 10 6 (format "Points: %s." points))
          (put-string rstate :ui 10 8 "Inventory:")
          (doall (map-indexed
            (fn [idx item] (put-string rstate :ui 20 (+ idx 8) (format "%s%s" (if (pos? (get item :count 0))
                                                                                     (format "%dx " (get item :count))
                                                                                     "")
                                                                                    (item :name))))
            (-> state :world :player :inventory)))
          (put-chars rstate :ui (markup->chars 10 22 "Play again? [<color fg=\"highlight\">y</color>/<color fg=\"highlight\">n</color>] <color fg=\"highlight\">space</color>-share and compare with other players")))
      :game-over-rescued
        (let [rescue-mode (rendgame/rescue-mode state)]
          ;; Title
          (put-string rstate :ui 10 1 (format "%s: %s." player-name madlib))
          (put-string rstate :ui 18 2 (format "Rescued by %s after surviving for %d days." rescue-mode days-survived))
          (put-string rstate :ui 10 3 (format "Points: %s." points))
          (put-string rstate :ui 10 4 "Inventory:")
          (doall (map-indexed
            (fn [idx item] (put-string rstate :ui 18 (+ idx 5) (item :name)))
            (-> state :world :player :inventory)))
          (put-string rstate :ui 10 22 "Play again? [yn]")))))

(defn cp437->unicode
  [c]
  (case (int c)
      0 \ 
      3 \u2665
    179 \u2502
    219 \u2588
    220 \u2584
    249 "·"
    250 \u00B7
    c))

(defn render-histogram
  [rstate x y title value histogram]
  (let [group-size (get histogram "group-size")]
  ;; render x-axis
  ; FIXME
  (doseq [i (range 8)]
    (put-chars rstate :ui [{:c (get single-border :horizontal) :x (+ x i 1) :y (+ y 8) :fg (rcolor/color->rgb :white) :bg (rcolor/color->rgb :black) :style #{}}]))
  ;; put caret
  (put-chars rstate :ui [{:c \^ :x (+ x (int (/ value group-size)) 1) :y (+ y 8) :fg (rcolor/color->rgb :highlight) :bg (rcolor/color->rgb :black) :style #{}}])
  ;; render y-axis
  ; FIXME
  (doseq [i (range 7)]
    (put-chars rstate :ui [{:c (get single-border :vertical)  :x x :y (+ y i 1) :fg (rcolor/color->rgb :white) :bg (rcolor/color->rgb :black) :style #{}}]))
  (put-chars rstate :ui [{:c (get single-border :bottom-left)  :x x :y (+ y 8) :fg (rcolor/color->rgb :white) :bg (rcolor/color->rgb :black) :style #{}}])
  ;; print title
  (put-chars rstate :ui (markup->chars x y title))
  ;; print bars
  (let [max-count (reduce (fn [m data](max m (get data "count"))) 0 (get histogram "data"))]
    (log/debug "data" histogram)
    (log/debug "max-count" max-count)
    (log/debug "group-size" group-size)
    ;; for each bar
    ; FIXME
    (doseq [data (get histogram "data")
            :when (get data "group")
            :let [group (get data "group")
                  x (+ x 1 (/ group group-size))
                  fg (rcolor/color->rgb
                       (if (< group (inc value) (+ group group-size 1))
                         :highlight
                         :dark-gray))]]
      ;; for each step in the bar
      (let [from (int (- (+ y 7) (* 7 (/ (get data "count") max-count))))
            to   (+ y 7)]
        (log/debug "from" from "to" to "x" x)
      ; FIXME
      (doseq [y (range from to)]
        (put-chars rstate :ui [{:c (cp437->unicode 219)#_"*" :x x :y (inc y) :fg fg :bg (rcolor/color->rgb :black) :style #{}}])))))))

(defn render-share-score
  [rstate state]
  (let [score      (get state :last-score)
        top-scores (get state :top-scores)]
    ;; Title
    (put-string rstate :ui 10 1 "Top scores")
    ;; highscore list
    ; FIXME
    (doseq [[idx score] (map-indexed vector (take 10 (concat top-scores (repeat nil))))]
      (if score
        (let [player-name    (get score "player-name" "?name?")
              points         (get score "points" 0)
              days-survived  (get score :days-survived 0 )
              turns-survived (get score :turns-survived 0 )]
          ;;(put-string (state :screen) 30 (+ idx 3) (format "%d. %s survived for %d %s. (%d points)"
;;                                                                                       (inc idx)
;;                                                                                       player-name days-survived
;;                                                                                       (if (> 1 days-survived)
;;                                                                                         "days"
;;                                                                                          "day")
;;                                                                                      points)))
          (put-string rstate :ui 1 (+ idx 3) (format "%2d.%-20s (%d points)" (inc idx) player-name points) (if (and (= player-name (get-in state [:world :player :name]))
                                                                                                                         (= points (get state :points)))
                                                                                                                  :highlight
                                                                                                                  :white)
                                                                                                                :black))
        (put-string rstate :ui 1 (+ idx 3) "...")))
    ;; Performance
    (put-string rstate :ui 50 1 "Performance")
    (render-histogram state 45 3  "Points"        (get state :points)                                                                  (get state :point-data))
    (render-histogram state 61 3  "Turns"         (rw/get-time state)                                                                  (get state :time-data))
    (render-histogram state 45 13 "Kills"         (reduce + 0 (map second (get-in state [:world :player :stats :num-animals-killed]))) (get state :kills-data))
    (render-histogram state 61 13 "Items Crafted" (reduce + 0 (map second (get-in state [:world :player :stats :num-items-crafted])))  (get state :crafted-data))
    (put-chars rstate :ui (markup->chars 45 22 "Your performance - <color fg=\"highlight\">^</color>"))
    (put-chars rstate :ui (markup->chars 7 22 "Play again? [<color fg=\"highlight\">y</color>/<color fg=\"highlight\">n</color>]"))))

(defn rockpick->render-map
  [layers]
  (let [layer (first layers)]
    (log/info "layer" (vec layer))
    (reduce concat
            []
            (map-indexed (fn [y line]
                           (log/info "line" (vec line))
                           (map-indexed (fn [x tile]
                                          ;(log/info "tile" tile x y)
                                          {:c (cp437->unicode (get tile :ch))
                                           :fg [(get-in tile [:fg :r])
                                                (get-in tile [:fg :g])
                                                (get-in tile [:fg :b])]
                                           :bg [(get-in tile [:bg :r])
                                                (get-in tile [:bg :g])
                                                (get-in tile [:bg :b])]
                                           :x  x
                                           :y  y})
                                        line))
                         layer))))

(defn render-data
  [rstate state id]
  (let [characters (rockpick->render-map (get-in state [:data id]))]
    (log/info "render-map" (vec characters))
    (put-chars rstate :ui characters)))

(defn render-keyboardcontrols-help
  "Render the help screen."
  [rstate state]
  (render-data rstate state :keyboard-controls))

(defn render-ui-help
  "Render the help screen."
  [rstate state]
  (render-data rstate state :ui))

(defn render-gameplay-help
  "Render the help screen."
  [rstate state]
  (render-data rstate state :gameplay))


(defn render-full-log
  "Render the log as a full screen."
  [rstate state]
  (let [log (get-in state [:world :log])]
    (doall (map-indexed (fn [idx message]
                          (put-string rstate :ui 0 idx (get message :text) (get message :color) :black))
                        log))))

(defn render
  "Pick between the normal render function and the
   game over render function based on the dead state
   of the player."
  [state]
  {:pre [(not (nil? state))]
   :post [renderstate?]}
  (log/info "render current-state" (current-state state))
  ;; rstate = render state
  (let [rstate {:layers {
                   :map      []
                   :features []
                   :fx       []
                   :ui       []}
                 :fov           #{} ; fov in screen-space
                 :lantern       [0 0 0]
                 :night         [255 255 255]
                 :palette-cells []
                 :events        [] ; seq of state events translated into draw cmds
               }
        cs     (current-state state)]
    (cond
      (= cs :start)
        (render-start rstate state)
      (= cs :configure)
        (render-configure rstate state)
      (= cs :configure-font)
        (render-configure-font rstate state)
      (= cs :enter-name)
        (render-enter-name rstate state)
      (= cs :start-inventory)
        (render-start-inventory rstate state)
      (= cs :loading)
        (render-loading rstate state)
      (= cs :connecting)
        (render-connecting rstate state)
      (= cs :connection-failed)
        (render-connection-failed rstate state)
      ;(= (cs) :start-text)
      ;  (render-start-text state)
      ;; Is player dead?
      (contains? #{:game-over-dead :game-over-rescued} cs)
        ;; Render game over
        (render-game-over rstate state)
      (= cs :share-score)
        (render-share-score rstate state)
      (= cs :help-controls)
        (render-keyboardcontrols-help rstate state)
      (= cs :help-ui)
        (render-ui-help rstate state)
      (= cs :help-gameplay)
        (render-gameplay-help rstate state)
      (= cs :log)
        (render-full-log rstate state)
      :else (render-map rstate state))))


