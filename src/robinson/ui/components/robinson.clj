;; Functions for rendering state to screen
(ns robinson.ui.components.robinson
  (:require 
            [taoensso.timbre :as log]
            [robinson.random :as rr]
            [robinson.math :as rmath]
            [robinson.color :as rcolor]
            [robinson.catmull-rom :as rcr]
            [robinson.scores :as rs]
            [robinson.startgame :as sg]
            [robinson.popover :as rpop]
            [robinson.itemgen :as ig]
            [robinson.common :as rc :refer [farther-than?
                                            wrap-line
                                            fill-missing
                                            xy->pos]]
            [robinson.fs :as rfs]
            [robinson.world :as rw :refer [current-state
                                           get-time
                                           get-cell
                                           player-cellxy
                                           distance-from-player
                                           inventory-and-player-cell-items]]
            [robinson.font :as rfont]
            [robinson.log :as rlog]
            [robinson.viewport :as rv :refer [cells-in-viewport]]
            [robinson.player :as rp :refer [player-xy
                                            player-wounded?
                                            player-poisoned?
                                            player-infected?]]
            [robinson.inventory :as ri]
            [robinson.describe :as rdesc]
            [robinson.endgame :as rendgame :refer [gen-end-madlib]]
            [robinson.fx :as rfx]
            [robinson.fx.boomerang-item :as rfx-boomerang-item]
            [robinson.crafting :as rcrafting]
            [robinson.itemgen :refer [can-be-wielded?
                                      can-be-wielded-for-ranged-combat?
                                      id->name]]
            [robinson.lineofsight :as rlos :refer [visible?
                                                   cell-blocking?]]
            [robinson.dialog :as rdiag :refer [get-valid-input
                                               fsm-current-state]]
            [robinson.npc :as rnpc :refer [talking-npcs
                                           npcs-in-viewport]]
            [robinson.noise :as rnoise]
            [robinson.traps :as rt]
            [robinson.update :as ru]
            [robinson.ui.cell :as ruc]
            [robinson.ui.updater :as ruu]
            [robinson.ui.components.common :as ruicommon]
            [robinson.ui.components.crafting :as ruicrafting]
            [robinson.ui.components.recipes :as ruirecipes]
            [zaffre.terminal :as zat]
            [zaffre.color :as zcolor]
            [zaffre.components :as zc]
            [zaffre.components.ui :as zcui]
            [zaffre.util :as zutil]
            [tinter.core :as tinter]
            [loom.graph :as lg]
            clojure.set
            [clojure.core.async :as async :refer [go-loop]]
            [clojure.core.match :refer [match]]
            [clojure.xml :as xml]
            [clojure.zip :as zip]
            [clojure.pprint :as pprint]
            clojure.string
            [dk.salza.liq.editor :as editor]
            [dk.salza.liq.adapters.ghostadapter :as lag]))

(set! *warn-on-reflection* true)

(def cell-type-palette
  {:fire                   [:red :orange]
   :water                  [:blue :dark-blue]
   :surf                   [:white :sea-foam :blue-green]
   :shallow-water          [:white :sea-foam :blue-green :blue-green]
   :swamp                  [:white :sea-foam :blue-green]
   :lava                   [:red :orange :yellow]
   :bamboo-water-collector [:blue :light-blue :dark-blue]
   :solar-still            [:blue :light-blue :dark-blue]
   :freshwater-hole        [:blue :light-blue :dark-blue]
   :saltwater-hole         [:blue :light-blue :dark-blue]})

(def palette-noise (rnoise/create-noise))
(defn cell-type->color
  [wx wy cell-type]
  (let [r (rnoise/noise3d palette-noise wx wy (mod (/ (System/currentTimeMillis) 4000) 10000))
        palette (get cell-type-palette cell-type)
        start-idx (int (* r (count palette)))
        end-idx (mod (inc start-idx) (count palette))
        start (nth palette start-idx)
        end (nth palette end-idx)]
    (rand-nth palette)
    start))

(defn has-palette?
  [cell-type]
  (contains? #{:fire :water 
               :surf :shallow-water 
               :swamp :lava 
               :bamboo-water-collector :solar-still 
               :freshwater-hole :saltwater-hole} cell-type))


(defonce frame-count (atom 0))
(defonce frame-steps 1000000)

(defn size
  [^zaffre.terminal.Terminal screen]
  (juxt (zat/args screen) :screen-width :screen-height))

(defn is-menu-state? [state]
  (contains? #{:inventory :describe-inventory :pickup-selection :drop :eat} (get-in state [:world :current-state])))

(binding [zc/*updater* ruu/updater]
(zc/def-component ActionPrompt
  [this]
  (let [{:keys [children]} (zc/props this)]
    (zc/csx
      [:view {:style {:position :absolute :top 0 :left 0 :hight 1}} children])))

(zc/def-component Atmo
  [this]
  (let [{:keys [time atmo-data]} (zc/props this)
        frames (count atmo-data)
        t      (mod time frames)
        frame  (nth atmo-data t)
        indexed-colors (cons (repeat 7 [0 0 0]) (apply map vector (partition 3 frame)))]
    (zc/csx
      ; TODO: Is this :view required?
      [:view {} [
        [zcui/ListImage {:data indexed-colors}]]])))

(zc/def-component StatusUI
  [this]
  (let [{:keys [wounded poisoned infected style]} (zc/props this)
        wounded-color (rcolor/color->rgb (if wounded :red :black))
        poisoned-color (rcolor/color->rgb (if poisoned :green :black))
        infected-color (rcolor/color->rgb (if infected :yellow :black))
        gray-color (rcolor/color->rgb :gray)]
    (zc/csx
      [:view {:style (merge {:width 7
                             :height 1
                             :background-color gray-color}
                            style)} [
        [:text {:style {:background-color gray-color}} [
          [:text {} [" "]]
          [:text {:style {:color wounded-color}} ["\u2665"]]
          [:text {} [" "]]
          [:text {:style {:color poisoned-color}} ["\u2665"]]
          [:text {} [" "]]
          [:text {:style {:color infected-color}} ["\u2665"]]
          [:text {} [" "]]]]]])))

(zc/def-component DblBarChart
  [this]
  (let [{:keys [fg-1 fg-2 p1 p2 width direction style]} (zc/props this)
        width (dec width)
        root (int (* width (min p1 p2)))
        padding (int (* width (- 1 (max p1 p2))))
        diff (- width root padding)
        black-color (rcolor/color->rgb :black)]
          (zc/csx
              [:view {:style (merge {:height 1
                                     :display :flex
                                     :flex-direction (case direction
                                                       :left :row
                                                       :right :row-reverse)}
                                    style)} [
                #_[:text {} [(format "%.2f %.2f" (float p1) (float p2))]]
                [:text {:style {:color black-color
                                :background-color black-color}} [(apply str (repeat padding " "))]]
                [:text {:style {:color            (if (< p2 p1) fg-1 black-color)
                                :background-color (if (> p2 p1) fg-2 black-color)}} [(apply str (repeat diff "\u2584"))]]
                [:text {:style {:color fg-1 :background-color fg-2}} [(apply str (repeat root "\u2584"))]]]])))

(zc/def-component HpWtlBars
  [this]
  (let [{:keys [game-state style]} (zc/props this)
        wtl        (get-in game-state [:world :player :will-to-live])
        max-wtl    (get-in game-state [:world :player :max-will-to-live])
        hp         (get-in game-state [:world :player :hp])
        max-hp     (get-in game-state [:world :player :max-hp])]
    (zc/csx
      [DblBarChart {:fg-1 (rcolor/color->rgb :red)
                    :fg-2 (rcolor/color->rgb :green)
                    :p1  (/ hp max-hp)
                    :p2  (/ wtl max-wtl)
                    :width 37
                    :direction :left
                    :style style}])))

(zc/def-component ThirstHungerBars
  [this]
  (let [{:keys [game-state style]} (zc/props this)
        thirst      (get-in game-state [:world :player :thirst])
        max-thirst  (get-in game-state [:world :player :max-thirst])
        hunger      (get-in game-state [:world :player :hunger])
        max-hunger  (get-in game-state [:world :player :max-hunger])]
    (zc/csx
      [DblBarChart {:fg-1 (rcolor/color->rgb :blue)
                    :fg-2 (rcolor/color->rgb :yellow)
                    :p1  (- 1 (/ thirst max-thirst))
                    :p2  (- 1 (/ hunger max-hunger))
                    :width 37
                    :direction :right
                    :style style}])))

(zc/def-component Hud
  [this]
  (let [{:keys [game-state]} (zc/props this)]
    (zc/csx
      [:view {:style {:position :absolute
                      :top 21
                      :left 0
                      :width 80
                      :display :flex
                      :flex-direction :column
                      :align-items :center}} [
         [Atmo {:time (rw/get-time game-state)
                :atmo-data (get-in game-state [:data :atmo])}]
         [:view {:style {:display :flex
                         :flex-direction :row
                         :align-items :center}} [
           [HpWtlBars {:game-state game-state}]
           [StatusUI {:wounded (rp/player-wounded? game-state)
                      :poisoned (rp/player-poisoned? game-state)
                      :infected (rp/player-infected? game-state)
                      :style {}}]
           [ThirstHungerBars {:game-state game-state}]]]]])))

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

#_(defn render-quests
  "Render the pickup item menu if the world state is `:pickup`."
  [state]
  (render-multi-select (state :screen)
                       "Quests"
                       []
                       (filter (fn [quest]
                                 (not (nil? (get-in state [:world  :quests (quest :id) :stage] nil))))
                               (:quests state))))

(zc/def-component MapInViewport
  [this]
  (let [{:keys [cells current-time font-type]} (zc/props this)
        t (mod (/ (System/currentTimeMillis) 4000) 10000)]
    (zc/csx [:img {:width 80 :height 23}
                   (map-indexed (fn [y line]
                             (map-indexed (fn [x cell]
                               (ruc/render-cell cell x y t current-time font-type))
                               line))
                           cells)])))

(defn npc->cp437-character [npc]
  (case (get npc :race)
    :rat             \r
    :spider          \S
    :scorpion        \u00E7 ;;ς
    :snake           \u00A7 ;;§
    :bat             \B
    :boar            \b
    :gecko           \g
    :monkey          \y
    :bird            \a
    :centipede       \c
    :turtle          \t
    :red-frog        \u03B1 ;;α
    :orange-frog     \u03B1 ;;α
    :yellow-frog     \u03B1 ;;α
    :green-frog      \u03B1 ;;α
    :blue-frog       \u03B1 ;;α
    :purple-frog     \u03B1 ;;α
    :parrot          \p
    :shark           \^ ;;^
    :fish            \f
    :octopus         \#
    :sea-snake       \u00A7
    :clam            \c
    :urchin          \u
    :squid           \q
    :crocodile       \l
    :mosquito        \m
    :mongoose        \r
    :tarantula       \s
    :monitor-lizard  \l
    :komodo-dragon   \l
    :cobra           \u00A7 ;;§
    :puffer-fish     \f
    :crab            \c
    :hermit-crab     \c
    :electric-eel    \e
    :jellyfish       \j
    ;; pirate ship npc
    :giant-rat       \R
    :eel             \e
    :giant-lizard    \L
    ;; ruined temple ncs
    :giant-centipedge \C
    :gorilla         \M
    :giant-snake     \u00A7 ;;§
    :human           \@
    \?))

(defn npc->unicode-character
  [npc]
  (case (get npc :race)
     :shark           \u039B ;;Λ
     (npc->cp437-character npc)))

(defn npc->color
  [npc]
  (case (get npc :race)
    :rat             :white
    :spider          :white
    :scorpion        :white
    :snake           :white
    :bat             :white
    :boar            :brown
    :gecko           :green
    :monkey          :orange
    :bird            :red
    :centipede       :red
    :turtle          :green
    :red-frog        :red
    :orange-frog     :orange
    :yellow-frog     :yellow
    :green-frog      :green
    :blue-frog       :blue
    :purple-frog     :purple
    :parrot          :red
    :shark           :white ;;^
    :fish            :white
    :octopus         :orange
    :sea-snake       :white
    :clam            :white
    :urchin          :purple
    :squid           :orange
    :crocodile       :green
    :mosquito        :white
    :mongoose        :brown
    :tarantula       :brown
    :monitor-lizard  :gray
    :komodo-dragon   :dark-green
    :cobra           :white
    :puffer-fish     :yellow
    :crab            :orange
    :hermit-crab     :yellow
    :electric-eel    :brown
    :jellyfish       :white
    ;; pirate ship npc
    :giant-rat       :white
    :eel             :white
    :giant-lizard    :white
    ;; ruined temple ncs
    :giant-centipedge :white
    :gorilla         :white
    :giant-snake     :green
    :human           :white
    :white))

(defn render-npc [npc current-time font-type]
  (ruc/fill-put-string-color-style-defaults
    0 0 0
    ((case font-type
             :ttf npc->unicode-character
             :cp437 npc->cp437-character) npc)
    (rcolor/color-bloodied-char 
      (< current-time (get npc :bloodied 0))
       (npc->color npc))
       :black))

(zc/def-component CharactersInViewport
  [this]
  (let [{:keys [npcs player-pos player-bloodied player-on-raft? vx vy current-time font-type]} (zc/props this)]
    (zc/csx [:view {:style {:position :absolute
                            :top 0
                            :left 0}}
                   (reduce (fn [children npc]
                             (let [x         (->> npc :pos :x)
                                   y         (->> npc :pos :y)
                                   ;targeted? (when (= (current-state state) :select-ranged-target)
                                   ;            (let [target-ranged-index (get-in state [:world :target-ranged-index])
                                   ;                  target-ranged-pos-coll (get-in state [:world :target-ranged-pos-coll])
                                   ;                  target-pos             (nth target-ranged-pos-coll target-ranged-index)]
                                   ;              (= target-pos (get npc :pos))))
                                   ;t       (rw/get-time state)
                                   {:keys [c fg bg]} (render-npc npc current-time font-type)]
                               ;(log/debug "npc@" x y "visible?" visible)
                               (conj children (zc/csx [:text {:style {:position :absolute
                                                                      :top (- y vy)
                                                                      :left (- x vx)
                                                                      :color fg
                                                                      :background-color bg}} [(str c)]]))))
                           ; Always render player
                           [(zc/csx [:text {:style {:position :absolute
                                                    :top (- (:y player-pos) vy)
                                                    :left (- (:x player-pos) vx)
                                                    :mix-blend-mode (if player-on-raft? :lighten :normal)
                                                    :color (rcolor/color->rgb (if player-bloodied :dark-red :white))
                                                    :background-color (rcolor/color->rgb (if player-on-raft?
                                                                                            :transparent
                                                                                            :black))}}
                                           ["@"]])]
                           npcs)])))

(zc/def-component HighlightNpcs
  [this]
  (let [{:keys [visible-npcs vx vy start-pos end-pos font-type]} (zc/props this)
        [start-x start-y] (rc/pos->xy start-pos)
        [end-x end-y] (rc/pos->xy end-pos)
        points (set (rest (rlos/line-segment-fast
                            [start-x start-y]
                            [end-x end-y])))]
    (zc/csx [:view {:style {:position :absolute}}
                   (reduce (fn [children npc]
                             (let [sx        (- (-> npc :pos :x) vx)
                                   sy        (- (-> npc :pos :y) vy)
                                   targeted (contains? points [sx sy])
                                   {:keys [c fg bg]} (render-npc npc 0 font-type)]
                               (if targeted
                                 (conj children (zc/csx [:text {:style {:position :absolute
                                                                        :top sy
                                                                        :left sx
                                                                        :color (rcolor/color->rgb :black)
                                                                        :background-color (rcolor/color->rgb :green)}} [(str c)]]))
                                 children)))
                           []
                           visible-npcs)])))

(zc/def-component CrushingWall
  [this]
  (let [{:keys [game-state trap vx vy]} (zc/props this)
        current-time (rw/get-time game-state)
        ch (case (trap :direction)
             :up    "╧"
             :down  "╤"
             :left  "╢"
             :right "╟")]
    (zc/csx [:view {} 
              (for [[x y] (first (trap :locations))
                    :when (= (get (rw/get-cell game-state x y) :discovered) current-time)]
                    (zc/csx [:view {:style {:position :absolute
                                            :max-width 1 :max-height 1
                                            :left (- x vx) :top (- y vy)
                                            :color (rcolor/color->rgb :gray)}} [
                              [:text {} [ch]]]]))])))

(zc/def-component PoisonousGas
  [this]
  (let [{:keys [game-state trap vx vy]} (zc/props this)
        current-time (rw/get-time game-state)]
    (zc/csx [:view {} 
              (for [[x y] (keys (get trap :locations))
                    :when (= (get (rw/get-cell game-state x y) :discovered) current-time)]
                  (let [r1 (rnoise/noise3d palette-noise x y (mod (/ (System/currentTimeMillis) 10000) 10000))
                        r2 (rnoise/noise3d palette-noise x y (mod (/ (System/currentTimeMillis) 5001) 10000))
                        bg (rcolor/color->rgb (nth [:yellow :yellow :light-green :green :dark-green] (int (* r1 5))))
                        ch (nth ["\u2591" "\u2592" "\u2593"] (int (* r2 3)))]
                    (zc/csx [:view {:style {:position :absolute
                                            :max-width 1 :max-height 1
                                            :left (- x vx) :top (- y vy)
                                            :color (rcolor/color->rgb :black)
                                            :background-color bg}} [
                              [:text {} [ch]]]])))])))

(zc/def-component Traps
  [this]
  (let [{:keys [game-state vx vy]} (zc/props this)
        traps (rw/current-place-traps game-state)]
    (zc/csx [:view {}
      (map (fn [trap]
             (case (get trap :type)
               :crushing-wall
                 (zc/csx [CrushingWall {:game-state game-state :trap trap :vx vx :vy vy}])
               :poisonous-gas
                 (zc/csx [PoisonousGas {:game-state game-state :trap trap :vx vx :vy vy}])
               (zc/csx [:view {} []])))
            traps)])))

(zc/def-component CharacterFx
  [this]
  (let [{:keys [fx vx vy]} (zc/props this)
        {:keys [pos ch color background-color]} fx
        {:keys [x y]} pos]
    ;(log/info "rendering CharacterFX" ch y vy x vx)
    (zc/csx [:text {:style {:position :absolute
                            :top (- y vy) :left (- x vx)
                            :width 1 :height 1
                            :color (or color (rcolor/color->rgb :white))
                            :background-color (or background-color (rcolor/color->rgb :black))}} [(str ch)]])))

(zc/def-component MultiCharacterFx
  [this]
  (let [{:keys [fx vx vy]} (zc/props this)
        {:keys [ch-pos color background-color]} fx]
    ;(log/info "rendering CharacterFX" ch y vy x vx)
    (zc/csx [:view {} (map (fn [{:keys [ch pos]}]
                           (let [[x y] (rc/pos->xy pos)]
                        (zc/csx
                          [:text {:style {:position :absolute :top (- y vy) :left (- x vx)
                                          :color (or color (rcolor/color->rgb :white))
                                          :background-color (or background-color (rcolor/color->rgb :black))}} [(str ch)]])))
                           ch-pos)])))

(zc/def-component FishingPole
  [this]
  (let [{:keys [game-state vx vy]} (zc/props this)]
    ;; if character is fishing, draw pole
    (condp = (current-state game-state)
      :fishing-left
        (zc/csx [CharacterFx {:fx {:ch "\\" :pos (rc/sub-pos (rp/player-pos game-state)
                                                             (rc/direction->offset-pos :left))}
                               :vx vx :vy vy}])
      :fishing-right
        (zc/csx [CharacterFx {:fx {:ch "/" :pos (rc/sub-pos (rp/player-pos game-state)
                                                             (rc/direction->offset-pos :right))}
                               :vx vx :vy vy}])
      :fishing-up
        (zc/csx [CharacterFx {:fx {:ch "/" :pos (rc/sub-pos (rp/player-pos game-state)
                                                             (rc/direction->offset-pos :up))}
                               :vx vx :vy vy}])
      :fishing-down
        (zc/csx [CharacterFx {:fx {:ch "\\" :pos (rc/sub-pos (rp/player-pos game-state)
                                                             (rc/direction->offset-pos :down))}
                               :vx vx :vy vy}])
      (zc/csx [:view {}]))))

(zc/def-component FX
  [this]
  (let [{:keys [fx vx vy]} (zc/props this)]
    (zc/csx [:view {:style {:position :absolute :top 0 :left 0}}
      (map (fn [[effect-id effect]]
             (log/debug "rendering effect " effect)
             (case (get effect :type)
               :character-fx
                 (let [[x y] (rc/pos->xy (get effect :pos))]
                   (zc/csx [CharacterFx {:fx effect :vx vx :vy vy }]))
               :multi-character-fx
                   (zc/csx [MultiCharacterFx {:fx effect :vx vx :vy vy}])
               (zc/csx [:view {} []])))
            fx)])))

(defn light-producing?
  [cell-type]
  (contains? #{:fire :lava} cell-type))

(defn render-lighting [cell distance sight-distance lantern-on]
  (if (light-producing? (get cell :type))
    (zcolor/color 0 0 0 0)
    (if lantern-on
      (let [r1 (rnoise/noise3d palette-noise 0 0 (mod (/ (System/currentTimeMillis) 1000) 10000))
            r2 (rnoise/noise3d palette-noise 0 0 (mod (/ (System/currentTimeMillis) 100) 10000))
            r (max 0 (min 1 (+ 0.4 (* 0.5 r1) (* 0.1 r2))))]
        (zcolor/color
          (unchecked-byte (* r 255))
          (unchecked-byte (* r 204))
          (unchecked-byte (* r 124))
          (unchecked-byte (min 255 (* 255 (/ distance (+ 1 (* r sight-distance))))))))
      (rcolor/lighting sight-distance))))

(zc/def-component ShadeImg
  [this]
  (let [{:keys [cells current-time player-screen-pos sight-distance lantern-on]} (zc/props this)]
    (zc/csx [:img {:width 80 :height 23 :style {:mix-blend-mode :multiply}}
                  (map-indexed (fn [y line]
                            (map-indexed (fn [x cell]
                              (if (= (or (if cell (:discovered cell) false) 0) current-time)
                                (let [distance-from-player (rc/distance player-screen-pos (rc/xy->pos x y))]
                                  {:c \space :fg [0 0 0 0]
                                   :bg (render-lighting cell distance-from-player sight-distance lantern-on)})
                                {:c \space :fg [0 0 0 0] :bg [0 0 0 128]}))
                              line))
                          cells)])))

(zc/def-component Line
  [this]
  (let [{:keys [ch color background-color start-pos end-pos]} (zc/props this)
        [start-x start-y] (rc/pos->xy start-pos)
        [end-x end-y] (rc/pos->xy end-pos)]
            ;(put-string screen :ui sx sy "\u25CF" :green :black))))
    (zc/csx [:view {}
      (map (fn [[x y]]
               ;(log/info x y)
               (zc/csx [:view {} [
                 [:text {:style {:position :absolute :top y :left x
                                 :color (or color [0 255 0 255])
                                 :background-color (or background-color [0 0 0 0])}} [(or (str ch) "*")]]]]))
        (rlos/line-segment-fast-without-endpoints
          [start-x start-y]
          [end-x end-y]))])))

(zc/def-component Spline
  [this]
  (let [{:keys [ch color background-color control-points]} (zc/props this)]
    (zc/csx [:view {}
      (map (fn [[x y]]
               (zc/csx [:view {} [
                 [:text {:style {:position :absolute :top y :left x
                                 :color (or color [0 255 0 255])
                                 :background-color (or background-color [0 0 0 0])}} [(or (str ch) "*")]]]]))
        (rcr/catmull-rom-chain (map rc/pos->xy control-points)))])))

; Render the pickup item menu if the world state is `:pickup-selection`.
(zc/def-component PickupSelection
  [this]
  (let [{:keys [game-state]} (zc/props this)
        direction        (get-in game-state [:world :pickup-direction])
        {x :x y :y}      (rw/player-adjacent-pos game-state direction)
        cell             (get-cell game-state x y)
        cell-items       (or (cell :items) [])
        hotkeys          (-> game-state :world :remaining-hotkeys)
        selected-hotkeys (-> game-state :world :selected-hotkeys)
        items            (fill-missing (fn missing [item] (not (contains? item :hotkey)))
                                       (fn apply-hotkey [item hotkey] (assoc item :hotkey hotkey))
                                       hotkeys
                                       cell-items)]
   (zc/csx [:view {:style {:width 40
                           :height 20
                           :position :absolute
                           :left 40
                           :top 0
                           :padding 1
                           :display :flex
                           :flex-direction :column
                           :background-color (rcolor/color->rgb :black)}} [
            [ruicommon/MultiSelect {:title "Pick up"
                          :selected-hotkeys selected-hotkeys
                          :items (concat (translate-identified-items game-state items)
                                         [{:name "All" :hotkey \space}])
                          :style {:min-height "100%" :flex 1}}]
            [:text {:style {:color (rcolor/color->rgb :black) :top -2}} [
              [ruicommon/Highlight {} ["Enter "]]
              [:text {} ["to pick up."]]]]]])))

(zc/def-component RightPane
  [this]
  (let [{:keys [children]} (zc/props this)]
    (zc/csx [:view {:style {:position :absolute
                            :width 40
                            :height 21
                            :left 40
                            :top 0
                            :padding 1
                            :background-color (rcolor/color->rgb :black)}} children])))

(zc/def-component Inventory
  [this]
  (let [{:keys [game-state]} (zc/props this)
        player-items (-> game-state :world :player :inventory)]
   (zc/csx [RightPane {} [
            [ruicommon/MultiSelect {:title "Inventory"
                          :items (translate-identified-items game-state player-items)}]]])))

(zc/def-component Abilities
  [this]
  (let [{:keys [game-state]} (zc/props this)
        abilities (rp/player-abilities game-state)
        height (if (seq abilities)
                 (+ 3 (* 3 (count abilities)))
                 4)]
    (if (seq abilities)
      (zc/csx [zcui/Popup {} [
                [:view {} [
                  [ruicommon/MultiSelect {:title "Abilities"
                                :items abilities}]]]]])
      (zc/csx [zcui/Popup {} [
        [:view {} [
          [:text {} ["No abilities."]]
          [:text {} [""]]
          [:view {:style {:display :flex :flex-direction :row}} [
            [:text {} ["Press "]]
            [ruicommon/Highlight {} ["Esc "]]
            [:text {} ["to exit."]]]]]]]]))))

; Render the player character stats  menu if the world state is `:player-stats`.
(zc/def-component PlayerStats
  [this]
  (let [{:keys [game-state]} (zc/props this)
        x             18
        y             5
        height        11
        player-name   (rp/get-player-attribute game-state :name)
        max-hp        (int (rp/player-max-hp game-state))
        level         (rp/player-level game-state)
        xp            (or (rp/xp-acc-for-next-level game-state) -9)
        xp-next-level (or (rp/xp-for-next-level game-state) -99)
        strength      (int (rp/get-player-attribute game-state :strength))
        dexterity     (rp/get-player-attribute game-state :dexterity)
        toughness     (rp/get-player-attribute game-state :toughness)]
    (zc/csx [zcui/Popup {:style {:top -5}} [
        [:text {} [(format "Name:      %s" player-name)]]
        [:text {} [(format "Level:     %d (%d/%d)" level xp xp-next-level)]]
        [:text {} [""]]
        [:text {} [(format "Max HP:    %d" max-hp )]]
        [:text {} [""]]
        [:text {} [(format "Strength:  %d" strength )]]
        [:text {} [(format "Dexterity: %d" dexterity )]]
        [:text {} [(format "Toughness: %d" toughness )]]
        [:text {} [""]]
        [:text {} [
          [:text {} ["Press "]]
          [ruicommon/Highlight {} ["Esc "]]
          [:text {} ["to exit."]]]]]])))

(zc/def-component AbilityChoices
  [this]
  (let [{:keys [game-state]} (zc/props this)
        abilities (get-in game-state [:world :ability-choices])]
    (zc/csx [zcui/Popup {:style {:top -5
                                 :color (rcolor/color->rgb :black)
                                 :background-color (rcolor/color->rgb :black)}} [
                [ruicommon/MultiSelect {:title "Choose A New Ability"
                              :items
                                (concat
                                  (mapcat
                                    (fn [ability]
                                      [{:name (get ability :name)
                                        :hotkey (get ability :hotkey)}
                                       {:name (get ability :description)}
                                       {:name ""}])
                                    abilities))}]]])))

(zc/def-component ActionChoices
  [this]
  (let [{:keys [game-state]} (zc/props this)
        abilities (get-in game-state [:world :action-select])
        height (if (seq abilities)
                 (+ 3 (* 3 (count abilities)))
                 4)]  
    (zc/csx [zcui/Popup {:style {:top -5
                                 :color (rcolor/color->rgb :black)
                                 :background-color (rcolor/color->rgb :black)}} [
                (if (seq abilities)
                  (zc/csx [ruicommon/MultiSelect {:title "Choose Action"
                                        :items
                                          (concat
                                            (mapcat
                                              (fn [ability]
                                                [{:name (get ability :name)
                                                  :hotkey (get ability :hotkey)}
                                                 {:name (get ability :description "")}
                                                 {:name ""}])
                                              abilities)
                                            [{:name ""}
                                             {:name "Select hotkey or press Esc to exit."}])}])
                    (zc/csx
                      [:view {} [
                        [:text {} ["Nothing to do."]]
                        [:text {} [""]]
                        [:text {} [
                          [:text {} ["Press "]]
                          [ruicommon/Highlight {} ["Esc "]]
                          [:text {} ["to exit."]]]]]]))]])))

(zc/def-component Describe
  [this]
  (let [{:keys [game-state]} (zc/props this)
        {cursor-x :x
         cursor-y :y} (get-in game-state [:world :cursor])
        [x y]         (rv/viewport-xy game-state)
        [w _]         (rv/viewport-wh game-state)
        #_#__             (log/info "cursor-x" cursor-x "cursor-y" cursor-y)
        position      (if (< cursor-x (/ w 2))
                        :right
                        :left)
        description   (rdesc/describe-cell-at-xy game-state (+ x cursor-x) (+ y cursor-y))
        cursor-x      (if (= position :left)
                        (dec (- cursor-x (count description)))
                        (inc cursor-x))]
  (zc/csx [:view {:style {:position :absolute
                          :top cursor-y
                          :left cursor-x
                          :max-height 2}} [
            [:text {:style {:height 1 :background-color (zcolor/with-alpha (rcolor/color->rgb :black) 246)}} [description]]
            (when (get-in game-state [:world :dev-mode])
              (zc/csx [:text {:style {:background-color (zcolor/with-alpha (rcolor/color->rgb :black) 246)}} [
                        (str (get-cell game-state (+ x cursor-x) (+ y cursor-y)))]]))]])))



(zc/def-component Apply
  [this]
  (let [{:keys [game-state]} (zc/props this)
        player-items (-> game-state :world :player :inventory)]
    (zc/csx [RightPane {} [
             [ruicommon/MultiSelect {:title "Apply Inventory"
                           :items (translate-identified-items game-state player-items)}]]])))

(zc/def-component ApplyTo
  [this]
  (let [{:keys [game-state]} (zc/props this)
        player-items (-> game-state :world :player :inventory)]
    (zc/csx [RightPane {} [
             [ruicommon/MultiSelect {:title "Apply To"
                           :items (translate-identified-items game-state player-items)}]]])))


(zc/def-component QuaffInventory
  [this]
  (let [{:keys [game-state]} (zc/props this)]
    (zc/csx [RightPane {} [
             [ruicommon/MultiSelect {:title "Quaff"
                           :items (translate-identified-items game-state (filter ig/is-quaffable?
                                                                           (-> game-state :world :player :inventory)))}]]])))

(zc/def-component Drop
  [this]
  (let [{:keys [game-state]} (zc/props this)
        player-items (-> game-state :world :player :inventory)]
    (zc/csx [RightPane {} [
             [ruicommon/MultiSelect {:title "Drop Inventory"
                           :items (translate-identified-items game-state player-items)}]]])))

(zc/def-component DescribeInventory
  [this]
  (let [{:keys [game-state]} (zc/props this)
        player-items (-> game-state :world :player :inventory)]
    (zc/csx [RightPane {} [
             [ruicommon/MultiSelect {:title "Describe"
                           :items (translate-identified-items game-state player-items)}]]])))

(zc/def-component ThrowInventory
  [this]
  (let [{:keys [game-state]} (zc/props this)]
  (let [{:keys [game-state]} (zc/props this)
        player-items (-> game-state :world :player :inventory)]
    (zc/csx [RightPane {} [
             [ruicommon/MultiSelect {:title "Throw"
                           :items (translate-identified-items game-state player-items)}]]]))))

(zc/def-component Eat
  [this]
  (let [{:keys [game-state]} (zc/props this)]
    (zc/csx [RightPane {} [
             [ruicommon/MultiSelect {:title "Eat Inventory"
                           :items (filter #(contains? % :hunger)
                                         (inventory-and-player-cell-items game-state))}]]])))

(zc/def-component Quests
  [this]
  (let [{:keys [game-state]} (zc/props this)]
    nil))

(zc/def-component Wield
  [this]
  (let [{:keys [game-state]} (zc/props this)
        player-items (filter can-be-wielded? (-> game-state :world :player :inventory))]
   (zc/csx [RightPane {} [
            [ruicommon/MultiSelect {:title "Wield"
                          :items (translate-identified-items game-state player-items)}]]])))

(zc/def-component WieldRanged
  [this]
  (let [{:keys [game-state]} (zc/props this)
        player-items (filter can-be-wielded-for-ranged-combat? (-> game-state :world :player :inventory))]
   (zc/csx [RightPane {} [
            [ruicommon/MultiSelect {:title "Wield Ranged"
                          :items (translate-identified-items game-state player-items)}]]])))

(zc/def-component StartText
  [this]
  (let [{:keys [game-state]} (zc/props this)
        start-text (sg/start-text game-state)]
    (zc/csx [zcui/Popup {:style {:border-style :double :padding 1 :top -5}} [
      [:text {:style {:width 40}} [(str start-text)]]
      [:view {:style {:display :flex :flex-direction :row :margin-top 2}} [
        [:text {} ["Press "]]
        [ruicommon/HotkeyLabel {:hotkey :space :sep " " :label "to continue "}]
        [:text {:style {:margin-right 1}} ["and"]]
        [ruicommon/HotkeyLabel {:hotkey \? :sep " " :label "to view help"}]]]]])))

(zc/def-component ContinuePopover
  [this]
  (let [{:keys [message]} (zc/props this)]
    (zc/csx [zcui/Popup {} [
              [:text {} [message]]
              [:text {} [
                [:text {} ["Press "]]
                [ruicommon/Highlight {} ["space "]]
                [:text {} ["to continue."]]]]]])))

(zc/def-component YesNoPopover
  [this]
  (let [{:keys [message]} (zc/props this)]
    (zc/csx [zcui/Popup {} [
              [:text {} [message]]
              [:text {} [
                [:text {} ["["]]
                [ruicommon/Highlight {} ["y"]]
                [:text {} ["/"]]
                [ruicommon/Highlight {} ["n"]]
                [:text {} ["]"]]]]]])))

(zc/def-component RescuedPopover
  [this]
  (let [{:keys [game-state]} (zc/props this)
        rescue-mode (rendgame/rescue-mode game-state)]
    (zc/csx [ContinuePopover {:message (format "A passing %s spots you." rescue-mode)}])))

(zc/def-component DeadText
  [this]
  (let [{:keys [game-state]} (zc/props this)
        hp             (get-in game-state [:world :player :hp])
        hunger         (get-in game-state [:world :player :hunger])
        max-hunger     (get-in game-state [:world :player :max-hunger])
        thirst         (get-in game-state [:world :player :thirst])
        max-thirst     (get-in game-state [:world :player :max-thirst])
        will-to-live   (get-in game-state [:world :player :will-to-live])
        cause-of-death (or
                         (get-in game-state [:world :cause-of-death])
                         (format "%s"
                           (cond
                             (<= hp 0)             "massive injuries"
                             (> hunger max-hunger) "literally starving to death"
                             (> thirst max-thirst) "not drinking enough water"
                             (<= will-to-live 0)   "just giving up on life"
                             :else                 "mysterious causes")))]
      (zc/csx [zcui/Popup {} [
                  [:text {} ["You died."]]
                  [:text {} [(format "From %s" cause-of-death)]]
                  [:text {} [""]]
                  [:view {:style {:display :flex :flex-direction :row}} [
                    [:text {} ["Press "]]
                    [ruicommon/Highlight {} ["space "]]
                    [:text {} ["to continue."]]]]]])))

(zc/def-component QuitPrompt
  [this]
  (let [{:keys [game-state]} (zc/props this)]
    (zc/csx [zcui/Popup {:style {:top -6}} [
              [:view {:style {:display :flex
                              :flex-direction :row}} [
                [:text {} ["Quit? ["]]
                [ruicommon/Highlight {} ["y"]]
                [:text {} ["/"]]
                [ruicommon/Highlight {} ["n"]]
                [:text {} ["]"]]]]]])))

(zc/def-component Harvest
  [this]
  (let [{:keys [game-state]} (zc/props this)]
    nil))

(zc/def-component DebugEval
  [this]
  (let [{:keys [game-state]} (zc/props this)
        dsp (lag/get-display)
        fgplain [255 255 255 255]
        bgplain [0 0 0 255]
        statusline [128 128 128 255]
        cursor0 (rcolor/color->rgb :highlight 255)
        cursor1 (rcolor/color->rgb :highlight 255)
        cursor2 (rcolor/color->rgb :highlight 255)
        green (rcolor/color->rgb :green 255)
        yellow (rcolor/color->rgb :yellow 255)
        red (rcolor/color->rgb :red 255)
        type1 (rcolor/color->rgb :green 255)
        type2 (rcolor/color->rgb :yellow 255)
        type3 (rcolor/color->rgb :orange 255)
        comment-color (rcolor/color->rgb :blue 255)
        str-color (rcolor/color->rgb :red 255)
        l (partition 60
            (reduce (fn [l span]
                      #_(log/info span)
                      (let [{:keys [column row line]} span
                            offset (+ (* (dec row) 60) (dec column))
                            fg-face (atom fgplain)
                            bg-face (atom bgplain)]
                        (reduce (fn [l [i ch]]
                                  (cond
                                    (string? ch)
                                      (assoc l (+ offset i) {:c (or (first ch) \space)
                                                             :fg @fg-face :bg @bg-face})
                                    (map? ch)
                                      (let [{:keys [char face bgface]} ch]
                                        (reset! fg-face (case face
                                                           :plain fgplain
                                                           :statusline statusline
                                                           :cursor0 cursor0
                                                           :cursor1 cursor0
                                                           :cursor2 cursor0
                                                           :type1 type1
                                                           :type2 type2
                                                           :type3 type3
                                                           :green green
                                                           :yellow yellow
                                                           :red red
                                                           :comment comment-color
                                                           :string str-color
                                                           [255 255 255 255]))
                                        (reset! bg-face (case bgface
                                                          :plain bgplain
                                                          :statusline statusline
                                                          :cursor0 cursor0
                                                          :cursor1 cursor0
                                                          :cursor2 cursor0
                                                          [0 0 0 228]))
                                        (assoc l (+ offset i) {:c (first char)
                                                               :fg @fg-face
                                                               :bg @bg-face}))
                                    :else
                                      l))
                                l
                                (map-indexed vector line))))
                 (vec (repeat (* 60 20) {:c \space :fg [0 0 0 0] :bg [0 0 0 228]}))
                 (last dsp)))]
    (zc/csx [zcui/Popup {:style {:top -5}} [
              [:view {} [
                  [:img {:width 60 :height 20} l]]]]])))

(zc/def-component MapUI
  [this]
  (let [{:keys [game-state]} (zc/props this)]
    (case (current-state game-state)
      :pickup-selection     (zc/csx [PickupSelection {:game-state game-state}])
      :inventory            (zc/csx [Inventory {:game-state game-state}])
      :abilities            (zc/csx [Abilities {:game-state game-state}])
      :player-stats         (zc/csx [PlayerStats {:game-state game-state}])
      :gain-level           (zc/csx [AbilityChoices {:game-state game-state}])
      :action-select        (zc/csx [ActionChoices {:game-state game-state}])
      :describe             (zc/csx [Describe {:game-state game-state}])
      :apply                (zc/csx [Apply {:game-state game-state}])
      :apply-item-inventory
                            (zc/csx [ApplyTo {:game-state game-state}])
      :quaff-inventory
                            (zc/csx [QuaffInventory {:game-state game-state}])
      :drop                 (zc/csx [Drop {:game-state game-state}])
      :describe-inventory   (zc/csx [DescribeInventory {:game-state game-state}])
      :throw-inventory      (zc/csx [ThrowInventory {:game-state game-state}])
      :eat                  (zc/csx [Eat {:game-state game-state}])
      :quests               (zc/csx [Quests {:game-state game-state}])
      :recipes              (zc/csx [ruirecipes/Recipes {:game-state game-state}])
      :select-recipe-type   (zc/csx [ruirecipes/SelectRecipeType {:game-state game-state}])
      :in-progress-recipe   (zc/csx [ruirecipes/CraftInProgressRecipe {:game-state game-state}])
      :craft                (zc/csx [ruicrafting/Craft {:game-state game-state}])
      :wield                (zc/csx [Wield {:game-state game-state}])
      :wield-ranged         (zc/csx [WieldRanged {:game-state game-state}])
      :start-text           (zc/csx [StartText {:game-state game-state}])
      :popover              (zc/csx [ContinuePopover {:message (rpop/get-popover-message game-state)}])
      :quaff-popover        (zc/csx [YesNoPopover {:message (rpop/get-popover-message game-state)}])
      :dead                 (zc/csx [DeadText {:game-state game-state}])
      :rescued              (zc/csx [RescuedPopover {:game-state game-state}])
      :quit?                (zc/csx [QuitPrompt {:game-state game-state}])
      :harvest              (zc/csx [Harvest {:game-state game-state}])
      :debug-eval           (zc/csx [DebugEval {:game-state game-state}])
      (zc/csx [:view nil []]))))

(zc/def-component Message
  [this]
  ;; draw log
  (let [{:keys [game-state]} (zc/props this)
        current-time     (rw/get-time game-state)
        log-idx          (get-in game-state [:world :log-idx] 0)
        current-logs     (let [logs (rlog/current-logs game-state)]
                           (if (not-empty logs)
                             logs
                             (let [log (into {} (rlog/last-log game-state))]
                               (when (< current-time (+ (get log :time 0) 5))
                                 [log]))))
        num-logs         (count current-logs)
        msg-above?       (< log-idx (dec num-logs))
        msg-below?       (pos? log-idx)
        up-arrow-color   (if msg-above?
                           (rcolor/color->rgb (get (nth (reverse (get-in game-state [:world :log])) (inc log-idx)) :color))
                           (rcolor/color->rgb :white))
        down-arrow-color (if msg-below?
                           (rcolor/color->rgb (get (nth (reverse (get-in game-state [:world :log])) (dec log-idx)) :color))
                           (rcolor/color->rgb :white))
        message          (if (zero? num-logs)
                           {:message "" :time 0 :color :black} 
                           (nth (reverse current-logs) log-idx))
        darken-factor    (/ 1 (inc (- current-time (get message :time 0))))
        log-color        (rcolor/darken-rgb (rcolor/color->rgb (get message :color :gray)) darken-factor)]
    (zc/csx
        [:view {:style {:position :absolute
                        :top 0 :left 0
                        :height 1 :width 80}} [
          [:text {} [
            (if msg-above?
              (zc/csx [:text {} [
                        [ruicommon/Highlight {} ["/"]]
                        [:text {} ["-"]]
                        [:text {:style {:color up-arrow-color
                                        :background-color (zcolor/with-alpha (rcolor/color->rgb :black) 242)}}
                               [ruicommon/up-arrow-char]]]])
              (zc/csx [:text {} ["   "]]))
            (if msg-below?
              (zc/csx [:text {} [
                        [ruicommon/Highlight {} ["*"]]
                        [:text {} ["-"]]
                        [:text {:style {:color down-arrow-color
                                        :background-color (zcolor/with-alpha (rcolor/color->rgb :black) 242)}}
                               [ruicommon/down-arrow-char]]]])
              (zc/csx [:text {} ["   "]]))
            (if (get message :text)
              (zc/csx [:text {:style {:color log-color
                                      :background-color (zcolor/with-alpha (rcolor/color->rgb :black) 242)}}
                             [(str (get message :text))]])
              (zc/csx [:text {} [""]]))]]]])))

(defn target-pos [game-state]
  (case (current-state game-state)
    :select-ranged-target
      (let [target-ranged-index (get-in game-state [:world :target-ranged-index])
            target-ranged-pos-coll (get-in game-state [:world :target-ranged-pos-coll])
            target-pos             (nth target-ranged-pos-coll target-ranged-index)]
        (log/debug "target-ranged-index" target-ranged-index)
        (log/debug "target-ranged-pos-coll" (get-in game-state [:world :target-ranged-pos-coll]))
        (log/debug "target-pos" target-pos)
        (rv/world-pos->screen-pos game-state target-pos))
    :select-throw-target
      (rv/get-cursor-pos game-state)
    (rv/get-cursor-pos game-state)))

(defn parse-ui-hint
  [s]
  (loop [r [] i 0 start 0 state :text]
    (if (< i (count s))
      (let [c (get s i)]
        (case c
          \<
            (let [t (.substring (str s) start i)]
              (recur (conj r (case state
                               :text
                                 (zc/csx [:text {} [t]])
                               :tag
                                 (zc/csx [ruicommon/Highlight {} [t]])))
                     (inc i) (inc i) :tag))
          \>
            (let [t (.substring (str s) start i)]
              (recur (conj r (case state
                               :text
                                 (zc/csx [:text {} [t]])
                               :tag
                                 (zc/csx [ruicommon/Highlight {} [t]])))
                     (inc i) (inc i) :text))
          (recur r (inc i) start state)))
      (let [t (.substring (str s) start i)]
        (conj r (zc/csx [:text {} [t]]))))))

(zc/def-component UIHint
  [this]
  (let [{:keys [ui-hint]} (zc/props this)]
    (zc/csx [:view {:style {:position :absolute
                            :top 0 :left 0
                            :height 1
                            :display :flex
                            :flex-direction :row
                            :background-color [0 0 0 0]}} (parse-ui-hint ui-hint)])))

(zc/def-component Map
  [this]
  (let [{:keys [game-state]} (zc/props this)
        [columns rows] [80 24]
        current-time (get-in game-state [:world :time])
        {{player-x :x player-y :y :as player-pos}
         :pos :as player}                           (rp/get-player game-state)
        d                                           (rlos/sight-distance game-state)
        cells                                       (rv/cells-in-viewport game-state)
        visible-npcs                                (filter (fn [npc]
                                                              (let [{npc-x :x npc-y :y :as npc-pos} (get npc :pos)]
                                                                (and (not (farther-than?
                                                                             player-pos
                                                                             (get npc :pos)
                                                                             8))
                                                                     (= (get (rw/get-cell game-state npc-x npc-y) :discovered)
                                                                        current-time))))
                                                            (npcs-in-viewport game-state))
        place-id                                    (rw/current-place-id game-state)
        viewport-pos                                (if place-id {:x 0 :y 0} (-> game-state :world :viewport :pos))
        player-screen-pos                           (rv/world-pos->screen-pos game-state player-pos)
        sight-distance                              (rlos/sight-distance game-state)
        [vx vy]                                     (rc/pos->xy viewport-pos)
        lantern-on                                  (when-let [lantern (ri/inventory-id->item game-state :lantern)]
                                                      (= (get lantern :state :off) :on))
        font-type                                   (get (rfont/current-font game-state) :type :ttf)]
    (zc/csx
	  [:terminal {} [
		[:group {:id :app} [
		  [:layer {:id :map} [
            [:view {:on-click (fn [{:keys [target client-x client-y game-state]}]
                                (let [{player-screen-x :x player-screen-y :y} player-screen-pos]
                                (log/info client-x client-y)
                                (log/info
                                  (rc/find-point-relation-ext
                                    [player-screen-x player-screen-y]
                                    [client-x client-y]))
                                (ru/update-state game-state
                                  (rc/find-point-relation-ext
                                    [player-screen-x player-screen-y]
                                    [client-x client-y]))))
                   :style {:top 0 :left 0 :width 80 :height 24}} [
              [MapInViewport {:cells cells :current-time current-time :font-type font-type}]]]]]
		  [:layer {:id :features} [
            [:view {:style {:position :absolute :top 0 :left 0 :width 80 :height 24}} [
              [CharactersInViewport {:npcs visible-npcs
                                     :player-pos (rp/player-pos game-state)
                                     :player-bloodied (get player :bloodied)
                                     :player-on-raft? (contains? (set (map :item/id (get (first (player-cellxy game-state)) :items))) :raft)
                                     :vx vx
                                     :vy vy
                                     :current-time current-time
                                     :font-type font-type}]]]
            [:view {:style {:position :absolute :top 0 :left 0}} [
              [Traps {:game-state game-state
                      :player-pos (rp/player-pos game-state)
                      :player-bloodied (get player :bloodied)
                      :vx vx
                      :vy vy
                      :current-time current-time}]]]
            [FishingPole {:game-state game-state}]
            [FX {:vx vx
                 :vy vy
                 :fx (rfx/fx game-state)}]]]
          [:layer {:id :shading} [
            [:view {:style {:top 0 :left 0}} [
              [ShadeImg {:cells cells :current-time current-time
                         :player-screen-pos player-screen-pos
                         :sight-distance sight-distance
                         :lantern-on lantern-on}]]]]]
          [:layer {:id :ui} [
            [:view {:style {:top 0 :left 0 :width 80 :height 24}} [
              (when (contains? #{:select-ranged-target :select-throw-target} (current-state game-state))
                (let [target (target-pos game-state)]
                  (zc/csx [:view {:style {:position :absolute}} [
                            (let [ranged-weapon-item  (first (filter (fn [item] (get item :wielded-ranged))
                                                                     (ri/player-inventory game-state)))]
                              (if (= :boomerang (get ranged-weapon-item :item/id))
                                (zc/csx [Spline {:ch "\u25CF"
                                               :color (rcolor/color->rgb :green 255)
                                               :background-color [0 0 0 0]
                                               :control-points (rfx-boomerang-item/boomerang-control-points
                                                                    player-screen-pos
                                                                    target)}])
                                (zc/csx [Line {:ch "\u25CF"
                                               :color (rcolor/color->rgb :green 255)
                                               :background-color [0 0 0 0]
                                               :start-pos  player-screen-pos
                                               :end-pos target}])))
                              [HighlightNpcs {:visible-npcs visible-npcs
                                              :vx vx
                                              :vy vy
                                              :start-pos  (rv/world-pos->screen-pos game-state player-pos)
                                              :end-pos target
                                              :font-type font-type}]]])))
              (if-let [ui-hint (get-in game-state [:world :ui-hint])]
                ;; ui-hint
                (zc/csx [UIHint {:ui-hint ui-hint}])
                ;; regular concise message log
                (zc/csx [Message {:game-state game-state}]))
              (when-let [cursor-pos (target-pos game-state)]
                (zc/csx [ruicommon/Cursor {:pos cursor-pos}]))
              [Hud {:game-state game-state}]
              [MapUI {:game-state game-state}]]]]]]]]])))


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

(zc/def-component Histogram
  [this]
  (let [{:keys [title histogram value]} (zc/props this)
        group-size (get histogram "group-size")
        caret-x (+ (int (/ value group-size)) 1)
        max-count (reduce (fn [m data](max m (get data "count"))) 0 (get histogram "data"))
        height 6]
    (zc/csx
      [:view {:style {:width 15 :height 10}} [
        [:view {} [[:text {} [title]]]]
        ; histogram
        [:view {:style {:width 8 :height height :top 1 :left 1
                        :border-left 1 :border-bottom 1
                        :position :absolute
                        :display :flex
                        :flex-direction :row
                        :align-items :flex-end}} 
               (mapcat identity
                 (for [[index data] (map-indexed vector (sort-by #(get % "group") (get histogram "data")))
                       :when (get data "group")
                       :let [group (get data "group")
                             count (get data "count")
                             x (+ 1 (/ group group-size))
                             height (double (* (- height 2) (/ count max-count)))
                             bar-height (Math/ceil height)
                             bg (rcolor/color->rgb
                                  (if (< group (inc value) (+ group group-size 1))
                                    :highlight
                                    :dark-gray))]] [
                   ; histogram bar
                   (zc/csx [:view {:style {:display :flex
                                           :flex-direction :column
                                           :width 1
                                           #_#_:height height}} [
                     [:view {:style {:width 1 :height 1
                                     :color (if (= (double (Math/round height)) bar-height) (rcolor/color->rgb :black) bg)
                                     :background-color (rcolor/color->rgb :black)}} [
                       [:text {} ["\u2584"]]]]
                     [:view {:style {:width 1 :height (dec bar-height)
                                           :background-color bg}} []]]])]))]
        ; caret
        [:view {:style {:position :absolute
                        :width 1 :height 1
                        :top height :left (inc caret-x)
                        :color (rcolor/color->rgb :highlight)}} [[:text {} ["^"]]]]]])))

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

(zc/def-component Start
  [this]
  (zc/csx
    [:terminal {} [
      [:group {:id :app} [
        [:layer {:id :map} [
          [zcui/Image {:src (rfs/cwd-path "images/background-0a.png")}]]]
        [:layer {:id :ui} [
          [:view {:style {:position :absolute
                          :width 21
                          :height 6
                          :top 18
                          :left 30
                          :border 1
                          :background-color [0 0 0 218]}} [
            [:view {:style {:display :flex
                            :flex-direction :row}} [
              [:text {} ["Press "]]
              [ruicommon/HotkeyLabel {:hotkey :space :sep " " :label "to play"}]]]
            [:text {} [""]]
            [ruicommon/HotkeyLabel {:hotkey \c :label "configure"}]
            [ruicommon/HotkeyLabel {:hotkey \q :label "quit"}]]]]]]]]]))

(zc/def-component Configure
  [this]
  (let [{:keys [state]} (zc/props this)]
    (zc/csx
	  [:terminal {} [
		[:group {:id :app} [
		  [:layer {:id :ui} [
			[:view {:style {:position :absolute
                            :width 80
                            :height 24
							:top 3
							:left 35}} [
			  [:text {} ["Configure"]]
			  [:text {} [
                [ruicommon/Highlight {} ["f "]]
                [:text {} ["- font"]]]]
			  [:text {} [
                [ruicommon/Highlight {} ["Esc "]]
                [:text {} ["- back"]]]]]]]]]]]])))

(zc/def-component ConfigureFont
  [this]
  (let [{:keys [game-state]} (zc/props this)
        font   (get-in game-state [:fonts (get game-state :new-font (get (rc/get-settings game-state) :font))])]
    (zc/csx
	  [:terminal {} [
		[:group {:id :app} [
		  [:layer {:id :ui} [
			[:view {:style {:position :absolute
                            :width 80
                            :height 24
							:top 3
							:left 35}} [
			  [:text {} ["Configure Font"]]
              [:text {} [""]]
			  [:text {} [(str "Font: " (get font :name))]]
              [:text {} [""]]
			  [:text {} [
                [ruicommon/Highlight {} ["n "]]
                [:text {} ["- next font"]]]]
			  [:text {} [
                [ruicommon/Highlight {} ["p "]]
                [:text {} ["- previous font"]]]]
			  [:text {} [
                [ruicommon/Highlight {} ["s "]]
                [:text {} ["- save and apply"]]]]
			  [:text {} [
                [ruicommon/Highlight {} ["c "]]
                [:text {} ["- create new font"]]]]
			  [:text {} [
                [ruicommon/Highlight {} ["Esc "]]
                [:text {} ["- back"]]]]]]]]]]]])))

(zc/def-component CreateFont
  [this]
  (let [{:keys [game-state]} (zc/props this)
        {:keys [create-font-name
                create-font-size
                create-font-path
                create-font-error]} game-state]
    (zc/csx
	  [:terminal {} [
		[:group {:id :app} [
		  [:layer {:id :ui} [
			[:view {:style {:position :absolute
                            :width 80
                            :height 24
							:top 3
							:left 35}} [
			  [:text {} ["Create Font"]]]]
			[:view {:style {:position :absolute
							:top 4
							:left 22}} [
              [:text {} [""]]
              [:view {:style {
                       :display :flex
                       :flex-direction :row}} [
                [:text {:style {:width 5}} ["Name: "]]
                [:view {:style {:position :relative} } [
                  [zcui/Input {:value create-font-name
                               :focused true
                               :style {:cursor-fg (rcolor/color->rgb :highlight)}}]]]]]
              [:text {} [""]]
			  [:text {} [
                [:text {} [(str "Size Multiplier: " create-font-size " (")]]
                [ruicommon/Highlight {} ["+"]]
                [:text {} ["/"]]
                [ruicommon/Highlight {} ["-"]]
                [:text {} [")"]]]]
              [:text {} [""]]
			  [:text {} [(str "Path: " create-font-path)]]
              (if create-font-error
			    (zc/csx [:text {:style {:color (rcolor/color->rgb :red)}} [(str "Error: " create-font-error)]])
                (zc/csx [:text {} [""]]))]]
			[:view {:style {:position :absolute
							:top 12
							:left 30}} [
              [:view {:style {:border 1
                              :width 21
                              :height 8
                              :padding-left 1
                              :padding-top 2}} [[ruicommon/Highlight {} ["Drag Tileset Here"]]]]
              [:text {} [""]]
              [:view {:style {:margin-left 5}} [
			  [:text {} [
                [ruicommon/Highlight {} ["Enter "]]
                [:text {} ["- save"]]]]
			  [:text {} [
                [ruicommon/Highlight {} ["Esc "]]
                [:text {} ["- back"]]]]]]]]]]]]]])))

(zc/def-component EnterName
  [this]
  (let [{:keys [game-state]} (zc/props this)
        player-name (get-in game-state [:world :player :name])]
    (zc/csx
	  [:terminal {} [
		[:group {:id :app} [
		  [:layer {:id :ui} [
            [:view {:style {:width 80 :height 24 :background-color (rcolor/color->rgb :black)}} [
              [:view {:style {:top 10
                              :left 25
                              :width 50}} [
                [:view {:style {:display :flex
                                :flex-direction :row}} [
                  [:view {:style {:width 5}} [[:text {} ["Name:"]]]]
                  [zcui/Input {:value player-name
                               :focused true
                               :style {:width 30 :cursor-fg (rcolor/color->rgb :highlight)}}]]]
                [:view {:style {:height 1}}]
                [ruicommon/HotkeyLabel {:hotkey :enter :label " continue"}]]]]]]]]]]])))

(zc/def-component StartInventory
  [this]
  (let [{:keys [game-state]} (zc/props this)
        player-name      (get-in game-state [:world :player :name])
        selected-hotkeys (get-in game-state [:world :selected-hotkeys])
        start-inventory  (sg/start-inventory)]
    (zc/csx
	  [:terminal {} [
		[:group {:id :app} [
		  [:layer {:id :ui} [
            [:view {:style {:align-items :center
                            :justify-content :center
                            :position :absolute
                            :background-color (rcolor/color->rgb :black)
                            :width 80
                            :height 24
                            :left 0}} [
              [ruicommon/MultiSelect {:style {:margin-top 2
                                              :margin-bottom 2}
                               :title "Choose up to three things to take with you:"
                               :selected-hotkeys selected-hotkeys
                               :use-applicable true
                               :items start-inventory}]
              [ruicommon/HotkeyLabel {:hotkey :enter :label "continue"}]]]]]]]]])))

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
(def tidbit-index (atom 0))
(go-loop [tidbit-freqs (zipmap (range (count loading-tidbits)) (repeat 0))]
  (let [values (->> (sort-by val tidbit-freqs)
                    (partition-by val)
                    first)
        r (mod (int (/ (System/currentTimeMillis) 4000)) (count values))
        n (first (nth values r))]
    (reset! tidbit-index n)
    (async/<! (async/timeout (rand-nth [1200 600])))
    (recur (update tidbit-freqs n inc))))

(zc/def-component Loading
  [this]
  (let [{:keys [state]} (zc/props this)]
    (zc/csx 
	  [:terminal {} [
		[:group {:id :app} [
		  [:layer {:id :ui} [
            [:view {:style {:width "100%" :height "100%" :background-color (rcolor/color->rgb :black)}} [
              [:view {:style {:position :absolute
                              :top 10
                              :left 36
                              :width 80
                              :height 24}} [
                [:text {} ["Loading..."]]
                [:text {} [""]]
                [:text {:style {:left -6}} [(format "Generating %s..." (nth loading-tidbits @tidbit-index))]]
                [:text {} [""]]
                [:text {:style {:left 3}} [(nth ["/" "-" "\\" "|"] (mod (swap! loading-index inc) 4))]]]]]]]]]]]])))

(zc/def-component Connecting
  [this]
  (let [{:keys [game-state]} (zc/props this)]
    (zc/csx 
	  [:terminal {} [
		[:group {:id :app} [
		  [:layer {:id :ui} [
            [:view {:style {:width "100%" :height "100%" :background-color (rcolor/color->rgb :black)}} [
              [:view {:style {:position :absolute
                              :top 10
                              :left 36}} [
                [:text {} ["Connecting..."]]]]]]]]]]]])))

(zc/def-component ConnectionFailed
  [this]
  (let [{:keys [game-state]} (zc/props this)]
    (zc/csx 
	  [:terminal {} [
		[:group {:id :app} [
		  [:layer {:id :ui} [
            [:view {:style {:width "100%" :height "100%" :background-color (rcolor/color->rgb :black)}} [
              [:view {:style {:position :absolute
                              :top 10
                              :left 36}} [
                [:text {} ["Connection Failed."]]
                [:text {} [""]]
                [:text {} [[:text {} ["Play again? ["]]
                           [ruicommon/Highlight {} ["y"]]
                           [:text {} ["/"]]
                           [ruicommon/Highlight {} ["n"]]
                           [:text {} ["] "]]]]]]]]]]]]]])))

(zc/def-component GameOverDead
  [this]
  (let [{:keys [player cause-of-death madlib days-survived turns-survived points]} (zc/props this)
        player-name    (get player :name)
        hp             (get player :hp)
        hunger         (get player :hunger)
        max-hunger     (get player :max-hunger)
        thirst         (get player :thirst)
        max-thirst     (get player :max-thirst)
        will-to-live   (get player :will-to-live)
        cause-of-death (or
                         cause-of-death
                         (cond
                           (<= hp 0)             "massive injuries"
                           (> hunger max-hunger) "literall starving to death"
                           (> thirst max-thirst) "not drinking enough water"
                           (<= will-to-live 0)   "just giving up on life"
                           :else                 "mysterious causes"))]
    (zc/csx
	  [:terminal {} [
		[:group {:id :app} [
		  [:layer {:id :ui} [
            [:view {:style {:width "100%" :height "100%" :background-color (rcolor/color->rgb :black)}} [
              [:view {:style {:left 10}} [
                [:text {} [(format "%s: %s" player-name madlib)]]
                [:text {} [""]]
                [:text {} [(format "Points: %s." points)]]
                [:text {} [(format "Survived for %d %s. (%d turns)"
                                     days-survived
                                     (if (< 1 days-survived) "days" "day")
                                     turns-survived)]]
                [:text {} [(format "Died from %s" cause-of-death)]]
                [:text {} [""]]
                [:text {} ["Inventory:"]]
                [:view {} 
                  (map-indexed
                    (fn [idx item]
                      (zc/csx [:text {} [(format "%s%s" (if (pos? (get item :count 0))
                                                          (format "%dx " (get item :count))
                                                          "")
                                                        (item :name))]]))
                    (take 12 (get player :inventory)))]]]
                [:text {} [""]]
                [:view {:style {:left 10}} [
                  [:text {} [[:text {} ["Play again? ["]]
                             [ruicommon/Highlight {} ["y"]]
                             [:text {} ["/"]]
                             [ruicommon/Highlight {} ["n"]]
                             [:text {} ["] "]]
                             [ruicommon/Highlight {} ["space "]]
                             [:text {} ["- share and compare with other players"]]]]]]]]]]]]]])))

(zc/def-component GameOverRescued
  [this]
  (let [{:keys [player rescue-mode madlib days-survived turns-survived points]} (zc/props this)
        player-name    (get player :name)
        hp             (get player :hp)
        hunger         (get player :hunger)
        max-hunger     (get player :max-hunger)
        thirst         (get player :thirst)
        max-thirst     (get player :max-thirst)
        will-to-live   (get player :will-to-live)]
    (zc/csx
	  [:terminal {} [
		[:group {:id :app} [
		  [:layer {:id :ui} [
            [:view {:style {:width "100%" :height "100%" :background-color (rcolor/color->rgb :black)}} [
              [:view {:style {:left 10}} [
                [:text {} [(format "%s: %s" player-name madlib)]]
                [:text {} [""]]
                [:text {} [(format "Points: %s." points)]]
                [:text {} [(format "Survived for %d %s. (%d turns)"
                                     days-survived
                                     (if (> 1 days-survived) "days" "day")
                                     turns-survived)]]
                [:text {} [(format "Rescued by %s after surviving for %d days." rescue-mode days-survived)]]
                [:text {} [""]]
                [:text {} ["Inventory:"]]
                [:view {} 
                  (map-indexed
                    (fn [idx item]
                      (zc/csx [:text {} [(format "%s%s" (if (pos? (get item :count 0))
                                                          (format "%dx " (get item :count))
                                                          "")
                                                        (item :name))]]))
                    (get player :inventory))]]]
                [:text {} [""]]
                [:view {:style {:left 10}} [
                  [:text {} [[:text {} ["Play again? ["]]
                             [ruicommon/Highlight {} ["y"]]
                             [:text {} ["/"]]
                             [ruicommon/Highlight {} ["n"]]
                             [:text {} ["] "]]
                             [ruicommon/Highlight {} ["space "]]
                             [:text {} ["- share and compare with other players"]]]]]]]]]]]]]])))

(zc/def-component GameOver
  [this]
  (let [{:keys [game-state]} (zc/props this)
        cur-state      (current-state game-state)
        points         (rs/state->points game-state)
        turns-survived  (get-time game-state)
        turns-per-day   (count (get-in game-state [:data :atmo]))
        days-survived   (int (/ turns-survived turns-per-day))
        player-name     (get-in game-state [:world :player :name])
        madlib          (gen-end-madlib game-state)]
    (case cur-state
      :game-over-dead
        (zc/csx [GameOverDead {:player (get-in game-state [:world :player])
                               :days-survived days-survived
                               :turns-survived turns-survived
                               :points points
                               :cause-of-death (get-in game-state [:world :cause-of-death])
                               :madlib madlib}])
      :game-over-rescued
        (zc/csx [GameOverRescued {:player (get-in game-state [:world :player])
                                  :days-survived days-survived
                                  :turns-survived turns-survived
                                  :points points
                                  :rescue-mode (rendgame/rescue-mode game-state)
                                  :madlib madlib}]))))

(zc/def-component Privacy
  [this]
  (let [{:keys [game-state]} (zc/props this)
        privacy-scroll (get game-state :privacy-scroll)]
    (zc/csx
	  [:terminal {} [
		[:group {:id :app} [
		  [:layer {:id :ui} [
            [:view {:style {:width "100%" :height "100%" :background-color (rcolor/color->rgb :black)}} [
              [:text {} ["Privacy Policy"]]
              [:text {} [""]]
              [:text {} ["Your privacy is important to us. It is Robinson's policy to respect your privacy regarding any information we may collect from you. We only ask for personal information when we truly need it to provide a service to you. We collect it by fair and lawful means, with your knowledge and consent. We also let you know why we’re collecting it and how it will be used. We only retain collected information for as long as necessary to provide you with your requested service. What data we store, we’ll protect within commercially acceptable means to prevent loss and theft, as well as unauthorised access, disclosure, copying, use or modification. We don’t share any personally identifying information publicly or with third-parties, except when required to by law. You are free to refuse our request for your personal information, with the understanding that we may be unable to provide you with some of your desired services."]]
              [:text {} [""]]
              [:text {} ["This policy is effective as of 14 January 2019."]]
              [:text {} [""]]
              [:text {} [[:text {} ["Accept? ["]]
                         [ruicommon/Highlight {} ["y"]]
                         [:text {} ["/"]]
                         [ruicommon/Highlight {} ["n"]]
                         [:text {} ["] "]]]]]]]]]]]])))

(zc/def-component ShareScore
  [this]
  (let [{:keys [game-state]} (zc/props this)
        score      (get game-state :last-score)
        top-scores (get game-state :top-scores [])
        top-10-scores (take 10 (concat top-scores (repeat nil)))]
    (zc/csx
	  [:terminal {} [
		[:group {:id :app} [
		  [:layer {:id :ui} [
            [:view {:style {:width "100%" :height "100%" :background-color (rcolor/color->rgb :black)}} [
              [:view {:style {:top 2
                              :left 2
                              :display :flex
                              :flex-direction :row}} [
                 ;; Title
                 [:text {} ["Top scores"]]
                 [:view {:style {:position :absolute :top 3 :left 0}} [
                   [ruicommon/ItemList {:items (map-indexed
                                       (fn [index score]
                                         (if score
                                           (let [player-name    (get score "player-name" "?name?")
                                                 points         (get score "points" 0)
                                                 days-survived  (get score :days-survived 0 )
                                                 turns-survived (get score :turns-survived 0 )
                                                 fg (rcolor/color->rgb (if (= player-name (get (rp/get-player game-state) :name))
                                                                         :highlight
                                                                         :white))]
                                             {:s (format "*%2d %-20s (%5d points)" (inc index) player-name points)
                                              :fg fg
                                              :bg (rcolor/color->rgb :black)})
                                             {:s "..."
                                              :fg (rcolor/color->rgb :white)
                                              :bg (rcolor/color->rgb :black)
                                              :style nil}))
                                       top-10-scores)}]
                   [:view {:style {:top 1 :left 0}} [
                     [:text {} [[:text {} ["Play again? ["]]
                                [ruicommon/Highlight {} ["y"]]
                                [:text {} ["/"]]
                                [ruicommon/Highlight {} ["n"]]
                                [:text {} ["] "]]]]]]]]
                 [:view {:style {:position :absolute :top 0 :left 40}} [
                   [:text {} ["Performance"]]
                   [:view {:style {:position :absolute :top 3 :left 0}} [
                     [Histogram {:title "Points"
                                 :value     (get game-state :points)
                                 :histogram (get game-state :point-data)}]]]
                   [:view {:style {:position :absolute :top 3 :left 15}} [
                     [Histogram {:title "Turns"
                                 :value     (rw/get-time game-state)
                                 :histogram (get game-state :time-data)}]]]
                   [:view {:style {:position :absolute :top 11 :left 0}} [
                     [Histogram {:title "Kills"
                                 :value     (reduce + 0 (map second (get-in game-state [:world :player :stats :num-animals-killed])))
                                 :histogram (get game-state :kills-data)}]]]
                   [:view {:style {:position :absolute :top 11 :left 15}} [
                     [Histogram {:title "Items Crafted"
                                 :value     (reduce + 0 (map second (get-in game-state [:world :player :stats :num-items-crafted])))
                                 :histogram (get game-state :crafted-data)}]]]
                   [:view {:style {:position :absolute :top 19 :left 0}} [
                     [:text {} [
                       [:text {} ["Your Performance "]]
                       [ruicommon/Highlight {} ["^"]]]]]]]]]]]]]]]]]])))

(zc/def-component RexPaintFromData
  [this]
  (let [{:keys [game-state data-key]} (zc/props this)
        data (get-in game-state [:data data-key])]
    (zc/csx
	  [:terminal {} [
		[:group {:id :app} [
		  [:layer {:id :ui} [
            [:view {} [
              [zcui/RexPaintImage {:data data}]]]]]]]]])))

(zc/def-component KeyboardControlsHelp
  [this]
  (let [{:keys [game-state]} (zc/props this)]
    (zc/csx
	  [RexPaintFromData {:game-state game-state :data-key :keyboard-controls}])))


(zc/def-component UIHelp
  [this]
  (let [{:keys [game-state]} (zc/props this)]
    (zc/csx
	  [RexPaintFromData {:game-state game-state :data-key :ui}])))

(zc/def-component GameplayHelp
  [this]
  (let [{:keys [game-state]} (zc/props this)]
    (zc/csx
	  [RexPaintFromData {:game-state game-state :data-key :gameplay}])))

(zc/def-component FullLog
  [this]
  (let [{:keys [game-state]} (zc/props this)
        log (get-in game-state [:world :log])]
    (zc/csx
	  [:terminal {} [
		[:group {:id :app} [
		  [:layer {:id :ui} [
            [:view {:style {:width "100%" :height "100%" :background-color (rcolor/color->rgb :black)}} [
              [:view {}
                ;; debug log
                #_(for [{:keys [text color] :as log-item} (reverse (take 23 log))]
                  (zc/csx [:text {:style {:color (rcolor/color->rgb color)}} [(str (get log-item :time 0) " "  text)]]))
                (for [{:keys [text color]} (reverse (take 23 log))]
                  (zc/csx [:text {:style {:color (rcolor/color->rgb color)}} [(str text)]]))]]]]]]]]])))

(zc/def-component Robinson
  [this]
  (let [game-state (get (zc/props this) :game-state)]
    (assert (not (nil? game-state)))
    (log/debug "render current-state" (current-state game-state))
    (cond
      (= (current-state game-state) :start)
        (zc/csx [Start {}])
      (= (current-state game-state) :configure)
        (zc/csx [Configure {:game-state game-state}])
      (= (current-state game-state) :configure-font)
        (zc/csx [ConfigureFont {:game-state game-state}])
      (= (current-state game-state) :create-font)
        (zc/csx [CreateFont {:game-state game-state}])
      (= (current-state game-state) :enter-name)
        (zc/csx [EnterName {:game-state game-state}])
      (= (current-state game-state) :start-inventory)
        (zc/csx [StartInventory {:game-state game-state}])
      (= (current-state game-state) :loading)
        (zc/csx [Loading {:game-state game-state}])
      (= (current-state game-state) :connecting)
        (zc/csx [Connecting {:game-state game-state}])
      (= (current-state game-state) :connection-failed)
        (zc/csx [ConnectionFailed {:game-state game-state}])
      ;; Is player dead?
      (contains? #{:game-over-dead :game-over-rescued} (current-state game-state))
        ;; Render game over
        (zc/csx [GameOver {:game-state game-state}])
      (= (get-in game-state [:world :current-state]) :privacy)
        (zc/csx [Privacy {:game-state game-state}])
      (= (get-in game-state [:world :current-state]) :share-score)
        (zc/csx [ShareScore {:game-state game-state}])
      (= (get-in game-state [:world :current-state]) :help-controls)
        (zc/csx [KeyboardControlsHelp {:game-state game-state}])
      (= (get-in game-state [:world :current-state]) :help-ui)
        (zc/csx [UIHelp {:game-state game-state}])
      (= (get-in game-state [:world :current-state]) :help-gameplay)
        (zc/csx [GameplayHelp {:game-state game-state}])
      (= (get-in game-state [:world :current-state]) :log)
        (zc/csx [FullLog {:game-state game-state}])
      :else
        (zc/csx [Map {:game-state game-state}]))))
)
