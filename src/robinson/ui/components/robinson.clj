;; Functions for rendering state to screen
(ns robinson.ui.components.robinson
  (:require 
            [taoensso.timbre :as log]
            [robinson.random :as rr]
            [robinson.math :as rmath]
            [robinson.color :as rcolor]
            [robinson.renderutil :as rutil]
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
            [robinson.fx :as rfx]
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
            [robinson.noise :as rnoise]
            [robinson.traps :as rt]
            [robinson.ui.updater :as ruu]
            [zaffre.terminal :as zat]
            [zaffre.animation.wrapper :as zaw]
            [zaffre.components :as zc]
            [zaffre.components.ui :as zcui]
            [zaffre.util :as zutil]
            [tinter.core :as tinter]
            clojure.set
            [clojure.xml :as xml]
            [clojure.zip :as zip]
            [clojure.pprint :as pprint]
            clojure.string))

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
(zc/def-component Highlight
  [this]
  (let [{:keys [children]} (zc/props this)]
    (zc/csx
      [:text {:style {:color [229 155 8 255]}} children])))


(zc/def-component ActionPrompt
  [this]
  (let [{:keys [children]} (zc/props this)]
    (zc/csx
      [:view {:style {:position :fixed :top 0 :left 0 :hight 1}} children])))

(zc/def-component ItemList
  [this]
  (let [{:keys [items]} (zc/props this)]
    (zc/csx
      [:view {}
          (map (fn [{:keys [fg bg s]}]
            (zc/csx [:view {:style {:fg fg :bg bg}} [
                      [:text {} [s]]]]))
            items)])))

(zc/def-component SelectItemListItem
  [this]
  (let [{:keys [hotkey selected text]} (zc/props this)]
    (zc/csx
      [:text {} [
        [Highlight {} [(str hotkey " ")]]
        [:text {} [(format "%s %s"
                     (if selected
                       "+"
                       "-")
                     text)]]]])))

(zc/def-component SelectItemList
  [this]
  (let [{:keys [title selected-hotkeys use-applicable items]} (zc/props this)]
    (zc/csx [:view {:style {:width "60%"}} [
              [:text {} [title]]
              [:view {:style {:top 2 :left 10}} 
                     (map (fn [item]
                            (zc/csx [SelectItemListItem {:hotkey 
                                                           (or (item :hotkey)
                                                               \ )
                                                         :selected
                                                           (contains? selected-hotkeys (get item :hotkey))
                                                         :text
                                                           (str (get item :name)
                                                             (if (contains? item :count)
                                                               (format " (%dx)" (int (get item :count)))
                                                               ""))}]))
                          items)]
              [:text {:style {:top 5 :left 10}} [
                [Highlight {} ["enter "]]
                [:text {} ["to continue"]]]]]])))

(zc/def-component Atmo
  [this]
  (let [{:keys [time atmo-data]} (zc/props this)
        frames (count atmo-data)
        t      (mod time frames)
        frame  (nth atmo-data t)
        indexed-colors (cons (repeat 7 [0 0 0]) (apply map vector (partition 3 frame)))]
    (zc/csx
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
      [:view {:style (merge {:width 7 :background-color gray-color} style)} [
      [:text {:style {:background-color gray-color}} [
        [:text {} [" "]]
        [:text {:style {:color wounded-color}} ["\u2665"]]
        [:text {} [" "]]
        [:text {:style {:color poisoned-color}} ["\u2665"]]
        [:text {} [" "]]
        [:text {:style {:color infected-color}} ["\u2665"]]
        [:text {} [" "]]]]]])))

    ;    (int (-> state :world :player :hp))
    ;    (-> state :world :player :max-hp)
    ;    (apply str (interpose " " (-> state :world :player :status)))
(zc/def-component DblBarChart
  [this]
  (let [{:keys [fg-1 fg-2 p1 p2 width direction style]} (zc/props this)
        root (* width (min p1 p2))
        padding (* width (- 1 (max p1 p2)))
        diff (- width root padding)
        black-color (rcolor/color->rgb :black)]
      (case direction
        :left
          (zc/csx
              [:text {:style style} [
                #_[:text {} [(format "%.2f %.2f" (float p1) (float p2))]]
                [:text {:style {:color black-color
                                :background-color black-color}} [(apply str (repeat padding " "))]]
                [:text {:style {:color            (if (< p2 p1) fg-1 black-color)
                                :background-color (if (> p2 p1) fg-2 black-color)}} [(apply str (repeat diff "\u2584"))]]
                [:text {:style {:color fg-1 :background-color fg-2}} [(apply str (repeat root "\u2584"))]]]])
        :right
          (zc/csx
              [:text {:style style} [
                #_[:text {} [(format "%.2f %.2f" (float p1) (float p2))]]
                [:text {:style {:color fg-1 :background-color fg-2}} [(apply str (repeat root "\u2584"))]]
                [:text {:style {:color            (if (< p2 p1) fg-1 black-color)
                                :background-color (if (> p2 p1) fg-2 black-color)}} [(apply str (repeat diff "\u2584"))]]
                [:text {:style {:color black-color
                                :background-color black-color}} [(apply str (repeat padding " "))]]]]))))

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
      [:view {:style {:top 20
                      :left 0
                      :width 80
                      :display :flex
                      :align-items :center}} [
         [Atmo {:time (rw/get-time game-state)
                :atmo-data (get-in game-state [:data :atmo])}]
         [:view {} [
           [HpWtlBars {:game-state game-state :style {:left 1}}]
           [StatusUI {:game-state game-state :style {:left 37 :bottom 1}}]
           [ThirstHungerBars {:game-state game-state :style {:left (+ 37 7) :bottom 2}}]]]]])))

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

(zc/def-component MultiSelect
  [this]
  (let [{:keys [style]} (zc/props this)
        default-props {:selected-hotkeys []
                       :items []
                       :disable? (fn [_] false)}
        {:keys [title selected-hotkeys items disable?]} (merge default-props (zc/props this))
        children (map (fn [item]
                        (zc/csx [:text {:style {:color (rcolor/color->rgb (if (or (not (disable? item))
                                                                                  (get item :applicable))
                                                                            :black
                                                                            :gray))
                                                :background-color (rcolor/color->rgb :white)}} [
                                  [Highlight {} [(str (or (item :hotkey)
                                                          \space))]]
                                  [:text {} [(format "%c%s%s %s %s"
                                               (if (contains? (set selected-hotkeys) (get item :hotkey))
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
                                                   ""))]]]]))
                      items)]
    (zc/csx [:view {:style (or style {})} (concat [(zc/csx [:text {:style {:color (rcolor/color->rgb :black)}} [title]])
                               (zc/csx [:text {} [""]])]
                              children)])))


(zc/def-component CraftSubmenu
  [this]
  (let [{:keys [game-state recipe-type]} (zc/props this)
        selected-recipe-path (get-in game-state [:world :craft-recipe-path])
        hotkey               (when selected-recipe-path
                               (last selected-recipe-path))
        recipes              (get (get-recipes game-state) recipe-type)]
    (log/info hotkey)
    (zc/csx [:view {:style {:width 40
                            :height 10
                            :padding 1
                            :bottom 1
                            :display :flex
                            :flex-direction :row
                            :align-itmes :stretch
                            :background-color (rcolor/color->rgb :white)}} [
              [:view {:style {:flex 1}} [
                [MultiSelect {:style {:width 30}
                              :title " Types"
                              :selected-hotkeys [hotkey]
                              :items (map (fn [recipe]
                                             (log/info (get recipe :hotkey))
                                             {:name (get recipe :name) :hotkey (get recipe :hotkey)})
                                          recipes)}]]]
              [:view {:style {:flex 1}} [
                ;; render recipe-info
                (if hotkey
                  (let [matching-recipes   (filter (fn [recipe] (= (get recipe :hotkey) hotkey))
                                                   recipes)
                        recipe             (get (first matching-recipes) :recipe)
                        exhaust            (get recipe :exhaust [])
                        have               (get recipe :have-or [])
                        inventory-id-freqs (rp/inventory-id-freqs game-state)]
                    (log/info "exhaust" exhaust "have" have)
                    (zc/csx [ItemList {:items
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
                                            (map (fn [id] {:s (id->name id) :fg :black :bg :white :style #{}}) have)))}]))
                                          [:text {} ["Select a recipe"]])]]]])))
 

; FIXME render raft
; ; FIXME render raft
(comment
   ;; shade character based on visibility, harvestableness, raft
   shaded-out-char (cond
                     (not= (cell :discovered) current-time)
                       (-> out-char
                         (update-in [1] (fn [c] (rcolor/rgb->mono (rcolor/darken-rgb c 0.18))))
                         (update-in [2] (fn [c] (rcolor/rgb->mono (rcolor/darken-rgb c 0.15)))))
                     (contains? cell :harvestable)
                       (let [[chr fg bg] out-char]
                         [chr bg (rcolor/night-tint (rcolor/color->rgb fg) d)])
                     (contains? (set (map :id cell-items)) :raft)
                       (let [[chr fg bg] out-char]
                         (log/info "raft-cell" out-char cell-items)
                         (if (> (count cell-items) 1)
                           [chr fg (rcolor/color->rgb :brown)]
                           [\u01c1 (rcolor/color->rgb :black) (rcolor/color->rgb :brown)]))
                     :else
                       out-char))

(defn fill-put-string-color-style-defaults
  ([string]
    (fill-put-string-color-style-defaults 0 0 string :white :black #{}))
  ([wx wy string]
    (fill-put-string-color-style-defaults wx wy string :white :black #{}))
  ([wx wy string fg]
    (fill-put-string-color-style-defaults wx wy string fg :black #{}))
  ([wx wy string fg bg]
    (fill-put-string-color-style-defaults wx wy string fg bg #{}))
  ([wx wy string fg bg styles]
   {:pre [(clojure.set/superset? #{:underline :bold} styles)]}
   (let [new-fg (rcolor/color->rgb (if (has-palette? fg)
                                     (cell-type->color wx wy fg)
                                     fg))
         bg     (rcolor/color->rgb bg)]
     {:c string :fg new-fg :bg bg})))

(defn render-cell [cell wx wy current-time]
  {:post [(char? (get % :c))
          (vector? (get % :fg))
          (vector (get % :bg))]}
  (let [cell-items (get cell :items)
        in-view? (= current-time (get cell :discovered 0))
        has-been-discovered? (> (get cell :discovered 0) 1)
        harvestable? (contains? cell :harvestable)
        apply? (fn [pred f]
                 (if pred
                   f
                   identity))]
    (if (or in-view? has-been-discovered?)
      (->
        (apply fill-put-string-color-style-defaults
          wx wy
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
              (case (cell :type)
               :floor           [\·]
               :open-door       [\-  :brown  :black #{:bold}]
               :close-door      [\+  :brown  :black #{:bold}]
               :corridor        [\#] 
               :down-stairs     [\>] 
               :up-stairs       [\<] 
               :fire            [\u2240 (if (= (cell :discovered) current-time)
                                           :fire
                                           :red) :black] ;; ≀ 
               :water           [\u2248 (if (= (cell :discovered) current-time)
                                           :water
                                           :blue) :black] ;; ≈ 
               :surf            [\~ (if (= (cell :discovered) current-time)
                                       :surf
                                       :light-blue) :black]
               :shallow-water   [\~ (if (= (cell :discovered) current-time)
                                       :shallow-water
                                       :light-blue) :black]
               :swamp           [\~ (if (= (cell :discovered) current-time)
                                       :swamp
                                       :light-blue) :black]
               :lava            [\~ (if (= (cell :discovered) current-time)
                                       :lava
                                       :light-blue) :black]
               :mountain        [\u2206 :gray :black] ;; ∆
               :sand            [\·  :beige      :black]
               :dirt            [\·  :brown      :black]
               :dune            [\u1d16  :light-brown :black] ;; ᴖ
               :rocky-shore     [\u1d16  :dark-gray  :black] ;; ᴖ
               :gravel          [\·  :gray       :black]
               :short-grass     [\·  :green      :black]
               :tall-grass      [\" :dark-green :black]
               :tree            [\T  :dark-green :black]
               :bamboo          [\u01c1 :light-green :black] ;; ∥ 
               :palisade        [\# :brown :black]
               :ramada          [\# :beige :black]
               :tarp-shelter    [\# :blue  :black]
               :lean-to         [\# :light-green :black]
               :campfire        [\^ :brown :black]
               :bamboo-water-collector
                                (if (< 10 (get cell :water 0))
                                  [\O :bamboo-water-collector :black]
                                  [\O])
               :solar-still
                                (if (< 10 (get cell :water 0))
                                  [\O :solar-still :black]
                                  [\O])
               :palm-tree       [\7  :dark-green :black]
               :fruit-tree      [\u2648  :light-green :black] ;; ♈
               :freshwater-hole (if (< 10 (get cell :water 0))
                                  [\~ :freshwater-hole :black]
                                  [\O])
               :saltwater-hole  (if (< 10 (get cell :water 0))
                                  [\~ :saltwater-hole :black]
                                  [\O])
               :dry-hole        [\O]
               ;; pirate ship cell types
               :bulkhead        [\◘ :brown :black]
               :wheel           [\○ :dark-brown :black]
               :bulkhead2       [\◘ :brown :black]
               :wooden-wall     [\# :ship-brown :black]
               :railing         [\# :ship-brown :black]
               :hammock-v       [\) :brown :black]
               :hammock-h       [\- :brown :black]
               :deck            [\· :dark-brown :black]
               :canon-breach    [\║ :gray :dark-brown]
               :tackle          [\º :brown :black]
               :canon           [\║ :gray :black]
               :grate           [\╬ :dark-beige :black]
               :table           [\╤ :ship-light-brown :black]
               :chair           [\╥ :ship-light-brown :black]
               :mast            [\╨ :ship-light-brown :black]
               :beam            [\═ :brown :black]
               :canon-truck-1   [\▄ :dark-brown :black]
               :locker          [\▌ :brown :black]
               :locker2         [\▐ :brown :black]
               :canon-truck-2   [\▀ :dark-brown :black]
               :ships-wheel     [\Φ :brown :black]
               :ladder          [\≡ :dark-beige :black]
               :porthole        [\° :brown :black]
               :chest           [\■ :ship-dark-brown :black]
               :artifact-chest  [\■ :dark-beige :black]
               ;; ruined temple cell types
               :vertical-wall   [\║ :temple-beige :black]
               :horizontal-wall [\═ :temple-beige :black]
               :vertical-wall-alt [\° :white :black]
               :horizontal-wall-alt [\° :white :black]
               :upper-left-1    [\╔ :temple-beige :black]
               :upper-right-1   [\╗ :temple-beige :black]
               :bottom-left-1   [\╚ :temple-beige :black]
               :bottom-right-1  [\╝ :temple-beige :black]
               :upper-left-2    [\◙ :temple-beige :black]
               :upper-right-2   [\◙ :temple-beige :black]
               :bottom-left-2   [\◙ :temple-beige :black]
               :bottom-right-2  [\◙ :temple-beige :black]
               :altar           [\┬ :white :black]
               :vine            [\⌠ :moss-green :black]
               :moss-corridor   [\# :moss-green :black]
               :moss-vertical-wall  [\║ :moss-green :black]
               :moss-horizontal-wall [\═ :moss-green :black]
               :moss-vertical-wall-alt [\° :white :black]
               :moss-horizontal-wall-alt [\° :white :black]
               :moss-upper-left-1 [\╔ :moss-green :black]
               :moss-upper-right-1 [\╗ :moss-green :black]
               :moss-bottom-left-1 [\╚ :moss-green :black]
               :moss-bottom-right-1 [\╝ :moss-green :black]
               :moss-upper-left-2 [\◙ :moss-green :black]
               :moss-upper-right-2 [\◙ :moss-green :black]
               :moss-bottom-left-2 [\◙ :moss-green :black]
               :moss-bottom-right-2 [\◙ :moss-green :black]
               :white-corridor  [\# :white :black]
               :white-vertical-wall   [\║ :white :black]
               :white-horizontal-wall [\═ :white :black]
               :white-vertical-wall-alt [\° :white :black]
               :white-horizontal-wall-alt [\° :white :black]
               :white-upper-left-1 [\╔ :white :black]
               :white-upper-right-1 [\╗ :white :black]
               :white-bottom-left-1 [\╚ :white :black]
               :white-bottom-right-1 [\╝ :white :black]
               :white-upper-left-2 [\◙ :white :black]
               :white-upper-right-2 [\◙ :white :black]
               :white-bottom-left-2 [\◙ :white :black]
               :white-bottom-right-2 [\◙ :white :black]
               :empty                [\space :black :black]
               :crushing-wall-trigger
                                 (if (get cell :trap-found)
                                   [\^]
                                   [\·])
               :wall-darts-trigger
                                 (if (get cell :trap-found)
                                   [\^]
                                   [\·])
               :poisonous-gas-trigger
                                 (if (get cell :trap-found)
                                   [\^]
                                   [\·])
               :spike-pit
                                 (if (get cell :trap-found)
                                   [\^]
                                   [\·])
               :snakes-trigger
                                 (if (get cell :trap-found)
                                   [\^]
                                   [\·])
          
               (do (log/info (format "unknown type: %s %s" (str (get cell :type)) (str cell)))
               [\?])))))
        ((apply? (not in-view?)
           (fn [{:keys [c fg bg]}] {:c c :fg (rcolor/rgb->mono fg) :bg (rcolor/rgb->mono bg)})))
        ((apply? (and in-view? harvestable?)
           (fn [{:keys [c fg bg]}] {:c c :fg bg :bg fg}))))
      {:c \  :fg [0 0 0 0] :bg [0 0 0 0]})))

(zc/def-component MapInViewport
  [this]
  (let [{:keys [cells current-time]} (zc/props this)]
    (zc/csx [:img {:width 80 :height 23}
                  (map-indexed (fn [y line]
                            (map-indexed (fn [x cell]
                              (render-cell cell x y current-time))
                              line))
                          cells)])))

(defn render-npc [npc current-time]
  (apply fill-put-string-color-style-defaults
    0 0
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
        :human           [\@ :white :black]))))

(defn render-player [player current-time]
  (apply fill-put-string-color-style-defaults
    (rcolor/color-bloodied-char 
      (< current-time (get player :bloodied 0))
      ["@" :white :black])))
   ; TODO: draw raft? draw fishing pole?
        #_(if (contains? (set (map :id (get (first (player-cellxy state)) :items))) :raft)
          :brown
          :black)
        ;; if character is fishing, draw pole
        #_(condp = (current-state state)
          :fishing-left  (put-string screen :ui (dec (-> state :world :player :pos :x))
                                            (-> state :world :player :pos :y)
                                            "\\"
                                            :white :black)
          :fishing-right (put-string screen :ui (inc (-> state :world :player :pos :x))
                                            (-> state :world :player :pos :y)
                                            "/"
                                            :white :black)
          :fishing-up    (put-string screen :ui (-> state :world :player :pos :x)
                                            (dec (-> state :world :player :pos :y))
                                            "/"
                                            :white :black)
          :fishing-down  (put-string screen :ui (-> state :world :player :pos :x)
                                            (inc (-> state :world :player :pos :y))
                                            "\\"
                                            :white :black))
  
(zc/def-component CharactersInViewport
  [this]
  (let [{:keys [npcs player-pos player-bloodied vx vy current-time]} (zc/props this)]
    (zc/csx [:view {}
                   (reduce (fn [children npc]
                             (let [x         (-> npc :pos :x)
                                   y         (-> npc :pos :y)
                                   ;targeted? (when (= (current-state state) :select-ranged-target)
                                   ;            (let [target-ranged-index (get-in state [:world :target-ranged-index])
                                   ;                  target-ranged-pos-coll (get-in state [:world :target-ranged-pos-coll])
                                   ;                  target-pos             (nth target-ranged-pos-coll target-ranged-index)]
                                   ;              (= target-pos (get npc :pos))))
                                   ;t       (rw/get-time state)
                                   {:keys [c fg bg]} (render-npc npc current-time)]
                               ;(log/debug "npc@" x y "visible?" visible)
                               (conj children (zc/csx [:text {:style {:position :fixed
                                                                      :top (- y vy)
                                                                      :left (- x vx)
                                                                      :color fg
                                                                      :background-color bg}} [(str c)]]))))
                           ; Always render player
                           [(zc/csx [:text {:style {:position :fixed
                                                    :top (- (get player-pos :y) vy)
                                                    :left (- (get player-pos :x) vx)
                                                    :color (rcolor/color->rgb (if player-bloodied :dark-red :white))
                                                    :background-color (rcolor/color->rgb :black)}}
                                           ["@"]])]
                           npcs)])))

(zc/def-component HighlightNpcs
  [this]
  (let [{:keys [visible-npcs vx vy start-pos end-pos]} (zc/props this)
        [start-x start-y] (rc/pos->xy start-pos)
        [end-x end-y] (rc/pos->xy end-pos)
        points (set (rlos/line-segment-fast-without-endpoints
                      [start-x start-y]
                      [end-x end-y]))]
    (log/info "Highlight npcs" visible-npcs)
    (zc/csx [:view {}
                   (reduce (fn [children npc]
                             (let [sx         (- (-> npc :pos :x) vx)
                                   sy         (- (-> npc :pos :y) vy)
                                   targeted? (contains? points [sx sy])
                                   {:keys [c fg bg]} (render-npc npc 0)]
                               (log/info "npc@" sx sy)
                               (if targeted?
                                 (conj children (zc/csx [:text {:style {:position :fixed
                                                                        :top sy
                                                                        :left sx
                                                                        :color (rcolor/color->rgb :black)
                                                                        :background-color (rcolor/color->rgb :green)}} [(str c)]]))
                                 children)))
                           []
                           visible-npcs)])))

(zc/def-component CrushingWall
  [this]
  (let [{:keys [trap]} (zc/props this)]
    nil))
#_(defn render-crushing-wall
  [screen trap]
  (let [ch (case (get trap :direction)
             :up    \╧
             :down  \╤
             :left  \╢
             :right \╟)]
    (put-chars screen :features (for [[x y] (first (get trap :locations))]
                                  {:x x :y y :c ch :fg [90 90 90] :bg [0 0 0]}))))

(zc/def-component PoisonousGas
  [this]
  (let [{:keys [trap]} (zc/props this)]
    nil))
#_(defn render-poisonous-gas
  [screen state trap]
  (let [current-time (rw/get-time state)]
    (doseq [[x y] (keys (get trap :locations))]
      (when (= (get (rw/get-cell state x y) :discovered) current-time)
        (let [bg (rcolor/color->rgb (rand-nth [:beige :temple-beige :light-brown]))]
          (set-bg! screen :map x y bg))))))

(zc/def-component Traps
  [this]
  (let [{:keys [game-state]} (zc/props this)
        traps (rw/current-place-traps game-state)]
    (zc/csx [:view {}
      (map (fn [trap]
             (case (get trap :type)
               :crushing-wall
                 (zc/csx [CrushingWall trap])
               :poisonous-gas
                 (zc/csx [PoisonousGas trap])
               (zc/csx [:view {} []])))
            traps)])))

   ; TODO: draw raft? draw fishing pole?
        #_(if (contains? (set (map :id (get (first (player-cellxy state)) :items))) :raft)
          :brown
          :black)

(zc/def-component CharacterFx
  [this]
  (let [{:keys [fx vx vy]} (zc/props this)
        {:keys [pos ch color background-color]} fx
        {:keys [x y]} pos]
    (log/info "rendering CharacterFX" y vy x vx)
    (zc/csx [:text {:style {:position :fixed :top (- y vy) :left (- x vx)
                            :color (or color (rcolor/color->rgb :white))
                            :background-color (or background-color (rcolor/color->rgb :black))}} [(str ch)]])))

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
    (zc/csx [:view {}
      (map (fn [[effect-id effect]]
             (log/info "rendering effect " effect)
             (case (get effect :type)
               :character-fx
                 (let [[x y] (rc/pos->xy (get effect :pos))]
                   (zc/csx [CharacterFx {:fx effect :vx vx :vy vy }]))
               (zc/csx [:view {} []])))
            fx #_[{:type :character-fx :pos {:x -18 :y -239} :ch \-}])])))

(defn light-producing?
  [cell-type]
  (contains? #{:fire :lava} cell-type))

(defn render-lighting [cell distance sight-distance lantern-on]
  (if (light-producing? (get cell :type))
    [0 0 0 0]
    (if lantern-on
      (let [r1 (rnoise/noise3d palette-noise 0 0 (mod (/ (System/currentTimeMillis) 1000) 10000))
            r2 (rnoise/noise3d palette-noise 0 0 (mod (/ (System/currentTimeMillis) 100) 10000))
            r (max 0 (min 1 (+ 0.4 (* 0.5 r1) (* 0.1 r2))))]
        #_(log/info r a)
        [(* r 255) (* r 204) (* r 124) (min 255 (* 255 (/ distance (+ 1 (* r sight-distance))))) ])
      (rcolor/lighting sight-distance))))

(zc/def-component ShadeImg
  [this]
  (let [{:keys [cells current-time player-screen-pos sight-distance lantern-on]} (zc/props this)]
    (zc/csx [:img {:width 80 :height 23 :style {:mix-blend-mode :multiply}}
                  (map-indexed (fn [y line]
                            (map-indexed (fn [x cell]
                              (if (= (get cell :discovered 0) current-time)
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
               (zc/csx [:view {} [
                 [:text {:style {:position :fixed :top y :left x
                                 :color (or color [0 255 0 255])
                                 :background-color (or background-color [0 0 0 0])}} [(or (str ch) "*")]]]]))
        (rlos/line-segment-fast-without-endpoints
          [start-x start-y]
          [end-x end-y]))])))

(zc/def-component Cursor
  [this]
  (let [{:keys [pos]} (zc/props this)
        [x y] (rc/pos->xy pos)
        [color background-color]   (if (< (mod (/ (System/currentTimeMillis) 300) 2) 1)
                                     [[0 0 0 0] [0 0 0 0]]
                                     [(rcolor/color->rgb :highlight 255)
                                      (rcolor/color->rgb :black 255)])]
    (zc/csx [:view {} [
              [:text {:style {:position :fixed :top y :left x
                              :color color
                              :background-color background-color}} ["\u2592"]]]])))

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
                           :position :fixed
                           :left 40
                           :top 1
                           :padding 1
                           :background-color (rcolor/color->rgb :white)}} [
            [MultiSelect {:title "Pick up"
                          :items (concat (translate-identified-items game-state items)
                                         [{:name "All" :hotkey \space}])}]]])))

(zc/def-component Inventory
  [this]
  (let [{:keys [game-state]} (zc/props this)
        player-items (-> game-state :world :player :inventory)]
   (zc/csx [:view {:style {:width 40
                           :height 20
                           :position :fixed
                           :left 40
                           :top 1
                           :padding 1
                           :background-color (rcolor/color->rgb :white)}} [
            [MultiSelect {:title "Inventory"
                          :items (translate-identified-items game-state player-items)}]]])))

(zc/def-component Abilities
  [this]
  (let [{:keys [game-state]} (zc/props this)
        abilities (rp/player-abilities game-state)
        height (if (seq abilities)
                 (+ 3 (* 3 (count abilities)))
                 4)]
    (zc/csx [:view {:style {:width 43
                            :height height
                            :position :fixed
                            :left 0
                            :top 0
                            :background-color (rcolor/color->rgb :white)}} [
            (if (seq abilities)
              (zc/csx [zcui/Popup {} [
                        [:view {:style {:border 2}} [
                          [MultiSelect {:title "Abilities"
                                        :items abilities}]]]]])
               #_{:s (format "    %s" (get ability :description)) :fg :black :bg :white :style #{}}
               #_[{:s "" :fg :black :bg :white :style #{}}
                  {:s "Select hotkey or press <color fg=\"highlight\">Esc</color> to exit." :fg :black :bg :white :style #{}}]
              (zc/csx [zcui/Popup {} [
                [:view {} [
                  [:text {} ["No abilities."]]
                  [:text {} [""]]
                  [:text {} [
                    [:text {} ["Press "]]
                    [Highlight {} ["Esc "]]
                    [:text {} ["to exit."]]]]]]]]))]])))

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
    (zc/csx [zcui/Popup {:style {:top 5}} [
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
          [Highlight {} ["Esc "]]
          [:text {} ["to exit."]]]]]])))

(zc/def-component AbilityChoices
  [this]
  (let [{:keys [game-state]} (zc/props this)]
    nil))
#_(defn render-ability-choices 
  "Render the player ability choice menu if the world state is `:gain-level`."
  [state]
  (let [screen (get state :screen)
        abilities (get-in state [:world :ability-choices])
        height (+ 3 (* 3 (count abilities)))]
    (render-list screen :ui 17 4 43 height
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
    (render-rect-double-border screen 16 3 43 height :black :white)
    (put-string screen :ui 29 3 "Choose New Ability" :black :white)))


(zc/def-component ActionChoices
  [this]
  (let [{:keys [game-state]} (zc/props this)]
    nil))

#_(defn render-action-choices
  "Render the player action choices menu if the world state is `:action-select`."
  [state]
  (let [screen (get state :screen)
        abilities (get-in state [:world :action-select])
        height (if (seq abilities)
                 (+ 3 (* 3 (count abilities)))
                 4)]  
    (render-list screen :ui 17 4 43 height
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
        
    (render-rect-double-border screen 16 3 43 height :black :white)
    (put-string screen :ui 30 3 "Choose Action" :black :white)))


(zc/def-component Describe
  [this]
  (let [{:keys [game-state]} (zc/props this)]
    nil))
#_(defn render-describe
  "Render the describe info pane if the world state is `:describe`."
  [state]
  (let [{cursor-x :x
         cursor-y :y} (get-in state [:world :cursor])
        [x y]         (rv/viewport-xy state)
        [w _]         (rv/viewport-wh state)
        _             (println "cursor-x" cursor-x "w" w)
        position      (if (< cursor-x (/ w 2))
                        :right
                        :left)
        description   (rdesc/describe-cell-at-xy state (+ x cursor-x) (+ y cursor-y))]
  (render-text (state :screen) position "Look" (if (get-in state [:world :dev-mode])
                                                 (str description "[" (+ x cursor-x) " " (+ y cursor-y) "]"
                                                      (get-cell state (+ x cursor-x) (+ y cursor-y)))
                                                 description))))



(zc/def-component Apply
  [this]
  (let [{:keys [game-state]} (zc/props this)
        player-items (-> game-state :world :player :inventory)]
    (zc/csx [:view {:style {:width 40
                            :height 20
                            :position :fixed
                            :left 40
                            :top 1
                            :padding 1
                            :background-color (rcolor/color->rgb :white)}} [
             [MultiSelect {:title "Apply Inventory"
                           :items (translate-identified-items game-state player-items)}]]])))

(zc/def-component ApplyTo
  [this]
  (let [{:keys [game-state]} (zc/props this)
        player-items (-> game-state :world :player :inventory)]
    (zc/csx [:view {:style {:width 40
                            :height 20
                            :position :fixed
                            :left 40
                            :top 1
                            :padding 1
                            :background-color (rcolor/color->rgb :white)}} [
             [MultiSelect {:title "Apply To"
                           :items (translate-identified-items game-state player-items)}]]])))


(zc/def-component QuaffInventory
  [this]
  (let [{:keys [game-state]} (zc/props this)]
    (zc/csx [:view {:style {:width 40
                            :height 20
                            :position :fixed
                            :left 40
                            :top 1
                            :padding 1
                            :background-color (rcolor/color->rgb :white)}} [
             [MultiSelect {:title "Quaff"
                           :items (translate-identified-items game-state (filter ig/is-quaffable?
                                                                           (-> game-state :world :player :inventory)))}]]])))

(zc/def-component Drop
  [this]
  (let [{:keys [game-state]} (zc/props this)
        player-items (-> game-state :world :player :inventory)]
    (zc/csx [:view {:style {:width 40
                            :height 20
                            :position :fixed
                            :left 40
                            :top 1
                            :padding 1
                            :background-color (rcolor/color->rgb :white)}} [
             [MultiSelect {:title "Drop Inventory"
                           :items (translate-identified-items game-state player-items)}]]])))

(zc/def-component DescribeInventory
  [this]
  (let [{:keys [game-state]} (zc/props this)
        player-items (-> game-state :world :player :inventory)]
    (zc/csx [:view {:style {:width 40
                            :height 20
                            :position :fixed
                            :left 40
                            :top 1
                            :padding 1
                            :background-color (rcolor/color->rgb :white)}} [
             [MultiSelect {:title "Describe"
                           :items (translate-identified-items game-state player-items)}]]])))

(zc/def-component ThrowInventory
  [this]
  (let [{:keys [game-state]} (zc/props this)]
  (let [{:keys [game-state]} (zc/props this)
        player-items (-> game-state :world :player :inventory)]
    (zc/csx [:view {:style {:width 40
                            :height 20
                            :position :fixed
                            :left 40
                            :top 1
                            :padding 1
                            :background-color (rcolor/color->rgb :white)}} [
             [MultiSelect {:title "Throw"
                           :items (translate-identified-items game-state player-items)}]]]))))

(zc/def-component Eat
  [this]
  (let [{:keys [game-state]} (zc/props this)]
    (zc/csx [:view {:style {:width 40
                            :height 20
                            :position :fixed
                            :left 40
                            :top 1
                            :padding 1
                            :background-color (rcolor/color->rgb :white)}} [
             [MultiSelect {:title "Eat Inventory"
                           :items (filter #(contains? % :hunger)
                                         (inventory-and-player-cell-items game-state))}]]])))

(zc/def-component Quests
  [this]
  (let [{:keys [game-state]} (zc/props this)]
    nil))

(zc/def-component Craft
  [this]
  (let [{:keys [game-state]} (zc/props this)]
    (zc/csx [zcui/Popup {:style {:color (rcolor/color->rgb :black)
                                 :background-color (rcolor/color->rgb :white)}} [
                [MultiSelect {:title "Craft"
                              :items [{:name "Weapons" :hotkey \w}
                                      {:name "Survival" :hotkey \s}
                                      {:name "Shelter" :hotkey \c}
                                      {:name "Transportation" :hotkey \t}]}]]])))

(zc/def-component CraftWeapon
  [this]
  (let [{:keys [game-state]} (zc/props this)]
    (zc/csx [zcui/Popup {:style {:margin-top 5
                                 :color (rcolor/color->rgb :black)
                                 :background-color (rcolor/color->rgb :white)}} [
              [:text {:style {:bottom 1}} ["| Craft Weapon |"]]
              [CraftSubmenu {:game-state game-state :recipe-type :weapons}]]])))

(zc/def-component CraftSurvival
  [this]
  (let [{:keys [game-state]} (zc/props this)]
    (zc/csx [zcui/Popup {:style {:margin-top 5
                                 :height 17
                                 :color (rcolor/color->rgb :black)
                                 :background-color (rcolor/color->rgb :white)}} [
              [:text {:style {:bottom 1}} ["| Craft Survival |"]]
    (zc/csx [CraftSubmenu {:game-state game-state :recipe-type :survival}])]])))

(zc/def-component CraftShelter
  [this]
  (let [{:keys [game-state]} (zc/props this)]
    (zc/csx [zcui/Popup {:style {:margin-top 5
                                 :color (rcolor/color->rgb :black)
                                 :background-color (rcolor/color->rgb :white)}} [
              [:text {:style {:bottom 1}} ["| Craft Shelter |"]]
    (zc/csx [CraftSubmenu {:game-state game-state :recipe-type :shelter}])]])))

(zc/def-component CraftTransportation
  [this]
  (let [{:keys [game-state]} (zc/props this)]
    (zc/csx [zcui/Popup {:style {:margin-top 5
                                 :height 15
                                 :color (rcolor/color->rgb :black)
                                 :background-color (rcolor/color->rgb :white)}} [
              [:text {:style {:bottom 1}} ["| Craft Transportation |"]]
    (zc/csx [CraftSubmenu {:game-state game-state :recipe-type :transportation}])]])))

(zc/def-component Wield
  [this]
  (let [{:keys [game-state]} (zc/props this)
        player-items (filter can-be-wielded? (-> game-state :world :player :inventory))]
   (zc/csx [:view {:style {:width 40
                           :height 20
                           :position :fixed
                           :left 40
                           :top 1
                           :padding 1
                           :background-color (rcolor/color->rgb :white)}} [
            [MultiSelect {:title "Wield"
                          :items (translate-identified-items game-state player-items)}]]])))

(zc/def-component WieldRanged
  [this]
  (let [{:keys [game-state]} (zc/props this)
        player-items (filter can-be-wielded-for-ranged-combat? (-> game-state :world :player :inventory))]
   (zc/csx [:view {:style {:width 40
                           :height 20
                           :position :fixed
                           :left 40
                           :top 1
                           :padding 1
                           :background-color (rcolor/color->rgb :white)}} [
            [MultiSelect {:title "Wield"
                          :items (translate-identified-items game-state player-items)}]]])))

(zc/def-component StartText
  [this]
  (let [{:keys [game-state]} (zc/props this)
        start-text (sg/start-text game-state)]
    (zc/csx
	  [:terminal {} [
		[:group {:id :app} [
		  [:layer {:id :ui} [
			[:view {:style {:position :fixed
							:top 4
							:left 20}} [
              [:text {:style {:width 40}} [(str start-text)]]
              [:text {:style {:top 10}} [
                [:text {} ["Press "]]
                [Highlight {} ["any key "]]
                [:text {} ["to continue and "]]
                [Highlight {} ["? "]]
                [:text {} ["to view help."]]]]]]]]]]]])))

(zc/def-component ContinuePopover
  [this]
  (let [{:keys [message]} (zc/props this)]
    (zc/csx [zcui/Popup {} [
              [:text {} [message]]
              [:text {} [
                [:text {} ["Press "]]
                [Highlight {} ["space"]]
                [:text {} [" to continue."]]]]]])))

(zc/def-component YesNoPopover
  [this]
  (let [{:keys [message]} (zc/props this)]
    (zc/csx [zcui/Popup {} [
              [:text {} [message]]
              [:text {} [
                [:text {} ["["]]
                [Highlight {} ["y"]]
                [:text {} ["/"]]
                [Highlight {} ["n"]]
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
      (zc/csx [zcui/Popup {:style {:border 2}} [
                  [:text {} ["You died."]]
                  [:text {} [(format "From %s" cause-of-death)]]
                  [:text {} [""]]
                  [:text {} [
                    [:text {} ["Press "]]
                    [Highlight {} ["space "]]
                    [:text {} ["to continue."]]]]]])))

(zc/def-component QuitPrompt
  [this]
  (let [{:keys [game-state]} (zc/props this)]
    (zc/csx [zcui/Popup {} [
              [:text {} [
                [:text {} ["Quit? ["]]
                [Highlight {} ["y"]]
                [:text {} ["/"]]
                [Highlight {} ["n"]]
                [:text {} ["]"]]]]]])))

(zc/def-component Harvest
  [this]
  (let [{:keys [game-state]} (zc/props this)]
    nil))


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
      :craft                (zc/csx [Craft {:game-state game-state}])
      :craft-weapon         (zc/csx [CraftWeapon {:game-state game-state}])
      :craft-survival       (zc/csx [CraftSurvival {:game-state game-state}])
      :craft-shelter        (zc/csx [CraftShelter {:game-state game-state}])
      :craft-transportation (zc/csx [CraftTransportation {:game-state game-state}])
      :wield                (zc/csx [Wield {:game-state game-state}])
      :wield-ranged         (zc/csx [WieldRanged {:game-state game-state}])
      :start-text           (zc/csx [StartText {:game-state game-state}])
      :popover              (zc/csx [ContinuePopover {:message (rpop/get-popover-message game-state)}])
      :quaff-popover        (zc/csx [YesNoPopover {:message (rpop/get-popover-message game-state)}])
      :dead                 (zc/csx [DeadText {:game-state game-state}])
      :rescued              (zc/csx [RescuedPopover {:game-state game-state}])
      :quit?                (zc/csx [QuitPrompt {:game-state game-state}])
      :harvest              (zc/csx [Harvest {:game-state game-state}])
      (zc/csx [:view {} []]))
    ;; draw cursor
    #_(if-let [cursor-pos (-> state :world :cursor)]
      (move-cursor screen (cursor-pos :x) (cursor-pos :y))
      (move-cursor screen -1 -1))))

(zc/def-component Message
  [this]
  ;; draw log
  (let [{:keys [game-state]} (zc/props this)
        current-time     (dec (rw/get-time game-state))
        log-idx          (get-in game-state [:world :log-idx] 0)
        num-logs         (count (filter #(= current-time (get % :time)) (get-in game-state [:world :log])))
        up-arrow-char    "\u2191"
        down-arrow-char  "\u2193"
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
                           (nth (reverse (get-in game-state [:world :log])) log-idx))
        darken-factor    (inc  (* (/ -1 5) (- current-time (message :time))))
        log-color        (rcolor/darken-rgb (rcolor/color->rgb (get message :color)) darken-factor)]
    (zc/csx
        [:view {} [
          [:text {:style {:position :fixed :top -4 :left 0}} [
            (if msg-above?
              (zc/csx [:text {} [
                        [Highlight {} ["/"]]
                        [:text {} ["-"]]
                        [:text {:style {:color up-arrow-color}} [up-arrow-char]]]])
              (zc/csx [:text {} ["   "]]))
            (if msg-below?
              (zc/csx [:text {} [
                        [Highlight {} ["*"]]
                        [:text {} ["-"]]
                        [:text {:style {:color down-arrow-color}} [down-arrow-char]]]])
              (zc/csx [:text {} ["   "]]))
            (if (get message :text)
              (zc/csx [:text {:style {:color (rcolor/color->rgb (get message :color))}} [(str (get message :text))]])
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
      (rv/get-cursor-pos game-state)))

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
        lantern-on                                  (when-let [lantern (rp/inventory-id->item game-state :lantern)]
                                                      (= (get lantern :state :off) :on))]
    (zc/csx
	  [:terminal {} [
		[:group {:id :app} [
		  [:layer {:id :map} [
            [:view {:style {:top 0 :left 0}} [
              [MapInViewport {:cells cells :current-time current-time}]]]]]
		  [:layer {:id :features} [
            [:view {:style {:top 0 :left 0}} [
              [CharactersInViewport {:npcs visible-npcs
                                     :player-pos (rp/player-pos game-state)
                                     :player-bloodied (get player :bloodied)
                                     :vx vx
                                     :vy vy
                                     :current-time current-time}]]]
            #_[:view {:style {:top 0 :left 0}} [
              [Traps {:traps visible-npcs
                                     :player-pos (rp/player-pos game-state)
                                     :player-bloodied (get player :bloodied)
                                     :vx vx
                                     :vy vy
                                     :current-time current-time}]]]
            [FishingPole {:game-state game-state}]
            [:view {:style {:top 0 :left 0}} [
              [FX {:vx vx
                   :vy vy
                   :fx (rfx/fx game-state)}]]]]]
          [:layer {:id :shading} [
            [:view {:style {:top 0 :left 0}} [
              [ShadeImg {:cells cells :current-time current-time
                         :player-screen-pos player-screen-pos
                         :sight-distance sight-distance
                         :lantern-on lantern-on}]]]]]
          [:layer {:id :ui} [
            (if-let [cursor-pos (-> game-state :world :cursor)]
              (zc/csx [:view {} [
                        [Cursor {:pos cursor-pos}]
                        (if (contains? #{:select-ranged-target :select-throw-target} (current-state game-state))
                          (zc/csx [:view {} [
                                   [Line {:ch "\u25CF"
                                         :color (rcolor/color->rgb :green 255)
                                         :background-color [0 0 0 0]
                                         :start-pos  player-screen-pos
                                         :end-pos (target-pos game-state)}]
                                    [HighlightNpcs {:visible-npcs visible-npcs
                                                    :vx vx
                                                    :vy vy
                                                    :start-pos  (rv/world-pos->screen-pos game-state player-pos)
                                                    :end-pos (target-pos game-state)}]]])
                          (zc/csx [:view {}]))]])
              (zc/csx [:view {}]))
            (if-let [ui-hint (get-in game-state [:world :ui-hint])]
              ;; ui-hint
              (zc/csx [:view {} [
                [:text {:style {:position :fixed :top 1 :left 1
                                :background-color [0 0 0 0]}} [
                          [:text {:style {:background-color [0 0 0]}} [ui-hint]]]]]])
              ;; regular concise message log
              (zc/csx [Message {:game-state game-state}]))
            [Hud {:game-state game-state}]
            [MapUI {:game-state game-state}]]]]]]])))

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

#_(defn render-loading [state]
  (let [screen     (state :screen)
        n          (->> (sort-by val @tidbit-freqs)
                        (partition-by val)
                        first
                        rand-nth
                        first)]
    (swap! tidbit-freqs (fn [freqs] (update freqs n inc)))
    (clear (state :screen))
    (put-string screen :ui 30 12 (format "Generating %s..." (nth loading-tidbits n)))
    (put-string screen :ui 40 18 (nth ["/" "-" "\\" "|"] (mod (swap! loading-index inc) 4)))
    (refresh screen)))

#_(defn render-connection-failed [state]
  (let [screen     (state :screen)]
    (clear screen)
    (put-string screen 30 12 :ui "Connection failed")
          (put-chars (state :screen) :ui (markup->chars 30 22 "Play again? [<color fg=\"highlight\">y</color>/<color fg=\"highlight\">n</color>]"))
    (refresh screen)))

(def connecting-index (atom 0))
#_(defn render-connecting [state]
  (let [screen     (state :screen)]
    (clear screen)
    (put-string screen 30 12 :ui (format "Connecting%s" (apply str (repeat (mod (swap! connecting-index inc) 4) "."))))
    (refresh screen)))

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
      [:view {} [
        [:text {:style {:left 1}} [title]]
        ; histogram
        [:view {:style {:width 8 :height height :top 1 :left 1
                        :border-left 1 :border-bottom 1
                        :position :absolute
                        :display :flex :align-items :flex-end}} 
               (for [[index data] (map-indexed vector (sort-by #(get % "group") (get histogram "data")))
                     :when (get data "group")
                     :let [group (get data "group")
                           count (get data "count")
                           x (+ 1 (/ group group-size))
                           bar-height (max 1 (* (- height 2) (/ count max-count)))
                           bg (rcolor/color->rgb
                                (if (< group (inc value) (+ group group-size 1))
                                  :highlight
                                  :dark-gray))]]
                 ; histogram bar
                 (zc/csx [:view {:style {:width 1 :height bar-height
                                         :position :absolute
                                         :left index
                                         :bottom 0
                                         :background-color bg}} []]))]
        ; caret
        [:text {:style {:top (dec height) :left (inc caret-x)
                        :color (rcolor/color->rgb :highlight)}} ["^"]]]])))

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
          [zcui/Image {:src "/home/santos/src/robinson/images/robinson-mainmenu.jpg"}]]]
        [:layer {:id :ui} [
              [:view {:style {:color [255 255 255 255]
                              :background-color [0 0 0 0]
                              :position :fixed
                              :top 20
                              :left 30}} [
                [:text {} [
                  [:text {} ["Press "]]
                  [Highlight {}  ["space "]]
                  [:text {} ["to play"]]]]
                [:text {} [[Highlight {} ["c"]] [:text  {} [" - configure"]]]]]]]]]]]]))

(zc/def-component Configure
  [this]
  (let [{:keys [state]} (zc/props this)]
    (zc/csx
	  [:terminal {} [
		[:group {:id :app} [
		  [:layer {:id :ui} [
			[:view {:style {:position :fixed
							:top 30
							:left 20}} [
			  [:text {} ["Configure"]]
			  [:text {} ["f - font"]]]]]]]]]])))

(zc/def-component ConfigureFont
  [this]
  (let [{:keys [state]} (zc/props this)
        font   (get-in state [:fonts (get state :new-font (get (rc/get-settings state) :font))])]
    (zc/csx
	  [:terminal {} [
		[:group {:id :app} [
		  [:layer {:id :ui} [
			[:view {:style {:position-type :absolute
							:position-top 30
							:position-left 20}} [
			  [:text {} ["Configure Font"]]
			  [:text {} ["Font: " (get font :name)]]
			  [:text {} ["n - next font" (get font :name)]]
			  [:text {} ["p - previous font" (get font :name)]]
			  [:text {} ["s - save and apply" (get font :name)]]]]]]]]]])))

(zc/def-component EnterName
  [this]
  (let [{:keys [game-state]} (zc/props this)
        player-name (get-in game-state [:world :player :name])]
    (zc/csx
	  [:terminal {} [
		[:group {:id :app} [
		  [:layer {:id :ui} [
			[:view {:style {:top 10
							:left 30
                            :width 80
                            :display :flex
                            :flex-direction :row}} [
              [:text {:style {:width 5}} ["Name:"]]
              [:view {:style {:position :relative} } [
			    [zcui/Input {:value player-name
                             :focused true}]]]]]]]]]]])))

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
                            :position :fixed
                            :top 2
                            :left 0}} [
              [SelectItemList {:title "Choose up to three things to take with you:"
                               :selected-hotkeys selected-hotkeys
                               :use-applicable true
                               :items start-inventory}]]]]]]]]])))

(zc/def-component Loading
  [this]
  (let [{:keys [state]} (zc/props this)]
    (zc/csx 
	  [:terminal {} [
		[:group {:id :app} [
		  [:layer {:id :ui} [
            [:view {:style {:position :fixed
                            :top 10
                            :left 36}} [
              [:text {} ["Loading..."]]]]]]]]]])))

(zc/def-component Connecting
  [this]
  (let [{:keys [game-state]} (zc/props this)]
    (zc/csx 
	  [:terminal {} [
		[:group {:id :app} [
		  [:layer {:id :ui} [
            [:view {:style {:position :fixed
                            :top 10
                            :left 36}} [
              [:text {} ["Connecting..."]]]]]]]]]])))

(zc/def-component ConnectionFailed
  [this]
  (let [{:keys [game-state]} (zc/props this)]
    (zc/csx 
	  [:terminal {} [
		[:group {:id :app} [
		  [:layer {:id :ui} [
            [:view {:style {:position :fixed
                            :top 10
                            :left 36}} [
              [:text {} ["Connection Failed."]]]]]]]]]])))


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
            [:view {:style {:left 10}} [
              [:text {} [(format "%s: %s" player-name madlib)]]
              [:text {} [(format "%s: %s" player-name madlib)]]
              [:text {} [""]]
              [:text {} [(format "Points: %s." points)]]
              [:text {} [(format "Survived for %d %s. (%d turns)"
                                   days-survived
                                   (if (> 1 days-survived) "days" "day")
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
                  (get player :inventory))]]]
              [:text {} [""]]
              [:view {:style {:left 10}} [
                [:text {} [[:text {} ["Play again? ["]]
                           [Highlight {} ["y"]]
                           [:text {} ["/"]]
                           [Highlight {} ["n"]]
                           [:text {} ["] "]]
                           [Highlight {} ["space "]]
                           [:text {} ["- share and compare with other players"]]]]]]]]]]]])))

(zc/def-component GameOverRescued
  [this]
  (let [{:keys [game-state]} (zc/props this)
        rescue-mode (rendgame/rescue-mode game-state)]
    ;; Title
    ;(put-string (game-state :screen) :ui 10 1 (format "%s: %s." player-name madlib))
    ;(put-string (game-state :screen) :ui 18 2 (format "Rescued by %s after surviving for %d days." rescue-mode days-survived))
    ;(put-string (game-state :screen) :ui 10 3 (format "Points: %s." points))
    ;(put-string (game-state :screen) :ui 10 4 "Inventory:")
    ;(doall (map-indexed
    ;  (fn [idx item] (put-string (game-state :screen) :ui 18 (+ idx 5) (item :name)))
    ;  (-> game-state :world :player :inventory)))
    ;(put-string (game-state :screen) :ui 10 22 "Play again? [yn]")))
))

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
        (zc/csx [GameOverRescued {:game-state game-state}]))))

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
			[:view {:style {:top 2
							:left 2
                            :display :flex
                            :flex-direction :row}} [
               ;; Title
               [:text {} ["Top scores"]]
               [:view {:style {:position :fixed :top 3 :left 0}} [
                 [ItemList {:items (map-indexed
                                     (fn [index score]
                                       (if score
                                         (let [player-name    (get score "player-name" "?name?")
                                               points         (get score "points" 0)
                                               days-survived  (get score :days-survived 0 )
                                               turns-survived (get score :turns-survived 0 )
                                               fg (rcolor/color->rgb (if (= player-name (get (rp/get-player game-state) :name))
                                                                       :highlight
                                                                       :white))]
                                           {:s (format "%-2d.%-20s (%-5d points)" (inc index) player-name points)
                                            :fg fg
                                            :bg (rcolor/color->rgb :black)})
                                           {:s "..."
                                            :fg [255 255 255 255] #_(rcolor/color->rgb :white)
                                            :bg [0 0 0 0] #_(rcolor/color->rgb :black)
                                            :style nil}))
                                     top-10-scores)}]
                 [:view {:style {:top 1 :left 0}} [
                   [:text {} [[:text {} ["Play again? ["]]
                              [Highlight {} ["y"]]
                              [:text {} ["/"]]
                              [Highlight {} ["n"]]
                              [:text {} ["] "]]]]]]]]
               [:view {:style {:position :fixed :top 0 :left 40}} [
                 [:text {} ["Performance"]]
                 [:view {:style {:position :fixed :top 3 :left 0}} [
                   [Histogram {:title "Points"
                               :value     (get game-state :points)
                               :histogram (get game-state :point-data)}]]]
                 [:view {:style {:position :fixed :top 3 :left 10}} [
                   [Histogram {:title "Turns"
                               :value     (rw/get-time game-state)
                               :histogram (get game-state :time-data)}]]]
                 [:view {:style {:position :fixed :top 11 :left 0}} [
                   [Histogram {:title "Kills"
                               :value     (reduce + 0 (map second (get-in game-state [:world :player :stats :num-animals-killed])))
                               :histogram (get game-state :kills-data)}]]]
                 [:view {:style {:position :fixed :top 11 :left 10}} [
                   [Histogram {:title "Items Crafted"
                               :value     (reduce + 0 (map second (get-in game-state [:world :player :stats :num-items-crafted])))
                               :histogram (get game-state :crafted-data)}]]]
                 [:view {:style {:position :fixed :top 19 :left 0}} [
                   [:text {} [
                     [:text {} ["Your Performance "]]
                     [Highlight {} ["^"]]]]]]]]]]]]]]]])))

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
            [:view {}
              (for [{:keys [text color]} (reverse (take 23 log))]
                (zc/csx [:text {:style {:color (rcolor/color->rgb color)}} [(str text)]]))]]]]]]])))

(zc/def-component Robinson
  [this]
  (let [game-state (get (zc/props this) :game-state)]
    (assert (not (nil? game-state)))
    (log/info "render current-state" (current-state game-state))
    (cond
      (= (current-state game-state) :start)
        (zc/csx [Start {}])
      (= (current-state game-state) :configure)
        (zc/csx [Configure {:game-state game-state}])
      (= (current-state game-state) :configure-font)
        (zc/csx [ConfigureFont {:game-state game-state}])
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
      (= (current-state game-state) :start-text)
        (zc/csx [StartText {:game-state game-state}])
      ;  (render-start-text game-state)
      ;; Is player dead?
      (contains? #{:game-over-dead :game-over-rescued} (current-state game-state))
        ;; Render game over
        (zc/csx [GameOver {:game-state game-state}])
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
