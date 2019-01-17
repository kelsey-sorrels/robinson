;; Functions for rendering state to screen
(ns robinson.ui.cell
  (:require 
            [taoensso.timbre :as log]
            [robinson.random :as rr]
            [robinson.color :as rcolor]
            [robinson.renderutil :as rutil]
            [robinson.noise :as rnoise]
            [robinson.traps :as rt]
            [zaffre.color :as zcolor]
            [clojure.core.match :refer [match]]))

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

(defn cell->cp437-character
  [cell]
  (let [water-level (get cell :water 0)
        trap-found (get cell :trap-found)]
    (match [(get cell :type) (< water-level 10) trap-found]
      [(:or :floor
            :sand
            :dirt) _ _]      \·
      [:open-door  _ _]      \-
      [:close-door  _ _]     \+
      [:corridor  _ _]       \#
      [:down-stairs  _ _]    \>
      [:up-stairs  _ _]      \<
      [:fire  _ _]           \u2240
      [:water  _ _]          \≈ 
      [(:or :surf
            :shallow-water
            :swamp
            :lava) _ _]    \~
      [:mountain _ _]       \u25b2
      [:dune _ _]           \u2229
      [:rocky-shore _ _]    \u2229
      [:gravel _ _]         \·
      [:short-grass _ _]    \·
      [:tall-grass _ _]     \"
      [:tree _ _]           \T
      [:bamboo _ _]         \u2510
      [:palisade  _ _]      \#
      [:ramada _ _]         \#
      [:tarp-shelter _ _]   \#
      [:lean-to _ _]        \#
      [:campfire  _ _]      \^
      [:bamboo-water-collector _ _] \O
      [:solar-still _ _]         \O
      [:palm-tree _ _]           \7
      [:fruit-tree _ _]          \♣ 
      [:freshwater-hole true _]  \O
      [:freshwater-hole false _] \~
      [:saltwater-hole true _]   \O
      [:saltwater-hole false _]  \~
      [:dry-hole _ _]            \O
      ;; pirate ship cell types
      [:bulkhead _ _]       \◘
      [:wheel _ _]          \○
      [:bulkhead2  _ _]     \◘
      [:wooden-wall _ _]    \#
      [:railing _ _]        \#
      [:hammock-v _ _]      \)
      [:hammock-h _ _]      \-
      [:deck _ _]           \·
      [:canon-breach _ _]   \║
      [:tackle _ _]         \º
      [:canon _ _]          \║
      [:grate _ _]          \╬
      [:table _ _]          \╤
      [:chair _ _]          \╥
      [:mast _ _]           \╨
      [:beam _ _]           \═
      [:canon-truck-1 _ _]  \▄
      [:locker _ _]         \▌
      [:locker2 _ _]        \▐
      [:canon-truck-2 _ _]  \▀
      [:ships-wheel _ _]    \Φ
      [:ladder _ _]         \≡
      [:porthole _ _]       \°
      [:chest _ _]          \■
      [:artifact-chest _ _] \■
      ;; ruined temple cell types
      [:vertical-wall _ _]         \║
      [:horizontal-wall _ _]       \═
      [:vertical-wall-alt _ _]     \°
      [:horizontal-wall-alt _ _]   \°
      [:upper-left-1 _ _]          \╔
      [:upper-right-1 _ _]         \╗
      [:bottom-left-1 _ _]         \╚
      [:bottom-right-1 _ _]        \╝
      [:upper-left-2 _ _]          \◙
      [:upper-right-2 _ _]         \◙
      [:bottom-left-2 _ _]         \◙
      [:bottom-right-2 _ _]        \◙
      [:altar _ _]                 \┬
      [:vine _ _]                  \⌠
      [:moss-corridor _ _]         \#
      [:moss-vertical-wall _ _]    \║
      [:moss-horizontal-wall _ _]  \═
      [:moss-vertical-wall-alt _ _]   \°
      [:moss-horizontal-wall-alt _ _] \°
      [:moss-upper-left-1 _ _]     \╔
      [:moss-upper-right-1 _ _]    \╗
      [:moss-bottom-left-1 _ _]    \╚
      [:moss-bottom-right-1 _ _]   \╝
      [:moss-upper-left-2 _ _]     \◙
      [:moss-upper-right-2 _ _]    \◙
      [:moss-bottom-left-2 _ _]    \◙
      [:moss-bottom-right-2 _ _]   \◙
      [:white-corridor  _ _]       \#
      [:white-vertical-wall   _ _] \║
      [:white-horizontal-wall _ _] \═
      [:white-vertical-wall-alt _ _]   \°
      [:white-horizontal-wall-alt _ _] \°
      [:white-upper-left-1 _ _]    \╔
      [:white-upper-right-1 _ _]   \╗
      [:white-bottom-left-1 _ _]   \╚
      [:white-bottom-right-1 _ _]  \╝
      [:white-upper-left-2 _ _]    \◙
      [:white-upper-right-2 _ _]   \◙
      [:white-bottom-left-2 _ _]   \◙
      [:white-bottom-right-2 _ _]  \◙
      [:empty                _ _]  \space
      [(:or :crushing-wall-trigger
            :wall-darts-trigger
            :poisonous-gas-trigger
            :spike-pit
            :snakes-trigger) _ true] \^
      [(:or :crushing-wall-trigger
            :wall-darts-trigger
            :poisonous-gas-trigger
            :spike-pit
            :snakes-trigger) _ false] \.
      [_ _ _] \?)))

(defn cell->unicode-character
  [cell]
  (case (get cell :type)
     :mountain    \u2206 ;; ∆
     :fruit-tree  \u2648 ;; ♈
     :dune        \u1d16 ;; ᴖ
     :rocky-shore \u1d16 ;; ᴖ
     :bamboo      \u01c1 ;; ∥ 
     (cell->cp437-character cell)))


(defn cell->color
  [cell current-time]
  (case (get cell :type)
     :open-door       :brown
     :close-door      :brown
     :corridor        :light-gray
     :fire            (if (= (cell :discovered) current-time)
                         :fire
                         :red)
     :water           (if (= (cell :discovered) current-time)
                         :water
                         :blue)
     :surf            (if (= (cell :discovered) current-time)
                         :surf
                         :light-blue)
     :shallow-water   (if (= (cell :discovered) current-time)
                         :shallow-water
                         :light-blue)
     :swamp           (if (= (cell :discovered) current-time)
                         :swamp
                         :light-blue)
     :lava            (if (= (cell :discovered) current-time)
                         :lava
                         :light-blue)
     :mountain        :gray
     :sand            :beige
     :dirt            :brown
     :dune            :light-brown
     :rocky-shore     :dark-gray
     :gravel          :gray
     :short-grass     :green
     :tall-grass      :dark-green
     :tree            :dark-green
     :bamboo          :light-green
     :palisade        :brown
     :ramada          :beige
     :tarp-shelter    :blue
     :lean-to         :light-green
     :campfire        :brown
     :bamboo-water-collector
                      (if (< 10 (get cell :water 0))
                        :bamboo-water-collector
                        :white)
     :solar-still
                      (if (< 10 (get cell :water 0))
                        :solar-still
                        :white)
     :palm-tree       :dark-green
     :fruit-tree      :light-green
     :freshwater-hole (if (< 10 (get cell :water 0))
                        :freshwater-hole
                        :white)
     :saltwater-hole  (if (< 10 (get cell :water 0))
                        :saltwater-hole
                        :white)
     :dry-hole        :white
     ;; pirate ship cell types
     :bulkhead        :brown
     :wheel           :dark-brown
     :bulkhead2       :brown
     :wooden-wall     :ship-brown
     :railing         :ship-brown
     :hammock-v       :brown
     :hammock-h       :brown
     :deck            :dark-brown
     :canon-breach    :gray
     :tackle          :brown
     :canon           :gray
     :grate           :dark-beige
     :table           :ship-light-brown
     :chair           :ship-light-brown
     :mast            :ship-light-brown
     :beam            :brown
     :canon-truck-1   :dark-brown
     :locker          :brown
     :locker2         :brown
     :canon-truck-2   :dark-brown
     :ships-wheel     :brown
     :ladder          :dark-beige
     :porthole        :brown
     :chest           :ship-dark-brown
     :artifact-chest  :dark-beige
     ;; ruined temple cell types
     :vertical-wall   :temple-beige
     :horizontal-wall :temple-beige
     :vertical-wall-alt :white
     :horizontal-wall-alt :white
     :upper-left-1    :temple-beige
     :upper-right-1   :temple-beige
     :bottom-left-1   :temple-beige
     :bottom-right-1  :temple-beige
     :upper-left-2    :temple-beige
     :upper-right-2   :temple-beige
     :bottom-left-2   :temple-beige
     :bottom-right-2  :temple-beige
     :altar           :white
     :vine            :moss-green
     :moss-corridor   :moss-green
     :moss-vertical-wall :moss-green
     :moss-horizontal-wall :moss-green
     :moss-vertical-wall-alt :white
     :moss-horizontal-wall-alt :white
     :moss-upper-left-1 :moss-green
     :moss-upper-right-1 :moss-green
     :moss-bottom-left-1 :moss-green
     :moss-bottom-right-1 :moss-green
     :moss-upper-left-2 :moss-green
     :moss-upper-right-2 :moss-green
     :moss-bottom-left-2 :moss-green
     :moss-bottom-right-2 :moss-green
     :white-corridor :light-gray
     :white-vertical-wall   :white
     :white-horizontal-wall :white
     :white-vertical-wall-alt :white
     :white-horizontal-wall-alt :white
     :white-upper-left-1 :white
     :white-upper-right-1 :white
     :white-bottom-left-1 :white
     :white-bottom-right-1 :white
     :white-upper-left-2 :white
     :white-upper-right-2 :white
     :white-bottom-left-2 :white
     :white-bottom-right-2 :white
     :empty                :black
     :crushing-wall-trigger :white
     :wall-darts-trigger :white
     :poisonous-gas-trigger :white
     :spike-pit :white
     :snakes-trigger :white
     :white))

(defn render-cell [cell wx wy current-time font-type]
  {:post [(fn [c] (log/info c) (char? (get c :c)))
          (integer? (get % :fg))
          (integer? (get % :bg))]}
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
              (cond
                (contains? #{:chest :artifact-chest} (get cell :type))
                  [\■ :dark-beige :black]
                (some (fn [item] (= (get item :id) :raft)) cell-items)
                  [\░ :beige :brown]
                :default
                  [(rutil/item->char (first cell-items))
                   (rutil/item->fg   (first cell-items))
                   :black])
              [((case font-type
                  :ttf   cell->unicode-character
                  :cp437 cell->cp437-character) cell)
               (cell->color cell current-time) :black])))
        ((apply? (not in-view?)
           (fn [{:keys [c fg bg]}] {:c c :fg (rcolor/rgb->mono fg) :bg (rcolor/rgb->mono bg)})))
        ((apply? (and in-view? harvestable?)
           (fn [{:keys [c fg bg]}] {:c c :fg bg :bg fg}))))
      {:c \  :fg (zcolor/color 0 0 0 0) :bg (zcolor/color 0 0 0 0)})))

