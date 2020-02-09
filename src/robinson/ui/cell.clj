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
            [zaffre.glterminal :as zgl]
            [clojure.core.match :refer [match]]))

(set! *warn-on-reflection* true)

(def cell-type-palette
  {:fire                   [:red :orange]
   :campfire               [:red :orange :yellow]
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
  [wx wy t cell-type]
  (let [r (rnoise/noise3d palette-noise wx wy
            (* t
              (if (contains? #{:fire :campfire} cell-type)
                16
                1)))
        palette (cell-type-palette cell-type)
        start-idx (int (* r (count palette)))
        end-idx (mod (inc start-idx) (count palette))
        start (nth palette start-idx)
        end (nth palette end-idx)]
    (rand-nth palette)
    start))

(defn has-palette?
  [cell-type]
  (contains? #{:fire :campfire :water 
               :surf :shallow-water 
               :swamp :lava 
               :bamboo-water-collector :solar-still 
               :freshwater-hole :saltwater-hole} cell-type))

(defn fill-put-string-color-style-defaults
  ([string]
    (fill-put-string-color-style-defaults 0 0 (System/currentTimeMillis) string :white :black))
  ([wx wy t string]
    (fill-put-string-color-style-defaults wx wy t string :white :black))
  ([wx wy t string fg]
    (fill-put-string-color-style-defaults wx wy t string fg :black))
  ([wx wy t string fg bg]
    (fill-put-string-color-style-defaults wx wy t string fg bg :normal))
  ([wx wy t string fg bg blend-mode]
   (let [new-fg (cond-> (if (has-palette? fg)
                         (cell-type->color wx wy t fg)
                         fg)
                  (not (integer? fg)) rcolor/color->rgb)
         bg     (if (integer? bg) bg (rcolor/color->rgb bg))]
     (zgl/make-terminal-character string new-fg bg blend-mode))))

(def cell-type->cp437-character {:locker \▌
                                 :fire \▲
                                 :hammock-h \-
                                 :shallow-water \~
                                 :horizontal-wall-alt \°
                                 :bottom-right-1 \╝
                                 :bulkhead \◘
                                 :bottom-left-2 \◙
                                 :white-horizontal-wall-alt \°
                                 :upper-right-1 \╗
                                 :dune \∩
                                 :upper-left-2 \◙
                                 :corridor \#
                                 :locker2 \▐
                                 :tree \T
                                 :moss-bottom-left-2 \◙
                                 :white-upper-left-1 \╔
                                 :table \╤
                                 :sand \.
                                 :canon-truck-2 \▀
                                 :white-corridor \#
                                 :campfire \▲
                                 :campfire-base \#
                                 :rocky-shore \∩
                                 :horizontal-wall \═
                                 :canon \║
                                 :moss-upper-right-2 \◙
                                 :close-door \+
                                 :upper-right-2 \◙
                                 :bulkhead2 \◘
                                 :white-upper-right-2 \◙
                                 :white-bottom-left-1 \╚
                                 :deck \·
                                 :lava \~
                                 :canon-truck-1 \▄
                                 :moss-bottom-right-2 \◙
                                 :moss-bottom-left-1 \╚
                                 :open-door \-
                                 :chair \╥
                                 :moss-upper-left-2 \◙
                                 :white-horizontal-wall \═
                                 :tarp-shelter \#
                                 :vertical-wall \║
                                 :moss-bottom-right-1 \╝
                                 :white-bottom-left-2 \◙
                                 :short-grass \·
                                 :empty \space
                                 :white-upper-left-2 \◙
                                 :moss-corridor \#
                                 :tall-grass \"
                                 :moss-vertical-wall-alt \°
                                 :mast \╨
                                 :ramada \#
                                 :gravel \·
                                 :chest \■
                                 :white-vertical-wall \║
                                 :down-stairs \>
                                 :dry-hole \O
                                 :white-upper-right-1 \╗
                                 :up-stairs \<
                                 :vertical-wall-alt \°
                                 :lean-to \#
                                 :railing \#
                                 :fruit-tree \♣
                                 :moss-horizontal-wall-alt \°
                                 :upper-left-1 \╔
                                 :solar-still \O
                                 :bamboo \┐
                                 :ships-wheel \Φ
                                 :hammock-v \)
                                 :vine \⌠
                                 :bottom-right-2 \◙
                                 :white-vertical-wall-alt \°
                                 :surf \~
                                 :white-bottom-right-2 \◙
                                 :bottom-left-1 \╚
                                 :palm-tree \7
                                 :canon-breach \║
                                 :ladder \≡
                                 :moss-vertical-wall \║
                                 :porthole \°
                                 :wooden-wall \#
                                 :artifact-chest \■
                                 :beam \═
                                 :moss-horizontal-wall \═
                                 :wheel \○
                                 :moss-upper-right-1 \╗
                                 :water \≈
                                 :moss-upper-left-1 \╔
                                 :palisade \#
                                 :grate \╬
                                 :altar \┬
                                 :tackle \º
                                 :mountain \▲
                                 :bamboo-water-collector \O
                                 :floor \.
                                 :dirt \.
                                 :swamp \~
                                 :white-bottom-right-1 \╝})

(def trap-type? #{:crushing-wall-trigger
                  :wall-darts-trigger
                  :poisonous-gas-trigger
                  :spike-pit
                  :snakes-trigger})


(defn cell->cp437-character
  [{:keys [type water trap-found]
    :or {water 0}}]
  {:post [(char? %)]}
  (or (and (#{:freshwater-hole :saltwater-hole} type) (if (< water 10) \0 \~))
      (and (trap-type? type) (if trap-found \^ \.))
      (or (cell-type->cp437-character type) \?)
      \?))


(defn cell->unicode-character
  [cell]
  (case (:type cell)
     :mountain    \u2206 ;; ∆
     :fruit-tree  \u2648 ;; ♈
     :dune        \u1d16 ;; ᴖ
     :rocky-shore \u1d16 ;; ᴖ
     :bamboo      \u01c1 ;; ∥ 
     :fire        \u2240 ;; ≀

     (cell->cp437-character cell)))


(defn cell->color
  [cell current-time]
  (case (:type cell)
     :open-door       :brown
     :close-door      :brown
     :corridor        :light-gray
     :fire            (if (= (:discovered cell) current-time)
                         :fire
                         :red)
     :campfire        (if (= (:discovered cell) current-time)
                         :campfire
                         :red)
     :campfire-base   :brown
     :water           (if (= (:discovered cell) current-time)
                         :water
                         :blue)
     :surf            (if (= (:discovered cell) current-time)
                         :surf
                         :light-blue)
     :shallow-water   (if (= (:discovered cell) current-time)
                         :shallow-water
                         :light-blue)
     :swamp           (if (= (:discovered cell) current-time)
                         :swamp
                         :light-blue)
     :lava            (if (= (:discovered cell) current-time)
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

(def empty-cell
  (zgl/make-terminal-character
    \ 
    (zcolor/color 0 0 0 0)
    (zcolor/color 0 0 0 0)))

(defn ceiling-item?
  [item]
  (contains? #{:tarp-hung} (:item/id item)))

(defn translate-item-id
  [item]
  (let [item-id (item :item/id)
        translated-item-id (case item-id
                             :tarp-hung
                               (let [t (item :tarp-type)]
                                 (if (= t :tarp-hung-corner)
                                   :tarp-corner
                                   t))
                             item-id)]
  (assoc item :item/id translated-item-id)))

(defn translate-ceiling-item-id
  [item]
  (let [item-id (item :item/id)
        translated-item-id (case item-id
                             :tarp-hung
                               (item :tarp-type)
                             item-id)]
    (assoc item :item/id translated-item-id)))

(defn render-cell [cell wx wy t current-time font-type]
  {:post [(fn [c] (log/info c) (char? (:c c)))
          (integer? (:fg %))
          (integer? (:bg %))]}
  (let [cell-items (:items cell)
        cell (update cell :type (fn [type] (if (= type :campfire) :campfire-base type)))
        floor-items (remove ceiling-item? cell-items)
        in-view? (= current-time (or (:discovered cell) 0))
        has-been-discovered? (> (or (:discovered cell) 0) 1)
        harvestable? (contains? cell :harvestable)
        render-path (if (and cell-items
                             (seq cell-items)
                             (= (:discovered cell) current-time))
                      (cond
                        (contains? #{:chest :artifact-chest} (:type cell))
                            :chest
                        (some (fn [item] (= (:item/id item) :raft)) cell-items)
                          :raft
                        (and (some ceiling-item? cell-items)
                             (not (some (fn [item] (= (:tarp-type item) :tarp-hung-corner)) cell-items)))
                          :default
                        :default
                          :item)
                      :default)]
    (if (or in-view? has-been-discovered?)
      (->
        (fill-put-string-color-style-defaults
          wx wy t
          ; char
          (case render-path
            :chest \■ 
            :raft \░
            :item (-> cell-items
                    first
                    translate-item-id
                    rutil/item->char)
            :default ((case font-type
                      :ttf   cell->unicode-character
                      :cp437 cell->cp437-character) cell))
          ; fg
          (rcolor/color-bloodied-char 
            (< current-time (or (:bloodied cell) 0))
            (case render-path
              :chest :dark-beige
              :raft :brown
              :item (-> cell-items
                    first
                    translate-item-id
                    rutil/item->fg)
              :default (cell->color cell current-time)))
            ; bg
            (case render-path
              :chest :black
              :raft :dark-brown
              :item (-> cell-items
                      first
                      translate-item-id
                      rutil/item->bg)
              :default :black))
        (cond-> (not in-view?)
           (as-> c-fg-bg
             (let [{:keys [c fg bg]} c-fg-bg]
               (zgl/make-terminal-character
                 c
                 (rcolor/rgb->mono fg)
                 (rcolor/rgb->mono bg)))))
        (cond-> (and in-view? harvestable?)
           (as-> c-fg-bg
             (let [{:keys [c fg bg]} c-fg-bg]
               {:c c :fg bg :bg fg}))))
      empty-cell)))

(defn render-ceiling-cell [cell wx wy t current-time font-type]
  {:post [(fn [c] (log/info c) (char? (:c c)))
          (integer? (:fg %))
          (integer? (:bg %))]}
  (let [campfire? (= (cell :type) :campfire)
        ceiling-items (filter ceiling-item? (:items cell))
        in-view? (= current-time (or (:discovered cell) 0))
        has-been-discovered? (> (or (:discovered cell) 0) 1)]
    ;(log/info ceiling-item cell-items)
    (if (and in-view? has-been-discovered?)
      (cond
        campfire?
          (assoc
          (fill-put-string-color-style-defaults
            wx wy t
            ; char
            ((case font-type
              :ttf   cell->unicode-character
              :cp437 cell->cp437-character) cell)
            ; fg
            (cell-type->color
              wx wy t :campfire)
            ; bg
            (zcolor/color 0 0 0 0))
            :blend-mode :screen)
        (not-empty ceiling-items)
          (->
            (assoc
              (fill-put-string-color-style-defaults
                wx wy t
                ; char
                (-> ceiling-items
                  first
                  translate-ceiling-item-id
                  rutil/item->char )
                ; fg
                (rcolor/color->rgb
                  (rcolor/color-bloodied-char 
                    (< current-time (or (:bloodied cell) 0))
                    (-> ceiling-items
                      first
                      translate-ceiling-item-id
                      (rutil/item->fg)))
                  128)
                  ; bg
                  (-> ceiling-items
                    first
                    translate-ceiling-item-id
                    (rutil/item->fg)))
               :blend-mode
                 (-> ceiling-items
                    first
                    translate-ceiling-item-id
                    (rutil/item->blend-mode)))
            (cond-> (not in-view?)
               (as-> c-fg-bg
                 (let [{:keys [c fg bg]} c-fg-bg]
                   (zgl/make-terminal-character
                     c
                     (rcolor/rgb->mono fg)
                     (rcolor/rgb->mono bg))))))
        :else
          empty-cell)
      empty-cell)))
