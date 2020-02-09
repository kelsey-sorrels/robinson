;; Utility functions for rendering state
(ns robinson.renderutil
  (:require 
            [taoensso.timbre :as log]
            [robinson.random :as rr]
            [robinson.math :as rmath]
            [robinson.color :as rcolor]))

(def items
  {:knife           [\)]
   :obsidian-knife  [\)]
   :obsidian-axe    [\)]
   :obsidian-spear  [\)]
   :flint-knife     [\)]
   :flint-axe       [\)]
   :flint-spear     [\)]
   :sharpened-stick [\)]
   :plant-fiber     [\,]
   :sword           [\)]
   :armor           [\[]
   :shoes           [\!]
   :fishing-pole    [\/]
   :match           [\/]
   :lantern         [\,]
   :bandage         [\&]
   :saw             [\,]
   :tarp            [\,]
   :tarp-corner     [\| :brown :transparent :screen] 
   :tarp-hung       [\▓ :blue :transparent :screen]
   :tarp-hung-mid   [\▓ :brilliant-blue :transparent :screen]
   :tarp-hung-corner [\▓ :blue]
   :spellbook       [\+]
   :scroll          [\?]
   :rock            [\*]
   :obsidian        [\*]
   :coconut         [\* :brown]
   :unhusked-coconut
                    [\* :brown]
   :empty-coconut   [\* :brown]
   :red-fruit       [\* :red]
   :orange-fruit    [\* :orange]
   :yellow-fruit    [\* :yellow]
   :green-fruit     [\* :green]
   :blue-fruit      [\* :blue]
   :purple-fruit    [\* :purple]
   :white-fruit     [\* :white]
   :black-fruit     [\* :gray]
   :bamboo          [\/ :light-green]
   :stick           [\/ :brown]
   :grass           [\/ :green]
   :rope            [\, :green]
   :log             [\/ :brown]
   :bedroll         [\▓ :dark-green :dark-beige]
   :$               [\$  :yellow]
   :amulet          [\" :blue]
   :food            [\%]
   :fire-plough     [\,]
   :hand-drill      [\,]
   :bow-drill       [\,]
   :jack-o-lantern  [\☻ :orange :black]
   :door            [\+]
   ;; pirate ship ite\s
   :spices          [\^]
   :sail            [\#]
   :dice            [\&]
   :blanket         [\#]
   :cup             [\&]
   :silver-bar      [\$]
   :bowl            [\&]
   :fork            [\/]
   :spoon           [\/]
   :rag             [\#]
   :cutlass         [\)]
   :pistol          [\)]
   :paper-cartridge [\&]
   :ale             [\!]
   :pirate-clothes  [\[]
   :navy-uniform    [\[]
   ;; ruined temple items
   :blowdart         [\-]
   :jewlery          [\"]
   :statue           [\&]
   :human-skull      [\☻ :white]
   :gong             [\&]
   :stone-tablet     [\&]
   :robe             [\[]
   :codex            [\&]})

(defn item->char
  [item]
  (let [item-char-fg-bg (get items
                             (or (item :type)
                                 (item :item/id)))]
    (when-not item-char-fg-bg
      (log/info item))
    (or (first item-char-fg-bg) \?)))

(defn item->fg
  [item]
  (let [item-char-fg-bg (get items (or (item :type)
                                       (item :item/id)))]
    (or (second item-char-fg-bg)
        :white)))

(defn item->bg
  [item]
  (let [[_ _ bg _] (get items (or (item :type)
                                       (item :item/id)))]
    (or bg
        :black)))

(defn item->blend-mode
  [item]
  (let [[_ _ _ blend-mode](get items (or (item :type)
                                         (item :item/id)))]
    (or blend-mode
        :normal)))
