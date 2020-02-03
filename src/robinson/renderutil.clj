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
   :tarp-hung-corner [\▓ :blue :transparent]
   :spellbook       [\+]
   :scroll          [\?]
   :rock            [\*]
   :obsidian        [\*]
   :coconut         [\* :brown :black]
   :unhusked-coconut
                    [\* :brown  :black]
   :empty-coconut   [\* :brown  :black]
   :red-fruit       [\* :red    :black]
   :orange-fruit    [\* :orange :black]
   :yellow-fruit    [\* :yellow :black]
   :green-fruit     [\* :green  :black]
   :blue-fruit      [\* :blue   :black]
   :purple-fruit    [\* :purple :black]
   :white-fruit     [\* :white  :black]
   :black-fruit     [\* :gray   :black]
   :bamboo          [\/ :light-green  :black]
   :stick           [\/ :brown  :black]
   :grass           [\/ :green  :black]
   :rope            [\, :green  :black]
   :log             [\/ :brown  :black #{:bold}]
   :bedroll         [\▓ :dark-green :dark-beige]
   :$               [\$  :yellow :black #{:bold}]
   :amulet          [\" :blue   :black #{:bold}]
   :food            [\%]
   :fire-plough     [\,]
   :hand-drill      [\,]
   :bow-drill       [\,]
   :jack-o-lantern  [\☻ :orange :black]
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
   :human-skull      [\☻ :white :black]
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
        :transparent)))

(defn item->blend-mode
  [item]
  (let [[_ _ _ blend-mode](get items (or (item :type)
                                         (item :item/id)))]
    (or blend-mode
        :normal)))
