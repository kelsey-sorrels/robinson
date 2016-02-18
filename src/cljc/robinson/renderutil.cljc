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
   :flashlight      [\,]
   :bandage         [\&]
   :saw             [\,]
   :tarp            [\,]
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
   :bedroll         [\_ :white :black]
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
   :jewlery          [\"]
   :statue           [\&]
   :human-skull      [\☻ :white :black]
   :gong             [\&]
   :stone-tablet     [\&]
   :codex            [\&]})

(defn item->char
  [item]
  (let [item-char-fg-bg (get items
                             (or (item :type)
                                 (item :id))
                             [\?])]
    (first item-char-fg-bg)))

(defn item->fg
  [item]
  (let [item-char-fg-bg (get items (or (item :type)
                                       (item :id)))]
    (or (second item-char-fg-bg)
        :white)))
