;; Functions for helping with start of game
(ns robinson.startgame
  (:use     robinson.common)
  (:require [robinson.monstergen :as mg]
            [robinson.itemgen :as ig]
            [clojure.data.generators :as dg]
            [taoensso.timbre :as timbre]
            [pallet.thread-expr :as tx]))

(timbre/refer-timbre)

(defn start-inventory []
  (let [inventory              [(ig/gen-rope 1)
                                (ig/gen-matches 1)
                                (ig/gen-knives 1)
                                (ig/gen-plant-guides 1)
                                (ig/gen-bandages 10)
                                (ig/gen-fishing-line-and-hooks 2)
                                (ig/gen-rations 2)
                                (ig/gen-flashlights 1)
                                (ig/gen-bedrolls 1)
                                (ig/gen-tarps 1)
                                (ig/gen-saws 1)]
        hotkeys                (vec (seq "abcdefghijklmnopqrstuvwxyzABCdEFGHIJKLMNOPQRSTUVWQYZ"))
        inventory-with-hotkeys (mapv #(assoc %1 :hotkey %2) inventory hotkeys)]
    inventory-with-hotkeys))

(defn start-text []
  (let [mode-of-transport (dg/rand-nth ["boat" "airplane" "train" "blimp" "hovercraft" "bicycle"
                                        "sailboat" "steamboat" "barge" "oceanliner" "ferry" "helicopter"
                                        "biplane"])
        natural-disaster  (dg/rand-nth ["hurricane" "tornado" "dark storm" "cyclone" "squall"])]
    (format "While traveling by %s, a %s engulfs you.\n        You awake on an island."
             mode-of-transport
             natural-disaster)))
