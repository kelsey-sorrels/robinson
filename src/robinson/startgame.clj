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
  (let [inventory              [(ig/gen-rope)
                                (repeat 10 (ig/gen-match))
                                (ig/gen-knife)
                                (ig/gen-plant-guide)
                                (repeat 10 (ig/gen-bandage))
                                (repeat 2 (ig/gen-fishing-line-and-hook))
                                (repeat 2 (ig/gen-ration))
                                (ig/gen-flashlight)
                                (ig/gen-bedroll)
                                (ig/gen-tarp)
                                (ig/gen-saw)]
        hotkeys                (vec (seq "abcdefghijklmnopqrstuvwxyzABCdEFGHIJKLMNOPQRSTUVWQYZ"))
        inventory-with-hotkeys (mapv #(assoc %1 :hotkey %2) inventory hotkeys)]
    inventory-with-hotkeys))

(defn start-text []
  (let [mode-of-transport (dg/rand-nth ["boat" "airplane" "train" "blimp" "jetpack" "hovercraft" "bicycle"
                                        "sailboat" "steamboat" "barge" "oceanliner" "ferry" "helicopter"
                                        "biplane"])
        natural-disaster  (dg/rand-nth ["hurricane" "tornado" "dark storm" "cyclone" "squall"])]
    (format "While traveling by %s, a %s engulfs you.\n        You awake on an island."
             mode-of-transport
             natural-disaster)))
