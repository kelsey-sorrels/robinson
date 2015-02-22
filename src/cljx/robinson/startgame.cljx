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
                                (assoc (ig/gen-match) :count 10)
                                (ig/gen-knife)
                                (ig/gen-plant-guide)
                                (assoc (ig/gen-bandage) :count 4)
                                (assoc (ig/gen-fishing-line-and-hook) :count 2)
                                (assoc (ig/gen-ration) :count 2)
                                (ig/gen-flashlight)
                                (ig/gen-bedroll)
                                (ig/gen-tarp)
                                (ig/gen-saw)]
        hotkeys                (vec (seq "abcdefghijklmnopqrstuvwxyzABCdEFGHIJKLMNOPQRSTUVWQYZ"))
        inventory-with-hotkeys (mapv #(assoc %1 :hotkey %2) inventory hotkeys)]
    (info "start-inventory" inventory-with-hotkeys)
    inventory-with-hotkeys))

(defn start-text []
  (let [mode-of-transport (dg/rand-nth ["boat" "airplane" "train" "blimp" "jetpack" "hovercraft" "bicycle"
                                        "sailboat" "steamboat" "barge" "oceanliner" "ferry" "helicopter"
                                        "biplane"])
        natural-disaster  (dg/rand-nth ["hurricane" "tornado" "dark storm" "cyclone" "squall"])]
    (format "While traveling by %s, a %s engulfs you.\n\n             You awake on an island.\n\n     One thing is certain - you'll have to escape."
             mode-of-transport
             natural-disaster)))
