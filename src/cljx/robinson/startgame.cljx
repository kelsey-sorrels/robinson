;; Functions for helping with start of game
(ns robinson.startgame
  (:require [robinson.common :as rc]
            [robinson.random :as rr]
            [robinson.monstergen :as mg]
            [robinson.itemgen :as ig]
            #+clj
            [taoensso.timbre :as log]
            #+cljs
            [shodan.console :as log :include-macros true]
            #+cljs
            [goog.string :as gstring]
            #+cljs
            [goog.string.format]))

(defn format [s & args]
  #+clj
  (apply clojure.core/format s args)
  #+cljs
  (apply gstring/format s args))

(defn start-inventory []
  (let [inventory              [(ig/gen-item :rope)
                                (assoc (ig/gen-item :match) :count 10)
                                (ig/gen-item :knife)
                                (ig/gen-item :plant-guide)
                                (assoc (ig/gen-item :bandage) :count 4)
                                (assoc (ig/gen-item :fishing-line-and-hook) :count 2)
                                (assoc (ig/gen-item :ration) :count 2)
                                (ig/gen-item :flashlight)
                                (ig/gen-item :bedroll)
                                (ig/gen-item :tarp)
                                (ig/gen-item :saw)]
        hotkeys                (vec (seq "abcdefghijklmnopqrstuvwxyzABCdEFGHIJKLMNOPQRSTUVWQYZ"))
        inventory-with-hotkeys (mapv #(assoc %1 :hotkey %2) inventory hotkeys)]
    (log/info "start-inventory" inventory-with-hotkeys)
    inventory-with-hotkeys))

(defn start-text []
  (let [mode-of-transport (rr/rand-nth ["boat" "airplane" "train" "blimp" "jetpack" "hovercraft" "bicycle"
                                        "sailboat" "steamboat" "barge" "oceanliner" "ferry" "helicopter"
                                        "biplane"])
        natural-disaster  (rr/rand-nth ["hurricane" "tornado" "dark storm" "cyclone" "squall"])]
    (format "While traveling by %s, a %s engulfs you.\n\n             You awake on an island.\n\n     One thing is certain - you'll have to escape."
             mode-of-transport
             natural-disaster)))
