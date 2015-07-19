;; Functions for helping with start of game
(ns robinson.startgame
  (:require [robinson.common :as rc]
            [taoensso.timbre :as log]
            [robinson.random :as rr]
            [robinson.monstergen :as mg]
            [robinson.itemgen :as ig]
            #?@(:cljs (
                [goog.string :as gstring]
                [goog.string.format]))))

(defn format [s & args]
  #?(:clj
     (apply clojure.core/format s args)
     :cljs
     (apply gstring/format s args)))

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
    #_(log/info "start-inventory" inventory-with-hotkeys)
    inventory-with-hotkeys))

(defn start-text [state]
  (let [[n1 n2]            (get-in state [:world :random-numbers])
        modes-of-transport ["boat" "carriage" "horseback" "wagon" "stagewagon"
                           "sailboat" "barge" "ship" "ferry" "foot"]
        mode-of-transport (nth modes-of-transport (mod n1 (count modes-of-transport)))
        natural-disasters ["hurricane" "tornado" "dark storm" "cyclone" "squall"]
        natural-disaster  (nth natural-disasters (mod n2 (count natural-disasters)))
        text              (format "While traveling by %s, a %s engulfs you.\n\n             You awake on an island.\n\n     One thing is certain - you'll have to escape."
             mode-of-transport
             natural-disaster)]
    text))
