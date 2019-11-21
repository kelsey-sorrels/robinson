;; Functions for helping with start of game
(ns robinson.startgame
  (:require [robinson.common :as rc]
            [taoensso.timbre :as log]
            [robinson.random :as rr]
            [robinson.monstergen :as mg]
            [robinson.itemgen :as ig]))

(defn format [s & args]
  (apply clojure.core/format s args))

(defn start-inventory []
  (let [inventory              [(ig/gen-item :rope)
                                (assoc (ig/gen-item :match) :count 10)
                                (ig/gen-item :knife)
                                (ig/gen-item :plant-guide)
                                (assoc (ig/gen-item :bandage) :count 4)
                                (assoc (ig/gen-item :fishing-line-and-hook) :count 2)
                                (assoc (ig/gen-item :ration) :count 2)
                                (ig/gen-item :lantern)
                                (ig/gen-item :bedroll)
                                (ig/gen-item :tarp)
                                (ig/gen-item :saw)]
        hotkeys                (vec (seq "abcdefghijklmnopqrstuvwxyzABCdEFGHIJKLMNOPQRSTUVWQYZ"))
        inventory-with-hotkeys (mapv #(assoc %1 :hotkey %2) inventory hotkeys)]
    #_(log/info "start-inventory" inventory-with-hotkeys)
    inventory-with-hotkeys))

(defn start-text [state]
  (let [[mode-of-transport
         natural-disaster
         group
         group-name
         attack
         unconscious-action] (mapv (fn [corpus rnd]
                                     (nth corpus (mod rnd (count corpus))))
                                   [["boat" "carriage" "horseback" "wagon" "stagewagon"
                                     "sailboat" "barge" "ship" "ferry" "foot"]
                                    ["hurricane" "tornado" "dark storm" "cyclone" "squall"]
                                    ["band" "troop" "company" "gand" "party" "horde"]
                                    ["bandits" "pirates" "marauders" "theives" "cultists" "sectarians"]
                                    ["ambush" "stalk and overpower" "trick and hypnotize" "surround and beat" "assail"]
                                    ["falling unconscious" "being stunned" "getting knocked out" "being overpowered" "being clobbered" "getting trounced"]]
                                   (get-in state [:world :random-numbers]))
        which             (first (take 1 (get-in state [:world :random-numbers])))
        text              (if (even? which)
                            (format "While traveling by %s, a %s engulfs you.\nYou awake on an island.\nOne thing is certain - you'll have to escape."
                              mode-of-transport natural-disaster)
                             (format "While traveling by %s, a %s of %s %s you.\nAfter %s, you awake on an island.\nOne thing is certain - you'll have to escape."
                               mode-of-transport group group-name attack unconscious-action))]
    text))
