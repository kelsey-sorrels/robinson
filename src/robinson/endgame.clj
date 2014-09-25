;; Functions for manipulating player state
(ns robinson.endgame
  (:use     robinson.common)
  (:require [robinson.monstergen :as mg]
            [robinson.itemgen :as ig]
            [taoensso.timbre :as timbre]
            [pallet.thread-expr :as tx]))

(timbre/refer-timbre)


(defn gen-death-madlib
  [state]
  (let [death-msg-type (rand-nth [:num-animals-killed
                                  :num-items-crafted
                                  :num-items-harvested
                                  :num-kills-by-attack-type
                                  :num-items-eaten])
        death-text     (case death-msg-type
                         :num-animals-killed
                           (let [[monster-id n] (first (reduce (fn [[id1 n1] [id2 n2]] (if (> n1 n2)
                                                                                         [id1 n1]
                                                                                         [id2 n2]))
                                                  (get-in state [:world :player :stats :num-animals-killed])))]
                             (format (rand-nth ["Slayer of %s" "Defeater of %s" "Eliminator of %s" "Dominator of %" "The nightmare of %s"])
                                     (mg/id->name-plural monster-id)))
                         :num-items-crafted
                           (let [[item-id n] (first (reduce (fn [[id1 n1] [id2 n2]] (if (> n1 n2)
                                                                                      [id1 n1]
                                                                                      [id2 n2]))
                                                  (get-in state [:world :player :stats :num-items-crafted])))]
                             (format (rand-nth ["Artisan of %s" "Maker of %s" "Crafter of %s"])
                                     (ig/id->name-plural item-id)))
                         :num-items-harvested
                           (let [[item-id n] (first (reduce (fn [[id1 n1] [id2 n2]] (if (> n1 n2)
                                                                                      [id1 n1]
                                                                                      [id2 n2]))
                                                  (get-in state [:world :player :stats :num-items-harvested])))]
                             (format (rand-nth ["Gatherer of %s" "Farmer of %s" "Finder of %s"])
                                     (ig/id->name-plural item-id)))
                         :num-kills-by-attack-type
                           (let [[attack-id n] (first (reduce (fn [[id1 n1] [id2 n2]] (if (> n1 n2)
                                                                                      [id1 n1]
                                                                                      [id2 n2]))
                                                  (get-in state [:world :player :stats :num-kills-by-attack-type])))]
                             (rand-nth
                               (case attack-id
                                 :punch (rand-nth ["Puncher of enemies." "Who fought with fists."])
                                 :axe   (rand-nth ["Who axed enemies." "Axer of enemies."]))))
                         :num-items-eaten
                           (let [[item-id n] (first (reduce (fn [[id1 n1] [id2 n2]] (if (> n1 n2)
                                                                                      [id1 n1]
                                                                                      [id2 n2]))
                                                  (get-in state [:world :player :stats :num-items-harvested])))]
                             (format (rand-nth ["Eater of %s" "Who gorged on %s" "Purveyor of %s"])
                                     (ig/id->name-plural item-id))))]
    death-text))

