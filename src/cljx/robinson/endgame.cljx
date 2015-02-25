;; Functions for manipulating player state
(ns robinson.endgame
  (:require [robinson.common :refer :all]
            [robinson.monstergen :as mg]
            [robinson.itemgen :as ig]
            [taoensso.timbre :as timbre]
            [clojure.data.generators :as dg]
            [pallet.thread-expr :as tx]))

(timbre/refer-timbre)


(defn gen-end-madlib
  [state]
  (let [death-msg-type (dg/rand-nth [:num-animals-killed
                                  :num-items-crafted
                                  :num-items-harvested
                                  :num-kills-by-attack-type
                                  :num-items-eaten])
        _ (info "stats" (get-in state [:world :player :stats]))
        death-text     (case death-msg-type
                         :num-animals-killed
                           (let [max-stat (reduce (fn [acc [id n]]
                                                    (cond
                                                      (nil? acc) [id n]
                                                      (> (last acc) n) acc
                                                      :else [id n]))
                                                  nil (get-in state [:world :player :stats :num-animals-killed]))]
                             (if (nil? max-stat)
                               "A pacifist"
                               (let [[monster-id n] max-stat]
                                 (format (dg/rand-nth ["Slayer of %s" "Defeater of %s" "Eliminator of %s" "Dominator of %" "The nightmare of %s"])
                                         (mg/id->name-plural monster-id)))))
                         :num-items-crafted
                           (let [max-stat (reduce (fn [acc [id n]]
                                                    (cond
                                                      (nil? acc) [id n]
                                                      (> (last acc) n) acc
                                                      :else [id n]))
                                                  nil (get-in state [:world :player :stats :num-items-crafted]))]
                             (if (nil? max-stat)
                               "The dainty-handed, who never made anything"
                                (let [[item-id n] max-stat]
                                  (info "item-id" item-id)
                                  (format (dg/rand-nth ["Artisan of %s" "Maker of %s" "Crafter of %s"])
                                          (ig/id->name-plural item-id)))))
                         :num-items-harvested
                           (let [max-stat (reduce (fn [acc [id n]]
                                                    (cond
                                                      (nil? acc) [id n]
                                                      (> (last acc) n) acc
                                                      :else [id n]))
                                                  nil (get-in state [:world :player :stats :num-items-harvested]))]
                             (if (nil? max-stat)
                               "Oblivious to nature's bounty"
                               (let [[item-id n] max-stat]
                                 (format (dg/rand-nth ["Gatherer of %s" "Farmer of %s" "Harvester of %s" "Finder of %s"])
                                         (ig/id->name-plural item-id)))))
                         :num-kills-by-attack-type
                           (let [max-stat (reduce (fn [acc [id n]]
                                                    (cond
                                                      (nil? acc) [id n]
                                                      (> (last acc) n) acc
                                                      :else [id n]))
                                                  nil (get-in state [:world :player :stats :num-kills-by-attack-type]))]
                             (if (nil? max-stat)
                               "A pacifist"
                               (let [[attack-id n] max-stat]
                                  (dg/rand-nth
                                    (case attack-id
                                      :punch (dg/rand-nth ["Puncher of enemies." "Who fought with fists."])
                                      :axe   (dg/rand-nth ["Who speared enemies." "Spearer of enemies."])
                                      :spear   (dg/rand-nth ["Who axed enemies." "Axer of enemies."]))))))
                         :num-items-eaten
                           (let [max-stat (reduce (fn [acc [id n]]
                                                    (cond
                                                      (nil? acc) [id n]
                                                      (> (last acc) n) acc
                                                      :else [id n]))
                                                  nil (get-in state [:world :player :stats :num-items-eaten]))]
                             (if (nil? max-stat)
                               "Too picky to eat"
                               (let [[item-id n] max-stat]
                             (format (dg/rand-nth ["Eater of %s" "Who gorged on %s" "Purveyor of %s" "Connoisseur of %s"])
                                     (ig/id->name-plural item-id))))))]
    death-text))

