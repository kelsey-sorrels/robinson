;; Functions for manipulating player state
(ns robinson.endgame
  (:require [robinson.common :as rc]
            [taoensso.timbre :as log]
            [robinson.random :as rr]
            [robinson.monstergen :as mg]
            [robinson.itemgen :as ig]))

(defn format [s & args]
  (apply clojure.core/format s args))

(defn rnd-nth
  [coll n]
  (nth coll (mod n (count coll))))

(defn rescue-mode [state]
  (let [rescue-modes   ["boat" "helicopter" "hovercraft" "ocean liner"]]
    (nth rescue-modes (mod (get-in state [:world :random-numbers 2]) (count rescue-modes)))))

(defn gen-end-madlib
  [state]
  (let [[_ _ n1 n2 n3] (get-in state [:world :random-numbers])
        death-msg-type (rnd-nth [:num-animals-attacked
                                 :num-animals-killed
                                 :num-items-crafted
                                 :num-items-harvested
                                 :num-kills-by-attack-type
                                 :num-items-eaten]
                                n1)
        #_#__ (log/info "stats" (get-in state [:world :player :stats]))
        death-text     (case death-msg-type
                         :num-animals-attacked
                           (let [max-stat (reduce (fn [acc [id n]]
                                                    (cond
                                                      (nil? acc) [id n]
                                                      (> (last acc) n) acc
                                                      :else [id n]))
                                                  nil (get-in state [:world :player :stats :num-animals-attacked]))]
                             (if (nil? max-stat)
                               "A pacifist"
                               (let [[monster-id n] max-stat]
                                 (format (rnd-nth ["Threat to %s" "The nightmare of %s"] n2)
                                         (mg/id->name-plural monster-id)))))
                         :num-animals-killed
                           (let [max-stat (reduce (fn [acc [id n]]
                                                    (cond
                                                      (nil? acc) [id n]
                                                      (> (last acc) n) acc
                                                      :else [id n]))
                                                  nil (get-in state [:world :player :stats :num-animals-killed]))]
                             (if (nil? max-stat)
                               "Who didn't kill anything"
                               (let [[monster-id n] max-stat]
                                 (format (rnd-nth ["Slayer of %s" "Defeater of %s" "Eliminator of %s" "Dominator of %s" "The nightmare of %s"] n2)
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
                                  (log/info "item-id" item-id)
                                  (format (rnd-nth ["Artisan of %s" "Maker of %s" "Crafter of %s"] n2)
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
                                 (format (rnd-nth ["Gatherer of %s" "Farmer of %s" "Harvester of %s" "Finder of %s"] n2)
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
                                 (case attack-id
                                   :punch (rnd-nth ["Puncher of enemies." "Who fought with fists."] n2)
                                   :axe   (rnd-nth ["Who speared enemies." "Spearer of enemies."] n2)
                                   :spear (rnd-nth ["Who axed enemies." "Axer of enemies."] n2)))))
                         :num-items-eaten
                           (let [max-stat (reduce-kv (fn [acc id n]
                                                       (cond
                                                         (nil? acc) [id n]
                                                         (> (last acc) n) acc
                                                         :else [id n]))
                                                     nil (get-in state [:world :player :stats :num-items-eaten]))]
                             (if (nil? max-stat)
                               "Too picky to eat"
                               (let [[item-id n] max-stat]
                             (format (rnd-nth ["Eater of %s" "Who gorged on %s" "Purveyor of %s" "Connoisseur of %s"] n2)
                                     (ig/id->name-plural item-id))))))]
    death-text))

