;; Functions that help construct description messages.
(ns robinson.describe
  (:use
    [clojure.string :only [lower-case]]
    clojure.contrib.core
    robinson.common
    robinson.npc
    robinson.player)
  (:require
    [robinson.itemgen  :as ig]
    [robinson.monstergen :as mg]
    [taoensso.timbre :as timbre]))

(timbre/refer-timbre)

(defn describe-cell-type
  [cell]
  (and cell
       (case (get cell :type)
         :water                  "water"
         :sand                   "sand"
         :dirt                   "dirt"
         :gravel                 "gravel"
         :tree                   "a tree"
         :palm-tree              "a palm tree"
         :fruit-tree             "a fruit tree"
         :tall-grass             "tall grass"
         :short-grass            "short grass"
         :bamboo                 "a bamboo grove"
         :freshwater-hole        (if (> (get cell :water 0) 10)
                                   "a hole full of water"
                                   "an empty hole")
         :saltwater-hole         (if (> (get cell :water 0) 10)
                                    "a hole full of water"
                                    "an empty hole")
         :palisade               "a palisade"
         :bamboo-water-collector "a bamboo water collector")))

(defn describe-npc
  [npc]
  (clojure.string/join " " [(noun->indefinite-article (get npc :name)) (get npc :name)]))

(defn describe-items
  [items]
  (clojure.string/join ", " (map (fn [[item-id num-items]]
                                   (clojure.string/join " "
                                                        [(if (= num-items 1)
                                                           (noun->indefinite-article (ig/id->name item-id))
                                                           num-items)
                                                         (if (= num-items 1)
                                                           (ig/id->name item-id)
                                                           (ig/id->name-plural item-id))]))
                                (frequencies (map :id items)))))
(defn describe-cell-at-xy
  [state x y]
  (let [cell (get-cell-at-current-place state x y)
        npc  (npc-at-xy state x y)
        items (get cell :items)]
    (cond
      (and npc (seq items))
        (format "there is %s, and %s" (describe-npc npc) (describe-items items))
      npc
        (format "there is %s, on %s" (describe-npc npc) (describe-cell-type cell))
      (seq items)
        (format "on the %s, there %s %s" (describe-cell-type cell)
                                         (if (> (count items) 1)
                                           "are"
                                           "is")
                                         (describe-items items))
      :else
        (format "there is %s" (describe-cell-type cell)))))

(defn search
  "Describe the player's cell."
  [state]
  (let [xy (player-xy state)]
    (append-log state (format "Here %s." (apply describe-cell-at-xy state xy)))))

(defn extended-search
  "Search the Moore neighborhood for hidden things and make them visible. Describe interesting items in the log."
  [state]
  (let [[x y]            (player-xy state)
        directions       [["To the north-west" -1 -1] ["To the north" 0 -1] ["To the north-east" 1 -1]
                          ["To the west" -1 0]        ["At your feet" 0 0]  ["To the east" 1 0]
                          ["To the south-west" -1 1]  ["To the south" 0 1]  ["To the south-east" 1 1]]
        describe-cell-fn (fn [state direction x y]
                           (if (get-cell-at-current-place state x y)
                             (append-log state (format "%s %s." direction (describe-cell-at-xy state x y)))
                             state))]
    (-> state
      (search)
      ((fn [state]
        (reduce (fn [state [direction dx dy]]
                  (describe-cell-fn state direction (+ x dx) (+ y dy)))
                state
                directions))))))

