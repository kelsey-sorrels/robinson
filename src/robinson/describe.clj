;; Functions that help construct description messages.
(ns robinson.describe
  (:require
    [robinson.random :as rr]
    [robinson.itemgen  :as ig]
    [robinson.monstergen :as mg]
    [clojure.string :refer [lower-case]]
    [robinson.common :as rc :refer [append-log
                                    noun->indefinite-article]]
    [robinson.world :as rw :refer [get-cell
                                   npc-at-xy]]
    [robinson.npc :as rnpc :refer []]
    [robinson.player :as rp :refer [player-xy]]
    [robinson.macros :as rm]
    clojure.string))

(defn format [s & args]
  (apply clojure.core/format s args))

(defn describe-cell-type
  [cell]
  (or
    (and cell
         (case (get cell :type)
           :water                  "water"
           :surf                   "surf"
           :sand                   "sand"
           :dune                   "dune"
           :rocky-shore            "rocky shore"
           :dirt                   "dirt"
           :gravel                 "gravel"
           :tree                   "a tree"
           :palm-tree              "a palm tree"
           :fruit-tree             "a fruit tree"
           :tall-grass             "tall grass"
           :short-grass            "short grass"
           :bamboo                 "a bamboo grove"
           :mountain               "a mountain"
           :shallow-water          "shallow water"
           :swamp                  "swamp"
           :lava                   "a lava flow"
           :freshwater-hole        (if (> (get cell :water 0) 10)
                                     "a hole full of water"
                                     "an empty hole")
           :saltwater-hole         (if (> (get cell :water 0) 10)
                                      "a hole full of water"
                                      "an empty hole")
           :palisade               "a palisade"
           :bamboo-water-collector "a bamboo water collector"
           :open-door              "an open door"
           :close-door             "a closed door"
           :down-stairs            "a set of stairs going down"
           :up-stairs              "a set of stairs going up"
           ;; pirate ship
           :bulkhead               "a bulkhead"
           :wheel                  "a wheel"
           :bulkhead2              "a bulkhead"
           :wooden-wall            "a wooden wall"
           :railing                "railing"
           :hammock-v              "a hammock"
           :hammock-h              "a hammock"
           :deck                   "the deck"
           :tackle                 "tackle"
           :cannon                 "a cannon"
           :cannon-trunk-1         "a cannon truck"
           :cannon-trunk-2         "a cannon truck"
           :cannon-breach          "a cannon breach"
           :grate                  "a grate"
           :table                  "a table"
           :chair                  "a chair"
           :mast                   "a mast"
           :beam                   "a beam"
           :locker                 "a locker"
           :locker2                "a locker"
           :ships-wheel            "the ship's wheel"
           :ladder                 "a ladder"
           :porthole               "a porthole"
           :artifact-chest         "a chest"
           :chest                  "a chest"
           ;; ruined temple
           :floor                  "floor"
           :corridor               "path"
           :vertical-wall          "a wall"
           :horizontal-wall        "a wall"
           :vertical-wall-alt      "a decorated wall"
           :horizontal-wall-alt    "a decorated wall"
           :upper-left-1           "a corner"
           :upper-right-1          "a corner"
           :bottom-left-1          "a corner"
           :bottom-right-1         "a corner"
           :upper-left-2           "an ornate corner"
           :upper-right-2          "an ornate corner"
           :bottom-left-2          "an ornate corner"
           :bottom-right-2         "an orante corner"
           :altar                  "an altar"
           :vine                   "a vine"
           :moss-corridor          "a mossy corridor"
           :moss-vertical-wall     "a mossy wall"
           :moss-horizontal-wall   "a mossy wall"
           :moss-vertical-wall-alt "a mossy decorated wall"
           :moss-horizontal-wall-alt "a mossy decorated wall"
           :moss-upper-left-1      "a mossy corner"
           :moss-upper-right-1     "a mossy corner"
           :moss-bottom-left-1     "a mossy coner"
           :moss-bottom-right-1    "a mossy corner"
           :moss-upper-left-2      "a mossy ornate corner"
           :moss-upper-right-2     "a mossy ornate corner"                                                
           :moss-bottom-left-2     "a mossy ornate corner"
           :moss-bottom-right-2    "a mossy ornate corner"
           :white-corridor         "a mossy ornate corner"
           :white-vertical-wall    "a pale wall"
           :white-horizontal-wall  "a pale wall"
           :white-vertical-wall-alt "a pale ornate wall"
           :white-horizontal-wall-alt "a pale ornate wall"
           :white-upper-left-1     "a pale corner"
           :white-upper-right-1    "a pale corner"
           :white-bottom-left-1    "a pale corner"
           :white-bottom-right-1   "a pale corner"
           :white-upper-left-2     "a pale ornate corner"
           :white-upper-right-2    "a pale ornate corner"
           :white-bottom-left-2    "a pale ornate corner"
           :white-bottom-right-2   "a pale ornate corner"
           :crushing-wall-trigger  (if (get cell :trap-found)
                                     "a spiked wall trap trigger"
                                     (describe-cell-type (assoc cell :type :floor)))
           :wall-darts-trigger     (if (get cell :trap-found)
                                     "a dart trap trigger"
                                     (describe-cell-type (assoc cell :type :floor)))
           :poisonous-gas-trigger  (if (get cell :trap-found)
                                     "a poisonous gas trap trigger"
                                     (describe-cell-type (assoc cell :type :floor)))
           :spike-pit              (if (get cell :trap-found)
                                     "a spike pit"
                                     (describe-cell-type (assoc cell :type :floor)))
           :snakes-trigger         (if (get cell :trap-found)
                                     "a snake trap trigger"
                                     (describe-cell-type (assoc cell :type :floor)))))
    "strange land"))

(defn describe-npc
  [npc]
  (let [status (case (rnpc/npc-health-status npc)
                 :critical "critically wounded"
                 :badly-wounded "badly wounded"
                 :wounded "wounded"
                 :injured "injured"
                 :bruised "bruised"
                 :fine)]
    (if (= status :fine)
      (clojure.string/join " " [(noun->indefinite-article (get npc :name)) (get npc :name)])
      (clojure.string/join " " [(noun->indefinite-article status) status (get npc :name)])))) 

(defn describe-items
  [items]
  (clojure.string/join ", " (map (fn [[item-id num-items]]
                                   (clojure.string/join " "
                                                        [(if (= num-items 1)
                                                           (noun->indefinite-article (ig/id->name item-id))
                                                           num-items)
                                                         (if (= num-items 1)
                                                           (get (first (filter #(= (get % :item/id) item-id)
                                                                               items))
                                                                :name)
                                                           (get (first (filter #(= (get % :item/id) item-id)
                                                                               items))
                                                                :name-plural))
                                                         (if (= item-id :raft)
                                                           "(M)ount" "")]))
                                (frequencies (map :item/id items)))))
(defn describe-cell-at-xy
  [state x y]
  (let [cell        (get-cell state x y)
        npc         (npc-at-xy state x y)
        items       (get cell :items)
        harvest-msg (if (get cell :harvestable false)
                      " (harvestable)"
                      "")]

    (cond
      (let [discovered (get cell :discovered)]
          (or (nil? discovered)
              (< discovered (rw/get-time state))))
        (format "You can't see that.")
      (and npc (seq items))
        (format "There is %s, and %s.%s"
                (describe-npc npc)
                (describe-items items)
                harvest-msg)
      npc
        (format "There is %s on %s.%s"
                (describe-npc npc)
                (describe-cell-type cell)
                harvest-msg)
      (seq items)
        (format "On the %s there %s %s.%s"
                (str (describe-cell-type cell))
                (str (if (> (count items) 1)
                  "are"
                  "is"))
                (str (describe-items items))
                harvest-msg)
      :else
        (format "There is %s.%s"
                (describe-cell-type cell)
                harvest-msg))))

(defn search
  "Describe the player's cell."
  [state]
  (let [xy (player-xy state)
        description (apply describe-cell-at-xy (update-in state [:world :time] dec) xy)]
    (append-log state (format "Here %s%s" (clojure.string/lower-case (first description))
                                           (apply str (rest description))))))

(defn extended-search
  "Search the Moore neighborhood for hidden things and make them visible. Describe interesting items in the log."
  [state]
  (let [[x y]            (player-xy state)
        directions       [["To the north-west" -1 -1] ["To the north" 0 -1] ["To the north-east" 1 -1]
                          ["To the west" -1 0]        ["At your feet" 0 0]  ["To the east" 1 0]
                          ["To the south-west" -1 1]  ["To the south" 0 1]  ["To the south-east" 1 1]]
        describe-cell-fn (fn [state direction x y]
                           (if (get-cell state x y)
                             (append-log state (format "%s %s." direction (describe-cell-at-xy state x y)))
                             state))]
    (-> state
      (search)
      ((fn [state]
        (reduce (fn [state [direction dx dy]]
                  (describe-cell-fn state direction (+ x dx) (+ y dy)))
                state
                directions))))))

(defn gen-temple-text
  [text-id]
  (case text-id
    :secret-history "the secret histories of deathless kings"
    :mirror "a mirror that reflects incorporeal attributes"
    :cistern "a cistern of sacrificial blood"
    :cocoon "a cocoon of the God-Thing"
    :skulls "a collection of heretic skulls"
    :angles "a list of names of angels"
    :demons "a list of names of demons"
    :sins   "a bag of stones with sins inscribed on them"
    :red-fruit-safe "the virtues of red fruit"
    :red-fruit-poisonous "the evils of red fruit"
    :orange-fruit-safe "the purity of orange fruit"
    :orange-fruit-poisonous "the corruptness of orange fruit"
    :yellow-fruit-safe "the righteousness of yellow fruit"
    :yellow-fruit-poisonous "the wickedness of yellow fruit"
    :green-fruit-safe "the fineness of green fruit"
    :green-fruit-poisonous "the wrath of green fruit"
    :blue-fruit-safe "the worthiness of blue fruit"
    :blue-fruit-poisonous "the harm of blue fruit"
    :purple-fruit-safe "the superiority of purple fruit"
    :purple-fruit-poisonous "the foulness of purple fruit"
    :white-fruit-safe "the prestige of white fruit"
    :white-fruit-poisonous "the inferiority of white fruit"
    :black-fruit-safe "the excellence of black fruit"
    :black-fruit-poisonous "the poisonousness of black fruit"
    "unspeakable horrors of the island"))

(defn describe-encounter
  [encounter]
  (case encounter
    :temple
      (rr/rand-nth ["You come upon a temple.\nMoss covers its walls and a mist seeps\nout of its entrances."])
    :pirate-ship
      (rr/rand-nth ["In the clearing you see the impossible - a washed up ship.\nThat's strange."])))
