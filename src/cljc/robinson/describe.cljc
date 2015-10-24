;; Functions that help construct description messages.
(ns robinson.describe
  (:require
    [robinson.itemgen  :as ig]
    [robinson.monstergen :as mg]
    [clojure.string :refer [lower-case]]
    [robinson.common :as rc :refer [append-log
                                    noun->indefinite-article]]
    [robinson.world :as rw :refer [get-cell
                                   npc-at-xy]]
    [robinson.npc :as rnpc :refer []]
    [robinson.player :as rp :refer [player-xy]]
    #?@(:clj (
        [robinson.macros :as rm]
        clojure.string)
        :cljs (
        [goog.string :as gstring]
        [goog.string.format])))
  #?(:cljs
  (:require-macros [robinson.macros :as rm])))

(defn format [s & args]
  #?(:clj
     (apply clojure.core/format s args)
     :cljs
     (apply gstring/format s args)))

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
           :cannon-trunk           "a cannon truck"
           :grate                  "a grate"
           :table                  "a table"
           :chair                  "a chair"
           :mast                   "a mast"
           :beam                   "a beam"
           :locker                 "a locker"
           :locker2                "a locker"
           :ships-wheel            "the ship's wheel"
           :porthole               "a porthole"
           :chest                  "a chest"))
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
                                                           (get (first (filter #(= (get % :id) item-id)
                                                                               items))
                                                                :name)
                                                           (get (first (filter #(= (get % :id) item-id)
                                                                               items))
                                                                :name-plural))]))
                                (frequencies (map :id items)))))
(defn describe-cell-at-xy
  [state x y]
  (let [cell        (get-cell state x y)
        npc         (npc-at-xy state x y)
        items       (get cell :items)
        harvest-msg (if (get cell :harvestable false)
                      " You can harvest(<color fg=\"highlight\" bg=\"white\">x</color>) from this place."
                      "")]

    (cond
      (not= (get cell :discovered) (rw/get-time state))
        (format "You can't see that.")
      (and npc (seq items))
        (format "There is %s, and %s.%s"
                (describe-npc npc)
                (describe-items items)
                harvest-msg)
      npc
        (format "There is %s, on %s.%s"
                (describe-npc npc)
                (describe-cell-type cell)
                harvest-msg)
      (seq items)
        (format "On the %s, there %s %s.%s"
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

