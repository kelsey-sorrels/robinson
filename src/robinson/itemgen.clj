;; Functions for generating random items.
(ns robinson.itemgen
  (:use     robinson.common)
  (:require [clojure.math.combinatorics :as combo]))


(defn gen-corpse
  "Generate a corpse from an npc."
  [npc]
  {:type :food
   :name (format "%s corpse" (name (get npc :race)))
   :name-plural (format "%s corpses" (name (get npc :race)))
   ;; TODO: base on type of monster by including a weight field in all monsters
   :hunger (uniform-rand-int 5 10)})

(defn gen-sticks
  "Generate sticks."
  [n]
  {:id :stick :name "stick" :name-plural "sticks" :count n})

(defn gen-plant-fibers
  "Generate plant fibers"
  [n]
  {:id :plant-fiber :name "plant fiber" :name-plural "plant fibers" :count n})

(defn gen-coconuts
  "Generate coconuts"
  [n]
  {:id :coconut :name "coconut" :name-plural "coconuts" :count n})

(defn gen-wood-logs
  "Generate wood logs"
  [n]
  {:id :wood-log :name "wood log" :name-plural "wood logs" :count n})

(defn gen-sticks
  "Generate sticks"
  [n]
  {:id :stick :name "stick" :name-plural "sticks" :count n})

(defn gen-rocks
  "Generate rocks"
  [n]
  {:id :rock :name "rock" :name-plural "rocks" :count n})

(defn gen-obsidian
  "Generate obsidian"
  [n]
  {:id :obsidian :name "obsidian stone" :name-plural "obsidian stones" :count n})

(defn gen-grass
  "Generate grass"
  [n]
  {:id :grass :name "grass" :name-plural "grass" :count n})


(defn gen-food
  "Generate one random food item."
  []
  (let [food [{:id :ration       :type :food :name "ration"        :plural-name "rations"        :hunger 100}
              {:id :meatball     :type :food :name "meatball"      :plural-name "meatballs"      :hunger 40}
              {:id :berry        :type :food :name "berry"         :plural-name "berries"        :hunger 50}
              {:id :lemon        :type :food :name "lemon"         :plural-name "lemons"         :hunger 30}
              {:id :yogurt-leaf  :type :food :name "yogurt leaf"   :plural-name "yogurt leaves"  :hunger 20}
              {:id :lyrium-leaf  :type :food :name "lyrium leaf"   :plural-name "lyrium leaves"  :hunger 10}
              {:id :mystery-meat :type :food :name "mystery meat"  :plural-name "mystery meats"  :hunger (rand-int 100)}]]
    (rand-nth food)))

(defn gen-ring []
  "Generate one random ring."
  (let [rings [{:id :ring-of-gold          :type :ring :name "ring of gold"           :plural-name "rings of gold"}
               {:id :ring-of-meatballs     :type :ring :name "ring of meatballs"      :plural-name "rings of meatballs"}
               {:id :ancient-ring          :type :ring :name "ancient ring"           :plural-name "ancient rings"}
               {:id :ring-of-invisibility  :type :ring :name "ring of invisibility"   :plural-name "rings of invisibility"}
               {:id :mystery-ring          :type :ring :name "mystery ring"           :plural-name "mystery rings"}]]
    (rand-nth rings)))

(defn gen-scroll []
  "Generate one random scroll."
  (let [scrolls [{:id :scroll-of-gold         :type :scroll :name "scroll of gold (IOU)"   :plural-name "scrolls of gold (IOUs)"}
                 {:id :scroll-of-meatballs    :type :scroll :name "scroll of meatballs"    :plural-name "scrolls of meatballs"}
                 {:id :ancient-scroll         :type :scroll :name "ancient scroll"         :plural-name "ancient scrolls"}
                 {:id :scroll-of-invisibility :type :scroll :name "scroll of invisibility" :plural-name "scrolls of invisibility"}
                 {:id :robot-plans            :type :scroll :name "robot plans"            :plural-name "robot plans"}
                 {:id :mystery-scroll         :type :scroll :name "mystery scroll"         :plural-name "mystery scrolls"}]]
    (rand-nth scrolls)))

(defn gen-cash
  [max-cash]
  "Generate a random amount of cash between [1 and max-cash]."
  {:type :$ :amount (inc (rand-int max-cash))})

(defn gen-item []
  "Generate one random food, ring, or scroll item."
  (let [item-fns [gen-food
                  gen-ring
                  gen-scroll]]
    ((rand-nth item-fns))))

(defn gen-items
  "Generate `n` random items using `gen-item`."
  [n]
  (repeatedly n gen-item))


(defn -main
  "Generate five random items and display them."
  [& args]
  (println "generating...")
  (println (gen-items 5)))
