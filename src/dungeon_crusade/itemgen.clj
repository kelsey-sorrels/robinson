;; Functions for generating random items.
(ns dungeon-crusade.itemgen
  (:require [clojure.math.combinatorics :as combo]))

(defn gen-food
  "Generate one random food item."
  []
  (let [food [{:type :food :name "Ration" :hunger 100}
              {:type :food :name "Meatball" :hunger 40}
              {:type :food :name "Berries" :hunger 50}
              {:type :food :name "Lemon" :hunger 30}
              {:type :food :name "Yogurt leaves" :hunger 20}
              {:type :food :name "Lyrium leaves" :hunger 10}
              {:type :food :name "Mystery meat" :hunger (rand-int 100)}]]
    (rand-nth food)))

(defn gen-ring []
  "Generate one random ring."
  (let [rings [{:type :ring :name "Ring of Gold"}
               {:type :ring :name "Ring of Meatballs"}
               {:type :ring :name "Ancient Ring"}
               {:type :ring :name "Ring of Invisibility"}
               {:type :ring :name "Ring of Cure Robotitis"}
               {:type :ring :name "Mystery Ring"}]]
    (rand-nth rings)))

(defn gen-scroll []
  "Generate one random scroll."
  (let [scrolls [{:type :scroll :name "Scroll of Gold (IOU)"}
                 {:type :scroll :name "Scroll of Meatballs"}
                 {:type :scroll :name "Ancient Scroll"}
                 {:type :scroll :name "Scroll of Invisibility"}
                 {:type :scroll :name "Robot Plans"}
                 {:type :scroll :name "Mystery Scroll"}]]
    (rand-nth scrolls)))

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
