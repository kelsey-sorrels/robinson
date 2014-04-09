(ns dungeon-crusade.itemgen
  (:require [clojure.math.combinatorics :as combo]))

(defn random-element [col]
  (nth col (rand-int (count col))))

(defn gen-food []
  (let [food [{:type :food :name "Ration" :hunger 100}
              {:type :food :name "Meatball" :hunger 40}
              {:type :food :name "Berries" :hunger 50}
              {:type :food :name "Lemon" :hunger 30}
              {:type :food :name "Yogurt leaves" :hunger 20}
              {:type :food :name "Lyrium leaves" :hunger 10}
              {:type :food :name "Mystery meat" :hunger (rand-int 100)}]]
    (random-element food)))

(defn gen-ring []
  (let [rings [{:type :ring :name "Ring of Gold"}
               {:type :ring :name "Ring of Meatballs"}
               {:type :ring :name "Ancient Ring"}
               {:type :ring :name "Ring of Invisibility"}
               {:type :ring :name "Ring of Cure Robotitis"}
               {:type :ring :name "Mystery Ring"}]]
    (random-element rings)))

(defn gen-scroll []
  (let [scrolls [{:type :scroll :name "Scroll of Gold (IOU)"}
                 {:type :scroll :name "Scroll of Meatballs"}
                 {:type :scroll :name "Ancient Scroll"}
                 {:type :scroll :name "Scroll of Invisibility"}
                 {:type :scroll :name "Robot Plans"}
                 {:type :scroll :name "Mystery Scroll"}]]
    (random-element scrolls)))

(defn gen-item []
  (let [item-fns [gen-food
                  gen-ring
                  gen-scroll]]
    ((random-element item-fns))))

(defn gen-items [n]
  (repeatedly n gen-item))


(defn -main [& args]
  (println "generating...")
  (println (gen-items 5)))
