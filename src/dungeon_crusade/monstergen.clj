;; Functions for generating random monsters
(ns dungeon-crusade.monstergen
  (:require [clojure.math.combinatorics :as combo]))

(defrecord Monster [race name hp body-parts attacks movement-policy dispositiond])

(defn gen-rat
  "Generate one rat."
  []
  (Monster.
   :rat
   "rat"
   9
   #{:head :neck :body :leg :tail}
   #{:bite :claws}
   :follow-player
   #{:hostile}))

(defn gen-spider
  "Generate one spider"
  []
  (Monster.
   :spider
   "spider"
   4
   #{:face :leg :abdomen}
   #{:bite}
   :follow-player
   #{:hostile}))

(defn gen-scorpion
  "Generate one scorpion"
  []
  (Monster.
   :scorpion
   "scorpion"
   3
   #{:head :claw :abdomen :tail}
   #{:bite :claws :sting}
   :follow-player
   #{:hostile}))

(defn gen-snake
  "Generate one snake"
  []
  (Monster.
   :snake
   "snake"
   8
   #{:head :body :tail}
   #{:bite}
   :follow-player
   #{:hostile}))


(defn gen-monster [level cell-type]
  "Generate one random monster."
  (let [monster-fns [gen-rat
                     gen-spider
                     gen-scorpion
                     gen-snake]]
    ((rand-nth monster-fns))))

(defn gen-monsters
  "Generate `n` random monsters using `gen-monster`."
  [n]
  (repeatedly n gen-monster 1 :floor))


(defn -main
  "Generate five random monsters and display them."
  [& args]
  (println "generating...")
  (println (gen-monsters 5)))
