;; Functions for generating random monsters
(ns dungeon-crusade.monstergen
  (:require [clojure.math.combinatorics :as combo]))

(defn can-move-in-water?
  [race]
  (contains? #{:shark :fish :octopus :sea-snake :clam :urchin :squid} race))

(defrecord Monster [race name hp body-parts attacks movement-policy disposition]
  Object
  (toString [this] (str "#Monster" (into {} this))))

(defn gen-rat
  "Generate one rat."
  []
  (Monster.
   :rat
   "rat"
   9
   #{:head :neck :body :leg :tail}
   #{:bite :claw}
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
   #{:bite :claw :sting}
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

(defn gen-bat
  "Generate one bat"
  []
  (Monster.
   :bat
   "bat"
   8
   #{:head :body :wing :leg :face}
   #{:bite}
   :follow-player
   #{:hostile}))

(defn gen-boar
  "Generate one boar"
  []
  (Monster.
   :boar
   "boar"
   18
   #{:head :body :tail :snout :face :eye :leg}
   #{:bite :gore}
   :follow-player
   #{:hostile}))

(defn gen-gecko
  "Generate one gecko"
  []
  (Monster.
   :gecko
   "gecko"
   2
   #{:head :body :tail :leg}
   #{:bite}
   :follow-player
   #{:hostile}))

(defn gen-monkey
  "Generate one monkey"
  []
  (Monster.
   :monkey
   "monkey"
   13
   #{:head :body :tail :leg :face :arm}
   #{:bite :punch}
   :follow-player
   #{:hostile}))

(defn gen-bird
  "Generate one bird"
  []
  (Monster.
   :bird
   "bird"
   6
   #{:head :body :tail :leg :beak :wing}
   #{:bite :claw}
   :follow-player
   #{:hostile}))

(defn gen-centipede
  "Generate one centipede"
  []
  (Monster.
   :centipede
   "centipede"
   4
   #{:head :body :leg}
   #{:bite}
   :follow-player
   #{:hostile}))

(defn gen-turtle
  "Generate one turtle"
  []
  (Monster.
   :turtle
   "turtle"
   4
   #{:head :body :leg :face :shell}
   #{:bite}
   :follow-player
   #{:hostile}))

(defn gen-frog
  "Generate one frog"
  []
  (Monster.
   :frog
   "frog"
   2
   #{:head :body :leg :face}
   #{:claw}
   :follow-player
   #{:hostile}))

(defn gen-parrot
  "Generate one parrot"
  []
  (Monster.
   :parrot
   "parrot"
   7
   #{:head :body :leg :face :wing :tail}
   #{:claw :bite}
   :follow-player
   #{:hostile}))

(defn gen-shark
  "Generate one shark"
  []
  (Monster.
   :shark
   "shark"
   16
   #{:head :body :fin :nose :tail}
   #{:bite}
   :follow-player
   #{:hostile}))

(defn gen-fish
  "Generate one fish"
  []
  (Monster.
   :fish
   "fish"
   4
   #{:head :body :fin :tail}
   #{:bite}
   :follow-player
   #{:hostile}))

(defn gen-monster [level cell-type]
  "Generate one random monster."
  (let [land-monster-fns  [gen-rat
                           gen-spider
                           gen-scorpion
                           gen-snake
                           gen-bat
                           gen-boar
                           gen-gecko
                           gen-monkey
                           gen-bird
                           gen-centipede
                           gen-turtle
                           gen-frog
                           gen-parrot]
        water-monster-fns [gen-shark
                           gen-fish]]
    (case cell-type
      :water ((rand-nth water-monster-fns))
      ((rand-nth land-monster-fns)))))

(defn gen-monsters
  "Generate `n` random monsters using `gen-monster`."
  [n]
  (repeatedly n gen-monster 1 :floor))


(defn -main
  "Generate five random monsters and display them."
  [& args]
  (println "generating...")
  (println (gen-monsters 5)))
