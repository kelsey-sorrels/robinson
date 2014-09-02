;; Functions for generating random monsters
(ns dungeon-crusade.monstergen
  (:require [clojure.math.combinatorics :as combo]))

(defn can-move-in-water?
  [race]
  (contains? #{:shark :fish :octopus :sea-snake :clam :urchin :squid} race))

(defrecord Monster [race name hp energy speed body-parts attacks movement-policy range-threshold disposition]
  Object
  (toString [this] (str "#Monster" (into {} this))))

(defn gen-rat
  "Generate one rat."
  []
  (Monster.
   :rat
   "rat"
   9
   0
   1.1
   #{:head :neck :body :leg :tail}
   #{:bite :claw}
   :follow-player-in-range-or-random
   5
   #{:hostile}))

(defn gen-spider
  "Generate one spider"
  []
  (Monster.
   :spider
   "spider"
   4
   0
   0.9
   #{:face :leg :abdomen}
   #{:bite}
   :follow-player-in-range-or-random
   5
   #{:hostile}))

(defn gen-scorpion
  "Generate one scorpion"
  []
  (Monster.
   :scorpion
   "scorpion"
   3
   0
   1.1
   #{:head :claw :abdomen :tail}
   #{:bite :claw :sting}
   :follow-player-in-range-or-random
   4
   #{:hostile}))

(defn gen-snake
  "Generate one snake"
  []
  (Monster.
   :snake
   "snake"
   8
   0
   0.8
   #{:head :body :tail}
   #{:bite}
   :follow-player-in-range-or-random
   5
   #{:hostile}))

(defn gen-bat
  "Generate one bat"
  []
  (Monster.
   :bat
   "bat"
   8
   0
   1.6
   #{:head :body :wing :leg :face}
   #{:bite}
   :follow-player-in-range-or-random
   7
   #{:hostile}))

(defn gen-boar
  "Generate one boar"
  []
  (Monster.
   :boar
   "boar"
   18
   0
   1.2
   #{:head :body :tail :snout :face :eye :leg}
   #{:bite :gore}
   :follow-player-in-range-or-random
   7
   #{:hostile}))

(defn gen-gecko
  "Generate one gecko"
  []
  (Monster.
   :gecko
   "gecko"
   2
   0
   0.9
   #{:head :body :tail :leg}
   #{:bite}
   :follow-player-in-range-or-random
   4
   #{:hostile}))

(defn gen-monkey
  "Generate one monkey"
  []
  (Monster.
   :monkey
   "monkey"
   13
   0
   1.2
   #{:head :body :tail :leg :face :arm}
   #{:bite :punch}
   :follow-player-in-range-or-random
   10
   #{:hostile}))

(defn gen-bird
  "Generate one bird"
  []
  (Monster.
   :bird
   "bird"
   6
   0
   2.1
   #{:head :body :tail :leg :beak :wing}
   #{:bite :claw}
   :follow-player-in-range-or-random
   8
   #{:hostile}))

(defn gen-centipede
  "Generate one centipede"
  []
  (Monster.
   :centipede
   "centipede"
   4
   0
   0.5
   #{:head :body :leg}
   #{:bite}
   :follow-player-in-range-or-random
   3
   #{:hostile}))

(defn gen-turtle
  "Generate one turtle"
  []
  (Monster.
   :turtle
   "turtle"
   4
   0
   0.5
   #{:head :body :leg :face :shell}
   #{:bite}
   :follow-player-in-range-or-random
   5
   #{:hostile}))

(defn gen-frog
  "Generate one frog"
  []
  (Monster.
   :frog
   "frog"
   2
   0
   0.9
   #{:head :body :leg :face}
   #{:claw}
   :follow-player-in-range-or-random
   4
   #{:hostile}))

(defn gen-parrot
  "Generate one parrot"
  []
  (Monster.
   :parrot
   "parrot"
   7
   0
   2.1
   #{:head :body :leg :face :wing :tail}
   #{:claw :bite}
   :follow-player-in-range-or-random
   10
   #{:hostile}))

(defn gen-shark
  "Generate one shark"
  []
  (Monster.
   :shark
   "shark"
   16
   0
   1.4
   #{:head :body :fin :nose :tail}
   #{:bite}
   :follow-player-in-range-or-random
   10
   #{:hostile}))

(defn gen-fish
  "Generate one fish"
  []
  (Monster.
   :fish
   "fish"
   4
   0
   1.2
   #{:head :body :fin :tail}
   #{:bite}
   :follow-player-in-range-or-random
   4
   #{:hostile}))

(defn gen-octopus
  "Generate one octopus"
  []
  (Monster.
   :octopus
   "octopus"
   4
   0
   1.5
   #{:head :body :tentacle}
   #{:bite :squeeze}
   :follow-player-in-range-or-random
   2
   #{:hostile}))

(defn gen-sea-snake
  "Generate one sea snake"
  []
  (Monster.
   :sea-snake
   "sea snake"
   4
   0
   1.5
   #{:head :body}
   #{:bite}
   :follow-player-in-range-or-random
   2
   #{:hostile :venomous}))

(defn gen-clam
  "Generate one clam"
  []
  (Monster.
   :clam
   "clam"
   2
   0
   0.1
   #{:shell}
   #{:clamp}
   :constant
   1
   #{:hostile}))

(defn gen-urchin
  "Generate one urchin"
  []
  (Monster.
   :urchin
   "urchin"
   2
   0
   0.1
   #{:body}
   #{:spike}
   :constant
   1
   #{:hostile}))

(defn gen-squid
  "Generate one squid"
  []
  (Monster.
   :squid
   "squid"
   4
   0
   1.5
   #{:head :body :tentacle}
   #{:bite :squeeze}
   :follow-player-in-range-or-random
   2
   #{:hostile}))



(defn gen-monster [level cell-type]
  "Generate one random monster."
  (let [land-monster-fns [ gen-rat
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
                           gen-fish
                           gen-octopus 
                           gen-sea-snake 
                           gen-clam 
                           gen-urchin 
                           gen-squid]]
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
