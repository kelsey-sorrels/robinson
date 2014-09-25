;; Functions for generating random monsters
(ns robinson.monstergen
  (:require [clojure.math.combinatorics :as combo]))

(defn can-move-in-water?
  [race]
  (contains? #{:shark :fish :octopus :sea-snake :clam :urchin :squid} race))

(defrecord Monster [race name name-plural hp energy speed body-parts attacks movement-policy range-threshold status]
  Object
  (toString [this] (str "#Monster" (into {} this))))

(defn gen-rat
  "Generate one rat."
  []
  (Monster.
   :rat
   "rat"
   "rats"
   9
   0
   1.1
   #{:face :head :neck :body :leg :tail}
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
   "spiders"
   4
   0
   0.9
   #{:face :leg :abdomen}
   #{:bite-venom}
   :follow-player-in-range-or-random
   5
   #{:hostile}))

(defn gen-scorpion
  "Generate one scorpion"
  []
  (Monster.
   :scorpion
   "scorpion"
   "scorpions"
   3
   0
   1.1
   #{:head :claw :leg :abdomen :tail}
   #{:bite :claw :sting-venom}
   :follow-player-in-range-or-random
   4
   #{:hostile}))

(defn gen-snake
  "Generate one snake"
  []
  (Monster.
   :snake
   "snake"
   "snakes"
   8
   0
   0.8
   #{:head :body :tail}
   #{:bite :bite-venom}
   :follow-player-in-range-or-random
   5
   #{:hostile}))

(defn gen-bat
  "Generate one bat"
  []
  (Monster.
   :bat
   "bat"
   "bats"
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
   "boars"
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
   "geckos"
   2
   0
   0.9
   #{:head :face :body :tail :leg}
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
   "monkies"
   13
   0
   1.2
   #{:head :neck :body :tail :leg :face :arm}
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
   "birds"
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
   "centipedes"
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
   "turtles"
   4
   0
   0.5
   #{:head :neck :body :leg :face :shell}
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
   "frogs"
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
   "parrots"
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
   "sharks"
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
   "octopodes"
   4
   0
   1.5
   #{:head :body :tentacle}
   #{:bite :bite-venom :squeeze}
   :follow-player-in-range-or-random
   2
   #{:hostile}))

(defn gen-sea-snake
  "Generate one sea snake"
  []
  (Monster.
   :sea-snake
   "sea snake"
   "sea snakes"
   4
   0
   1.5
   #{:head :body}
   #{:bite :bite-venom}
   :follow-player-in-range-or-random
   2
   #{:hostile}))

(defn gen-clam
  "Generate one clam"
  []
  (Monster.
   :clam
   "clam"
   "clams"
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
   "urchins"
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
   "squids"
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

(defn id->monster
  "Generate monster from id."
  [id]
  ((case id
    :rat gen-rat
    :spider gen-spider
    :scorpion gen-scorpion
    :snake gen-snake
    :bat gen-bat
    :boar gen-boar
    :gecko gen-gecko
    :monkey gen-monkey
    :bird gen-bird
    :centipede gen-centipede
    :turtle gen-turtle
    :frog gen-frog
    :parrot gen-parrot
    :shark gen-shark
    :fish gen-fish
    :octopus  gen-octopus 
    :sea-snake  gen-sea-snake 
    :clam  gen-clam 
    :urchin  gen-urchin 
    :squid gen-squid)))
   
(defn id->name
  [id]
  (get (id->monster id) :name))

(defn id->name-plural
  [id]
  (get (id->monster id) :name-plural))

(defn -main
  "Generate five random monsters and display them."
  [& args]
  (if (contains? (set args) "--list")
    (let [monsters (map #(%)
                              [gen-rat
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
                              gen-parrot
                              gen-shark
                              gen-fish
                              gen-octopus 
                              gen-sea-snake 
                              gen-clam 
                              gen-urchin 
                              gen-squid])]
      (doseq [monster monsters]
        (doseq [part (get monster :body-parts)]
          (println "[:human :punch" (get monster :race) part "] (+ (rand) 0.5)"))
        (doseq [attack (get monster :attacks)]
          (doseq [part #{:head :neck :face :abdomen :arm :leg :foot}]
            (println "[" (get monster :race) attack ":human" part "] (+ (rand) 0.5)")))))
          
    (println (gen-monsters 5))))
