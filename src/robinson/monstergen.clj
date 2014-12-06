;; Functions for generating random monsters
(ns robinson.monstergen
  (:use     robinson.common)
  (:require [clojure.math.combinatorics :as combo]
            [clojure.data.generators :as dg]
            [taoensso.timbre :as timbre]))

(timbre/refer-timbre)

(defn can-move-in-water?
  [race]
  (contains? #{:shark :fish :octopus :sea-snake :clam :urchin :squid
               :crocodile :puffer-fish :crab :hermit-crab} race))

(defn can-move-on-land?
  [race]
  (contains? #{:rat :spider :scorpion :bat :boar :gecko :monkey :bird
               :centipede :turtle :red-frog :orange-frog :yellow-from
               :green-frog :blue-frog :purple-frog :parrot :crocodile
               :mosquito :mongoose :tarantula :monitor-lizard :komodo-dragon
               :cobra :crab :hermit-crab} race))

(defn can-spawn-intertidally?
  [race]
  (contains? #{:clam :urchin
               :turtle :crocodile
               :crab :hermit-crab} race))
  
(defrecord Monster [race name name-plural hp energy speed size strength toughness body-parts attacks temperament movement-policy range-threshold status]
  Object
  (toString [this] (str "#Monster" (into {} this))))

(def ^:private monsters
  [
  ;;        race            name             name-plural       hp
  ;;                                                             energy
  ;;                                                               speed size (kg)
  ;;                                                                           strength
  ;;                                                                               toughness
  ;;                                                                                  body-parts                                  attacks                       temperament             movement policy                       range-threshold
  ;;                                                                                                                                                                                                                            status
  (Monster. :rat            "rat"            "rats"            5 0 1.1   0.2   1    5 #{:face :head :neck :body :leg :tail}       #{:bite :claw}                :hostile-after-attacked :hide-from-player-in-range-or-random  5 #{:docile})
  (Monster. :spider         "spider"         "spiders"         2 0 0.9   0.01  0.1  1 #{:face :leg :abdomen}                      #{:bite-venom}                :retreat-after-attacked :random                               5 #{:docile})
  (Monster. :scorpion       "scorpion"       "scorpions"       3 0 1.1   0.01  0.1  1 #{:head :claw :leg :abdomen :tail}          #{:bite :claw :sting-venom}   :hostile-after-attacked :random                               4 #{:hostile})
  (Monster. :snake          "snake"          "snakes"          5 0 0.8   1     1    8 #{:head :body :tail}                        #{:bite :bite-venom}          :retreat-after-attackd  :random                               5 #{:hostile})
  (Monster. :bat            "bat"            "bats"            5 0 1.6   1     0.5  4 #{:head :body :wing :leg :face}             #{:bite}                      :hostile-after-attacked :random                               7 #{:hostile})
  (Monster. :boar           "boar"           "boars"          18 0 1.2  70    20    8 #{:head :body :tail :snout :face :eye :leg} #{:bite :gore}                :hostile                :random                               7 #{:hostile})
  (Monster. :gecko          "gecko"          "geckos"          2 0 0.9   0.1   0.5  5 #{:head :face :body :tail :leg}             #{:bite}                      :hostile-after-sound    :random                               4 #{:hostile})
  (Monster. :monkey         "monkey"         "monkies"        13 0 1.2  50    10    5 #{:head :neck :body :tail :leg :face :arm}  #{:bite :punch}               :hostile-after-attacked :follow-player-in-range-or-random    10 #{:hostile})
  (Monster. :bird           "bird"           "birds"           4 0 2.1   1     0.3  4 #{:head :body :tail :leg :beak :wing}       #{:bite :claw}                :retreat-after-sound    :random                               8 #{:docile})
  (Monster. :centipede      "centipede"      "centipedes"      1 0 0.5   0.1   0.1  1 #{:head :body :leg}                         #{:bite}                      :hostile-after-attacked :random                               3 #{:hostile})
  (Monster. :turtle         "turtle"         "turtles"         4 0 0.5  10     1.5 20 #{:head :neck :body :leg :face :shell}      #{:bite}                      :retreat-after-attacked :random                               5 #{:hostile})
  (Monster. :red-frog       "red frog"       "red frogs"       2 0 0.9   1     0.7  3 #{:head :body :leg :face}                   #{:claw}                      :hostile-after-attacked :random                               4 #{:docile})
  (Monster. :orange-frog    "orange frog"    "orange frogs"    2 0 0.9   1     0.7  3 #{:head :body :leg :face}                   #{:claw}                      :hostile-after-attacked :random                               4 #{:docile})
  (Monster. :yellow-frog    "yellow frog"    "yellow frogs"    2 0 0.9   1     0.7  3 #{:head :body :leg :face}                   #{:claw}                      :hostile-after-attacked :random                               4 #{:docile})
  (Monster. :green-frog     "green frog"     "green frogs"     2 0 0.9   1     0.7  3 #{:head :body :leg :face}                   #{:claw}                      :hostile-after-attacked :random                               4 #{:docile})
  (Monster. :blue-frog      "blue frog"      "blue frogs"      2 0 0.9   1     0.7  3 #{:head :body :leg :face}                   #{:claw}                      :hostile-after-attacked :random                               4 #{:docile})
  (Monster. :purple-frog    "purple frog"    "purple frogs"    2 0 0.9   1     0.7  3 #{:head :body :leg :face}                   #{:claw}                      :hostile-after-attacked :random                               4 #{:docile})
  (Monster. :parrot         "parrot"         "parrots"         5 0 2.1   1     1    3 #{:head :body :leg :face :wing :tail}       #{:claw :bite}                :hostile-during-day     :random                              10 #{:hostile})
  (Monster. :shark          "shark"          "sharks"         16 0 1.4 800    30   15 #{:head :body :fin :nose :tail}             #{:bite}                      :hostile                :follow-player-in-range-or-random    10 #{:hostile})
  (Monster. :fish           "fish"           "fish"            4 0 1.2   4     1    8 #{:head :body :fin :tail}                   #{:bite}                      :retreat-after-attacked :random                               1 #{:hostile})
  (Monster. :octopus        "octopus"        "octopodes"       4 0 1.5  15     5    4 #{:head :body :tentacle}                    #{:bite :bite-venom :squeeze} :hostile-after-attack   :hide-from-player-in-range-or-random  2 #{:hostile})
  (Monster. :sea-snake      "sea snake"      "sea snakes"      4 0 1.5   2     1    7 #{:head :body}                              #{:bite :bite-venom}          :hostile-at-night       :follow-player-in-range-or-random     2 #{:hostile})
  (Monster. :clam           "clam"           "clams"           2 0 0.1   0.2   0.2 25 #{:shell}                                   #{:clamp}                     :hostile                :constant                             1 #{:hostile})
  (Monster. :urchin         "urchin"         "urchins"         2 0 0.1   0.1  10   10 #{:body}                                    #{:spike}                     :hostile                :constant                             1 #{:hostile})
  (Monster. :squid          "squid"          "squids"          4 0 1.5  10     6    4 #{:head :body :tentacle}                    #{:bite :squeeze}             :hostile-after-attacked :hide-from-player-in-range-or-random  2 #{:hostile})
  (Monster. :crocodile      "crocodile"      "crocodiles"     10 0 0.8 150    15    9 #{:head :body :arm :leg :tail :snout}       #{:bite :claw}                :hostile-after-attacked :follow-player-in-range-or-random     2 #{:hostile})
  (Monster. :mosquito       "mosquito"       "mosquitoes"      1 0 1.4   0.01  0.1  1 #{:head :body :leg :wing}                   #{:bite}                      :hostile-after-attacked :follow-player-in-range-or-random     3 #{:hostile})
  (Monster. :mongoose       "mongoose"       "mongeese"        4 0 1.4   5     4    6 #{:head :body :leg :tail}                   #{:bite :claw}                :hostile-during-day     :follow-player-in-range-or-random     2 #{:hostile})
  (Monster. :tarantula      "tarantula"      "tarantulas"      2 0 1.4   0.1   0.2  2 #{:head :body :leg}                         #{:bite}                      :retreat-after-attacked :random                               2 #{:hostile})
  (Monster. :monitor-lizard "monitor lizard" "monitor lizards" 7 0 1.1  10     5   10 #{:head :body :leg :tail}                   #{:bite :claw}                :hostile-after-sound    :random                               2 #{:hostile})
  (Monster. :komodo-dragon  "komodo dragon"  "komodo dragons" 12 0 0.8  60    10   14 #{:head :body :leg :tail}                   #{:bite :claw}                :hostile-after-attacked :random                               2 #{:hostile})
  (Monster. :cobra          "cobra"          "cobras"          5 0 0.8   6     3    7 #{:head :body :tail}                        #{:bite :bite-venom}          :hostile-after-attacked :random                               2 #{:hostile})
  (Monster. :puffer-fish    "puffer fish"    "puffer fish"     3 0 1.1   1     0.6  7 #{:head :body :tail}                        #{:sting-venom}               :hostile-during-day     :random                               2 #{:hostile})
  (Monster. :crab           "crab"           "crabs"           4 0 0.8   2     1.5  9 #{:head :body}                              #{:claw}                      :hostile-after-sound    :random                               2 #{:hostile})
  (Monster. :hermit-crab    "hermit crab"    "hermit crabs"    3 0 0.6   1     1   15 #{:head :shell :leg}                        #{:claw}                      :hostile-during-day     :random                               1 #{:hostile})
  (Monster. :electric-eel   "electric eel"   "electric eels"   5 0 0.6  10     5    8 #{:head :body}                              #{:bite}                      :hostile                :follow-player-in-range-or-random     2 #{:hostile})
  (Monster. :jellyfish      "jellyfish"      "jellyfish"       3 0 0.6   1     0.1  4 #{:body}                                    #{:sting-venom}               :retreat-after-attacked :random                               1 #{:hostile})])

(def ^:private race->monster-map
  (apply hash-map (mapcat (fn [[k v]] [k (first v)])
                          (group-by :race monsters))))

(make-gen-fns *ns* race->monster-map)

(defn id->monster [id]
  #_(info "id->monster" id)
  #_(info race->monster-map)
  (get race->monster-map id))

(defn gen-monster [level cell-type]
  "Generate one random monster."
  (let [land-monster-ids {
                          0 [:red-frog
                             :orange-frog
                             :yellow-frog
                             :green-frog
                             :blue-frog
                             :purple-frog
                             :bird
                             :gecko]
                          1 [:rat
                             :mosquito]
                          2 [:spider
                             :centipede]
                          3 [:tarantula
                             :scorpion]
                          4 [:cobra
                             :snake]
                          5 [:bat
                             :turtle]
                          6 [:monitor-lizard
                             :crocodile]
                          7 [:parrot
                             :mongoose]
                          8 [:komodo-dragon]
                          9 [:boar
                             :monkey]}
        water-monster-ids {
                          0 [:clam 
                             :hermit-crab]
                          1 [:jellyfish]
                          2 [:fish]
                          3 [:crab]
                          4 [:urchin] 
                          5 [:sea-snake] 
                          6 [:puffer-fish]
                          7 [:electric-eel]
                          8 [:octopus ]
                          9 [:squid
                             :shark]}]

    (cond
      (contains? #{:water :surf} cell-type)
      (id->monster (dg/rand-nth (get water-monster-ids (int (* level 10/10)))))
      :else
      (id->monster (dg/rand-nth (get land-monster-ids (int (* level 10/10))))))))

(defn gen-monsters
  "Generate `n` random monsters using `gen-monster`."
  [n]
  (repeatedly n gen-monster 1 :floor))

(defn id->name
  [id]
  (get (race->monster-map id) :name))

(defn id->name-plural
  [id]
  (get (race->monster-map id) :name-plural))

(defn -main
  "Generate five random monsters and display them."
  [& args]
  (if (contains? (set args) "--list")
    (let [monsters (map #(%) [gen-rat
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
                              gen-red-frog
                              gen-orange-frog
                              gen-yellow-frog
                              gen-green-frog
                              gen-blue-frog
                              gen-purple-frog
                              gen-parrot
                              gen-shark
                              gen-fish
                              gen-octopus 
                              gen-sea-snake 
                              gen-clam 
                              gen-urchin 
                              gen-squid
                              gen-crocodile
                              gen-mosquito
                              gen-mongoose
                              gen-tarantula
                              gen-monitor-lizard
                              gen-komodo-dragon
                              gen-cobra
                              gen-puffer-fish
                              gen-crab
                              gen-hermit-crab
                              gen-electric-eel
                              gen-jellyfish])]
      (doseq [monster monsters]
        (doseq [part (get monster :body-parts)]
          (println "[:human :punch" (get monster :race) part "] (+ (rand) 0.5)"))
        (doseq [attack (get monster :attacks)]
          (doseq [part #{:head :neck :face :abdomen :arm :leg :foot}]
            (println "[" (get monster :race) attack ":human" part "] (+ (rand) 0.5)")))))
          
    (println (gen-monsters 5))))
