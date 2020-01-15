;; Functions for generating random monsters
(ns robinson.monstergen
  (:require [robinson.common :as rc]
            [robinson.dynamiccharacterproperties :as dcp]
            [robinson.characterevents :as ce]
            [taoensso.timbre :as log]
            [robinson.random :as rr]
            [clojure.core.match :refer [match]]))


(defn dispatch-on-npc-race [npc _] (get npc :race))
;; multi methods for CharacterEvents
(defmulti do-on-successful-attack dispatch-on-npc-race)
(defmulti do-on-missed-attack dispatch-on-npc-race)
(defmulti do-on-death dispatch-on-npc-race)
(defmulti do-on-hit dispatch-on-npc-race)
(defmulti do-on-tick dispatch-on-npc-race)
;; multi methods for DynamicMonsterProperties
(defmulti do-get-energy dispatch-on-npc-race)
(defmulti do-get-speed dispatch-on-npc-race)
(defmulti do-get-size dispatch-on-npc-race)
(defmulti do-get-strength dispatch-on-npc-race)
(defmulti do-get-dexterity dispatch-on-npc-race)
(defmulti do-get-toughness dispatch-on-npc-race)
(defmulti do-get-attack-speed dispatch-on-npc-race)
(defmulti do-get-attack-strength dispatch-on-npc-race)
(defmulti do-get-attack-dexterity dispatch-on-npc-race)
(defmulti do-get-attack-toughness dispatch-on-npc-race)

;; default methods for CharacterEvents
(defmethod do-on-successful-attack :default [_ state] state)
(defmethod do-on-missed-attack :default [_ state] state)
(defmethod do-on-death :default [_ state] state)
(defmethod do-on-hit :default [_ state] state)
(defmethod do-on-tick :default [_ state] state)

;; default methods for DynamicMonsterProperties
(defmethod do-get-energy :default [npc _] (get npc :energy))
(defmethod do-get-speed :default [npc _] (get npc :speed))
(defmethod do-get-size :default [npc _] (get npc :size))
(defmethod do-get-strength :default [npc _] (get npc :strength))
(defmethod do-get-dexterity :default [npc _] (get npc :dexterity 1))
(defmethod do-get-toughness :default [npc _] (get npc :toughness))
(defmethod do-get-attack-speed :default [npc _] (get npc :strength))
(defmethod do-get-attack-strength :default [npc _] (get npc :strength))
(defmethod do-get-attack-dexterity :default [npc _] (get npc :dexterity 1))
(defmethod do-get-attack-toughness :default [npc _] (get npc :toughness))


;; Monster protocol
(defrecord Monster [race
                    level
                    base-xp
                    name 
                    name-plural 
                    pos
                    hp 
                    energy 
                    speed 
                    size 
                    strength 
                    toughness 
                    body-parts 
                    attacks 
                    temperament 
                    movement-policy 
                    range-threshold 
                    status]
  Object
  (toString [this] (str "#Monster" (into {} this)))
  ;; Dispatch CharacterEvents and DynaimicCharacterProperties to multi-methods
  ce/CharacterEvents
  (on-successful-attack [this state]
    (do-on-successful-attack this state))
  (on-missed-attack [this state]
    (do-on-missed-attack this state))
  (on-hit [this state]
    (do-on-hit this state))
  (on-death [this state]
    (do-on-death this state))
  (on-tick [this state]
    (do-on-tick this state))
  dcp/DynamicCharacterProperties
  (get-energy [this state]
    (do-get-energy this state))
  (get-speed [this state]
    (do-get-speed this state))
  (get-size [this state]
    (do-get-size this state))
  (get-strength [this state]
    (do-get-strength this state))
  (get-dexterity [this state]
    (do-get-dexterity this state))
  (get-toughness [this state]
    (do-get-toughness this state))
  (get-attack-speed [this state attack-type]
    (do-get-attack-speed this state))
  (get-attack-strength [this state attack-type]
    (do-get-attack-strength this state))
  (get-attack-dexterity [this state attack-type]
    (do-get-attack-dexterity this state))
  (get-attack-toughness [this state attack-type]
    (do-get-attack-toughness this state)))

(def ^:private monsters
  [
  ;;        race               base-xp             name-plural      pos
  ;;        |           level  |                   |                |    hp
  ;;        |               |  |  name             |                |    | energy
  ;;        |               |  |  |                |                |    | | speed
  ;;        |               |  |  |                |                |    | | |     size (kg)
  ;;        |               |  |  |                |                |    | | |     |     strength
  ;;        |               |  |  |                |                |    | | |     |     |    toughness
  ;;        |               |  |  |                |                |    | | |     |     |    | body-parts                                  attacks                       temperament             movement policy                        range-threshold
  ;;        |               |  |  |                |                |    | | |     |     |    | |                                           |                             |                       |                                       | status
  (Monster. :rat            1  90 "rat"            "rats"           nil  1 0 0.9   0.2   2    1 #{:face :head :neck :body :leg :tail}       #{:bite :claw}                :hostile                :follow-player-in-range-or-random       8 #{:hostile})
  (Monster. :gecko          1 130 "gecko"          "geckos"         nil  9 0 0.9   0.2   2    1 #{:head :face :body :tail :leg}             #{:bite}                      :hostile-after-sound    :follow-player-in-range-or-random       4 #{:hostile})
  (Monster. :bird           1 130 "bird"           "birds"          nil  7 0 2.1   1     3    2 #{:head :body :tail :leg :beak :wing}       #{:bite :claw}                :retreat-after-sound    :random                                 8 #{:docile})
  (Monster. :mosquito       2 180 "mosquito"       "mosquitoes"     nil  5 0 1.4   0.01  3    2 #{:head :body :leg :wing}                   #{:bite}                      :hostile-after-attacked :follow-player-in-range-or-random       3 #{:hostile})
  (Monster. :spider         2 120 "spider"         "spiders"        nil  6 0 0.9   0.1   3    2 #{:face :leg :abdomen}                      #{:bite-venom}                :retreat-after-attacked :follow-player-in-range-or-random       5 #{:docile})
  (Monster. :centipede      6 160 "centipede"      "centipedes"     nil  4 0 0.5   0.1   3    3 #{:head :body :leg}                         #{:bite}                      :hostile-after-attacked :random                                 3 #{:hostile})
  (Monster. :tarantula      2 220 "tarantula"      "tarantulas"     nil  8 0 1.4   0.1   4    2 #{:head :body :leg}                         #{:bite}                      :retreat-after-attacked :random                                 2 #{:hostile})
  (Monster. :scorpion       2 120 "scorpion"       "scorpions"      nil  5 0 0.3   0.1   3    3 #{:head :claw :leg :abdomen :tail}          #{:bite :claw :sting-venom}   :hostile-after-attacked :follow-player-in-range-or-random       4 #{:hostile})
  (Monster. :red-frog       2  90 "red frog"       "red frogs"      nil  8 0 1.1   2     1    1 #{:head :body :leg :face}                   #{:claw}                      :hostile-after-attacked :follow-player-in-range-or-random       4 #{:hostile})
  (Monster. :orange-frog    2  90 "orange frog"    "orange frogs"   nil  8 0 1.1   2     1    1 #{:head :body :leg :face}                   #{:claw}                      :hostile-after-attacked :follow-player-in-range-or-random       4 #{:hostile})
  (Monster. :yellow-frog    2  90 "yellow frog"    "yellow frogs"   nil  8 0 1.1   2     1    1 #{:head :body :leg :face}                   #{:claw}                      :hostile-after-attacked :follow-player-in-range-or-random       4 #{:hostile})
  (Monster. :green-frog     2  90 "green frog"     "green frogs"    nil  8 0 1.1   2     1    1 #{:head :body :leg :face}                   #{:claw}                      :hostile-after-attacked :follow-player-in-range-or-random       4 #{:hostile})
  (Monster. :blue-frog      2  90 "blue frog"      "blue frogs"     nil  8 0 1.1   2     1    1 #{:head :body :leg :face}                   #{:claw}                      :hostile-after-attacked :follow-player-in-range-or-random       4 #{:hostile})
  (Monster. :purple-frog    2  90 "purple frog"    "purple frogs"   nil  8 0 1.1   2     1    1 #{:head :body :leg :face}                   #{:claw}                      :hostile-after-attacked :follow-player-in-range-or-random       4 #{:hostile})
  (Monster. :cobra          3 200 "cobra"          "cobras"         nil  6 0 0.8   1     4    5 #{:head :body :tail}                        #{:bite :bite-venom}          :hostile-after-attacked :random                                 2 #{:hostile})
  (Monster. :snake          2 120 "snake"          "snakes"         nil  3 0 0.8   1     7    5 #{:head :body :tail}                        #{:bite :bite-venom}          :retreat-after-attacked :follow-player-in-range-or-random       5 #{:hostile})
  (Monster. :bat            2 120 "bat"            "bats"           nil  4 0 1.6   1     2    3 #{:head :body :wing :leg :face}             #{:bite}                      :hostile-after-attacked :follow-player-in-range-or-random       7 #{:hostile})
  (Monster. :boar           3 240 "boar"           "boars"          nil  8 0 1.2  70     6    3 #{:head :body :tail :snout :face :eye :leg} #{:bite :gore}                :hostile                :follow-player-in-range-or-random       7 #{:hostile})
  (Monster. :monkey         4 300 "monkey"         "monkies"        nil 12 0 1.2  50     5    3 #{:head :neck :body :tail :leg :face :arm}  #{:bite :punch}               :hostile-after-attacked :follow-player-in-range-or-random      10 #{:hostile})
  (Monster. :turtle         6 200 "turtle"         "turtles"        nil  4 0 0.5  10     8   20 #{:head :neck :body :leg :face :shell}      #{:bite}                      :retreat-after-attacked :random                                 5 #{:hostile})
  (Monster. :parrot         3 120 "parrot"         "parrots"        nil 15 0 2.1   2     6    9 #{:head :body :leg :face :wing :tail}       #{:claw :bite}                :hostile-during-day     :random                                10 #{:hostile})
  (Monster. :shark          7 400 "shark"          "sharks"         nil 16 0 1.4 800     9   15 #{:head :body :fin :nose :tail}             #{:bite}                      :hostile                :follow-player-in-range-or-random      10 #{:hostile})
  (Monster. :fish           1  90 "fish"           "fish"           nil  9 0 1.0   4     7    8 #{:head :body :fin :tail}                   #{:bite}                      :retreat-after-attacked :random                                 1 #{:hostile})
  (Monster. :octopus        4 120 "octopus"        "octopodes"      nil 14 0 0.8  15     9    9 #{:head :body :tentacle}                    #{:bite :bite-venom :squeeze} :hostile-after-attack   :hide-from-player-in-range-or-random    2 #{:hostile})
  (Monster. :sea-snake      2 120 "sea snake"      "sea snakes"     nil  9 0 1.1   2     4    7 #{:head :body}                              #{:bite :bite-venom}          :hostile-at-night       :follow-player-in-range-or-random       2 #{:hostile})
  (Monster. :clam           1 200 "clam"           "clams"          nil  9 0 0.1   0.2   3   25 #{:shell}                                   #{:clamp}                     :hostile                :constant                               1 #{:hostile})
  (Monster. :urchin         2 220 "urchin"         "urchins"        nil  9 0 0.1   0.1  10   10 #{:body}                                    #{:spike}                     :hostile                :constant                               1 #{:hostile})
  (Monster. :squid          4 330 "squid"          "squids"         nil 14 0 1.5  10     6    4 #{:head :body :tentacle}                    #{:bite :squeeze}             :hostile-after-attacked :hide-from-player-in-range-or-random    2 #{:hostile})
  (Monster. :crocodile      7 880 "crocodile"      "crocodiles"     nil 10 0 0.8 150     5    7 #{:head :body :arm :leg :tail :snout}       #{:bite :claw}                :hostile-after-attacked :follow-player-in-range-or-random       2 #{:hostile})
  (Monster. :mongoose       5 660 "mongoose"       "mongeese"       nil 16 0 1.4   5     6    6 #{:head :body :leg :tail}                   #{:bite :claw}                :hostile-during-day     :follow-player-in-range-or-random       2 #{:hostile})
  (Monster. :monitor-lizard 5 750 "monitor lizard" "monitor lizards" nil 7 0 1.1  10     7   10 #{:head :body :leg :tail}                   #{:bite :claw}                :hostile-after-sound    :random                                 2 #{:hostile})
  (Monster. :komodo-dragon  9 990 "komodo dragon"  "komodo dragons" nil  8 0 0.8  60    10   10 #{:head :body :leg :tail}                   #{:bite :claw}                :hostile-after-attacked :random                                 2 #{:hostile})
  (Monster. :puffer-fish    4 245 "puffer fish"    "puffer fish"    nil  9 0 0.7   1     6    7 #{:head :body :tail}                        #{:sting-venom}               :hostile-during-day     :random                                 2 #{:hostile})
  (Monster. :crab           2 220 "crab"           "crabs"          nil 14 0 0.8   2     5    9 #{:head :body}                              #{:claw}                      :hostile-after-sound    :random                                 2 #{:hostile})
  (Monster. :hermit-crab    1 230 "hermit crab"    "hermit crabs"   nil 13 0 0.6   1     7   15 #{:head :shell :leg}                        #{:claw}                      :hostile-during-day     :random                                 1 #{:hostile})
  (Monster. :electric-eel   6 340 "electric eel"   "electric eels"  nil 15 0 0.6  10     5    8 #{:head :body}                              #{:bite}                      :hostile                :follow-player-in-range-or-random       2 #{:hostile})
  (Monster. :jellyfish      3 190 "jellyfish"      "jellyfish"      nil  7 0 0.6   1     4    4 #{:body}                                    #{:sting-venom}               :retreat-after-attacked :random                                 1 #{:hostile})
  ;; pirate ship bosses
  (Monster. :giant-rat     11 990 "giant rat"      "giant rats"     nil  7 0 1.1  10    10   10 #{:head :body :arm :leg :tail :face}        #{:bite :claw}                :retreat-after-attacked :follow-player-in-range-or-random       10 #{:hostile})
  (Monster. :giant-lizard  11 990 "giant lizard"   "giant lizards"  nil  7 0 1.1  10    10   10 #{:head :body :arm :leg :tail :face}        #{:bite :claw}                :retreat-after-attacked :follow-player-in-range-or-random       10 #{:hostile})
  (Monster. :eel           11 390 "eel"            "eels"           nil  7 0 1.1   5     8    7 #{:head :body :face}                        #{:bite}                      :retreat-after-attacked :follow-player-in-range-or-random       10 #{:hostile})
  ;; ruined temple bosses
  (Monster. :giant-cenitpete
                           11 990 "giant centipede""giant centipedges"
                                                                    nil  7 0 1.1  10    10   10 #{:head :body :leg :face}                   #{:bite}                      :hostile                :follow-player-in-range-or-random       40 #{:hostile})
  (Monster. :giant-lizard  11 990 "gorilla"        "gorillas"       nil  7 0 1.1  10    10   10 #{:head :body :arm :leg :tail :face}        #{:bite :claw}                :hostile                :follow-player-in-range-or-random       40 #{:hostile})
  (Monster. :eel           11 390 "giant snake"    "giant snakes"   nil  7 0 1.1  10    10   10 #{:head :body :face}                        #{:bite}                      :hostile                :follow-player-in-range-or-random       40 #{:hostile})])

(def ^:private race->monster-map
  (apply hash-map (mapcat (fn [[k v]] [k (first v)])
                          (group-by :race monsters))))

(defn derive-multi [h child & parents]
  (reduce (fn [h parent] (derive h child parent)) h parents))

(def monster-hierarchy
  (->
    (make-hierarchy)
    (derive-multi ::mammal ::land-animal ::tetrapod)
    (derive-multi ::bird ::land-animal ::tetrabod)
    (derive-multi ::reptile ::land-animal ::tetrapod)
    (derive-multi ::frog ::land-animal ::tetrapod)
    (derive-multi ::fish ::water-animal)
    (derive-multi ::eel ::water-animal)
    (derive-multi ::mollusk ::water-animal)
    (derive-multi ::crustacean ::water-animal ::amphibian)
    (derive-multi ::snake ::reptile)
    
    (derive-multi :rat ::mammal)
    (derive-multi :gecko ::reptile)
    (derive-multi :bird ::bird ::flying)
    (derive-multi :mosquito ::insect)
    (derive-multi :spider ::arachnid)
    (derive-multi :centipede ::insect ::venomous)
    (derive-multi :tarantula ::arachnid)
    (derive-multi :scorpion ::arachnid ::venomous)
    (derive-multi :red-frog ::frog)
    (derive-multi :orange-frog ::frog)
    (derive-multi :yellow-frog ::frog)
    (derive-multi :green-frog ::frog)
    (derive-multi :blue-frog ::frog)
    (derive-multi :purple-frog ::frog)
    (derive-multi :cobra ::snake ::venemous)
    (derive-multi :snake ::snake)
    (derive-multi :bat ::mammal ::flying)
    (derive-multi :boar ::mammal)
    (derive-multi :monkey ::mammal)
    (derive-multi :turtle ::reptile)
    (derive-multi :parrot ::bird)
    (derive-multi :shark ::fish)
    (derive-multi :fish ::fish)
    (derive-multi :octopus ::mollusk)
    (derive-multi :sea-snake ::snake)
    (derive-multi :clam ::mollusk ::immobile)
    (derive-multi :urchin ::mollusk ::immobile)
    (derive-multi :squid ::mollusk)
    (derive-multi :crocodile ::reptile)
    (derive-multi :mongoose ::mammal)
    (derive-multi :monitor-lizard ::reptile)
    (derive-multi :komodo-dragon ::reptile)
    (derive-multi :puffer-fish ::fish)
    (derive-multi :crab ::crustacean)
    (derive-multi :hermit-crab ::crustacean)
    (derive-multi :electric-eel ::eel)
    (derive-multi :jellyfish ::fish)
    (derive-multi :giant-rat ::mammal)
    (derive-multi :giant-lizard ::reptile)
    (derive-multi :eel ::eel)
    (derive-multi :giant-cenitpete ::insect ::venomous)
    (derive-multi :eel ::eel)))

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
               :cobra :crab :hermit-crab
               ;; pirate ship bosses
               :giant-rat :giant-lizard
               ;; ruined temple bosses
               :giant-centipede :gorilla :giant-snake} race))

(defn can-spawn-intertidally?
  [race]
  (contains? #{:clam :urchin
               :turtle :crocodile
               :crab :hermit-crab} race))

(defn has-hide?
  [race]
  (and
    (isa? monster-hierarchy race ::mammal)
    (< 3 (-> race race->monster-map :size))))

(defn has-skin?
  [race]
  (or
    (isa? race ::reptile)
    (and
      (isa? monster-hierarchy race ::mammal)
      (< (-> race race->monster-map :size) 500))))

(defn has-feathers?
  [race]
  (isa? monster-hierarchy race ::bird))

(defn has-endoskeleton?
  [race]
  (isa? monster-hierarchy race ::tetrapod))

(def  ^:private frog-types
  #{:red-frog
    :orange-frog
    :yellow-frog
    :green-frog
    :blue-frog
    :purple-frog})

(defn is-frog? [monster-type]
  (contains? frog-types monster-type))

(defn is-poisonous-frog? [state monster-type]
  (contains? (get-in state [:world :frogs :poisonous])
             monster-type))

(defn id->monster [id]
  #_(log/info "id->monster" id)
  #_(log/info race->monster-map)
  (get race->monster-map id))

(def land-monster-probabilities
  {0 [1 :bird
      2 :rat
      1 :gecko]
   1 [2 :rat
      1 :gecko
      1 :mosquito]
   2 [3 :spider
      1 :rat
      1 :gecko
      3 :centipede]
   3 [9 :spider
      2 :tarantula
      1 :red-frog
      1 :orange-frog
      1 :yellow-frog
      1 :green-frog
      1 :blue-frog
      1 :purple-frog
      1 :rat
      1 :gecko
      9 :scorpion]
   4 [5 :cobra
      1 :tarantula
      1 :rat
      5 :snake]
   5 [5 :bat
      1 :cobra
      1 :rat
      5 :turtle]
   6 [5 :monitor-lizard
      1 :rat
      5 :crocodile]
   7 [5 :parrot
      1 :rat
      5 :mongoose]
   8 [5 :komodo-dragon
      1 :rat]
   9 [5 :boar
      3 :monkey]})

(def water-monster-probabilities
  {
   0 [1 :clam 
      2 :fish
      1 :hermit-crab
      1 :jellyfish]
   1 [1 :jellyfish
      1 :fish
      1 :crab]
   2 [1 :fish]
   3 [1 :crab
      1 :urchin]
   4 [1 :urchin
      1 :fist
      1 :crab] 
   5 [1 :sea-snake
      1 :urchin
      1 :crab] 
   6 [1 :puffer-fish
      1 :urchin
      1 :sea-snake]
   7 [1 :electric-eel
      1 :puffer-fish
      1 :sea-snake]
   8 [1 :octopus
      1 :puffer-fish
      1 :electric-eel]
   9 [1 :squid
      1 :electric-eel
      2 :shark]})

(def land-monster-ids-by-level
  (reduce-kv (fn [m k v]
               (assoc m k (map second (partition 2 v))))
             {}
             land-monster-probabilities))

(def water-monster-ids-by-level
  (reduce-kv (fn [m k v]
               (assoc m k (map second (partition 2 v))))
             {}
             water-monster-probabilities))

(defn gen-random-monster [level cell-type]
  "Generate one random monster."
  (let [level (min 9 level)]
    (log/info "Generating monster at level" level cell-type)
    (cond
      (contains? #{:water :surf} cell-type)
        (id->monster (rr/rand-weighted-nth (partition 2 (get water-monster-probabilities level))))
      :else
        (id->monster (rr/rand-weighted-nth (partition 2 (get land-monster-probabilities level)))))))

(defn gen-monster [id]
  (id->monster id))

(defn gen-monsters
  "Generate `n` random monsters using `gen-monster`."
  [n]
  (repeatedly n #(gen-random-monster 1 :floor)))

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
    (let [monsters (map #(%) [(partial gen-monster :rat)
                              (partial gen-monster :spider)
                              (partial gen-monster :scorpion)
                              (partial gen-monster :snake)
                              (partial gen-monster :bat)
                              (partial gen-monster :boar)
                              (partial gen-monster :gecko)
                              (partial gen-monster :monkey)
                              (partial gen-monster :bird)
                              (partial gen-monster :centipede)
                              (partial gen-monster :turtle)
                              (partial gen-monster :red-frog)
                              (partial gen-monster :orange-frog)
                              (partial gen-monster :yellow-frog)
                              (partial gen-monster :green-frog)
                              (partial gen-monster :blue-frog)
                              (partial gen-monster :purple-frog)
                              (partial gen-monster :parrot)
                              (partial gen-monster :shark)
                              (partial gen-monster :fish)
                              (partial gen-monster :octopus )
                              (partial gen-monster :sea-snake )
                              (partial gen-monster :clam )
                              (partial gen-monster :urchin )
                              (partial gen-monster :squid)
                              (partial gen-monster :crocodile)
                              (partial gen-monster :mosquito)
                              (partial gen-monster :mongoose)
                              (partial gen-monster :tarantula)
                              (partial gen-monster :monitor-lizard)
                              (partial gen-monster :komodo-dragon)
                              (partial gen-monster :cobra)
                              (partial gen-monster :puffer-fish)
                              (partial gen-monster :crab)
                              (partial gen-monster :hermit-crab)
                              (partial gen-monster :electric-eel)
                              (partial gen-monster :jellyfish)])]
      (doseq [monster monsters]
        (doseq [part (get monster :body-parts)]
          (println "[:human :punch" (get monster :race) part "] (+ (rand) 0.5)"))
        (doseq [attack (get monster :attacks)]
          (doseq [part #{:head :neck :face :abdomen :arm :leg :foot}]
            (println "[" (get monster :race) attack ":human" part "] (+ (rand) 0.5)")))))
          
    (println (gen-monsters 5))))
