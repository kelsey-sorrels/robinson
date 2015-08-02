;; Functions for generating random monsters
(ns robinson.monstergen
  (:require [robinson.common :as rc]
            [robinson.dynamiccharacterproperties :as dcp]
            [robinson.characterevents :as ce]
            [taoensso.timbre :as log]
            [robinson.random :as rr]
            [clojure.core.match :refer [match]]))

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
(defmulti do-get-toughness dispatch-on-npc-race)

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
(defmethod do-get-toughness :default [npc _] (get npc :toughness))


;; Monster protocol
(defrecord Monster [race
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
  (get-toughness [this state]
    (do-get-toughness this state)))

(def ^:private monsters
  [
  ;;        race            name             name-plural      pos
  ;;        |               |                |                |    hp
  ;;        |               |                |                |    | energy
  ;;        |               |                |                |    | | speed
  ;;        |               |                |                |    | | |     size (kg)
  ;;        |               |                |                |    | | |     |     strength
  ;;        |               |                |                |    | | |     |     |    toughness
  ;;        |               |                |                |    | | |     |     |    | body-parts                                  attacks                       temperament             movement policy                        range-threshold
  ;;        |               |                |                |    | | |     |     |    | |                                           |                             |                       |                                       | status
  (Monster. :rat            "rat"            "rats"           nil  7 0 0.9   0.2   5    5 #{:face :head :neck :body :leg :tail}       #{:bite :claw}                :hostile                :follow-player-in-range-or-random       8 #{:hostile})
  (Monster. :spider         "spider"         "spiders"        nil  8 0 0.9   0.01  7    3 #{:face :leg :abdomen}                      #{:bite-venom}                :retreat-after-attacked :random                                 5 #{:docile})
  (Monster. :scorpion       "scorpion"       "scorpions"      nil  6 0 0.3   0.01  7    3 #{:head :claw :leg :abdomen :tail}          #{:bite :claw :sting-venom}   :hostile-after-attacked :random                                 4 #{:hostile})
  (Monster. :snake          "snake"          "snakes"         nil  3 0 0.8   1     7    8 #{:head :body :tail}                        #{:bite :bite-venom}          :retreat-after-attackd  :random                                 5 #{:hostile})
  (Monster. :bat            "bat"            "bats"           nil  9 0 1.6   1     5    4 #{:head :body :wing :leg :face}             #{:bite}                      :hostile-after-attacked :random                                 7 #{:hostile})
  (Monster. :boar           "boar"           "boars"          nil  8 0 1.2  70     6    8 #{:head :body :tail :snout :face :eye :leg} #{:bite :gore}                :hostile                :random                                 7 #{:hostile})
  (Monster. :gecko          "gecko"          "geckos"         nil  5 0 0.9   0.1   5    5 #{:head :face :body :tail :leg}             #{:bite}                      :hostile-after-sound    :random                                 4 #{:hostile})
  (Monster. :monkey         "monkey"         "monkies"        nil 12 0 1.2  50     5    3 #{:head :neck :body :tail :leg :face :arm}  #{:bite :punch}               :hostile-after-attacked :follow-player-in-range-or-random      10 #{:hostile})
  (Monster. :bird           "bird"           "birds"          nil  9 0 2.1   1     6    4 #{:head :body :tail :leg :beak :wing}       #{:bite :claw}                :retreat-after-sound    :random                                 8 #{:docile})
  (Monster. :centipede      "centipede"      "centipedes"     nil 10 0 0.5   0.1   7    3 #{:head :body :leg}                         #{:bite}                      :hostile-after-attacked :random                                 3 #{:hostile})
  (Monster. :turtle         "turtle"         "turtles"        nil  4 0 0.5  10     8   20 #{:head :neck :body :leg :face :shell}      #{:bite}                      :retreat-after-attacked :random                                 5 #{:hostile})
  (Monster. :red-frog       "red frog"       "red frogs"      nil  5 0 1.1   1     5    5 #{:head :body :leg :face}                   #{:claw}                      :hostile-after-attacked :follow-player-in-range-or-random       4 #{:hostile})
  (Monster. :orange-frog    "orange frog"    "orange frogs"   nil  5 0 1.1   1     5    5 #{:head :body :leg :face}                   #{:claw}                      :hostile-after-attacked :follow-player-in-range-or-random       4 #{:hostile})
  (Monster. :yellow-frog    "yellow frog"    "yellow frogs"   nil  5 0 1.1   1     5    5 #{:head :body :leg :face}                   #{:claw}                      :hostile-after-attacked :follow-player-in-range-or-random       4 #{:hostile})
  (Monster. :green-frog     "green frog"     "green frogs"    nil  5 0 1.1   1     5    5 #{:head :body :leg :face}                   #{:claw}                      :hostile-after-attacked :follow-player-in-range-or-random       4 #{:hostile})
  (Monster. :blue-frog      "blue frog"      "blue frogs"     nil  5 0 1.1   1     5    5 #{:head :body :leg :face}                   #{:claw}                      :hostile-after-attacked :follow-player-in-range-or-random       4 #{:hostile})
  (Monster. :purple-frog    "purple frog"    "purple frogs"   nil  5 0 1.1   1     5    5 #{:head :body :leg :face}                   #{:claw}                      :hostile-after-attacked :follow-player-in-range-or-random       4 #{:hostile})
  (Monster. :parrot         "parrot"         "parrots"        nil 15 0 2.1   1     6    9 #{:head :body :leg :face :wing :tail}       #{:claw :bite}                :hostile-during-day     :random                                10 #{:hostile})
  (Monster. :shark          "shark"          "sharks"         nil 16 0 1.4 800     9   15 #{:head :body :fin :nose :tail}             #{:bite}                      :hostile                :follow-player-in-range-or-random      10 #{:hostile})
  (Monster. :fish           "fish"           "fish"           nil  9 0 1.0   4     7    8 #{:head :body :fin :tail}                   #{:bite}                      :retreat-after-attacked :random                                 1 #{:hostile})
  (Monster. :octopus        "octopus"        "octopodes"      nil 14 0 0.8  15     9    9 #{:head :body :tentacle}                    #{:bite :bite-venom :squeeze} :hostile-after-attack   :hide-from-player-in-range-or-random    2 #{:hostile})
  (Monster. :sea-snake      "sea snake"      "sea snakes"     nil  9 0 1.1   2     4    7 #{:head :body}                              #{:bite :bite-venom}          :hostile-at-night       :follow-player-in-range-or-random       2 #{:hostile})
  (Monster. :clam           "clam"           "clams"          nil  9 0 0.1   0.2   3   25 #{:shell}                                   #{:clamp}                     :hostile                :constant                               1 #{:hostile})
  (Monster. :urchin         "urchin"         "urchins"        nil  9 0 0.1   0.1  10   10 #{:body}                                    #{:spike}                     :hostile                :constant                               1 #{:hostile})
  (Monster. :squid          "squid"          "squids"         nil 14 0 1.5  10     6    4 #{:head :body :tentacle}                    #{:bite :squeeze}             :hostile-after-attacked :hide-from-player-in-range-or-random    2 #{:hostile})
  (Monster. :crocodile      "crocodile"      "crocodiles"     nil 10 0 0.8 150     5    7 #{:head :body :arm :leg :tail :snout}       #{:bite :claw}                :hostile-after-attacked :follow-player-in-range-or-random       2 #{:hostile})
  (Monster. :mosquito       "mosquito"       "mosquitoes"     nil  5 0 1.4   0.01  3    1 #{:head :body :leg :wing}                   #{:bite}                      :hostile-after-attacked :follow-player-in-range-or-random       3 #{:hostile})
  (Monster. :mongoose       "mongoose"       "mongeese"       nil 16 0 1.4   5     6    6 #{:head :body :leg :tail}                   #{:bite :claw}                :hostile-during-day     :follow-player-in-range-or-random       2 #{:hostile})
  (Monster. :tarantula      "tarantula"      "tarantulas"     nil 12 0 1.4   0.1   8    2 #{:head :body :leg}                         #{:bite}                      :retreat-after-attacked :random                                 2 #{:hostile})
  (Monster. :monitor-lizard "monitor lizard" "monitor lizards" nil 7 0 1.1  10     7   10 #{:head :body :leg :tail}                   #{:bite :claw}                :hostile-after-sound    :random                                 2 #{:hostile})
  (Monster. :komodo-dragon  "komodo dragon"  "komodo dragons" nil  8 0 0.8  60    10   10 #{:head :body :leg :tail}                   #{:bite :claw}                :hostile-after-attacked :random                                 2 #{:hostile})
  (Monster. :cobra          "cobra"          "cobras"         nil  3 0 0.8   6     7    7 #{:head :body :tail}                        #{:bite :bite-venom}          :hostile-after-attacked :random                                 2 #{:hostile})
  (Monster. :puffer-fish    "puffer fish"    "puffer fish"    nil  9 0 0.7   1     6    7 #{:head :body :tail}                        #{:sting-venom}               :hostile-during-day     :random                                 2 #{:hostile})
  (Monster. :crab           "crab"           "crabs"          nil 14 0 0.8   2     5    9 #{:head :body}                              #{:claw}                      :hostile-after-sound    :random                                 2 #{:hostile})
  (Monster. :hermit-crab    "hermit crab"    "hermit crabs"   nil 13 0 0.6   1     7   15 #{:head :shell :leg}                        #{:claw}                      :hostile-during-day     :random                                 1 #{:hostile})
  (Monster. :electric-eel   "electric eel"   "electric eels"  nil 15 0 0.6  10     5    8 #{:head :body}                              #{:bite}                      :hostile                :follow-player-in-range-or-random       2 #{:hostile})
  (Monster. :jellyfish      "jellyfish"      "jellyfish"      nil  7 0 0.6   1     4    4 #{:body}                                    #{:sting-venom}               :retreat-after-attacked :random                                 1 #{:hostile})])

(def ^:private race->monster-map
  (apply hash-map (mapcat (fn [[k v]] [k (first v)])
                          (group-by :race monsters))))

(defn id->monster [id]
  #_(log/info "id->monster" id)
  #_(log/info race->monster-map)
  (get race->monster-map id))

(defn gen-random-monster [level cell-type]
  "Generate one random monster."
  (log/info "Generating monster at level" level)
  (let [land-monster-ids {
                          0 [:red-frog
                             :orange-frog
                             :yellow-frog
                             :green-frog
                             :blue-frog
                             :purple-frog
                             :bird
                             :rat
                             :rat
                             :rat
                             :gecko]
                          1 [:rat
                             :rat
                             :gecko
                             :mosquito]
                          2 [:spider
                             :rat
                             :gecko
                             :centipede]
                          3 [:spider
                             :tarantula
                             :rat
                             :gecko
                             :scorpion]
                          4 [:cobra
                             :tarantula
                             :rat
                             :snake]
                          5 [:bat
                             :cobra
                             :rat
                             :turtle]
                          6 [:monitor-lizard
                             :rat
                             :crocodile]
                          7 [:parrot
                             :rat
                             :mongoose]
                          8 [:komodo-dragon
                             :rat]
                          9 [:boar
                             :monkey]}
        water-monster-ids {
                          0 [:clam 
                             :hermit-crab
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
      (id->monster (rr/rand-nth (get water-monster-ids (int (* level (/ 9 10))))))
      :else
      (id->monster (rr/rand-nth (get land-monster-ids (int (* level (/ 9 10)))))))))

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

(defn maybe-spawn-additional-npc
  "Spawns a new npc of the same type of the given npc near the given npc."
  [state [x y] npc]
  state)

(defn on-death-fn
  "Returns a function that takes [state [x y] npc] and returns a new state.
   To be invoked when the referred to npc dies."
   [state npc]
   (match (get npc :race)
     :rat  maybe-spawn-additional-npc
     :else (fn [state & more] state)))
     
#?(:clj
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
          
    (println (gen-monsters 5)))))
