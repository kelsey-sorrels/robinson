;; Functions for generating random items.
(ns robinson.itemgen
  (:require [robinson.random :as rr]
            [robinson.monstergen :as mg]
            [robinson.materials :as rmat]
            [robinson.crafting.mod :as rcmod]
            [taoensso.timbre :as log]
            [datascript.core :as d]))

(defn gen-corpse
  "Generate a corpse from an npc."
  [npc]
  {:item/id     (keyword (str (name (get npc :race)) "-corpse"))
   :type        :food
   :race        (:race npc)
   :weight      (:size npc)
   :name        (format "%s corpse" (name (get npc :race)))
   :name-plural (format "%s corpses" (name (get npc :race)))
   :alive-name (get npc :race)
   ;; food=log((size+1)/10000)+15
   :hunger      (+ (Math/log10 (/ (inc (get npc :size)) 1000)) 15)})

(defn gen-meat
  "Generate meat from a corpse."
  [corpse]
  (let [id (clojure.string/join "-" (butlast (clojure.string/split (name (get corpse :item/id)) #"-")))]
    (-> corpse
      (assoc
       :item/id     (keyword (str id "-meat"))
       :name        (format "%s meat" (name id))
       :name-plural (format "%s meat" (name id))
       :hunger      (* 2 (get corpse :hunger)))
      (dissoc :hotkey))))

(defn gen-hide
  "Generate hide from a corpse."
  [corpse]
  (let [id (clojure.string/join "-" (butlast (clojure.string/split (name (get corpse :item/id)) #"-")))]
    (-> corpse
      (assoc 
       :item/id     (keyword (str id "-hide"))
       :name        (format "%s hide" (name id))
       :name-plural (format "%s hide" (name id))
       ; TODO: base on size? weight? something else?
       :tensile-strength 10
       :properties  #{:flexible :planar})
      (dissoc :hotkey :type :hunger))))

(defn gen-bones
  "Generate bones from a corpse."
  [corpse]
  (let [id (clojure.string/join "-" (butlast (clojure.string/split (name (get corpse :item/id)) #"-")))]
    (-> corpse
      (assoc 
       :item/id     (keyword (str id "-bones"))
       :name        (format "%s bones" (name id))
       :name-plural (format "%s bones" (name id))
       :properties #{:stick-like :tube-like})
      (dissoc :hotkey :type :hunger))))

(defn is-corpse-id?
  [id]
  (log/info "is-corpse-id?" id (name id))
  (re-matches
    #".*-corpse"
    (name id)))

(defn is-corpse-poisonous?
  [state id]
  (contains? (get-in state [:world :frogs :poisonous])
             (-> (clojure.string/split (name id) #"-")
                 first
                 keyword)))

(defn is-quaffable? [item]
  (pos? (get item :thirst 0)))

(defn id-is-sharp? [id]
  (contains? #{:knife
               :saw
               :obsidian-knife
               :obsidian-axe
               :obsidian-spear
               :flint-knife
               :flint-axe
               :flint-spear
               :sharpened-stick
               :cutlass}
             id))

(defn is-sharp? [item]
  (id-is-sharp? (get item :item/id)))

(defn id-is-metal? [id]
  (contains? #{:knife
               :saw
               :cutlass}
             id))

(defn is-metal? [item]
  (id-is-metal? (get item :item/id)))

(defn id-is-fruit?
  [id]
  (contains? #{:red-fruit :orange-fruit :yellow-fruit
               :green-fruit :blue-fruit :purple-fruit
               :white-fruit :black-fruit}
             id))

(defn is-fruit?
  [item]
  (id-is-fruit? (get item :item/id)))

(defn id-is-poisonous?
  [state id]
  (contains? (get-in state [:world :fruit :poisonous]) id))

(defn is-poisonous?
  [state item]
  (id-is-poisonous? (get item :item/id)))

(defn skin-identifiable?
  [state item]
  (contains? (get-in state [:world :fruit :skin-identifiable]) (get item :item/id)))

(defn tongue-identifiable?
  [state item]
  (contains? (get-in state [:world :fruit :tongue-identifiable]) (get item :item/id)))

(defn is-broken?
  [item]
  (not (pos? (get item :utility 1))))

(defn arrow-poison-tipped?
  [state item]
  (and (contains? #{:red-tipped-arrow :orange-tipped-arrow :yellow-tipped-arrrow
                    :green-tipped-arrow :blue-tipped-arrow :purple-tipped-arrow}
                  (get item :item/id))
       (contains? (get-in state [:world :frogs :poisonous])
                  (-> (clojure.string/split (name (get item :item/id)) #"-")
                      first
                      keyword))))
       
(defn id-is-clothes?
  [id]
  (contains? #{:pirate-clothes :navy-uniform :robe}
             id))

(defn is-clothes?
  [item]
  (id-is-clothes? (get item :item/id)))

(defn is-wearable?
  [item]
  (let [properties (get item :properties #{})]
    (and (contains? properties :flexible)
         (contains? properties :planar))))

(defn id-can-stun?
  [id]
  (contains? #{:yellow-fruit :tarp} id))

(def ^:private item-data [
       {:item/id  :stick                   :name  "stick"                      :name-plural "sticks"
        :fuel 100 :utility 100 :weight 0.5 :properties #{:stick-like :handled} :item/materials #{:wood}}
       {:item/id  :branch                  :name  "branch"                     :name-plural "branches"
        :fuel 100 :utility 100 :weight 2 :properties #{:stick-like :handled} :item/materials #{:wood}}
       {:item/id  :log                     :name  "log"                        :name-plural "logs"
        :fuel 500 :weight 20 :properties #{:stick-like} :item/materials #{:wood}}
       {:item/id  :door                     :name  "door"                        :name-plural "door"
        :fuel 500 :weight 20 :item/materials #{:wood}}
       {:item/id  :sharpened-stick         :name  "sharpened stick"            :name-plural "sharpened sticks"
        :attack :spear :fuel 100 :utility 100 :properties #{:stick-like} :item/materials #{:wood}}
       {:item/id  :arrow                   :name  "arrow"                      :name-plural "arrows"
        :attack :spear :fuel 10 :item/materials #{:wood}}
       {:item/id  :red-tipped-arrow        :name  "red-tipped arrow"           :name-plural "red-tipped arrows"
        :attack :spear :fuel 10}
       {:item/id  :orange-topped-arrow     :name  "orange-tipped arrow"        :name-plural "orange-tipped arrows"
        :attack :spear :fuel 10}
       {:item/id  :yellow-tipped-arrow     :name  "yellow-tipped arrow"        :name-plural "yellow-tipped arrows"
        :attack :spear :fuel 10}
       {:item/id  :green-tipped-arrow      :name  "green-tipped arrow"         :name-plural "green-tipped arrows"
        :attack :spear :fuel 10}
       {:item/id  :blue-tipped-arrow       :name  "blue-tipped arrow"          :name-plural "blue-tipped arrows"
        :attack :spear :fuel 10}
       {:item/id  :purple-tipped-arrow     :name  "prurple-tipped arrow"       :name-plural "arrows"
        :attack :spear :fuel 10}
       {:item/id  :bow                     :name  "bow"                        :name-plural "bow"
        :fuel 10 :utility 100 :recoverable-item [:rope :stick] :ammunition-id :arrow}
       {:item/id  :plant-fiber             :name  "plant fiber"                :name-plural "plant fibers"
        :fuel 50 :properties #{:flexible}}
       {:item/id  :leaves                  :name  "leaves"                     :name-plural "leaves"
        :fuel 10}
       {:item/id  :unhusked-coconut        :name  "unhusked coconut"           :name-plural "husked coconuts"
        :ammunition-id :unhusked-coconut}
       {:item/id  :coconut                 :name  "coconut (full)"             :name-plural "coconuts (full)"
        :thirst 30 :ammunition-id :coconut}
       {:item/id  :coconut-empty           :name  "coconut (empty)"            :name-plural "coconuts (empty)"
        :hunger 30 :ammunition-id :coconut-empty}
       {:item/id  :coconut-shell           :name  "coconut shell"              :name-plural "coconut shells"
        :fuel 30}
       {:item/id  :red-fruit               :name  "red fruit"                  :name-plural "red fruits"
        :hunger 30
        ::isa #{:fruit}}
       {:item/id  :orange-fruit            :name  "orange fruit"               :name-plural "orange fruits"
        :hunger 30
        ::isa #{:fruit}}
       {:item/id  :yellow-fruit            :name  "yellow fruit"               :name-plural "yellow fruits"
        :hunger 30
        ::isa #{:fruit}}
       {:item/id  :green-fruit             :name  "green fruit"                :name-plural "green fruits"
        :hunger 30
        ::isa #{:fruit}}
       {:item/id  :blue-fruit              :name  "blue fruit"                 :name-plural "blue fruits"
        :hunger 30
        ::isa #{:fruit}}
       {:item/id  :purple-fruit            :name  "purple fruit"               :name-plural "purple fruits"
        :hunger 30
        ::isa #{:fruit}}
       {:item/id  :white-fruit             :name  "white fruit"                :name-plural "white fruits"
        :hunger 30
        ::isa #{:fruit}}
       {:item/id  :black-fruit             :name  "black fruit"                :name-plural "black fruits"
        :hunger 30
        ::isa #{:fruit}}
       {:item/id  :jack-o-lantern          :name  "jack-o-lantern"             :name-plural "jack-o-lanterns"
        :hunger 10 :ammunition-id :jack-o-lantern
        ::isa #{:fruit}}
       {:item/id  :feather                 :name  "feather"                    :name-plural "feathers"}
       {:item/id  :bamboo                  :name  "bamboo"                     :name-plural "bamboo"
        :fuel 100 :properties #{:stick-like :tube-like}}
       {:item/id  :leather                 :name  "leather"                    :name-plural "leather"
        :properties #{:planar :flexible}}
       {:item/id  :raft                    :name  "raft"                       :name-plural "rafts"}
       ; weapons
       ; blunt
       {:item/id  :club                    :name  "club"                       :name-plural "clubs"
        :attack :blunt
        :properties #{:stick-like :handled}}
       {:item/id  :rock                    :name  "rock"                       :name-plural "rocks"
        :weight 0.5
        :roundness 0.8
        :attack :blunt
        :ranged-attack :airborn-item
        :ammunition-id :rock}
       {:item/id  :throwing-hammer         :name  "throwing hammer"            :name-plural "throwing hammers"
        :attack :club
        :properties #{:handled}
        :ranged-attack :airborn-item :ch-cycle [\╒ \╖ \╛ \╙]}
       {:item/id  :sling                   :name  "sling"                      :name-plural "slings"
        :ranged-attack :airborn-item :ammunition-id :rock}
       ; edged
       {:item/id  :dagger                  :name  "dagger"                     :name-plural "daggers"
        :attack :knife
        :properties #{:handled}
        :roundness 0.01}
       {:item/id  :throwing-axe            :name  "throwing axe"               :name-plural "throwing axes"
        :attack :knife
        :properties #{:handled}
        :ranged-attack :airborn-item :ch-cycle [\╒ \╖ \╛ \╙]}
       {:item/id  :boomerang               :name  "boomerang"                  :name-plural "boomerangs"
        :ranged-attack :boomerang
        :properties #{:stick-like}}
       ; piercing
       {:item/id  :spear                   :name  "spear"                      :name-plural "spears"}
       {:item/id  :throwing-spear          :name  "throwing spear"             :name-plural "throwing spears"
        :ranged-attack :airborn-item}
       {:item/id  :bow                     :name  "bow"                        :name-plural "bow"
        :ranged-attack :airborn-item}
       {:item/id  :blowgun                 :name  "blowgun"                    :name-plural "blowguns"
        :ranged-attack :airborn-item :ammunition-id :blowdart}
       ; flexible
       {:item/id  :garrote                 :name  "garrote"                    :name-plural "garrotes"
        :attack :strangle
        :properties #{:handled}}
       {:item/id  :bolas                   :name  "bolas"                      :name-plural "bolas"
        :ranged-attack :airborn-item :ch-cycle [\╒ \╖ \╛ \╙]
        :effects [(rcmod/tag-defender-on-attack "tangle" "tgl" :entangled 0.9)]}
       {:item/id  :whip                    :name  "whip"                       :name-plural "whips"
        :ranged-attack :whip
        :properties #{:handled}}
       ; weapon components
       {:item/id  :obsidian                :name  "obsidian stone"             :name-plural "obsidian stones"}
       {:item/id  :flint                   :name  "flint stone"                :name-plural "flint stones"
        :ammunition-id :flint}
       {:item/id  :large-flint             :name  "large flint stone"          :name-plural "large flint stones"}
       {:item/id  :grass                   :name  "grass"                      :name-plural "grass"
        :fuel 20}
       {:item/id  :obsidian-blade          :name  "obsidian blade"             :name-plural "obsidian blades"}
       {:item/id  :flint-blade             :name  "flint blade"                :name-plural "flint blades"}
       {:item/id  :flint-axe-blade         :name  "flint axe blade"            :name-plural "flint axe blades"}
       {:item/id  :rope                    :name  "rope"                       :name-plural "ropes"
        :fuel 10 :tensile-strength 20 :properties #{:flexible}}
       {:item/id  :bamboo                  :name  "bamboo"                     :name-plural "bamboo"
        :properties #{:tube-like :stick-like}}
       {:item/id  :obsidian-spear          :name  "obsidian spear"             :name-plural "obsidian spears"
        :attack :spear :fuel 50 :utility 50 :recoverable-items [:stick :obsidian-blade]}
       {:item/id  :obsidian-axe            :name  "obsidian axe"               :name-plural "obsidian axes"
        :attack :axe :fuel 50 :utility 50 :recoverable-items [:stick :obsidian-blade]}
       {:item/id  :obsidian-knife          :name  "obsidian knife"             :name-plural "obsidian knives"
        :attack :knife :fuel 10 :utility 50 :recoverable-items [:stick :obsidian-blade]}
       {:item/id  :flint-spear             :name  "flint spear"                :name-plural "flint spears"
        :attack :spear :fuel 50 :utility 100 :recoverable-items [:stick :flint-blade]}
       {:item/id  :flint-axe               :name  "flint axe"                  :name-plural "flint axes"
        :attack :axe :fuel 50 :utility 100 :recoverable-items [:stick :flint-blade]}
       {:item/id  :flint-knife             :name  "flint knife"                :name-plural "flint knives"
        :attack :knife :fuel 10 :utility 100 :recoverable-items [:stick :flint-blade]}
       {:item/id  :bamboo-water-collector  :name  "bamboo water collector"     :name-plural "bamboo water collectors"}
       {:item/id  :hand-drill              :name  "hand drill"                 :name-plural "hand drills"
        :utility 3}
       {:item/id  :fire-plough             :name  "fire plough"                :name-plural "fire ploughs"
        :utility 3}
       {:item/id  :bow-drill               :name  "bow drill"                  :name-plural "bow drills"
        :utility 5}
       {:item/id  :hand-drill              :name  "hand drill"                 :name-plural "hand-drills"
        :utility 5}
       {:item/id  :fishing-pole            :name  "fishing pole"               :name-plural "fishing poles"
        :fuel 50}
       {:item/id  :match                   :name  "match"                      :name-plural "matches"
        :fuel 10}
       {:item/id  :knife                   :name  "knife"                      :name-plural "knives"
        :attack :knife :roundness 0.01
        :utility 100 :properties #{:edged :handled}}
       {:item/id  :plant-guide             :name  "plant guide"                :name-plural "plant guides"
        :fuel 100}
       {:item/id  :bandage                 :name  "bandage"                    :name-plural "bandages"
        :fuel 30}
       {:item/id  :fishing-line-and-hook   :name  "fishing line and hook"      :name-plural "fishing lines and hooks"}
       {:item/id  :ration                  :name  "ration"                     :name-plural "rations"
        :type :food :hunger 100}
       {:item/id  :lantern                 :name  "lantern"                    :name-plural "lanterns"
        :charge 1500}
       {:item/id  :bedroll                 :name  "bedroll"                    :name-plural "bedrolls"}
       {:item/id  :tarp                    :name  "tarp"                       :name-plural "tarps"}
       {:item/id  :tarp-hung               :name  "tarp (hung)"                :name-plural "tarps (hung)"}
       {:item/id  :saw                     :name  "saw"                        :name-plural "saws"
        :attack :knife :roundness 0.02
        :utility 100 :properties #{:edged :handled}}
       {:item/id  :glass-bottle            :name  "glass bottle"               :name-plural "glass-bottles"}
       {:item/id  :paper                   :name  "paper"                      :name-plural "pages of paper"}
       {:item/id  :pen                     :name  "pen"                        :name-plural "pens"
        :utility 1}
       {:item/id  :radio                   :name  "radio"                      :name-plural "radios"
        :utility 1}
       {:item/id  :flare-gun               :name  "flare gun"                  :name-plural "flare guns"
        :utility 1}
       {:item/id  :signal-mirror           :name  "signal mirror"              :name-plural "signal mirrors"
        :utility 100}
       {:item/id  :flag                    :name  "flag"                       :name-plural "flags"}
       {:item/id  :locator-beacon          :name  "locator beacon"             :name-plural "locator beacons"
        :utility 100}
       ;; pirate ship items
       {:item/id  :spices                  :name "spices"                      :name-plural "spices"}
       {:item/id  :file                    :name "file"                        :name-plural "files"}
       {:item/id  :oil                     :name "oil"                         :name-plural "oil"}
       {:item/id  :sail                    :name "sail"                        :name-plural "sails"
        :properties #{:flexible :planar}}
       {:item/id  :plank                   :name "plank"                       :name-plural "planks"
        :item/materials #{:wood}}
       {:item/id  :dice                    :name "dice"                        :name-plural "dice"}
       {:item/id  :blanket                 :name "blanket"                     :name-plural "blankets"}
       {:item/id  :cup                     :name "cup"                         :name-plural "cup"}
       {:item/id  :silver-bar              :name "silver bar"                  :name-plural "silver bars"}
       {:item/id  :bowl                    :name "bowl"                        :name-plural "bowls"}
       {:item/id  :fork                    :name "fork"                        :name-plural "forks"
        :properties #{:stick-like}}
       {:item/id  :spoon                   :name "spoon"                       :name-plural "spoons"
        :properties #{:stick-like}}
       {:item/id  :rag                     :name "rag"                         :name-plural "rags"
        :properties #{:flexible :planar}}
       {:item/id  :cutlass                 :name "cutlass"                     :name-plural "cutlasses"
        :attack :cutlass
        :properties #{:handled}
        :utility 50}
       {:item/id  :pistol                  :name "flintlock pistol"            :name-plural "flintlock pistols"
        :ammunition-id :paper-cartridge
        :properties #{:handled}}
       {:item/id  :paper-cartridge         :name "paper cartridge"             :name-plural "paper cartridges"
        :fuel 10}
       {:item/id  :ale                     :name "bottle of ale"               :name-plural "bottles of ale"
        :thirst 50}
       {:item/id  :pirate-clothes          :name "pirate clothes"              :name-plural "pirate clothes"
        :utility 100 :toughness 0.2 :tensile-strength 15 :properties #{:flexible :planar}}
       {:item/id  :navy-uniform            :name "navy uniform"                :name-plural "navy uniforms"
        :utility 100 :toughness 0.2 :tensile-strength 15 :properties #{:flexible :planar}}
       {:item/id  :ointment                :name "ointment"                    :name-plural "ointments"}
       ;; ruined temple items
       {:item/id  :jewlery                 :name "jewlery"                     :name-plural "jewelery"}
       {:item/id  :statue                  :name "statue"                      :name-plural "statues"}
       {:item/id  :human-skull             :name "human skull"                 :name-plural "human skulls"}
       {:item/id  :gong                    :name "gong"                        :name-plural "gongs"}
       {:item/id  :stone-tablet            :name "stone tablet"                :name-plural "stone tablets"}
       {:item/id  :codex                   :name "codex"                       :name-plural "codices"}
       {:item/id  :robe                    :name "robe"                        :name-plural "robes"
        :fuel 20 :tensile-strength 12 :properties #{:flexible :planar}}
       {:item/id  :ritual-knife            :name "ritualistic knife"           :name-plural "ritualistic knives"
        :attack :ritualistic-knife
        :properties #{:handled}
        :utility 20}
       {:item/id  :ancient-spear           :name "ancient spear"               :name-plural "ancient spears"
        :attack :ancient-spear :utility 20 :ammunition-id :ancient-spear}
       {:item/id  :blowdart                :name "blowdart"                    :name-plural "blowdarts"
        :fuel 10}
       {:item/id  :herbs                   :name "herbs"                        :name-plural "herbs"}])

(def items item-data)

(def ^:private item-db
  (-> (d/empty-db (merge rmat/material-schema rmat/item-schema))
      (d/db-with rmat/material-heirarchy)
      (d/db-with item-data)))

(def ^:private id->item-map
  (apply hash-map (mapcat (fn [[k v]] [k (first v)])
                          (group-by :item/id item-data))))

(defn id->item
  [id]
  (get id->item-map id))

(defn gen-item [id]
  (id->item id))

(defn id->name
  [id]
  (get (id->item id) :name))

(defn id->name-plural
  [id]
  (get (id->item id) :name-plural))

(defn can-be-wielded?
  [item]
  (let [q '[:find ?a
            :in $ ?item-id
            :where
            [?e :item/id ?item-id]
            [?e :attack ?a]]]
    ;; execute query: item
    (not-empty (d/q q
                    item-db
                    (get item :item/id)))))

(defn can-be-wielded-for-ranged-combat?
  [item]
  (let [q '[:find ?a
            :in $ ?item-id
            :where
            [?e :item/id ?item-id]
            [?e :ranged-attack ?a]]]
    ;; execute query: item
    (not-empty (d/q q
                    item-db
                    (get item :item/id)))))

(defn item->ranged-combat-ammunition-item-id
  [item]
  (let [q '[:find ?a
            :in $ ?item-id
            :where
            [?e :item/id ?item-id]
            [?e :ammunition-id ?a]]]
    ;; execute query: item
    (ffirst (d/q q
                 item-db
                 (get item :item/id)))))

(defn requires-reload?
  [item]
  (some? (item->ranged-combat-ammunition-item-id item)))

(defn is-drinkable?
  [item]
  (get item :drinkable false))

(defn id->fuel
  [id]
  (get (id->item id) :fuel 0))

(defn gen-text-id
  [state]
  (if (< (rr/uniform-double) 0.2)
    (let [fruit-stories (reduce (fn [fruit-stories fruit-id]
                                  (conj fruit-stories
                                        (keyword (str (name fruit-id)
                                                      (if (id-is-poisonous? state fruit-id)
                                                        "-poisonous"
                                                        "-safe")))))
                                []
                                [:red-fruit :orange-fruit :yellow-fruit :green-fruit
                                 :blue-fruit :purple-fruit :white-fruit :black-fruit])]
      (rr/rand-nth fruit-stories))
      (rr/rand-nth [:secret-history :mirror :cistern :cocoon :skulls :angles :demons :sins])))

(defn is-fruit-text-id?
  [id]
  (= (second (clojure.string/split (str id) #"-")) "fruit"))

(defn fruit-text-id->fruit-id [text-id]
  (keyword (clojure.string/replace (name text-id) #"-poisonous|-safe" "")))

(defn flammable?
  [item]
  (contains? item :fuel))

(defn -main
  "Generate five random items and display them."
  [& args]
  nil)
