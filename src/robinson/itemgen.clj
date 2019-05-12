;; Functions for generating random items.
(ns robinson.itemgen
  (:require [robinson.random :as rr]
            [robinson.monstergen :as mg]
            [robinson.materials :as rmat]
            [taoensso.timbre :as log]
            [datascript.core :as d]))

(defn gen-corpse
  "Generate a corpse from an npc."
  [npc]
  {:id          (keyword (str (name (get npc :race)) "-corpse"))
   :type        :food
   :name        (format "%s corpse" (name (get npc :race)))
   :name-plural (format "%s corpses" (name (get npc :race)))
   ;; food=log((size+1)/10000)+15
   :hunger      (+ (Math/log10 (/ (inc (get npc :size)) 1000)) 15)})

(defn is-corpse-id? [id] (re-matches #"-corpse$" (name id)))

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

(defn id-can-stun?
  [id]
  (contains? #{:yellow-fruit :tarp} id))

(def ^:private items [
       {:item/id  :stick                   :name  "stick"                      :name-plural "sticks"
        :fuel 100 :utility 100 :weight 0.5 :properties #{:stick-like} :item/materials #{:wood}}
       {:item/id  :log                     :name  "log"                        :name-plural "logs"
        :fuel 500 :item/materials #{:wood}}
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
        :fuel 10 :utility 100 :recoverable-item [:rope :stick]}
       {:item/id  :plant-fiber             :name  "plant fiber"                :name-plural "plant fibers"
        :fuel 50}
       {:item/id  :leaves                  :name  "leaves"                     :name-plural "leaves"
        :fuel 10}
       {:item/id  :unhusked-coconut        :name  "unhusked coconut"           :name-plural "husked coconuts"}
       {:item/id  :coconut                 :name  "coconut (full)"             :name-plural "coconuts (full)"
        :thirst 30}
       {:item/id  :coconut-empty           :name  "coconut (empty)"            :name-plural "coconuts (empty)"
        :hunger 30}
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
        :hunger 10
        ::isa #{:fruit}}
       {:item/id  :bamboo                  :name  "bamboo"                     :name-plural "bamboo"
        :fuel 100 :properties #{:stick-like}}
       {:item/id  :raft                    :name  "raft"                       :name-plural "rafts"}
       ; weapons
       ; blunt
       {:item/id  :club                    :name  "club"                       :name-plural "clubs"
        :attack :blunt
        :properties #{:stick-like}}
       {:item/id  :rock                    :name  "rock"                       :name-plural "rocks"
        :attack :blunt
        :ranged-attack :thrown}
       {:item/id  :sling                   :name  "sling"                      :name-plural "slings"
        :ranged-attack :thrown}
       ; edged
       {:item/id  :dagger                  :name  "dagger"                     :name-plural "daggers"
        :attack :knife}
       {:item/id  :throwing-axe            :name  "throwing axe"               :name-plural "throwing axes"
        :attack :knife
        :ranged-attack :thrown}
       {:item/id  :boomarang               :name  "boomarang"                  :name-plural "boomarangs"
        :ranged-attack :boomerang
        :properties #{:stick-like}}
       ; piercing
       {:item/id  :spear                   :name  "spear"                      :name-plural "spears"}
       {:item/id  :throwing-spear          :name  "throwing spear"             :name-plural "throwing spears"
        :ranged-attack :thrown}
       {:item/id  :bow                     :name  "bow"                        :name-plural "bow"
        :ranged-attack :thrown}
       {:item/id  :blowgun                 :name  "blowgun"                    :name-plural "blowguns"
        :ranged-attack :thrown}
       ; flexible
       {:item/id  :garrote                 :name  "garrote"                    :name-plural "garrotes"
        :attack :strangle}
       {:item/id  :bolas                   :name  "bolas"                      :name-plural "bolas"
        :ranged-attack :tangle}
       {:item/id  :whip                    :name  "whip"                       :name-plural "whips"
        :ranged-attack :whip}

       ; weapon components
       {:item/id  :obsidian                :name  "obsidian stone"             :name-plural "obsidian stones"}
       {:item/id  :flint                   :name  "flint stone"                :name-plural "flint stones"}
       {:item/id  :large-flint             :name  "large flint stone"          :name-plural "large flint stones"}
       {:item/id  :grass                   :name  "grass"                      :name-plural "grass"
        :fuel 20}
       {:item/id  :obsidian-blade          :name  "obsidian blade"             :name-plural "obsidian blades"}
       {:item/id  :flint-blade             :name  "flint blade"                :name-plural "flint blades"}
       {:item/id  :flint-axe-blade         :name  "flint axe blade"            :name-plural "flint axe blades"}
       {:item/id  :rope                    :name  "rope"                       :name-plural "ropes"
        :fuel 10 :properties #{:flexible}}
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
        :attack :knife :utility 100 :properties #{:edged}}
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
       {:item/id  :saw                     :name  "saw"                        :name-plural "saws"
        :utility 100 :properties #{:edged}}
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
       {:item/id  :sail                    :name "sail"                        :name-plural "sails"}
       {:item/id  :dice                    :name "dice"                        :name-plural "dice"}
       {:item/id  :blanket                 :name "blanket"                     :name-plural "blankets"}
       {:item/id  :cup                     :name "cup"                         :name-plural "cup"}
       {:item/id  :silver-bar              :name "silver bar"                  :name-plural "silver bars"}
       {:item/id  :bowl                    :name "bowl"                        :name-plural "bowls"}
       {:item/id  :fork                    :name "fork"                        :name-plural "forks"
        :properties #{:stick-like}}
       {:item/id  :spoon                   :name "spoon"                       :name-plural "spoons"
        :properties #{:stick-like}}
       {:item/id  :rag                     :name "rag"                         :name-plural "rags"}
       {:item/id  :cutlass                 :name "cutlass"                     :name-plural "cutlasses"
        :attack :cutlass :utility 50}
       {:item/id  :pistol                  :name "flintlock pistol"            :name-plural "flintlock pistols"}
       {:item/id  :paper-cartridge         :name "paper cartridge"             :name-plural "paper cartridges"
        :fuel 10}
       {:item/id  :ale                     :name "bottle of ale"               :name-plural "bottles of ale"
        :thirst 50}
       {:item/id  :pirate-clothes          :name "pirate clothes"              :name-plural "pirate clothes"
        :utility 100 :toughness 0.2}
       {:item/id  :navy-uniform            :name "navy uniform"                :name-plural "navy uniforms"
        :utility 100 :toughness 0.2}
       ;; ruined temple items
       {:item/id  :jewlery                 :name "jewlery"                     :name-plural "jewelery"}
       {:item/id  :statue                  :name "statue"                      :name-plural "statues"}
       {:item/id  :human-skull             :name "human skull"                 :name-plural "human skulls"}
       {:item/id  :gong                    :name "gong"                        :name-plural "gongs"}
       {:item/id  :stone-tablet            :name "stone tablet"                :name-plural "stone tablets"}
       {:item/id  :codex                   :name "codex"                       :name-plural "codices"}
       {:item/id  :robe                    :name "robe"                        :name-plural "robes"
        :fuel 20}
       {:item/id  :ritual-knife            :name "ritualistic knife"           :name-plural "ritualistic knives"
        :attack :ritualistic-knife :utility 20}
       {:item/id  :ancient-spear           :name "ancient spear"               :name-plural "ancient spears"
        :attack :ancient-spear :utility 20}
       {:item/id  :blowgun                 :name "blowgun"                     :name-plural "blowguns"}
       {:item/id  :blowdart                :name "blowdart"                    :name-plural "blowdarts"
        :fuel 10}
       {:item/id  :cure                    :name "cure"                        :name-plural "cures"}])

(def ^:private item-db
  (-> (d/empty-db (merge rmat/material-schema rmat/item-schema))
      (d/db-with rmat/material-heirarchy)
      (d/db-with items)))

(def ^:private id->item-map
  (apply hash-map (mapcat (fn [[k v]] [k (first v)])
                          (group-by :item/id items))))

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
  (case (get item :item/id)
    :bow :arrow
    :flint :flint
    :rock :rock
    :unhusked-coconut :unhusked-coconut
    :coconut :coconut
    :empty-coconut :empty-coconut
    :jack-o-lantern :jack-o-lantern
    :pistol :paper-cartridge
    :ancient-spear :ancient-spear
    :blowgun :blowdart))

(defn requires-reload?
  [item]
  (contains? #{:bow}
             (get item :item/id)))

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

(defn -main
  "Generate five random items and display them."
  [& args]
  nil)
