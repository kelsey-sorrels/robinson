;; Functions for generating random items.
(ns robinson.itemgen
  (:require [robinson.random :as rr]
            [robinson.monstergen :as mg]
            #?@(:cljs (
               [goog.string :as gstring]
               [goog.string.format]))))


(defn gen-corpse
  "Generate a corpse from an npc."
  [npc]
  {:id          (keyword (str (name (get npc :race)) "-corpse"))
   :type        :food
   :name        #?(:clj  (format "%s corpse" (name (get npc :race)))
                   :cljs (gstring/format "%s corpse" (name (get npc :race))))
   :name-plural #?(:clj  (format "%s corpses" (name (get npc :race)))
                   :cljs (gstring/format "%s corpses" (name (get npc :race))))
   ;; food=log((size+1)/10000)+15
   :hunger      (+ (Math/log10 (/ (inc (get npc :size)) 1000)) 15)})

(defn is-corpse-id? [id] (re-matches #"-corpse$" (name id)))

(defn is-corpse-poisonous?
  [state id]
  (contains? (get-in state [:world :frogs :poisonous])
             (-> #?(:clj  (clojure.string/split (name id) #"-")
                    :cljs (gstring/split (name id) #"-"))
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
  (id-is-sharp? (get item :id)))

(defn id-is-metal? [id]
  (contains? #{:knife
               :saw
               :cutlass}
             id))

(defn is-metal? [item]
  (id-is-metal? (get item :id)))

(defn id-is-fruit?
  [id]
  (contains? #{:red-fruit :orange-fruit :yellow-fruit
               :green-fruit :blue-fruit :purple-fruit
               :white-fruit :black-fruit}
             id))

(defn is-fruit?
  [item]
  (id-is-fruit? (get item :id)))

(defn id-is-poisonous?
  [state id]
  (contains? (get-in state [:world :fruit :poisonous]) id))

(defn is-poisonous?
  [state item]
  (id-is-poisonous? (get item :id)))

(defn skin-identifiable?
  [state item]
  (contains? (get-in state [:world :fruit :skin-identifiable]) (get item :id)))

(defn tongue-identifiable?
  [state item]
  (contains? (get-in state [:world :fruit :tongue-identifiable]) (get item :id)))

(defn is-broken?
  [item]
  (not (pos? (get item :utility 1))))

(defn arrow-poison-tipped?
  [state item]
  (and (contains? #{:red-tipped-arrow :orange-tipped-arrow :yellow-tipped-arrrow
                    :green-tipped-arrow :blue-tipped-arrow :purple-tipped-arrow}
                  (get item :id))
       (contains? (get-in state [:world :frogs :poisonous])
                  (-> #?(:clj  (clojure.string/split (name (get item :id)) #"-")
                         :cljs (gstring/split (name (get item :id)) #"-"))
                      first
                      keyword))))
       
(defn id-is-clothes?
  [id]
  (contains? #{:pirate-clothes :navy-uniform :robe}
             id))

(defn is-clothes?
  [item]
  (id-is-clothes? (get item :id)))

(defn id-can-stun?
  [id]
  (contains? #{:yellow-fruit :tarp} id))

(def ^:private items
  [
   {:id  :stick                   :name  "stick"                      :name-plural "sticks" :fuel 100 :utility 100}
   {:id  :log                     :name  "log"                        :name-plural "logs" :fuel 500}
   {:id  :sharpened-stick         :name  "sharpened stick"            :name-plural "sharpened sticks" :attack :spear :fuel 100 :utility 100}
   {:id  :arrow                   :name  "arrow"                      :name-plural "arrows" :attack :spear :fuel 10}
   {:id  :red-tipped-arrow        :name  "red-tipped arrow"           :name-plural "red-tipped arrows" :attack :spear :fuel 10}
   {:id  :orange-topped-arrow     :name  "orange-tipped arrow"        :name-plural "orange-tipped arrows" :attack :spear :fuel 10}
   {:id  :yellow-tipped-arrow     :name  "yellow-tipped arrow"        :name-plural "yellow-tipped arrows" :attack :spear :fuel 10}
   {:id  :green-tipped-arrow      :name  "green-tipped arrow"         :name-plural "green-tipped arrows" :attack :spear :fuel 10}
   {:id  :blue-tipped-arrow       :name  "blue-tipped arrow"          :name-plural "blue-tipped arrows" :attack :spear :fuel 10}
   {:id  :purple-tipped-arrow     :name  "prurple-tipped arrow"       :name-plural "arrows" :attack :spear :fuel 10}
   {:id  :bow                     :name  "bow"                        :name-plural "bow" :fuel 10 :utility 100 :recoverable-item [:rope :stick]}
   {:id  :plant-fiber             :name  "plant fiber"                :name-plural "plant fibers" :fuel 50}
   {:id  :leaves                  :name  "leaves"                     :name-plural "leaves" :fuel 10}
   {:id  :unhusked-coconut        :name  "unhusked coconut"           :name-plural "husked coconuts"}
   {:id  :coconut                 :name  "coconut (full)"             :name-plural "coconuts (full)" :thirst 30}
   {:id  :coconut-empty           :name  "coconut (empty)"            :name-plural "coconuts (empty)" :hunger 30}
   {:id  :coconut-shell           :name  "coconut shell"              :name-plural "coconut shells" :fuel 30}
   {:id  :red-fruit               :name  "red fruit"                  :name-plural "red fruits" :hunger 30}
   {:id  :orange-fruit            :name  "orange fruit"               :name-plural "orange fruits" :hunger 30}
   {:id  :yellow-fruit            :name  "yellow fruit"               :name-plural "yellow fruits" :hunger 30}
   {:id  :green-fruit             :name  "green fruit"                :name-plural "green fruits" :hunger 30}
   {:id  :blue-fruit              :name  "blue fruit"                 :name-plural "blue fruits" :hunger 30}
   {:id  :purple-fruit            :name  "purple fruit"               :name-plural "purple fruits" :hunger 30}
   {:id  :white-fruit             :name  "white fruit"                :name-plural "white fruits" :hunger 30}
   {:id  :black-fruit             :name  "black fruit"                :name-plural "black fruits" :hunger 30}
   {:id  :jack-o-lantern          :name  "jack-o-lantern"             :name-plural "jack-o-lanterns" :hunger 10}
   {:id  :bamboo                  :name  "bamboo"                     :name-plural "bamboo" :fuel 100}
   {:id  :raft                    :name  "raft"                       :name-plural "rafts"}
   {:id  :rock                    :name  "rock"                       :name-plural "rocks"}
   {:id  :obsidian                :name  "obsidian stone"             :name-plural "obsidian stones"}
   {:id  :flint                   :name  "flint stone"                :name-plural "flint stones"}
   {:id  :large-flint             :name  "large flint stone"          :name-plural "large flint stones"}
   {:id  :grass                   :name  "grass"                      :name-plural "grass" :fuel 20}
   {:id  :obsidian-blade          :name  "obsidian blade"             :name-plural "obsidian blades"}
   {:id  :flint-blade             :name  "flint blade"                :name-plural "flint blades"}
   {:id  :flint-axe-blade         :name  "flint axe blade"            :name-plural "flint axe blades"}
   {:id  :rope                    :name  "rope"                       :name-plural "ropes" :fuel 10}
   {:id  :bamboo                  :name  "bamboo"                     :name-plural "bamboo"}
   {:id  :obsidian-spear          :name  "obsidian spear"             :name-plural "obsidian spears"       :attack :spear :fuel 50 :utility 50 :recoverable-items [:stick :obsidian-blade]}
   {:id  :obsidian-axe            :name  "obsidian axe"               :name-plural "obsidian axes"         :attack :axe :fuel 50 :utility 50 :recoverable-items [:stick :obsidian-blade]}
   {:id  :obsidian-knife          :name  "obsidian knife"             :name-plural "obsidian knives"       :attack :knife :fuel 10 :utility 50 :recoverable-items [:stick :obsidian-blade]}
   {:id  :flint-spear             :name  "flint spear"                :name-plural "flint spears"         :attack :spear :fuel 50 :utility 100 :recoverable-items [:stick :flint-blade]}
   {:id  :flint-axe               :name  "flint axe"                  :name-plural "flint axes"           :attack :axe :fuel 50 :utility 100 :recoverable-items [:stick :flint-blade]}
   {:id  :flint-knife             :name  "flint knife"                :name-plural "flint knives"         :attack :knife :fuel 10 :utility 100 :recoverable-items [:stick :flint-blade]}
   {:id  :bamboo-water-collector  :name  "bamboo water collector"     :name-plural "bamboo water collectors"}
   {:id  :hand-drill              :name  "hand drill"                 :name-plural "hand drills" :utility 3}
   {:id  :fire-plough             :name  "fire plough"                :name-plural "fire ploughs" :utility 3}
   {:id  :bow-drill               :name  "bow drill"                  :name-plural "bow drills" :utility 5}
   {:id  :hand-drill              :name  "hand drill"                 :name-plural "hand-drills" :utility 5}
   {:id  :fishing-pole            :name  "fishing pole"               :name-plural "fishing poles" :fuel 50}
   {:id  :match                   :name  "match"                      :name-plural "matches" :fuel 10}
   {:id  :knife                   :name  "knife"                      :name-plural "knives"                :attack :knife :utility 100}
   {:id  :plant-guide             :name  "plant guide"                :name-plural "plant guides" :fuel 100}
   {:id  :bandage                 :name  "bandage"                    :name-plural "bandages" :fuel 30}
   {:id  :fishing-line-and-hook   :name  "fishing line and hook"      :name-plural "fishing lines and hooks"}
   {:id  :ration                  :name  "ration"                     :name-plural "rations"               :type :food :hunger 100}
   {:id  :flashlight              :name  "flashlight"                 :name-plural "flashlights"           :charge 1500}
   {:id  :bedroll                 :name  "bedroll"                    :name-plural "bedrolls"}
   {:id  :tarp                    :name  "tarp"                       :name-plural "tarps"}
   {:id  :saw                     :name  "saw"                        :name-plural "saws" :utility 100}
   ;; pirate ship items
   {:id  :spices                  :name "spices"                      :name-plural "spices"}
   {:id  :sail                    :name "sail"                        :name-plural "sails"}
   {:id  :dice                    :name "dice"                        :name-plural "dice"}
   {:id  :blanket                 :name "blanket"                     :name-plural "blankets"}
   {:id  :cup                     :name "cup"                         :name-plural "cup"}
   {:id  :silver-bar              :name "silver bar"                  :name-plural "silver bars"}
   {:id  :bowl                    :name "bowl"                        :name-plural "bowls"}
   {:id  :fork                    :name "fork"                        :name-plural "forks"}
   {:id  :spoon                   :name "spoon"                       :name-plural "spoons"}
   {:id  :rag                     :name "rag"                         :name-plural "rags"}
   {:id  :cutlass                 :name "cutlass"                     :name-plural "cutlasses" :attack :cutlass :utility 50}
   {:id  :pistol                  :name "flintlock pistol"            :name-plural "flintlock pistols"}
   {:id  :paper-cartridge         :name "paper cartridge"             :name-plural "paper cartridges" :fuel 10}
   {:id  :ale                     :name "bottle of ale"               :name-plural "bottles of ale" :thirst 50}
   {:id  :pirate-clothes          :name "pirate clothes"              :name-plural "pirate clothes" :utility 100 :toughness 0.2}
   {:id  :navy-uniform            :name "navy uniform"                :name-plural "navy uniforms" :utility 100 :toughness 0.2}
   ;; ruined temple items
   {:id  :jewlery                 :name "jewlery"                     :name-plural "jewelery"}
   {:id  :statue                  :name "statue"                      :name-plural "statues"}
   {:id  :human-skull             :name "human skull"                 :name-plural "human skulls"}
   {:id  :gong                    :name "gong"                        :name-plural "gongs"}
   {:id  :stone-tablet            :name "stone tablet"                :name-plural "stone tablets"}
   {:id  :codex                   :name "codex"                       :name-plural "codices"}
   {:id  :robe                    :name "robe"                        :name-plural "robes" :fuel 20}
   {:id  :ritual-knife            :name "ritualistic knife"           :name-plural "ritualistic knives" :attack :ritualistic-knife :utility 20}
   {:id  :ancient-spear           :name "ancient spear"               :name-plural "ancient spears" :attack :ancient-spear :utility 20}
   {:id  :blowgun                 :name "blowgun"                     :name-plural "blowguns"}
   {:id  :blowdart                :name "blowdart"                    :name-plural "blowdarts" :fuel 10}
   {:id  :cure                    :name "cure"                        :name-plural "cures"}])

(def ^:private id->item-map
  (apply hash-map (mapcat (fn [[k v]] [k (first v)])
                          (group-by :id items))))

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
  (contains? #{:knife
               :bow
               :sharpened-stick
               :obsidian-spear
               :obsidian-axe
               :obsidian-knife
               :flint-spear
               :flint-axe
               :flint-knife
               :cutlass}
             (get item :id)))

(defn can-be-wielded-for-ranged-combat?
  [item]
  (contains? #{:bow
               :flint
               :rock
               :unhusked-coconut
               :coconut
               :coconut-empty
               :jack-o-lantern
               :pistol}
             (get item :id)))

(defn item->ranged-combat-ammunition-item-id
  [item]
  (case (get item :id)
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
             (get item :id)))

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
