;; Functions for generating random items.
(ns robinson.itemgen
  (:use     robinson.common)
  (:require [robinson.monstergen :as mg]
            [clojure.data.generators :as dg]
            [clojure.math.combinatorics :as combo]))


(defn gen-corpse
  "Generate a corpse from an npc."
  [npc]
  {:id          (keyword (str (name (get npc :race)) "-corpse"))
   :type        :food
   :name        (format "%s corpse" (name (get npc :race)))
   :name-plural (format "%s corpses" (name (get npc :race)))
   ;; TODO: base on type of monster by including a weight field in all monsters
   :hunger      (int (dg/uniform 5 10))})

(defn is-corpse-id? [id] (re-matches #"-corpse$" (name id)))

(defn is-quaffable? [item]
  (pos? (get item :thirst 0)))

(defn id-is-sharp? [id]
  (contains? #{:knife
               :obsidian-knife
               :obsidian-axe
               :obsidian-spear
               :sharpened-stick}
             id))

(defn is-sharp? [item]
  (id-is-sharp? (get item :id)))

(def ^:private items
  [
   {:id  :stick                   :name  "stick"                      :name-plural "sticks"}
   {:id  :sharpened-stick         :name  "sharpened stick"            :name-plural "sharpened sticks" :attack :spear}
   {:id  :plant-fiber             :name  "plant fiber"                :name-plural "plant fibers"}
   {:id  :unhusked-coconut        :name  "unhusked coconut"           :name-plural "husked coconuts"}
   {:id  :coconut                 :name  "coconut"                    :name-plural "coconuts" :thirst 30}
   {:id  :wood-log                :name  "wood log"                   :name-plural "wood logs"}
   {:id  :rock                    :name  "rock"                       :name-plural "rocks"}
   {:id  :obsidian                :name  "obsidian stone"             :name-plural "obsidian stones"}
   {:id  :grass                   :name  "grass"                      :name-plural "grass"}
   {:id  :obsidian-blade          :name  "obsidian blade"             :name-plural "obsidian blades"}
   {:id  :rope                    :name  "rope"                       :name-plural "ropes"}
   {:id  :bamboo                  :name  "bamboo"                     :name-plural "bamboo"}
   {:id  :obsidian-spear          :name  "obsidian spear"             :name-plural "obsidian spears"       :attack :spear}
   {:id  :obsidian-axe            :name  "obsidian axe"               :name-plural "obsidian axes"         :attack :axe}
   {:id  :obsidian-knife          :name  "obsidian knife"             :name-plural "obsidian knives"       :attack :knife}
   {:id  :bamboo-water-collector  :name  "bamboo water collector"     :name-plural "bamboo water collectors"}
   {:id  :match                   :name  "match"                      :name-plural "matches"}
   {:id  :knife                   :name  "knife"                      :name-plural "knives"                :attack :knife}
   {:id  :plant-guide             :name  "plant guide"                :name-plural "plant guides"}
   {:id  :bandage                 :name  "bandage"                    :name-plural "bandages"}
   {:id  :fishing-line-and-hook   :name  "fishing line and hook"      :name-plural "fishing lines and hooks"}
   {:id  :ration                  :name  "ration"                     :name-plural "rations"               :type :food :hunger 100}
   {:id  :flashlight              :name  "flashlight"                 :name-plural "flashlights"           :charge 500}
   {:id  :bedroll                 :name  "bedroll"                    :name-plural "bedrolls"}
   {:id  :tarp                    :name  "tarp"                       :name-plural "tarps"}
   {:id  :saw                     :name  "saw"                        :name-plural "saws"}])

(def ^:private id->item-map
  (apply hash-map (mapcat (fn [[k v]] [k (first v)])
                          (group-by :id items))))

(defn make-sym [id]
  (->> id name (str "gen-") symbol))

(defn make-gen-fn [id]
  (let [n (gensym)]
    (list `defn (make-sym id) [] (id->item-map id))))

(defmacro make-gen-fns []
  (let [ids (map :id items)]
    `(do ~@(map make-gen-fn ids))))

(make-gen-fns)

(def id->gen-fn
  (let [fns (map (fn [[k v]]
                   (let [f (var-get v)
                         id (get (f 1) :id)]
                     [id f]))
                  (filter (fn [[k v]] (.startsWith (name k) "gen-"))
                          (ns-publics *ns*)))]
    fns))

(defn id->items
  "Generate item from id."
  [id n]
  (if (is-corpse-id? id)
    (mg/id->monster (keyword (first (clojure.string/split (name id) #"-"))))
    (repeat n (get id->gen-fn id))))

(defn id->item
  [id]
  (get id->item-map id))

(defn id->name
  [id]
  (get (id->item id) :name))

(defn id->name-plural
  [id]
  (get (id->item id) :name-plural))

(defn can-be-wielded?
  [item]
  (contains? #{:sharpened-stick
               :obsidian-spear
               :obsidian-axe
               :obsidian-knife}
             (get item :id)))

(defn is-drinkable?
  [item]
  (get item :drinkable false))

(defn gen-food
  "Generate one random food item."
  []
  (let [food [{:id :ration       :type :food :name "ration"        :name-plural "rations"        :hunger 100}
              {:id :meatball     :type :food :name "meatball"      :name-plural "meatballs"      :hunger 40}
              {:id :berry        :type :food :name "berry"         :name-plural "berries"        :hunger 50}
              {:id :lemon        :type :food :name "lemon"         :name-plural "lemons"         :hunger 30}
              {:id :yogurt-leaf  :type :food :name "yogurt leaf"   :name-plural "yogurt leaves"  :hunger 20}
              {:id :lyrium-leaf  :type :food :name "lyrium leaf"   :name-plural "lyrium leaves"  :hunger 10}
              {:id :mystery-meat :type :food :name "mystery meat"  :name-plural "mystery meats"  :hunger (uniform-int 100)}]]
    (dg/rand-nth food)))

(defn gen-ring []
  "Generate one random ring."
  (let [rings [{:id :ring-of-gold          :type :ring :name "ring of gold"           :name-plural "rings of gold"}
               {:id :ring-of-meatballs     :type :ring :name "ring of meatballs"      :name-plural "rings of meatballs"}
               {:id :ancient-ring          :type :ring :name "ancient ring"           :name-plural "ancient rings"}
               {:id :ring-of-invisibility  :type :ring :name "ring of invisibility"   :name-plural "rings of invisibility"}
               {:id :mystery-ring          :type :ring :name "mystery ring"           :name-plural "mystery rings"}]]
    (dg/rand-nth rings)))

(defn gen-scroll []
  "Generate one random scroll."
  (let [scrolls [{:id :scroll-of-gold         :type :scroll :name "scroll of gold (IOU)"   :name-plural "scrolls of gold (IOUs)"}
                 {:id :scroll-of-meatballs    :type :scroll :name "scroll of meatballs"    :name-plural "scrolls of meatballs"}
                 {:id :ancient-scroll         :type :scroll :name "ancient scroll"         :name-plural "ancient scrolls"}
                 {:id :scroll-of-invisibility :type :scroll :name "scroll of invisibility" :name-plural "scrolls of invisibility"}
                 {:id :robot-plans            :type :scroll :name "robot plans"            :name-plural "robot plans"}
                 {:id :mystery-scroll         :type :scroll :name "mystery scroll"         :name-plural "mystery scrolls"}]]
    (dg/rand-nth scrolls)))

(defn gen-cash
  [max-cash]
  "Generate a random amount of cash between [1 and max-cash]."
  {:type :$ :amount (inc (uniform-int max-cash))})

(defn gen-item []
  "Generate one random food, ring, or scroll item."
  (let [item-fns [gen-food
                  gen-ring
                  gen-scroll]]
    ((dg/rand-nth item-fns))))

(defn gen-items
  "Generate `n` random items using `gen-item`."
  [n]
  (repeatedly n gen-item))


(defn -main
  "Generate five random items and display them."
  [& args]
  (println "generating...")
  (println (gen-items 5)))
