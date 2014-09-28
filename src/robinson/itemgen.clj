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

(defn gen-sticks
  "Generate sticks."
  [n]
  {:id :stick :name "stick" :name-plural "sticks" :count n})

(defn gen-plant-fibers
  "Generate plant fibers"
  [n]
  {:id :plant-fiber :name "plant fiber" :name-plural "plant fibers" :count n})

(defn gen-coconuts
  "Generate coconuts"
  [n]
  {:id :coconut :name "coconut" :name-plural "coconuts" :count n})

(defn gen-wood-logs
  "Generate wood logs"
  [n]
  {:id :wood-log :name "wood log" :name-plural "wood logs" :count n})

(defn gen-rocks
  "Generate rocks"
  [n]
  {:id :rock :name "rock" :name-plural "rocks" :count n})

(defn gen-obsidian
  "Generate obsidian"
  [n]
  {:id :obsidian :name "obsidian stone" :name-plural "obsidian stones" :count n})

(defn gen-grass
  "Generate grass"
  [n]
  {:id :grass :name "grass" :name-plural "grass" :count n})

(defn gen-obsidian-blades
  "Generate obsidian-blades"
  [n]
  {:id :obsidian-blade :name "obsidian blade" :name-plural "obsidian blades" :count n})

(defn gen-rope
  "Generate rope"
  [n]
  {:id :rope :name "rope" :name-plural "ropes" :count n})

(defn gen-bamboo
  "Generate bamboo"
  [n]
  {:id :bamboo :name "bamboo" :name-plural "bamboo" :count n})

(defn gen-obsidian-spears
  "Generate obsidian-spears"
  [n]
  {:id :obsidian-spear :name "obsidian spear" :name-plural "obsidian spears" :count n :attack :spear})

(defn gen-obsidian-axes
  "Generate obsidian-axes"
  [n]
  {:id :obsidian-axe :name "obsidian axe" :name-plural "obsidian axes" :count n :attack :axe})

(defn gen-obsidian-knives
  "Generate obsidian-knives"
  [n]
  {:id :obsidian-knife :name "obsidian knife" :name-plural "obsidian knives" :count n :attack :knife})

(defn gen-bamboo-water-collectors
  "Generate bamboo-water-collectors"
  [n]
  {:id :bamboo-water-collector :name "bamboo water collector" :name-plural "bamboo water collectors" :count n})

;; Starting items

(defn gen-matches
  "Generate matches"
  [n]
  {:id :matches :name "match" :plural-name "matches" :count n})

(defn gen-knives
  "Generate knife"
  [n]
  {:id :knife :name "knife" :plural-name "knives" :count n :attack :knife})

(defn gen-plant-guides
  "Generate plant-guides"
  [n]
  {:id :plant-guide :name "plant guide" :plural-name "plant-guides" :count n})

(defn gen-bandages
  "Generate bandages"
  [n]
  {:id :bandage :name "bandage" :plural-name "bandages" :count n})

(defn gen-fishing-line-and-hooks
  "Generate fishing line and hook"
  [n]
  {:id :fishing-line-and-hook :name "fishing line and hook" :plural-name "fishing lines and hooks" :count n})

(defn gen-rations
  "Generate raions"
  [n]
  {:id :ration :type :food :name "ration" :plural-name "rations" :hunger 100 :count n})

(defn gen-flashlights
  "Generate flashlights"
  [n]
  {:id :flashlight :name "flashlight" :plural-name "flashlights" :count n :charge 500})

(defn gen-bedrolls
  "Generate bedrolls"
  [n]
  {:id :bedroll :name "bedroll" :plural-name "bedrolls" :count n})

(defn gen-tarps
  "Generate tarps"
  [n]
  {:id :tarp :name "tarp" :plural-name "tarps" :count n})

(defn gen-saws
  "Generate saws"
  [n]
  {:id :saw :name "saw" :plural-name "saws" :count n})


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
    ((get id->gen-fn id) n)))

(defn id->item
  [id]
  (id->items id 1))

(defn id->name
  [id]
  (get (id->item id) :name))

(defn id->name-plural
  [id]
  (get (id->item id) :name-plural))

(defn can-be-wielded?
  [item]
  (contains? #{:obsidian-spear
               :obsidian-axe
               :obsidian-knife}
             (get item :id)))

(defn gen-food
  "Generate one random food item."
  []
  (let [food [{:id :ration       :type :food :name "ration"        :plural-name "rations"        :hunger 100}
              {:id :meatball     :type :food :name "meatball"      :plural-name "meatballs"      :hunger 40}
              {:id :berry        :type :food :name "berry"         :plural-name "berries"        :hunger 50}
              {:id :lemon        :type :food :name "lemon"         :plural-name "lemons"         :hunger 30}
              {:id :yogurt-leaf  :type :food :name "yogurt leaf"   :plural-name "yogurt leaves"  :hunger 20}
              {:id :lyrium-leaf  :type :food :name "lyrium leaf"   :plural-name "lyrium leaves"  :hunger 10}
              {:id :mystery-meat :type :food :name "mystery meat"  :plural-name "mystery meats"  :hunger (uniform-int 100)}]]
    (dg/rand-nth food)))

(defn gen-ring []
  "Generate one random ring."
  (let [rings [{:id :ring-of-gold          :type :ring :name "ring of gold"           :plural-name "rings of gold"}
               {:id :ring-of-meatballs     :type :ring :name "ring of meatballs"      :plural-name "rings of meatballs"}
               {:id :ancient-ring          :type :ring :name "ancient ring"           :plural-name "ancient rings"}
               {:id :ring-of-invisibility  :type :ring :name "ring of invisibility"   :plural-name "rings of invisibility"}
               {:id :mystery-ring          :type :ring :name "mystery ring"           :plural-name "mystery rings"}]]
    (dg/rand-nth rings)))

(defn gen-scroll []
  "Generate one random scroll."
  (let [scrolls [{:id :scroll-of-gold         :type :scroll :name "scroll of gold (IOU)"   :plural-name "scrolls of gold (IOUs)"}
                 {:id :scroll-of-meatballs    :type :scroll :name "scroll of meatballs"    :plural-name "scrolls of meatballs"}
                 {:id :ancient-scroll         :type :scroll :name "ancient scroll"         :plural-name "ancient scrolls"}
                 {:id :scroll-of-invisibility :type :scroll :name "scroll of invisibility" :plural-name "scrolls of invisibility"}
                 {:id :robot-plans            :type :scroll :name "robot plans"            :plural-name "robot plans"}
                 {:id :mystery-scroll         :type :scroll :name "mystery scroll"         :plural-name "mystery scrolls"}]]
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
