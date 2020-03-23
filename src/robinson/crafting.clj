;; Functions for generating random items.
(ns robinson.crafting
  (:require [robinson.common :as rc]
            [robinson.specs :as rspec]
            [robinson.error :as re]
            [robinson.crafting.mod-protocol :as rcmp]
            [robinson.itemgen :as ig]
            [robinson.world :as rw]
            [robinson.player :as rp]
            [robinson.inventory :as ri]
            [taoensso.timbre :as log]
            [loom.graph :as lg]
            [loom.label :as ll]
            [datascript.core :as d]
            [clojure.math.combinatorics :as combo]))

(defn current-recipe [state]
  (let [selected-recipe-hotkey (get-in state [:world :selected-recipe-hotkey])]
    (get-in state [:world :recipes selected-recipe-hotkey])))

(defn remove-current-recipe
  [state]
  {:pre [(re/validate ::rspec/state state)]
   :post [(re/validate ::rspec/state %)]}
  (let [selected-recipe-hotkey (get-in state [:world :selected-recipe-hotkey])]
    (update-in state [:world :recipes] dissoc selected-recipe-hotkey)))

(defn assoc-current-recipe [state & kvs]
  {:pre [(re/validate ::rspec/state state)]
   :post [(re/validate ::rspec/state %)]}
  (let [selected-recipe-hotkey (get-in state [:world :selected-recipe-hotkey])]
    (update-in state [:world :recipes selected-recipe-hotkey]
      (fn [recipe] (apply assoc recipe kvs)))))

(defn update-current-recipe [state f & xs]
  {:pre [(re/validate ::rspec/state state)]
   :post [(re/validate ::rspec/state %)]}
  (let [selected-recipe-hotkey (get-in state [:world :selected-recipe-hotkey])]
    (update-in state [:world :recipes selected-recipe-hotkey]
      (fn [recipe] (apply f recipe xs)))))

(defn current-stage [state]
  (-> state
    current-recipe
    :current-stage))

(defn update-current-stage
  [state k f & xs]
  {:pre [(re/validate ::rspec/state state)]
   :post [(re/validate ::rspec/state %)]}
  (update-current-recipe
    state
    (fn [recipe]
      (apply update-in recipe [:current-stage k] f xs))))

(defn pre-safe?
  [state]
  (not (some-> state current-stage :screen)))

(defn safe?
  [state]
  (not (some-> state current-stage :screen)))

(defn state-seq [x]
  (tree-seq sequential? seq x))


(defn recipe-event-types [recipe]
  (->> (get recipe :events)
    (map :event/type)
    (remove nil?)
    set))

(def recipe-schema {
  :recipe/id {:db/unique :db.unique/identity}
  #_#_:recipe/components {:db/cardinality :db.cardinality/many}
  :recipe/types {:db/cardinality :db.cardinality/many}})

(defn low-weight? [item]
  (< (get item :weight 0) 1))

(defn medium-weight? [item]
  (< 1 (get item :weight 0) 10))

(defn high-weight? [item]
  (< 10 (get item :weight 0)))

(defn stick-like? [item]
  (contains? (get item :properties) :stick-like))

(defn rock? [item]
  (= (get item :item/id) :rock))

(defn feather? [item]
  (= (get item :item/id) :feather))

(defn flexible? [item]
  (contains? (get item :properties) :flexible))

(defn tensile? [item]
  (< 1 (get item :tensile-strength 0)))

(defn planar? [item]
  (contains? (get item :properties) :planar))

(defn pointed? [item]
  (< (get item :roundness 1) 0.2))

(defn sharp? [item]
  (< 1 (get item :sharpness 0)))

(defn edged? [item]
  (contains? (get item :properties) :edged))

(defn round? [item]
  (< 0.5 (get item :roundness 1)))

(defn wooden? [item]
  (contains? (get item :item/materials) :wood))

(defn tube-like? [item]
  (contains? (get item :properties) :tube-like))

(defn handled? [item]
  (contains? (get item :properties) :handled))

(defn extreme-word
  [item]
  (let [m {low-weight? ["heavy"]
           medium-weight? ["light" "heavy"]
           high-weight? ["light"]
           stick-like? ["thin" "thick" "brittle" "curved"]
           rock? ["rough"]
           feather? ["downy"]
           flexible? ["stiff" "springy" "elastic"]
           tensile? ["brittle" "stiff" "springy" "elastic"]
           planar? ["thick" "thin"]
           pointed? ["dull" "jagged"]
           sharp? ["dull" "jagged" "rough"]
           edged? ["dull" "rough"]
           round? ["rough" "asymmetrical"]
           wooden? ["rotted"]
           tube-like? ["bent" "obstructed"]
           handled? ["loose"]}]
    (or
      (->> m
        (mapcat (fn [[f adjs]] (if (f item) adjs [])))
        vec
        rand-nth)
      (str item))))

(defn property-match?
  [item1 item2]
  (letfn [(property-vec [item]
            (juxt [low-weight?
                   medium-weight?
                   high-weight?
                   stick-like?
                   rock?
                   feather?
                   flexible?
                   tensile?
                   planar?
                   pointed?
                   sharp?
                   edged?
                   round?
                   wooden?
                   tube-like?
                   handled?] item))]
    (some identity
      (map #(and %1 %2)
        (property-vec item1)
        (property-vec item2)))))

(def recipe-pred->str {
  low-weight? "low weight"
  stick-like? "sticklike"
  rock? "rock"
  flexible? "flexible"
  tensile? "tensile"
  planar? "flat"
  pointed? "pointed"
  sharp? "sharp"
  edged? "edged"
  round? "round"
})

(defn add-example-item-properties
  "Takes recipe example items and runs them through item properties associating the result to :recipe/example-item-properties"
  [recipe]
  (let [flag-fns (into {} (map (fn [fn-sym] [(keyword fn-sym) (ns-resolve 'robinson.crafting fn-sym)])
                               ['low-weight?
                                'medium-weight?
                                'high-weight?
                                'stick-like?
                                'rock?
                                'feather?
                                'flexible?
                                'tensile?
                                'planar?
                                'pointed?
                                'sharp?
                                'edged?
                                'round?
                                'wooden?
                                'tube-like?]))]
  (assoc recipe
    :recipe/example-item-properties
    (->> recipe
      :recipe/example-item-requirements
      (map (fn [example-item-id]
             (reduce-kv (fn [s k v]
                          #_(log/info (:recipe/id recipe) example-item-id s k v (ig/id->item example-item-id))
                          (if (v (ig/id->item example-item-id))
                            (conj s (keyword "recipe.item.property" (name k)))
                            s))
                        #{}
                        flag-fns)))
      (apply clojure.set/union)))))

(def recipes
  (mapv add-example-item-properties [
  ;; weapons
     ; blunt
     {:recipe/id :club
      :recipe/category :weapon
      :recipe/types #{:blunt :melee}
      :recipe/example-item-requirements #{:stick :branch}
      :recipe/components #{:recipe.component/handle :recipe.component/head}
      :recipe/add [:club]
      :recipe/requirements '[each-of
                              [and
                                low-weight?
                                wooden?
                                stick-like?]]}
     {:recipe/id :throwing-hammer
      :recipe/category :weapon
      :recipe/types #{:blunt :thrown}
      :recipe/example-item-requirements #{:stick :branch :rock}
      :recipe/components #{:recipe.component/handle :recipe.component/head}
      :recipe/add [:throwing-hammer]
      :recipe/requirements '[each-of
                              stick-like?
                              [and
                                low-weight?
                                rock?]
                              flexible?]}
     {:recipe/id :sling
      :recipe/category :weapon
      :recipe/types #{:blunt :ranged}
      :recipe/example-item-requirements #{:rock :leather}
      :recipe/components #{:recipe.component/cord :recipe.component/pocket}
      :recipe/add [:sling]
      :recipe/requirements '[each-of
                              flexible?
                              [and tensile?
                                   flexible?
                                   planar?]]}
     ; edged
     {:recipe/id  :dagger
      :recipe/category :weapon
      :recipe/types #{:edged :melee}
      :recipe/example-item-requirements #{:rock :stick :branch :plant-fiber :leather}
      :recipe/components #{:recipe.component/handle :recipe.component/blade :recipe.component/binder}
      :recipe/add [:dagger]
      :recipe/requirements '[each-of
                              edged?
                              [and low-weight?
                                   stick-like?]]}
     {:recipe/id  :throwing-axe
      :recipe/category :weapon
      :recipe/types #{:edged :thrown}
      :recipe/example-item-requirements #{:rock :stick :branch :plant-fiber :leather}
      :recipe/components #{:recipe.component/handle :recipe.component/blade :recipe.component/binder}
      :recipe/add [:throwing-axe]
      :recipe/requirements '[each-of
                              edged?
                              [and low-weight?
                                   stick-like?]
                              flexible?]}
      {:recipe/id  :boomerang
      :recipe/category :weapon
      :recipe/types #{:edged :ranged}
      :recipe/example-item-requirements #{:stick :branch}
      :recipe/components #{:recipe.component/blade}
      :recipe/add [:boomerang]
      :recipe/requirements '[each-of
                              [and
                                wooden?
                                [or medium-weight?
                                    high-weight?]]
                              [tool? edged?]]}
     ; piercing
     {:recipe/id  :spear
      :recipe/category :weapon
      :recipe/types #{:piercing :melee}
      :recipe/example-item-requirements #{:rock :stick :branch :plant-fiber :leather}
      :recipe/components #{:recipe.component/shaft :recipe.component/point}
      :recipe/add [:spear]
      :recipe/requirements '[each-of
                              pointed?
                              [and low-weight?
                                   stick-like?]]}
     {:recipe/id  :throwing-spear
      :recipe/category :weapon
      :recipe/types #{:piercing :thrown}
      :recipe/example-item-requirements #{:rock :stick :branch :plant-fiber :leather}
      :recipe/components #{:recipe.component/shaft :recipe.component/point}
      :recipe/add [:throwing-spear]
      :recipe/requirements '[each-of
                              sharp?
                              [and low-weight?
                                   stick-like?]]}
     {:recipe/id  :bow
      :recipe/category :weapon
      :recipe/types #{:piercing :ranged}
      :recipe/example-item-requirements #{:rock :stick :branch :plant-fiber :leather :feather}
      :recipe/components #{:recipe.component/string :recipe.component/length}
      :recipe/add [:bow]
      :recipe/add-recipe #{:arrow}
      :recipe/requirements '[each-of
                              flexible?
                              [and low-weight?
                                   stick-like?]]}
     {:recipe/id  :blowgun
      :recipe/category :weapon
      :recipe/types #{:piercing :ranged}
      :recipe/example-item-requirements #{:bamboo :stick}
      :recipe/components #{:recipe.component/tube}
      :recipe/add [:blowgun]
      :recipe/add-recipe #{:blowdart}
      :recipe/requirements '[each-of
                              [and tube-like?
                                low-weight?]]}
      ; flexible
     {:recipe/id  :garrote
      :recipe/category :weapon
      :recipe/types #{:flexible :melee}
      :recipe/example-item-requirements #{:plant-fiber :stick}
      :recipe/components #{:recipe.component/string :recipe.component/grip}
      :recipe/add [:garrote]
      :recipe/requirements '[each-of
                              flexible?
                              [and low-weight?
                                   stick-like?]]}
     {:recipe/id  :bolas
      :recipe/category :weapon
      :recipe/types #{:flexible :thrown}
      :recipe/example-item-requirements #{:plant-fiber :stick :rope :rock}
      :recipe/components #{:recipe.component/string :recipe.component/ball}
      :recipe/add [:bolas]
      :recipe/requirements '[each-of
                              flexible?
                              [count 3 [and round?
                                            low-weight?]]]}
                             
     {:recipe/id  :whip
      :recipe/category :weapon
      :recipe/types #{:flexible :ranged}
      :recipe/example-item-requirements #{:plant-fiber :rope}
      :recipe/components #{:recipe.component/handle :recipe.component/thong}
      :recipe/add [:whip]
      :recipe/requirements '[each-of
                              flexible?]}
     ; Ammunition
     {:recipe/id  :arrow
      :recipe/category :ammunition
      :recipe/types #{}
      :recipe/add [:arrow]
      :recipe/requirements '[each-of
                              [and low-weight?
                                   pointed?]
                              [and low-weight?
                                   stick-like?]
                              feather?]}
     {:recipe/id  :blowdart
      :name "blowdart"
      :recipe/type :weapon
      :recipe/category :ammunition
      :recipe/types #{}
      :recipe/example-item-requirements #{:bamboo :stick}
      :recipe/add [:blowdart]
      :recipe/requirements '[each-of
                              [and low-weight? stick-like?]]}
    ;;; Survival
    ;; Tools
     {:recipe/id  :saw
      :recipe/category :survival
      :recipe/types #{:tool}
      :recipe/example-item-requirements #{:stick :rope :rock}
      :recipe/add [:saw]
      :recipe/requirements '[each-of
                             flexible?]}
     {:recipe/id  :hammer
      :recipe/category :survival
      :recipe/types #{:tool}
      :recipe/example-item-requirements #{:stick :rope :rock}
      :recipe/add [:hammer]
      :recipe/requirements '[each-of
                             flexible?]}
     {:recipe/id  :ruler
      :recipe/category :survival
      :recipe/types #{:tool}
      :recipe/example-item-requirements #{:stick :rope :rock}
      :recipe/add [:ruler]
      :recipe/requirements '[each-of
                             flexible?]}
     {:recipe/id  :fire-starter
      :recipe/category :survival
      :recipe/types #{:tool}
      :recipe/example-item-requirements #{:stick :rope :rock}
      :recipe/add [:fire-starter]
      :recipe/requirements '[each-of
                              flexible?]}
     ;; Rafts
     {:recipe/id  :reed-raft
      :recipe/category :survival
      :recipe/types #{:raft}
      :recipe/example-item-requirements #{:stick :rope :rock}
      :recipe/add [:reed-raft]
      :recipe/requirements '[each-of
                             flexible?]}
     {:recipe/id  :cannoe
      :recipe/category :survival
      :recipe/types #{:raft}
      :recipe/example-item-requirements #{:stick :rope :rock}
      :recipe/add [:cannoe]
      :recipe/requirements '[each-of
                             flexible?]}
     {:recipe/id  :kayak
      :recipe/category :survival
      :recipe/types #{:raft}
      :recipe/example-item-requirements #{:stick :rope :rock}
      :recipe/add [:kayak]
      :recipe/requirements '[each-of
                              flexible?]}
     {:recipe/id  :log-raft
      :recipe/category :survival
      :recipe/types #{:rafts}
      :recipe/example-item-requirements #{:stick :branch :log :rope}
      ; TODO make log-raft
      :recipe/add [:raft]
      :place :drop
      :recipe/requirements '[each-of
                              [count 2 [and high-weight? wooden?]]
                              [count 1 [and flexible? tensile?]]
                              [count 1 stick-like?]]}]))

(comment :weapons  [
     {:name "bow"                    :hotkey \g :hunger 10 :thirst 20 :recipe {:exhaust [:stick :rope] :add [:bow]}}
     {:name "arrow"                  :hotkey \h :hunger 10 :thirst 20 :recipe {:exhaust [:obsidian-blade :stick] :add [:arrow]}}]
   :survival [
     {:name "flint blade"            :hotkey \a :hunger 10 :thirst 20 :recipe {:exhaust [:rock :flint]                 :add [:flint-blade]}}
     {:name "flint axe blade"        :hotkey \b :hunger 10 :thirst 20 :recipe {:exhaust [:rock :large-flint]           :add [:flint-axe-blade]}}
     {:name "obsidian blade"         :hotkey \c :hunger 10 :thirst 20 :recipe {:exhaust [:rock :obsidian]              :add [:obsidian-blade]}}
     {:name "rope"                   :hotkey \d :hunger 10 :thirst 20 :recipe {:exhaust [:plant-fiber]                 :add [:rope]}}
     {:name "sharpened stick"        :hotkey \e :hunger 10 :thirst 20 :recipe {:exhaust [:stick]
                                                                               :have-or [:obsidian-knife
                                                                                         :obsidian-spear
                                                                                         :obsidian-axe
                                                                                         :knife]
                                                                               :add     [:sharpened-stick]}}
     {:name "bamboo water collector" :hotkey \f :hunger 10 :thirst 20 :recipe {:exhaust [:rope :bamboo :stick]
                                                                               :have-or [:obsidian-knife
                                                                                         :obsidian-spear
                                                                                         :obsidian-axe
                                                                                         :knife]
                                                                               :add [:bamboo-water-collector]} :place :cell-type}
     {:name "solar still"            :hotkey \g :hunger 10 :thirst 20 :recipe {:exhaust [:rock :tarp :stick :coconut-shell]
                                                                               :have-or [:stick]
                                                                               :add [:solar-still]} :place :cell-type}
     {:name "fishing pole"           :hotkey \h :hunger 10 :thirst 20 :recipe {:exhaust [:fishing-line-and-hook :stick]
                                                                               :add [:fishing-pole]}
                                                                      :place :inventory}
     {:name "fire plough"            :hotkey \i :hunger 10 :thirst 20 :recipe {:exhaust [:stick :stick]
                                                                               :add [:fire-plough]}
                                                                      :place :inventory}
     {:name "hand drill"             :hotkey \j :hunger 10 :thirst 20 :recipe {:exhaust [:stick :stick]
                                                                               :add [:hand-drill]}
                                                                      :place :inventory}
     {:name "bow drill"              :hotkey \k :hunger 10 :thirst 20 :recipe {:exhaust [:stick :stick :stick :rope :rock]
                                                                               :add [:bow-drill]}
                                                                      :place :inventory}
     {:name "campfire"               :hotkey \l :hunger 10 :thirst 20 :recipe {:exhaust [:match :stick :log :log :rock :rock :rock]
                                                                               :add [:campfire]}
                                                                      :place :cell-type}]
   :shelter [
     {:name "palisade"               :hotkey \a :hunger 10 :thirst 20 :recipe {:exhaust [:rope :sharpened-stick]       :add [:palisade]} :place :inventory}
     {:name "ramada"                 :hotkey \b :hunger 10 :thirst 20 :recipe {:exhaust [:rope :leaves :stick
                                                                                         :stick :stick :stick :stick]  :add [:ramada]}  :place :cell-type}
     {:name "tarp shelter"           :hotkey \c :hunger 10 :thirst 20 :recipe {:exhaust [:rope :tarp :stick
                                                                                         :stick :stick :stick]         :add [:tarp-shelter]}  :place :cell-type}
     {:name "lean-to"                :hotkey \d :hunger 10 :thirst 20 :recipe {:exhaust [:leaves :stick :stick
                                                                                         :stick :stick :stick]         :add [:lean-to]}  :place :cell-type}]
   :traps [
     {:name "snare"                  :hotkey \a :hunger 10 :thirst 20 :recipe {:exhaust [:rope :stick]                 :add [:snare]}}
     {:name "deadfall trap"          :hotkey \b :hunger 10 :thirst 20 :recipe {:exhaust [:rope :stick :rock]           :add [:deadfall-trap]}}]
   :transportation [
     {:name "raft"                   :hotkey \a :hunger 10 :thirst 20 :recipe {:exhaust [:rope :log :log
                                                                                         :log :log :log]
                                                                                   :add [:raft]} :place :drop}])

(def recipe-db
  (-> (d/empty-db recipe-schema)
      (d/db-with recipes)))

(defn get-recipes-by-category [category]
  (map first
    (d/q '[:find (pull ?e [*])
           :in $ ?category
           :where
           [?e :recipe/category ?category]]
            recipe-db
            category)))

(defn get-recipe [id]
  (ffirst
    (d/q '[:find (pull ?e [*])
           :in $ ?id
           :where
           [?e :recipe/id ?id]]
            recipe-db
            id)))

(defn get-recipes-by-types [types]
  (let [rules '[[(matches-all ?info ?seq ?first ?rest ?empty ?e ?a ?vs)
                 [(?seq ?vs)]
                 [(?first ?vs) ?v]
                 [?e ?a ?v]
                 [?e :recipe/id ?recipe-id]
                 [(?rest ?vs) ?vs-rest]
                 (matches-all ?info ?seq ?first ?rest ?empty ?e ?a ?vs-rest)]
                [(matches-all ?info ?seq ?first ?rest ?empty ?e ?a ?vs)
                 [?e :recipe/id ?recipe-id]
                 [(?empty ?vs)]]]]
    (d/q '[:find (pull ?e [*])
           :in $ % ?info ?seq ?first ?rest ?empty ?types
           :where
           (matches-all ?info ?seq ?first ?rest ?empty ?e :recipe/types ?types)]
          recipe-db
          rules
          (fn [msg x] (log/info msg x) true)
          seq
          first
          rest
          empty?
          types)))

(defn get-recipe-by-types [types]
  (ffirst (get-recipes-by-types types)))

(defn get-example-items-by-types [types]
  (let [rules '[[(matches-all ?info ?seq ?first ?rest ?empty ?e ?a ?vs)
                 [(?seq ?vs)]
                 [(?first ?vs) ?v]
                 [?e ?a ?v]
                 [?e :recipe/id ?recipe-id]
                 [(?rest ?vs) ?vs-rest]
                 (matches-all ?info ?seq ?first ?rest ?empty ?e ?a ?vs-rest)]
                [(matches-all ?info ?seq ?first ?rest ?empty ?e ?a ?vs)
                 [?e :recipe/id ?recipe-id]
                 [(?empty ?vs)]]]]
  (->>
    (d/q '[:find ?v
           :in $ % ?info ?seq ?first ?rest ?empty ?types
           :where
           (matches-all ?info ?seq ?first ?rest ?empty ?e :recipe/types ?types)
           [?e :recipe/example-item-requirements ?v]]
          recipe-db
          rules
          (fn [msg x] (log/trace msg x) true)
          seq
          first
          rest
          empty?
          types)
    (mapcat identity)
    (mapcat identity)
    set)))

(defn get-example-item-properties-by-types [types]
  (let [rules '[[(matches-all ?info ?seq ?first ?rest ?empty ?e ?a ?vs)
                 [(?seq ?vs)]
                 [(?first ?vs) ?v]
                 [?e ?a ?v]
                 [?e :recipe/id ?recipe-id]
                 [(?rest ?vs) ?vs-rest]
                 (matches-all ?info ?seq ?first ?rest ?empty ?e ?a ?vs-rest)]
                [(matches-all ?info ?seq ?first ?rest ?empty ?e ?a ?vs)
                 [?e :recipe/id ?recipe-id]
                 [(?empty ?vs)]]]]
  (->>
    (d/q '[:find ?v
           :in $ % ?info ?seq ?first ?rest ?empty ?types
           :where
           [(?info ?types)]
           (matches-all ?info ?seq ?first ?rest ?empty ?e :recipe/types ?types)
           [?e :recipe/example-item-properties ?v]]
          recipe-db
          rules
          (fn info [x] (log/trace x) true)
          seq
          first
          rest
          empty?
          types)
    (mapcat identity)
    (mapcat identity)
    set)))

(defn item-satisfies-requirement-clause?
  [item clause]
  (log/debug "item-satisfies-requirement-clause?" item clause)
  (cond
    (fn? clause)
      (clause item)
    (symbol? clause)
      (let [f (ns-resolve 'robinson.crafting clause)]
        (log/info item clause)
        (f item))
    (= (first clause) 'and)
      (every? (partial item-satisfies-requirement-clause? item) (rest clause))
    (= (first clause) 'or)
      (some (partial item-satisfies-requirement-clause? item) (rest clause))
    (= (first clause) 'count)
      (let [[n sub-clause] (rest clause)]
        (and
          (<= n (get item :count 1))
          (item-satisfies-requirement-clause? item sub-clause)))
    (contains? #{'tool} (first clause))
      (item-satisfies-requirement-clause? item (second clause))
    (= (first clause) not)
      (not ((second clause) item))
    (keyword? (first clause))
      (= (second clause) (get item (first clause)))))

(defn item-satisfies-any-clause?
  [item recipe]
  (log/debug "item-satisfies-any-clause?" item)
  (log/debug "item-satisfies-any-clause?" (get recipe :recipe/requirements))
  (some
    (partial item-satisfies-requirement-clause? item)
    (-> recipe :recipe/requirements rest)))

(defn item-satisfies-any-recipe-clause?
  [item]
  (some
    (partial item-satisfies-any-clause? item)
    recipes))

(defn inventory-crafting-components
  [state]
  (filter item-satisfies-any-recipe-clause? (ri/player-inventory state)))

(defn slot->item [state slot]
  (let [selected-recipe-hotkey (get-in state [:world :selected-recipe-hotkey])
        hotkey (get-in state [:world :recipes selected-recipe-hotkey :slots slot])]
    ;(log/info selected-recipe-hotkey hotkey slot)
    ;(log/info (type hotkey))
    (ri/inventory-hotkey->item state hotkey)))

(defn requirements-satisfied?
  [recipe]
  (let [rest-requirements (-> recipe :recipe/requirements rest)]
    (when (= (count (get recipe :slots))
             (count rest-requirements))
      (let [satisfied (every? identity
                              (map-indexed (fn [idx req]
                                             (let [slot-item (get-in recipe [:slots idx])]
                                               (item-satisfies-requirement-clause? slot-item req)))
                                rest-requirements))]
        satisfied))))

(defn valid-recipes [items recipes]
  (if-let [item-permutations (->> items
                               vec
                               combo/subsets
                               (remove empty?)
                               (mapcat combo/permutations))]
    (do
    (log/debug "testing" (count item-permutations) " item combos " (count recipes) " recipes")
    (or
      (set
        (for [recipe recipes
              item-permutation item-permutations
              :let [recipe-with-filled-slots (reduce (fn [recipe [idx item]]
                                                       (assoc-in recipe [:slots idx] item))
                                                     recipe
                                                     (map-indexed vector item-permutation))]
              :when (requirements-satisfied? recipe-with-filled-slots)]
          recipe))
      #{}))
    #{}))

(defn valid-recipe? [items recipe]
  (not-empty (valid-recipes items [recipe])))

(defn- exhaust-by-hotkeys
  [state hotkeys]
  (reduce (fn [state hotkey]
            (do 
              (log/trace "removing" hotkey)
              (ri/dec-item-count state hotkey)))
          state
          hotkeys))

(defn- place-cell-type
  [state id effects]
    (let [[x y] (rp/player-xy state)]
      (rw/assoc-cell state x y :type id)))
  
(defn- place-drop
  [state id effects name]
  (let [[x y] (rp/player-xy state)]
    (rw/conj-cell-items state x y (assoc (ig/id->item id) :name name))))

(defn- place-inventory
  [state id effects name]
  {:pre [(re/validate ::rspec/state state)
         (keyword? id)
         (string? name)]
   :post [(re/validate ::rspec/state %)]}
  (log/debug "placing item with id" id "in inventory")
  (let [item (ig/id->item id)
        _ (log/trace item)
        updated-item (reduce (fn [item effect]
                               (if (satisfies? rcmp/ModItemOnCreate effect)
                                 (rcmp/item-on-create effect item)
                                 item))
                             (assoc item :name name)
                             (get item :effects))]
    (log/debug "add-to-inventory" updated-item)
    (ri/add-to-inventory state [updated-item])))

(defn- add-by-ids
  [state ids effects place name]
  (reduce (fn [state id]
            (case place
              :cell-type
                (place-cell-type state id effects name)
              :drop
                (place-drop state id effects name)
              :inventory
                (place-inventory state id effects name)))
          state
          ids))
  
; Recipe node naviation
(defn next-nodes
  [recipe]
  (let [graph (get recipe :graph)
        current-node (get recipe :current-node)]
    (lg/successors graph current-node)))

(defn next-node-choices [recipe]
  (let [graph (get recipe :graph)
        current-node (get recipe :current-node)
        current-node-x (-> graph
                         (ll/label current-node)
                         :x)
        next-nodes (next-nodes recipe)]
    (if (seq next-nodes)
      (map (fn [n]
             (let [x (-> graph (ll/label n) :x)]
               (cond
                 (< x current-node-x)
                   {:name "left"
                    :hotkey \l
                    :next-node n}
                 (= x current-node-x)
                   {:name "down"
                    :hotkey \d
                    :next-node n}
                 (> x current-node-x)
                   {:name "right"
                    :hotkey \r
                    :next-node n})))
             next-nodes)
      [{:name "finish recipe"
        :hotkey :space
        :done true}])))

(defn complete? [recipe]
  (get recipe :done false))

(defn in-progress? [recipe]
  (and (some? recipe)
       (not (complete? recipe))))

(defn recipe-requirements
  [recipe]
  (-> recipe :recipe/requirements))

(defn dominate-item
  [items]
  {:pre [(not-empty items)]}
  (->> items (sort-by :mass) last))

(defn fancy-name
  [recipe]
  (let [qualitative-adjs {-5 ["awful"]
                          -4 ["defective" "makeshift" "shoddy" "slapdash"]
                          -3 ["inferior" "cumbersome" "ramshackle"]
                          -2 ["flimsy" "lousy" "shoddy"]
                          -1 ["awkward" "junky" "weak"]
                          1 ["average" "decent" "okay"]
                          2 ["balanced" "fine" "well made"]
                          3 ["excellent" "polished" "superior"]
                          4 ["overpowered" "flawless" "masterwork"]
                          5 ["godly"]}
        dominate-effect (->> recipe :effects (filter rcmp/quantifiable?) (sort-by rcmp/amount) last)
        adj (when dominate-effect
              (let [adjs (get qualitative-adjs (rcmp/amount dominate-effect) ["normal"])]
                (nth adjs (mod (hash recipe) (count adjs)))))
        dom-item (get recipe :recipe/dominate-item)
        name (-> recipe
               :recipe/types
               get-recipe-by-types
               :recipe/id
               ig/id->name)]

    ;(log/info name)
    (str (if adj
           (str adj " ")
           "")
          (if dominate-item
            (str (get dom-item :name) " ")
            "")
          name)))
  
(defn recipe-name
  ([recipe]
   (recipe-name recipe true))
  ([recipe show-progress]
    {:post [string?]}
    (let [types (get recipe :recipe/types)]
      (log/debug "recipe-name" (count types) (get recipe :recipe/id) (get recipe :recipe/type) types (and show-progress (in-progress? recipe)))
      (str
        (if (and show-progress (in-progress? recipe))
          "In progress " 
          "")
        (case (count types)
          0 (-> recipe :recipe/type name)
          1 (-> recipe :recipe/type name)
          2 (fancy-name recipe))))))

(defn merge-effects
  [effects]
  (->> effects
   (group-by rcmp/id)
   (map (fn [[_ effects]]
     (reduce rcmp/merge effects)))))

(defn recipe-short-desc
  [recipe]
  (let [merged-effects (->>
                         (get recipe :effects)
                         (group-by rcmp/id)
                         (map (fn [[_ effects]]
                           (rcmp/short-name (reduce rcmp/merge effects)))))]
    (clojure.string/join " " merged-effects)))

(defn craft-recipe
  "Perform the recipe."
  ([state recipe]
    (craft-recipe state recipe true))
  ([state recipe exhaust]
    {:pre [(some? recipe)]}
    (log/debug "crafting recipe" (get recipe :recipe/id))
    (log/trace recipe)
    (let [exhaust-hotkeys (->> (get recipe :recipe/requirements)
                            ; remove 'each-of
                            rest
                            ; add slot indexes
                            (map-indexed vector)
                            ; remove tool clauses
                            (remove (fn [[_ clause]] (contains? #{'tool} clause)))
                            ; find which items ids are in remaining slots
                            (map (fn [[slot _]] (get (slot->item state slot) :hotkey))))
          add          (get recipe :recipe/add)
          effects      (get recipe :effects [])
          _ (log/trace "add" add)
          state (as-> state state
                    (add-by-ids state add effects (get recipe :place :inventory) (get recipe :recipe/name))
                    ; exhaust non-tool slot items
                    (if exhaust
                      (exhaust-by-hotkeys state exhaust-hotkeys)
                      state)
                    #_(rp/player-update-hunger state (fn [current-hunger] (min (+ hunger current-hunger)
                                                                             (rp/player-max-hunger state))))
                    #_(rp/player-update-thirst state (fn [current-thirst] (min (+ hunger current-thirst)
                                                                             (rp/player-max-thirst state))))
                    (reduce rp/update-crafted state (map (fn [id] {:id id}) add)))]
      state)))

(defn requirements-based-on-dominate-item
  "Returns a requirements where one of the clauses has been replaced with a specific item requirement based
   on the dominate item in the recipe."
  [item requirements]
  (log/debug "requirements-based-on-dominate-item" item requirements)
  (log/debug (->> requirements
              ; add index
              (map-indexed vector)
              vec))
             
  (log/debug (->> requirements
              ; add index
              (map-indexed vector)
              rest
              ; filter where item satisfies the clause (keeping [index clause] strucuture
              (filter (comp (partial item-satisfies-requirement-clause? item) second))
              vec))
             
  (let [index-clauses (->> requirements
                        ; add index
                        (map-indexed vector)
                        ; remove the 'each symbol at the head
                        rest
                        ; filter where item satisfies the clause (keeping [index clause] strucuture
                        (filter (comp (partial item-satisfies-requirement-clause? item) second)))]
    (if (seq index-clauses)
      (let [idx (-> index-clauses
                  ; random [index clause]
                  rand-nth
                  ; extract index of clause to replace
                  first)]
        (log/trace "idx" idx)
        ; replace clause with
        (assoc requirements idx [:item/id (get item :item/id)]))
      ; if not clauses - don't update requirements
      requirements)))
                 
(defn save-recipe
  [state]
  {:post [(not (nil? %))]}
  (log/debug "Saving recipe")
  (log/debug (type (get-in state [:world :recipes])))
  #_(log/info (vec (get-in state [:world :recipes])))
  (let [recipe (current-recipe state)
        _ (assert (some? recipe))
        selected-recipe-hotkey (get-in state [:world :selected-recipe-hotkey])
        _ (log/trace "dominate-item" (get recipe :recipe/dominate-item))
        #_ (log/info recipe)
        recipe-blueprint (-> recipe
                           :recipe/types
                           get-recipe-by-types)
        merged-recipe (-> recipe
                        (merge recipe-blueprint)
                        ; change name from set to string
                        (update :recipe/name first)
                        ; replace random requirement clause with dominate item
                        (update :recipe/requirements
                                (partial requirements-based-on-dominate-item
                                         (get recipe :recipe/dominate-item))))]
    (log/debug (dissoc merged-recipe :recipe/events))
    (-> state
      (update-in [:world :recipes] assoc
        selected-recipe-hotkey merged-recipe)
      (craft-recipe merged-recipe false)
      (rw/assoc-current-state :normal))))

(defn fill-event
  [event recipe]
  {:pre [(re/validate ::rspec/recipe recipe)]
   :post [(re/validate ::rspec/event %)]}
  (letfn [(invoke-fn [f] (log/trace "invoke-fn" f recipe) (f recipe))]
    (log/info "fill-event" event)
    (log/trace "seq?" (sequential? (get event :description)))
    (-> event
      (cond-> (sequential? (get event :description))
        (update :description rand-nth))
      (update :event/choices (fn [choices]
        (log/debug "Updating choices" (count choices) choices)
        (reduce (fn [choices-acc [choice hotkey]]
                  (conj choices-acc
                    (cond-> choice
                      ; fill missing hotkeys
                      (not (contains? choice :hotkey))
                        (assoc :hotkey hotkey)
                      ;; fill missing names
                      (and (not (contains? choice :name))
                           (= 1 (count choices)))
                           ; if single choice, fill "continue"
                        (assoc :name "continue"))))
                 []
                 (map vector
                   choices
                   ; fill a-z
                   (->> (range (int \a) (int \z))
                     (map char)
                     (filter (fn [hotkey] (not (contains? (set (map :hotkey choices)) hotkey))))))))))))
 
(defn meta-or-into
  [recipe-ns val-in-result val-in-latter]
  (if-let [merge-sym (-> val-in-latter meta :merge)]
    (let [f (ns-resolve recipe-ns merge-sym)]
      (log/trace f)
      (f val-in-result val-in-latter))
    (into val-in-result val-in-latter)))

(defn trigger-immediate-effects
  [state effects]
  {:post [(not (nil? %))]}
  (reduce (fn [state effect]
            (cond
              (satisfies? rcmp/ModStateImmediate effect)
                (rcmp/state-immediate effect state)
              (satisfies? rcmp/ModPlayerImmediate effect)
                (rp/update-player state (partial rcmp/player-immediate effect))
              (satisfies? rcmp/ModPlayerDecInventoryImmediate effect)
                (rcmp/player-dec-inventory-immediate effect state)
              (satisfies? rcmp/ModRecipeRemoveEffectImmediate effect)
                (rcmp/recipe-remove-effect-immediate effect state)
              :else
                state))
          state
          effects))
  
(defn choice-requirements-satisfied?
  [state choice]
  (if-let [{:keys [material adjacent-to-fire]} (get choice :recipe/requirements)]
    (cond
      material
        (let [{:keys [id amount]} material]
          (<= amount (ri/inventory-id->count state id)))
      adjacent-to-fire
        (some (fn [cell] (rw/type->on-fire? (get cell :type)))
              (rw/player-adjacent-cells state)))
    true)
  (if-let [pred (get choice :pred)]
    (let [f (ns-resolve *ns* pred)]
      (f state))
    true))
 
(defn eval-in-ns
  [form ns]
  (let [cur (ns-name *ns*)]
    ;; Set *ns* in binding so that in-ns can set the var.
    (binding [*ns* *ns*]
      (try
        (in-ns ns)
        (log/info "evaluating" form "in" ns)
        (eval form)
        (finally
          (in-ns cur))))))

; Input handlers
(defn resolve-choice
  [state recipe-ns recipe keyin]
  {:pre [(re/validate ::rspec/state state)
         (re/validate ::rspec/recipe recipe)]
   :post [(re/validate ::rspec/state %)]}
  (let [current-stage (get recipe :current-stage)]
    ; find selected choice

    (log/trace "choices" (vec (->> (get current-stage :event/choices))))
    (log/trace "choice" (->> (get current-stage :event/choices)
                      (filter #(= (get % :hotkey) keyin))
                      first))
    (if-let [choice (->> (get current-stage :event/choices)
                      (filter #(= (get % :hotkey) keyin))
                      first)]
      ;; check choice requirement is met if any exists
      (if (choice-requirements-satisfied? state choice)
        (let [results (into {} (map (fn [[k v]]
                                      ; change non-collection values to sets so they are mergeable
                                      (if (coll? v)
                                        [k v]
                                        [k #{v}]))
                                    (merge (select-keys choice [:recipe/id
                                                                :recipe/types
                                                                ; procgen name of item
                                                                :recipe/name
                                                                :recipe/dominate-item
                                                                :effects
                                                                :items
                                                                ;:event/ids
                                                                ; ids of items "used" in events
                                                                ; so that items don't get contradictory events
                                                                ;:event.complication.item/ids
                                                                ;:event.enhancemnt.item/ids
                                                                ;:event.remedy.item/ids
                                                                :done
                                                                #_:choice/ids])
                                           (select-keys current-stage [:gen #_:event/id]))))
              _ (log/debug "results" results)
              choice-variables (->> choice
                                 (filter (fn [[k v]] (= "$" (namespace k))))
                                 (map (fn [[k v]] (let [f (eval-in-ns v recipe-ns)]
                                                    (log/trace "evaluating" v "into" f "arg" (get recipe k))
                                                    [k (f (get recipe k))])))
                                 (into {}))
              ; merge results into current recipe
              state-with-results (-> state
                                   (update-current-recipe
                                     (partial merge-with (partial meta-or-into recipe-ns))
                                     ; remove immediate effects from choice before merging into recipe
                                     (update results :effects (partial remove rcmp/immediate?)))
                                   (update-current-recipe merge choice-variables)
                                     ;(log/info "applying assoc" choice-variables)
                                     ;(fn [recipe] (apply assoc recipe choice-variables)))
                                   (trigger-immediate-effects (get choice :effects))
                                   ; track completed events in recipe
                                   (update-current-recipe
                                     update :events conj (get recipe :current-stage)))]
          (assert (not (nil? state-with-results)))
          (assert (re/validate ::rspec/state state-with-results))
          (log/trace "next-state" (rw/current-state state-with-results))
          ; done with recipe?
          (cond
            (contains? choice :done)
              (save-recipe state-with-results)
            (contains? choice :abort)
              (remove-current-recipe state-with-results)
            :else
              ; if choice has a events pick one,
              (if (not-empty (get choice :choice/events))
                ; find next event
                (let [next-event (rand-nth (get choice :choice/events))
                      _ (log/info "next-event" next-event (type next-event))
                      next-event (if
                                   (or (keyword? next-event)
                                       (symbol? next-event))
                                     (let [event-fn (ns-resolve recipe-ns (-> next-event name symbol))]
                                       (log/info "resolved" recipe-ns next-event "to" event-fn)
                                       (event-fn state recipe))
                                     next-event)
                      next-stage (fill-event 
                                   next-event
                                   recipe)]
                  (log/info "next-event" next-event)
                  (assoc-current-recipe
                    state-with-results
                    :current-stage
                    ; fill in event defaults
                    next-stage))
                ; else the choice has no events, then the next step is to move to the next node
                ; gen event for next node and advance to it
                (assert false (str "choice has no events" choice recipe)))))
        state)
      state)))

(defn update
  [state recipe-ns keyin]
  {:pre [(re/validate ::rspec/state state)]
   :post [(re/validate ::rspec/state %)]}
  (let [selected-recipe-hotkey (get-in state [:world :selected-recipe-hotkey])
        recipe (get-in state [:world :recipes selected-recipe-hotkey])
        new-state (resolve-choice state recipe-ns recipe keyin)]
    #_(log/info "current-recipe" (current-recipe new-state))
    new-state))

(defn init [state recipe-ns recipe]
  {:pre [(re/validate ::rspec/state state)]
   :post [(re/validate ::rspec/state %)]}
  (let [gen-random (ns-resolve recipe-ns 'gen-random)
        recipe-name (recipe-name recipe)
        current-stage (fill-event (gen-random state recipe) recipe)]
  (log/trace "gen-random" gen-random)
  (log/trace "recipe init" recipe)
  (assert (not (get current-stage :screen)))
  (assoc-current-recipe
      state
      :name recipe-name
      :events []
      :current-stage current-stage)))

(defn player-recipes [state]
  (let [empty-recipe {:name "Empty" :detail "----" :empty true}
        base-recipes (map (fn [[hotkey recipe]]
                             #_(log/info (dissoc recipe :img :graph))
                             #_(log/info (recipe-short-desc recipe))
                             (assoc recipe :hotkey hotkey
                                           :name (if (get recipe :empty)
                                                   "Empty"
                                                   (let [name (get recipe :recipe/name (recipe-name recipe))]
                                                     (if (set? name) (first name) name)))
                                           :detail (if (or (get recipe :empty) (empty? (get recipe :effects)))
                                                     "----"
                                                     true)
                                           :effects (merge-effects (get recipe :effects))))
                          (merge
                            {\a empty-recipe
                             \b empty-recipe
                             \c empty-recipe}
                            (get-in state [:world :recipes] {})))
         extra-recipes (->>
                         base-recipes
                         (mapcat :recipe/add-recipe)
                         set 
                         (map (fn [hotkey id]
                                (let [recipe (get-recipe id)]
                                  (-> recipe
                                    (assoc :done true
                                           :hotkey hotkey
                                           :detail "----")
                                    ; break into separate step so that recipe-name can take into account :done true
                                    (as-> recipe
                                      (assoc recipe
                                             :name (recipe-name recipe))))))
                              [\d \e \f]))]
    (concat base-recipes extra-recipes)))


(defn hotkey->recipe [state hotkey]
  (first (filter (fn [recipe] (= (get recipe :hotkey) hotkey))
                 (player-recipes state))))

(defn selected-recipe [state]
  (hotkey->recipe state
    (get-in state [:world :selected-recipe-hotkey])))


(defn selected-recipe-empty?
  [state]
  (let [recipe (selected-recipe state)]
    (or (nil? recipe)
        (get recipe :empty))))

(defn full-name
  [effect]
  (if (satisfies? rcmp/ModQuantifiable effect)
    (str (rcmp/full-name effect) (format "%+d" (rcmp/amount effect)))
    (rcmp/full-name effect)))

