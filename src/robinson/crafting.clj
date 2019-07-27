;; Functions for generating random items.
(ns robinson.crafting
  (:require [robinson.common :as rc]
            [robinson.crafting.mod-protocol :as rcmp]
            [robinson.itemgen :as ig]
            [robinson.world :as rw]
            [robinson.player :as rp]
            [robinson.inventory :as ri]
            [taoensso.timbre :as log]
            [loom.graph :as lg]
            [loom.label :as ll]
            [datascript.core :as d]))

(defn current-recipe [state]
  (let [selected-recipe-hotkey (get-in state [:world :selected-recipe-hotkey])]
    (get-in state [:world :recipes selected-recipe-hotkey])))

(defn assoc-current-recipe [state & kvs]
  {:pre [(not (nil? state))]
   :post [(not (nil? %))]}
  (let [selected-recipe-hotkey (get-in state [:world :selected-recipe-hotkey])]
    (update-in state [:world :recipes selected-recipe-hotkey]
      (fn [recipe] (apply assoc recipe kvs)))))

(defn update-current-recipe [state f & xs]
  {:pre [(not (nil? state))]
   :post [(not (nil? %))]}
  (let [selected-recipe-hotkey (get-in state [:world :selected-recipe-hotkey])]
    (update-in state [:world :recipes selected-recipe-hotkey]
      (fn [recipe] (apply f recipe xs)))))

(def recipe-schema {
  :recipe/id {:db/unique :db.unique/identity}
  #_#_:recipe/components {:db/cardinality :db.cardinality/many}
  :recipe/types {:db/cardinality :db.cardinality/many}})

(defn low-weight [item]
  (< (get item :weight 0) 1))

(defn medium-weight [item]
  (< 1 (get item :weight 0) 10))

(defn high-weight [item]
  (< 10 (get item :weight 0)))

(defn stick-like [item]
  (contains? (get item :properties) :stick-like))

(defn rock [item]
  (= (get item :item/id) :rock))

(defn feather [item]
  (= (get item :item/id) :feather))

(defn flexible [item]
  (contains? (get item :properties) :flexible))

(defn tensile [item]
  (< 1 (get item :tensile-strength 0)))

(defn planar [item]
  (contains? (get item :properties) :planar))

(defn pointed [item]
  (< (get item :roundness 1) 0.2))

(defn sharp [item]
  (< 1 (get item :sharpness 0)))

(defn edged [item]
  (contains? (get item :properties) :edged))

(defn round [item]
  (< 0.5 (get item :roundness 1)))

(defn wooden [item]
  (contains? (get item :item/materials) :wood))

(defn tube-like [item]
  (contains? (get item :properties) :tube-like))

(defn handled? [item]
  (contains? (get item :properties) :handled))

(def recipe-pred->str {
  low-weight "low weight"
  stick-like "sticklike"
  rock "rock"
  flexible "flexible"
  tensile "tensile"
  planar "flat"
  pointed "pointed"
  sharp "sharp"
  edged "edged"
  round "round"
})

(defn add-example-item-properties
  "Takes recipe example items and runs them through item properties associating the result to :recipe/example-item-properties"
  [recipe]
  (let [flag-fns (into {} (map (fn [fn-sym] [(keyword fn-sym) (ns-resolve 'robinson.crafting fn-sym)])
                               ['low-weight
                                'medium-weight
                                'high-weight
                                'stick-like
                                'rock
                                'feather
                                'flexible
                                'tensile
                                'planar
                                'pointed
                                'sharp
                                'edged
                                'round
                                'wooden
                                'tube-like]))]
  (assoc recipe
    :recipe/example-item-properties
    (->> recipe
      :recipe/example-item-requirements
      (map (fn [example-item-id]
             (reduce-kv (fn [s k v]
                          (log/info (:recipe/id recipe) example-item-id s k v (ig/id->item example-item-id))
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
     {:recipe/id  :club
      :recipe/category :weapon
      :recipe/types #{:blunt :melee}
      :recipe/example-item-requirements #{:stick :branch}
      :recipe/components #{:recipe.component/handle :recipe.component/head}
      :recipe/add [:club]
      :recipe/requirements '[each-of
                              [and
                                low-weight
                                stick-like]]}
     {:recipe/id :throwing-hammer
      :recipe/category :weapon
      :recipe/types #{:blunt :thrown}
      :recipe/example-item-requirements #{:stick :branch :rock}
      :recipe/components #{:recipe.component/handle :recipe.component/head}
      :recipe/add [:throwing-hammer]
      :recipe/requirements '[each-of
                              stick-like
                              [and
                                low-weight
                                rock]
                              flexible]}
     {:recipe/id :sling
      :recipe/category :weapon
      :recipe/types #{:blunt :ranged}
      :recipe/example-item-requirements #{:rock :leather}
      :recipe/components #{:recipe.component/cord :recipe.component/pocket}
      :recipe/add [:sling]
      :recipe/requirements '[each-of
                              flexible
                              [and tensile
                                   flexible
                                   planar]]}
     ; edged
     {:recipe/id  :dagger
      :recipe/category :weapon
      :recipe/types #{:edged :melee}
      :recipe/example-item-requirements #{:rock :stick :branch :plant-fiber :leather}
      :recipe/components #{:recipe.component/handle :recipe.component/blade :recipe.component/binder}
      :recipe/add [:dagger]
      :recipe/requirements '[each-of
                              edged
                              [and low-weight
                                   stick-like]]}
     {:recipe/id  :throwing-axe
      :recipe/category :weapon
      :recipe/types #{:edged :thrown}
      :recipe/example-item-requirements #{:rock :stick :branch :plant-fiber :leather}
      :recipe/components #{:recipe.component/handle :recipe.component/blade :recipe.component/binder}
      :recipe/add [:throwing-axe]
      :recipe/requirements '[each-of
                              edged
                              [and low-weight
                                   stick-like]
                              flexible]}
      {:recipe/id  :boomerang
      :recipe/category :weapon
      :recipe/types #{:edged :ranged}
      :recipe/example-item-requirements #{:stick :branch}
      :recipe/components #{:recipe.component/blade}
      :recipe/add [:boomerang]
      :recipe/requirements '[each-of
                              [and
                                wooden
                                [or medium-weight
                                    high-weight]]
                              [tool edged]]}
     ; piercing
     {:recipe/id  :spear
      :recipe/category :weapon
      :recipe/types #{:piercing :melee}
      :recipe/example-item-requirements #{:rock :stick :branch :plant-fiber :leather}
      :recipe/components #{:recipe.component/shaft :recipe.component/point}
      :recipe/add [:spear]
      :recipe/requirements '[each-of
                              pointed
                              [and low-weight
                                   stick-like]]}
     {:recipe/id  :throwing-spear
      :recipe/category :weapon
      :recipe/types #{:piercing :thrown}
      :recipe/example-item-requirements #{:rock :stick :branch :plant-fiber :leather}
      :recipe/components #{:recipe.component/shaft :recipe.component/point}
      :recipe/add [:throwing-spear]
      :recipe/requirements '[each-of
                              sharp
                              [and low-weight
                                   stick-like]]}
     {:recipe/id  :bow
      :recipe/category :weapon
      :recipe/types #{:piercing :ranged}
      :recipe/example-item-requirements #{:rock :stick :branch :plant-fiber :leather :feather}
      :recipe/components #{:recipe.component/string :recipe.component/length}
      :recipe/add [:bow]
      :recipe/add-recipe #{:arrow}
      :recipe/requirements '[each-of
                              flexible
                              [and low-weight
                                   stick-like]]}
     {:recipe/id  :blowgun
      :recipe/category :weapon
      :recipe/types #{:piercing :ranged}
      :recipe/example-item-requirements #{:bamboo :stick}
      :recipe/components #{:recipe.component/tube}
      :recipe/add [:blowgun]
      :recipe/add-recipe #{:blowdart}
      :recipe/requirements '[each-of
                              [and tube-like
                                high-weight]]}
      ; flexible
     {:recipe/id  :garrote
      :recipe/category :weapon
      :recipe/types #{:flexible :melee}
      :recipe/example-item-requirements #{:plant-fiber :stick}
      :recipe/components #{:recipe.component/string :recipe.component/grip}
      :recipe/add [:garrote]
      :recipe/requirements '[each-of
                              flexible
                              [and low-weight stick-like]]}
     {:recipe/id  :bolas
      :recipe/category :weapon
      :recipe/types #{:flexible :thrown}
      :recipe/example-item-requirements #{:plant-fiber :stick :rope :rock}
      :recipe/components #{:recipe.component/string :recipe.component/ball}
      :recipe/add [:bolas]
      :recipe/requirements '[each-of
                              flexible
                              [count 3 [and round low-weight]]]}
                             
     {:recipe/id  :whip
      :recipe/category :weapon
      :recipe/types #{:flexible :ranged}
      :recipe/example-item-requirements #{:plant-fiber :rope}
      :recipe/components #{:recipe.component/handle :recipe.component/thong}
      :recipe/add [:whip]
      :recipe/requirements '[each-of
                              flexible]}
     ; Ammunition
     {:recipe/id  :arrow
      :recipe/category :ammunition
      :recipe/types #{}
      :recipe/add [:arrow]
      :recipe/requirements '[each-of
                              [and low-weight
                                   piercing]
                              [and low-weight
                                   stick-like]
                              feather]}
     {:recipe/id  :blowdart
      :recipe/category :ammunition
      :recipe/types #{}
      :recipe/example-item-requirements #{:bamboo :stick}
      :recipe/add [:blowdart]
      :recipe/requirements '[each-of
                              [and low-weight stick-like]]}
    ;;; Survival
    ;; Tools
     {:recipe/id  :saw
      :recipe/category :survival
      :recipe/types #{:tool}
      :recipe/example-item-requirements #{:stick :rope :rock}
      :recipe/add [:saw]
      :recipe/requirements '[each-of
                             flexible]}
     {:recipe/id  :hammer
      :recipe/category :survival
      :recipe/types #{:tool}
      :recipe/example-item-requirements #{:stick :rope :rock}
      :recipe/add [:hammer]
      :recipe/requirements '[each-of
                             flexible]}
     {:recipe/id  :ruler
      :recipe/category :survival
      :recipe/types #{:tool}
      :recipe/example-item-requirements #{:stick :rope :rock}
      :recipe/add [:ruler]
      :recipe/requirements '[each-of
                             flexible]}
     {:recipe/id  :fire-starter
      :recipe/category :survival
      :recipe/types #{:tool}
      :recipe/example-item-requirements #{:stick :rope :rock}
      :recipe/add [:fire-starter]
      :recipe/requirements '[each-of
                              flexible]}
     ;; Rafts
     {:recipe/id  :reed-raft
      :recipe/category :survival
      :recipe/types #{:raft}
      :recipe/example-item-requirements #{:stick :rope :rock}
      :recipe/add [:reed-raft]
      :recipe/requirements '[each-of
                             flexible]}
     {:recipe/id  :cannoe
      :recipe/category :survival
      :recipe/types #{:raft}
      :recipe/example-item-requirements #{:stick :rope :rock}
      :recipe/add [:cannoe]
      :recipe/requirements '[each-of
                             flexible]}
     {:recipe/id  :kayak
      :recipe/category :survival
      :recipe/types #{:raft}
      :recipe/example-item-requirements #{:stick :rope :rock}
      :recipe/add [:kayak]
      :recipe/requirements '[each-of
                              flexible]}
     {:recipe/id  :log-raft
      :recipe/category :survival
      :recipe/types #{:rafts}
      :recipe/example-item-requirements #{:stick :branch :log :rope}
      :recipe/add [:log-raft]
      :recipe/requirements '[each-of
                              flexible]}]))

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
           ;[(?info ?types)]
           ;[?e :recipe/id :club]
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
           ;[(?info ?types)]
           ;[?e :recipe/id :club]
           (matches-all ?info ?seq ?first ?rest ?empty ?e :recipe/types ?types)
           [?e :recipe/example-item-requirements ?v]]
          recipe-db
          rules
          (fn [msg x] (log/info msg x) true)
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
           ;[?e :recipe/id :club]
           (matches-all ?info ?seq ?first ?rest ?empty ?e :recipe/types ?types)
           [?e :recipe/example-item-properties ?v]]
          recipe-db
          rules
          (fn info [x] (log/info x) true)
          seq
          first
          rest
          empty?
          types)
    (mapcat identity)
    (mapcat identity)
    set)))

(defn satisfies-helper?
  [l expr]
  "Counts the items in that 'satisfy' the expression.
   Results >= 1 indicate satisfaction. Results < 1
   indicate that the expression is unsatisfied."
  (cond
    (nil? expr)
      (assert false (str "Invalid expression" expr))
    (= (first expr) 'count)
      (let [[_ n arg] expr]
        ;(println "count" n arg)
        (/ (satisfies-helper? l arg) n))
    (= (first expr) 'and)
      (let [[_ & args] expr]
        ;(println "and" args)
        (reduce min (map (partial satisfies-helper? l) args)))
    (= (first expr) 'or)
      (let [[_ & args] expr]
        ;(println "or" args)
        (reduce + (map (partial satisfies-helper? l) args)))
    (keyword? (first expr))
      (let [[arg] expr]
        ;(println "keyword" arg expr)
        (->> l
           (map :item/id)
           (filter (partial = arg))
           (map count)
           (reduce + 0)))
    (symbol? (first expr))
      ; when multiple symbols are present, use the conjunction of all of them s0 and s1 and .... and sn
      (let [preds-juxt (apply juxt (map (partial ns-resolve 'robinson.crafting expr) expr))
            and-comp (fn [e] (every? identity (preds-juxt e)))]
        (->> l
          (filter and-comp)
          count))
    :else
      (assert false (str "Invalid expression" expr))))
  
(defn has-prerequisites?
  "Return true if the player has the ability to make the recipe."
  [state recipe]
  (let [inventory        (get-in state [:world :player :inventory])
        requirements     (get recipe :recipe/requirements)]
    (<= 1 (satisfies-helper? inventory requirements))))

(defn item-satisfies-requirement-clause?
  [item clause]
  ;(log/info item)
  ;(log/info clause)
  ;(log/info (type clause))
  (cond
    (fn? clause)
      (clause item)
    (symbol? clause)
      (let [f (ns-resolve 'robinson.crafting clause)]
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
      (not ((second clause) item))))

(defn slot->item [state slot]
  (let [selected-recipe-hotkey (get-in state [:world :selected-recipe-hotkey])
        hotkey (get-in state [:world :recipes selected-recipe-hotkey :slots slot])]
    ;(log/info selected-recipe-hotkey hotkey slot)
    ;(log/info (type hotkey))
    (ri/inventory-hotkey->item state hotkey)))

(defn requirements-satisfied?
  [state requirements]
  (let [rest-requirements (rest requirements)
        satisfied (every? identity
                          (map-indexed (fn [idx req]
                                         (let [slot-item (slot->item state idx)]
                                           (item-satisfies-requirement-clause? slot-item req)))
                            rest-requirements))]
    satisfied))

(defn get-recipes
  "Return recipes tagged with :applicable true if the recipe has the required pre-requisites."
  [state]
  (apply hash-map
    (mapcat
      (fn [[group-name group]]
        [group-name (map (fn [recipe] (if (has-prerequisites? state recipe)
                                        (assoc recipe :applicable true)
                                        recipe))
                         group)])
      (get-in state [:world :recipes]))))

(defn- exhaust-by-hotkeys
  [state hotkeys]
  (reduce (fn [state hotkey]
            (do 
              (log/info "removing" hotkey)
              (ri/dec-item-count state hotkey)))
          state
          hotkeys))

(defn- place-cell-type
  [state id effects]
    (let [[x y] (rp/player-xy state)]
      (rw/assoc-cell state x y :type id)))
  
(defn- place-drop
  [state id effects]
  (let [[x y] (rp/player-xy state)]
    (rw/conj-cell-items state x y (ig/id->item id))))

(defn- place-inventory
  [state id effects]
  (log/info "placing item with id" id "in inventory")
  (let [item (ig/id->item id)
        _ (log/info item)
        updated-item (reduce (fn [item effect]
                               (if (satisfies? rcmp/ModItemOnCreate effect)
                                 (rcmp/item-on-create effect item)
                                 item))
                             item
                             (get item :effects))]
    (ri/add-to-inventory state [updated-item])))

(defn- add-by-ids
  [state ids effects place]
  (reduce (fn [state id]
            (case place
              :cell-type
                (place-cell-type state id effects)
              :drop
                (place-drop state id effects)
              :inventory
                (place-inventory state id effects)))
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

(defn recipe-name
  ([recipe]
   (recipe-name recipe true))
  ([recipe show-progress]
    (let [types (get recipe :types)]
      (str
        (if (and show-progress (in-progress? recipe))
          "In-progress " 
          "")
        (case (count types)
          0 (name (get recipe :recipe/id "unknown"))
          1 (-> types first name)
          2 (-> recipe
              :types
              get-recipe-by-types
              :recipe/id
              ig/id->name))))))

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

(defn recipe-requirements
  [recipe]
  (-> recipe :types get-recipe-by-types :recipe/requirements))

(defn save-recipe
  [state]
  {:post [(not (nil? %))]}
  (log/info "Saving recipe")
  (log/info (type (get-in state [:world :recipes])))
  (log/info (vec (get-in state [:world :recipes])))
  (let [recipe (current-recipe state)
        selected-recipe-hotkey (get-in state [:world :selected-recipe-hotkey])
        _ (log/info recipe)
        recipe-blueprint (-> recipe
                           :types
                           get-recipe-by-types)]
    (log/info recipe-blueprint)
    (-> state
      (update-in [:world :recipes] assoc
        selected-recipe-hotkey (-> recipe
                                 (merge recipe-blueprint)
                                 (assoc 
                                   :name (recipe-name recipe))))
      (rw/assoc-current-state :recipes))))

(defn fill-event
  [event]
  (-> event
    (assoc :description (let [description (:description event)]
                          (if (sequential? description)
                            (rand-nth description)
                            description)))
    (cond-> (empty? (get event :event/choices))
      (assoc :event/choices [{
        :hotkey :space
        :name "continue"}]))))
 
(defn fill-choice
  [add-done choice]
  (if add-done
    (assoc choice :done true)
    choice))

(defn meta-or-into
  [recipe-ns val-in-result val-in-latter]
  (if-let [merge-sym (-> val-in-latter meta :merge)]
    (let [f (ns-resolve recipe-ns merge-sym)]
      (log/info f)
      (f val-in-result val-in-latter))
    (into val-in-result val-in-latter)))

(defn trigger-immediate-effects
  [state effects]
  {:post [(not (nil? %))]}
  (reduce (fn [state effect]
            (cond
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
  (if-let [{:keys [material adjacent-to-fire]} (get choice :requirements)]
    (cond
      material
        (let [{:keys [id amount]} material]
          (<= amount (ri/inventory-id->count state id)))
      adjacent-to-fire
        (some (fn [cell] (rw/type->on-fire? (get cell :type)))
              (rw/player-adjacent-cells state)))
    true))

; Input handlers
(defn resolve-choice
  [state recipe-ns recipe keyin]
  {:post [(not (nil? %))]}
  (let [current-stage (get recipe :current-stage)]
        ; find selected choice

    (log/info "choices" (vec (->> (get current-stage :event/choices))))
    (log/info "choice" (->> (get current-stage :event/choices)
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
                                    (merge (select-keys choice [:types
                                                                :effects
                                                                :materials
                                                                :done
                                                                :event/id
                                                                :choice/id])
                                           (select-keys current-stage [:gen]))))
              _ (log/info "results" results)
              ; merge results into current recipe
              state-with-results (-> state
                                   (update-current-recipe (partial merge-with (partial meta-or-into recipe-ns)) results)
                                   (trigger-immediate-effects (get choice :effects)))]
          (assert (not (nil? state-with-results)))
          ; done with recipe?
          (if (contains? choice :done)
            (save-recipe state-with-results)
            ;; either a regular event, or a direction event
            ; if choice has a events pick one,
            (if (seq (get choice :choice/events))
              ; find next event
              (let [next-event (rand-nth (get choice :choice/events))
                    ; fill in event defaults
                    next-event (fill-event next-event)]
                ; assign next event and return
                (assoc-current-recipe
                  state-with-results
                  :current-stage next-event))
              ; else the choice has no events, then the next step is to move to the next node
              (if-let [next-node (get choice :next-node)]
                ; choice points to next-node
                ; gen event for next node and advance to it
                ; typical direction choice
                (->
                  (let [next-node-type (get (ll/label (get recipe :graph) next-node) :type)]
                    ((case next-node-type
                      \? (ns-resolve recipe-ns 'gen-question)
                      \! (ns-resolve recipe-ns 'gen-complication)
                      \+ (ns-resolve recipe-ns 'gen-remedy)
                      \& (ns-resolve recipe-ns 'gen-material)
                      \☼ (ns-resolve recipe-ns 'gen-enhancement)
                      (assert false (str "next node type unknown " next-node next-node-type)))
                      state-with-results
                      (let [recipe (current-recipe state-with-results)]
                        (assoc recipe :recipe/example-item-properties (get-example-item-properties-by-types (get recipe :types))))))
                  (assoc-current-recipe :current-node next-node))
                ; choice does not point to next node
                (let [next-node-choices (next-node-choices recipe)]
                  (log/info "NO NEXT NODE in CHOICE")
                  (log/info "choice" choice)
                  (log/info "next-node-choices" (vec next-node-choices))
                  (cond
                    ; no more nodes, create finish recipe event
                    (not (seq (next-nodes recipe)))
                      (assoc-current-recipe state-with-results
                        :current-stage
                        {:title "Done"
                         :event/choices [
                            {:name "finish recipe"
                             :hotkey :space
                             :done true}]})
                    ; one path to next node? auto-increment
                    (= 1 (count next-node-choices))
                      (->
                        (let [next-node (-> next-node-choices first :next-node)
                              _ (log/info "next-node" next-node)
                              next-node-type (get (ll/label (get recipe :graph)
                                                            next-node)
                                                  :type)]
   
                          ((case next-node-type
                            \? (ns-resolve recipe-ns 'gen-question)
                            \! (ns-resolve recipe-ns 'gen-complication)
                            \+ (ns-resolve recipe-ns 'gen-remedy)
                            \& (ns-resolve recipe-ns 'gen-material)
                            \☼ (ns-resolve recipe-ns 'gen-enhancement)
                            (assert false (str "next node type unknown " next-node next-node-type)))
                            state-with-results
                            (let [recipe (current-recipe state-with-results)]
                              (assoc recipe :recipe/example-item-properties (get-example-item-properties-by-types (get recipe :types))))))
                        (assoc-current-recipe :current-node (-> next-node-choices first :next-node)))
                    ;; else multiple choices to next node, create event to choose
                    :else
                      (do
                        (log/info "multiple choices to next node. Choose from" next-node-choices)
                        (assoc-current-recipe
                          state-with-results
                          :current-stage
                          {:title "Choose path"
                           :event/choices next-node-choices}))))))))
        state))))

(defn update [state recipe-ns keyin]
  (let [selected-recipe-hotkey (get-in state [:world :selected-recipe-hotkey])
        recipe (get-in state [:world :recipes selected-recipe-hotkey])]
    (resolve-choice state recipe-ns recipe keyin)))

(defn init [state recipe-ns recipe]
  {:pre [(not (nil? state))]
   :post [(not (nil? %))]}
  (let [n (get recipe :current-node)]
    (log/info "current node label" (ll/label (get recipe :graph) n))
    ((case (get (ll/label (get recipe :graph) n) :type)
        \? (ns-resolve recipe-ns 'gen-question)
        (assert false (str "current node not question" n (get recipe :graph))))
       state recipe)))

(defn craft-recipe
  "Perform the recipe."
  [state recipe]
  (let [exhaust-hotkeys (->> (get-in recipe [:recipe/requirements])
                          ; remove 'each-of
                          rest
                          ; add slot indexes
                          (map-indexed vector)
                          ; remove tool clauses
                          (remove (fn [[_ clause]] (contains? #{'tool} clause)))
                          ; find which items ids are in remaining slots
                          (map (fn [[slot _]] (get (slot->item state slot) :hotkey))))
        add          (get-in recipe [:recipe/add])
        effects      (get recipe :effects [])
        state (as-> state state
                  (add-by-ids state add effects (get recipe :place :inventory))
                  ; exhaust non-tool slot items
                  (exhaust-by-hotkeys state exhaust-hotkeys)
                  #_(rp/player-update-hunger state (fn [current-hunger] (min (+ hunger current-hunger)
                                                                           (rp/player-max-hunger state))))
                  #_(rp/player-update-thirst state (fn [current-thirst] (min (+ hunger current-thirst)
                                                                           (rp/player-max-thirst state))))
                  (reduce rp/update-crafted state (map (fn [id] {:id id}) add)))]
    state))

(defn player-recipes [state]
  (let [empty-recipe {:name "Empty" :alt "----" :empty true}
        base-recipes (map (fn [[hotkey recipe]]
                             #_(log/info (dissoc recipe :img :graph))
                             #_(log/info (recipe-short-desc recipe))
                             (assoc recipe :hotkey hotkey
                                           :name (if (get recipe :empty)
                                                   "Empty"
                                                   (recipe-name recipe))
                                           :alt (if (get recipe :empty)
                                                  "----"
                                                  (recipe-short-desc recipe))
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
                                           :alt "----")
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

