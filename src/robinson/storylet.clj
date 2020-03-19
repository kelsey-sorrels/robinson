(ns robinson.storylet
  (:require [robinson.random :as rr]
            [robinson.crafting :as rcrafting]
            [clojure.walk :as walk]
            [taoensso.timbre :as log]))

;;; Predicate helpers
(defn- past-event-ids
  [state]
  (->> state
    rcrafting/current-recipe
    :events
    (map :event/id)
    (remove nil?)
    set))
 
(defn any
  [_] true)

(defn after?
  [event-id]
  (fn  [state]
    (= (get state :after) event-id)))

(defn in-history?
  [event-id]
  (fn [state]
    (contains? (past-event-ids state) event-id)))

(defn once
  [_]
  (assert false "This should not be invoked"))

(defn and
  [& clauses]
  (cons 'and clauses))

(defn or
  [& clauses]
  (cons 'or clauses))

(defn not
  [clause]
  ['not clause])

(defn expand-prerequisites
  [name prerequisites]
  (walk/postwalk (fn [node]
                   #_(log/info "expanding prereq" (if (sequential? node) (vec node) node) (type node) (= node once) once)
                   (if (= node once)
                     (fn [state]
                       (let [past-event-ids (past-event-ids state)]
                         (not (contains? past-event-ids name))))
                     node))
                  prerequisites))
                         
(defmacro def-storylet
  [name prerequisites & fdecl]
  `(defn ~name {:robinson.storylet/prerequisites ~prerequisites
                :robinson/type :storylet}
    [~'state ~'recipe]
    ~@fdecl))

(defn ns-storylets
  [ns]
  (log/info ns)
  (->> ns
    ns-publics
    vals
    (filter (comp (partial = :storylet) :robinson/type meta))))

(defmacro ns-bind
  [sym]
  `(symbol (var ~sym)))

;;; Predicate evaluation
(defn dispatch-pred
  [_ clause]
  (cond
    (fn? clause) :fn
    (symbol? clause) :symbol
    (= (first clause) 'and) :and
    (= (first clause) 'or) :or
    (= (first clause) not) :not
    (keyword? (first clause)) :keyword))

; returns [result score]
; where result is a boolean
; and score is a number where higher = more relevant
(defmulti eval-pred
  dispatch-pred)

(defmethod eval-pred :default
  [state clause]
  (log/error "invalid clause" clause)
  (assert false)
  [false 0.0])

(defmethod eval-pred :fn
  [state clause]
  (let [r (clause state)]
    [r (if r 1.0 -1.0)]))

(defmethod eval-pred :symbol
  [state clause]
  (let [f (ns-resolve 'robinson.crafting clause)
        r (f state)]
    [r (if r 1.0 -1.0)]))

(defmethod eval-pred :and
  [state clause]
  (let [rs (map (partial eval-pred state) (rest clause))
        results (map first rs)
        scores (map second rs)]
    [(every? identity results)
     (reduce + scores)]))

(defmethod eval-pred :or
  [state clause]
  (let [rs (map (partial eval-pred state) (rest clause))
        results (map first rs)
        scores (map second rs)]
    [(some identity results)
     (->> scores
       (map (partial max 0))
       (reduce +))]))

(defmethod eval-pred :not
  [state clause]
  (let [[r s] ((second clause) state)]
    [(not r) (- s)]))

(defmethod eval-pred :keyword
  [state clause]
  (let [r (= (second clause) (get state (first clause)))]
    [r (if r 1.0 -1.0)]))


;;; Quality based narrative for storylets
(defn rand-storylet
  [state recipe storylets after]
  {:pre [(seq storylets)]
   :post [(not (nil? %))]}
  (log/info "storylets" (vec storylets))
  (let [last-event-id (-> recipe :events last :event/id)
        past-event-ids (->> recipe :events (map :event/id))
        after (clojure.set/union #{after :any}
                                 (get recipe :event/ids #{})
                                 (get recipe :choice/ids #{}))
        state (assoc state :after after)
        candidate-storylets (->> storylets
                   ; check reqs match state
                   (map (fn [storylet]
                          #_(log/info storylet (meta storylet))
                          (let [storylet-name (-> storylet meta :name)
                                prerequisites (-> storylet meta :robinson.storylet/prerequisites)
                                expanded-prerequisites (expand-prerequisites storylet-name prerequisites)]
                          (log/info (if (coll? expanded-prerequisites) (vec expanded-prerequisites) expanded-prerequisites))
                          [(eval-pred state expanded-prerequisites) storylet-name])))
                   ; keep results which evaluate true
                   (filter (fn [[[r _] s]] (log/info r s) r))
                   ; map to [score storylet]
                   (map (fn [[[_ score] storylet]] [score storylet])))]
                   ; pick one based on score
     (assert (seq candidate-storylets) candidate-storylets)
     (log/info "after" after)
     (log/info "storylets" (count storylets))
     (log/info "candidate-storylets" (count candidate-storylets) (mapv first candidate-storylets))
     (log/info (vec candidate-storylets))
     (let [storylet (rr/rand-weighted-nth candidate-storylets)] 
       (log/info "next-event" storylet)
       storylet)))


