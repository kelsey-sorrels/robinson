(ns robinson.storylet
  (:require [robinson.random :as rr]
            [taoensso.timbre :as log]))

(defmacro def-storylet
  [name prerequisites & fdecl]
  (log/info `(defn ~name {:robinson.storylet/prerequisites ~prerequisites
                          :robinson/type :storylet}
    [~'state ~'recipe]
    ~@fdecl))
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

(defn any-in?
  [s1 s2]
  (log/info s1 s2 (not-empty (clojure.set/intersection s1 s2)))
  (not-empty (clojure.set/intersection s1 s2)))

(defn rand-storylet
  [state recipe storylets after]
  (log/info "storylets" (vec storylets))
  (let [last-event-id (-> recipe :events last :event/id)
          past-event-ids (->> recipe :events (map :event/id))
          after (clojure.set/union #{after :any}
                                   (get recipe :event/id #{})
                                   (get recipe :choice/id #{}))
          storylet (->> storylets
                     ; check reqs match any state
                     (filter (fn [storylet]
                            (log/info storylet (meta storylet))
                            (any-in? (-> storylet meta :robinson.storylet/prerequisites) after)))
                     vec
                     ; pick one
                     rand-nth)]
     (log/info "after" after)
     (log/info "storylets" (count storylets))
     (log/info "next-event" storylet)
     (log/info storylet)
     storylet))


