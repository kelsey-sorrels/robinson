;; DSL for defining quests.
(ns dungeon-crusade.quest)

;; Quest DSL
;; =========

(defn quest-data [state quest-id]
  (-> state :world :quests quest-id))

(defmacro quest
  "transform
       (quest-fn [state quest] body)
   to
       (fn [state] (let [quest (quest-data state id)]
                     body))
   for each quest-fn in m."
  [id m]
  (assoc (clojure.walk/postwalk (fn [form] (if (and (list? form)
                                             (= (first form) 'quest-fn))
                                      (let [[_ [state-sym quest-sym] body] form]
                                        `(fn [~state-sym]
                                           (let [~quest-sym (quest-data ~state-sym ~id)]
                                             ~body)))
                                      form))
                         m) :id id))

