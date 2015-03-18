;; Utility functions and functions for manipulating state
(ns robinson.macros)

(defmacro log-time
  "Log the time it takes to execute body."
  [msg & body]
  `(time
     (let [result# (do ~@body)]
       (println ~msg)
       result#)))

(defn vec-match?
  [test-expr expr]
  (let [arg-match? (fn [[test-term term]]
    (cond
      (fn? test-term)  (test-term term)
      (= :* test-term) true
      (set? test-term) (contains? test-term term)
      :else       (= test-term term)))]
  (every? arg-match? (map vector test-expr expr))))

(defmacro first-vec-match
  [match & body]
  `(condp vec-match? ~match
     ~@body))

