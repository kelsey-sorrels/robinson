(ns dungeon-crusade.core
  (:use dungeon-crusade.main
        clojure.stacktrace))

(defn -main []
  ; start with initial state from setup-fn
  (loop [state (setup)]
    (if (nil? state)
      (System/exit 0))
    ; tick the old state through the tick-fn to get the new state
    (recur (try (tick state)
      (catch Exception ex
        (do (print-stack-trace ex)
            (throw ex)))))))
