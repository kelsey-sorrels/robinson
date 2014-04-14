;; ## A roguelike focused on immutability.##
;;
(ns dungeon-crusade.core
  (:use dungeon-crusade.main
        clojure.stacktrace))

(defn -main
  "Entry default point to application.

   Uses `setup` and `tick` function from `dungeon-crusade.main`.

   `setup` returns the initial state of the application.
  
   `tick` takes the current state of the application and returns
   the next state after one iteration."
  []
  (do
    ; start with initial state from setup-fn
    (loop [state (setup)]
      (if (nil? state)
        (System/exit 0))
      ; tick the old state through the tick-fn to get the new state
      (recur (try (tick state)
        (catch Exception ex
          (do (print-stack-trace ex)
              (throw ex))))))))
