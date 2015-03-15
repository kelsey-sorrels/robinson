;; ## A roguelike focused on immutability.##
;;
;(set! *warn-on-reflection* true)
(ns robinson.core
  (:require [robinson.main :as main]
            #+clj
            [clojure.stacktrace :refer [print-stack-trace]]
            #+cljs
            [shodan.console :as log :include-macros true])
  #+clj
  (:gen-class))

;; Conveinience ref for accessing the last state when in repl.
(defonce state-ref (atom nil))

;; (clojure.core.async/thread (robinson.core/-main))
;; (robinson.core/-main)


(defn -main
  "Entry default point to application.

   Uses `setup` and `tick` function from `robinson.main`.

   `setup` returns the initial state of the application.
  
   `tick` takes the current state of the application and returns
   the next state after one iteration."
  []
  ; start with initial state from setup-fn
  (loop [state (main/setup)]
    (reset! state-ref state)
    (if (nil? state)
      #+clj
      (System/exit 0)
      #+cljs
      nil)
    ; tick the old state through the tick-fn to get the new state
    (recur
      (try (main/tick state)
        #+clj
        (catch Exception ex
          (do 
              (print-stack-trace ex)
              (throw ex)))
        #+cljs
        (catch js/Error ex
              (log/error (str ex))
              (throw ex))))))
          

#+cljs
(-main)

