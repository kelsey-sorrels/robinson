;; ## A roguelike focused on immutability.##
;;
;(set! *warn-on-reflection* true)
(ns robinson.core
  (:require [robinson.main :as main]
            [robinson.aterminal :as aterminal]
            [robinson.world :as rw]
            #+clj
            [clojure.core.async :as async :refer [go go-loop]]
            #+cljs
            [cljs.core.async :as async]
            #+clj
            [clojure.stacktrace :refer [print-stack-trace]]
            #+clj
            [taoensso.timbre :as log]
            #+cljs
            [shodan.console :as log :include-macros true])
  #+clj
  (:gen-class)
  #+cljs
  (:require-macros [cljs.core.async.macros :refer [go go-loop]]))

#+cljs
(enable-console-print!)

;; Conveinience ref for accessing the last state when in repl.
(defonce state-ref (atom nil))

;; (clojure.core.async/thread (robinson.core/-main))
;; (robinson.core/-main)

(def done-chan (async/chan))

(defn -main
  "Entry default point to application.

   Uses `setup` and `tick` function from `robinson.main`.

   `setup` returns the initial state of the application.
  
   `tick` takes the current state of the application and returns
   the next state after one iteration."
  []
  ; start with initial state from setup-fn
  (go-loop [state (main/setup)]
    (reset! state-ref state)
    (if (nil? state)
      (do
        (log/info "Got nil state. Exiting.")
        #+clj
        (async/>! done-chan true)
        #+clj
        (System/exit 0)
        #+cljs
        nil)
    ; tick the old state through the tick-fn to get the new state
    (recur
      (try
        (let [keyin (or (when (= (rw/current-state state) :sleep)
                          \.)
                        (let [key-chan (aterminal/get-key-chan (state :screen))]
                          (log/info  "waiting for key-chan")
                          (async/<! key-chan)))]
             (if keyin
               (main/tick state keyin)
               state))
        #+clj
        (catch Exception ex
          (do 
              (print-stack-trace ex)
              (throw ex)))
        #+cljs
        (catch js/Error ex
              (log/error (str ex))
              (throw ex))))))
  #+clj
  (async/<!! done-chan))
          
#+cljs
(-main)

