;; ## A roguelike focused on immutability.##
;;
;(set! *warn-on-reflection* true)
(ns robinson.core
  (:require [robinson.main :as main]
            [robinson.aterminal :as aterminal]
            [robinson.world :as rw]
            #?@(:clj (
                [clojure.core.async :as async :refer [go go-loop]]
                [clojure.stacktrace :refer [print-stack-trace]]
                [taoensso.timbre :as log])
                :cljs (
                [cljs.core.async :as async]
                [taoensso.timbre :as log :include-macros true])))
  #?(:clj
     (:gen-class)
     :cljs
     (:require-macros [cljs.core.async.macros :refer [go go-loop]])))

#?(:cljs
(enable-console-print!))

;; Conveinience ref for accessing the last state when in repl.
(defonce state-ref (atom nil))

;; (clojure.core.async/thread (robinson.core/-main))
;; (robinson.core/-main)

(defonce done-chan (async/chan))

(defn -main
  "Entry default point to application.

   Uses `setup` and `tick` function from `robinson.main`.

   `setup` returns the initial state of the application.
  
   `tick` takes the current state of the application and returns
   the next state after one iteration."
  []
  ; start with initial state from setup-fn
  (main/setup
    (fn [state]
      (go-loop [state state]
        (reset! state-ref state)
        (if (nil? state)
          (do
            (log/info "Got nil state. Exiting.")
            #?@(:clj (
                (async/>! done-chan true)
                (System/exit 0))
                :cljs (
                nil)))
          ; tick the old state through the tick-fn to get the new state
          (let [state (try
                        (log/info "Core current-state" (rw/current-state state))
                        (let [keyin (if (= (rw/current-state state) :sleep)
                                      (do
                                        (log/info "State = sleep, Auto-pressing .")
                                        \.)
                                      (let [key-chan (aterminal/get-key-chan (state :screen))]
                                        (log/info  "waiting for key-chan")
                                        (async/<! key-chan)))]
                             (log/info "Core got key" keyin)
                             (if keyin
                               (main/tick state keyin)
                               state))
                        #?(:clj
                           (catch Throwable ex
                             (log/error ex)
                             (print-stack-trace ex)
                             state)
                          :cljs
                          (catch js/Error ex
                             (log/error (str ex))
                             state)))]
            (log/info "End of game loop")
            (recur state))))
               
      ;#?(:clj
         (async/<!! done-chan))))
          
#?(:cljs
   (-main))

