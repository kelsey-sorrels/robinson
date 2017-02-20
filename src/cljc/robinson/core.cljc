;; ## A roguelike focused on immutability.##
;;
;(set! *warn-on-reflection* true)
(ns robinson.core
  (:require [robinson.main :as main]
            [zaffre.terminal :as zat]
            [zaffre.glterminal :as zgl]
            [zaffre.events :as zevents]
            [zaffre.tilesets :as ztiles]
            [zaffre.animation.wrapper :as zaw]
            [zaffre.util :as zutil]
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

(def font ztiles/pastiche-16x16)
 
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
     (log/info "terminal-groups" (get state :terminal-groups))
     (log/info "terminal-opts" (get state :terminal-opts))
      (zgl/create-terminal ;zaw/create-animated-terminal
        ;zgl/create-terminal
        (get state :terminal-groups)
        (get state :terminal-opts)
        (fn [terminal]
          (reset! state-ref (assoc state :screen terminal))
          ;; render loop
          (zat/do-frame terminal 33
            ;; Draw strings
             (zutil/put-string terminal :ui 20 8 "Hello world")
             (zat/refresh! terminal)
            nil)
          (zevents/add-event-listener terminal :keypress
            (fn [keyin]
              (swap! state-ref (fn [state]
                ; tick the old state through the tick-fn to get the new state
                (let [state (try
                              (let [keyin (cond
                                            (= (rw/current-state state) :sleep)
                                            (do
                                              (log/info "State = sleep, Auto-pressing .")
                                              \.)
                                            (contains? #{:loading :connecting} (rw/current-state state))
                                              :advance
                                            :else
                                              keyin)]
                                (when (= keyin :exit)
                                  (System/exit 0))
                                (if keyin
                                  (do
                                    (log/info "Core current-state" (rw/current-state state))
                                    (log/info "Core got key" keyin)
                                    (let [new-state (main/tick state keyin)]
                                      (log/info "End of game loop")
                                      new-state))
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
                  state))))))))))
#?(:cljs
   (-main))

