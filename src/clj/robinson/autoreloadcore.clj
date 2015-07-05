(ns robinson.autoreloadcore
  (:use ns-tracker.core
        clojure.stacktrace)
  (:require robinson.main
            [robinson.world :as rw]
            [robinson.aterminal :as aterminal]
            [clojure.core.async :as async :refer [go go-loop]]
            [taoensso.timbre :as log]))

(defn check-namespace-changes [track]
 (try
   (doseq [ns-sym (track)]
     (println "Reloading namespace:" ns-sym)
     (require ns-sym :reload)
     (println "Done."))
   (catch Throwable e (log/error e)))
   (Thread/sleep 500))

(defn start-nstracker []
 (let [track (ns-tracker ["src/clj" "src/cljc"])]
   (doto
     (Thread.
       #(while true
         (check-namespace-changes track)))
     (.setDaemon true)
     (.start))))

(def done-chan (async/chan))

(defn -main []
  (let [default-setup-fn (constantly {})
        default-tick-fn  (fn [state] (do (println "default tick fn") (Thread/sleep 5000) state))
        get-setup-fn     (fn [] (if-let [f (resolve 'robinson.main/setup)] f default-setup-fn))
        get-tick-fn      (fn [] (if-let [f (resolve 'robinson.main/tick)] f default-tick-fn))]
    (start-nstracker)
    ; on ticks, this loop will restart. If setup-fn changes,
    ; the state will be reset through setup-fn but the screen will cary over.
    (go-loop [setup-fn     (get-setup-fn)
              setup-fn-var (var-get setup-fn)
              state        (setup-fn nil)]
      ; start with initial state from setup-fn
      ; setup function changed? restart with new setup
      (if (identical? (var-get (get-setup-fn)) setup-fn-var)
        ; tick the old state through the tick-fn to get the new state
        (let [new-state (try
                          (let [keyin (or (when (= (rw/current-state state) :sleep)
                                          \.)
                                        (let [key-chan (aterminal/get-key-chan (state :screen))]
                                          (log/info  "waiting for key-chan")
                                          (async/<! key-chan)))]
                             (if keyin
                               ((get-tick-fn) state keyin)
                               state))
                          (catch Throwable e
                            (log/error e)
                            state))]
          (if (nil? new-state)
            (System/exit 0)
            (recur setup-fn setup-fn-var new-state)))
        ; setup function changed, restart with new setup
        (let [setup-fn  (get-setup-fn)
              setup-var (var-get setup-fn)
              state     (setup-fn (get state :screen))]
          (println "(Re)starting loop with new setup-fn")
          (recur setup-fn setup-var state))))
    (async/<!! done-chan)
    (println "Core exiting")))

