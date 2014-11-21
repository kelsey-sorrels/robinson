(ns robinson.autoreloadcore
  (:use ns-tracker.core
        clojure.stacktrace)
  (:require robinson.main
            [taoensso.timbre :as timbre]))

(timbre/refer-timbre)

(defn check-namespace-changes [track]
 (try
   (doseq [ns-sym (track)]
     (println "Reloading namespace:" ns-sym)
     (require ns-sym :reload)
     (println "Done."))
   (catch Throwable e (error e)))
   (Thread/sleep 500))

(defn start-nstracker []
 (let [track (ns-tracker ["src" "checkouts"])]
   (doto
     (Thread.
       #(while true
         (check-namespace-changes track)))
     (.setDaemon true)
     (.start))))

(defn -main []
  (let [default-setup-fn (constantly {})
        default-tick-fn  (fn [state] (do (println "default tick fn") (Thread/sleep 5000) state))
        get-setup-fn     (fn [] (if-let [f (resolve 'robinson.main/setup)] f default-setup-fn))
        get-tick-fn      (fn [] (if-let [f (resolve 'robinson.main/tick)] f default-tick-fn))]
    (start-nstracker)
    ; on ticks, this loop will restart. If setup-fn changes,
    ; the state will be reset through setup-fn but the screen will cary over.
    (loop [setup-fn     (get-setup-fn)
           setup-fn-var (var-get setup-fn)
           state        (setup-fn nil)]
      ; start with initial state from setup-fn
      ; setup function changed? restart with new setup
      (if (identical? (var-get (get-setup-fn)) setup-fn-var)
        ; tick the old state through the tick-fn to get the new state
        (let [new-state (try
                          ((get-tick-fn) state)
                          (catch Throwable e
                            (error e)
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
    (println "Core exiting")))

