(ns dungeon-crusade.core
  (:use ns-tracker.core)
  (:require dungeon-crusade.main))

(defn check-namespace-changes [track]
 (try
   (doseq [ns-sym (track)]
     (println "Reloading namespace:" ns-sym)
     (require ns-sym :reload))
   (catch Throwable e (.printStackTrace e)))
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
  (do 
    ;; use all symbols from all namespaces except some.
    (doseq [ns (map symbol (filter #(not (or (.startsWith % "clojure")
                                             (.startsWith % "complete")
                                             (.startsWith % "reply")
                                             (.startsWith % "lanterna")
                                             (.startsWith % "user")))
                                   (map str (all-ns))))]
      (use ns))
    (let [default-setup-fn (constantly {})
          default-tick-fn  (fn [state] (do (println "default tick fn") (Thread/sleep 5000) state))
          get-setup-fn     (fn [] (if-let [f (resolve 'setup)] f default-setup-fn))
          get-tick-fn      (fn [] (if-let [f (resolve 'tick)] f default-tick-fn))]
      (start-nstracker)
      ; up setup fn changes, this loop will restart
      (loop [setup-fn (get-setup-fn)]
        (let [setup-fn-var (var-get setup-fn)]
          (println "(Re)starting loop with new setup-fn")
          ; start with initial state from setup-fn
          (loop [state ((get-setup-fn))]
            ; setup function changed? stop ticking
            (when (= (:souce (meta (var-get (get-setup-fn)))) (:source (meta setup-fn-var)))
              ; tick the old state through the tick-fn to get the new state
              (recur (try ((get-tick-fn) state) (catch Exception e state))))))
          ; setup function changed, start outer loop over again
        (recur (get-setup-fn))))))
