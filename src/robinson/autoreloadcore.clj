(ns robinson.autoreloadcore
  (:use ns-tracker.core
        clojure.stacktrace)
  (:require robinson.main
            [robinson.world :as rw]
            ;[robinson.animation :as ranimation]
            [robinson.aanimatedterminal :as raat]
            [zaffre.terminal :as zat]
            [zaffre.animation.wrapper :as zaw]
            [zaffre.events :as zevents]
            [zaffre.glterminal :as zgl]
            [clojure.core.async :as async :refer [go go-loop]]
            [clojure.tools.nrepl.server :as nreplserver]
            [taoensso.timbre :as log]))

(defonce server (nreplserver/start-server :port 7888))

(defn check-namespace-changes [track]
 (try
   (doseq [ns-sym (track)]
     (when (not (contains? #{"robinson.autoreloadcore"
                             "robinson.main"
                             "robinson-tools.worldgen"
                             "robinson-tools.devtools"} (str ns-sym)))
     (log/info "Reloading namespace:" ns-sym)
       (require ns-sym :reload)
       (log/info "Done.")))
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

(defonce done-chan (async/chan))

(defn -main []
  (let [default-setup-fn (constantly {})
        default-tick-fn  (fn [state] (do (println "default tick fn") (Thread/sleep 5000) state))
        get-setup-fn     (fn [] (if-let [f (resolve 'robinson.main/setup)] f default-setup-fn))
        get-tick-fn      (fn [] (if-let [f (resolve 'robinson.main/tick)] f default-tick-fn))]
    (start-nstracker)
    ; start with initial state from setup-fn
    (let [setup-fn     (get-setup-fn)
          setup-fn-var (var-get setup-fn)]
      (setup-fn
        (fn [state]
          ; on ticks, this loop will restart. If setup-fn changes,
          ; the state will be reset through setup-fn but the screen will cary over.
          (zaw/create-animated-terminal
            zgl/create-terminal
            (get state :terminal-groups)
            (assoc
              (get state :terminal-opts)
              :effect-gen-fns
                [#_ranimation/make-rand-fg-effect
                 #_ranimation/make-blink-effect
                 #_ranimation/make-blip-effect
                 #_ranimation/make-transform-effect
                 #_ranimation/make-rain-effect]
              :filters 
                [#_ranimation/make-lantern-filter
                 #_ranimation/make-vignette-filter
                 #_ranimation/make-night-tint-filter])
            (fn [animated-terminal]
              (log/info "created animated terminal")
              (let [; state is used in the input reducing function
                    state        (atom (assoc state :screen animated-terminal))
                    ; render state keeps track of the state to be rendered to the screen
                    last-rendered-state (atom @state)]
                ;(raat/start! animated-terminal 15)
                (zat/do-frame animated-terminal 33
                  (let [render-fn (resolve 'robinson.render/render)]
                  ;; TODO render
                    (assert render-fn "render-fn nil")
                    (assert @last-rendered-state "@last-rendered-state nil")
                    (render-fn @last-rendered-state)))
                (log/info "adding keypress event listener")
                (zevents/add-event-listener animated-terminal :keypress
                  (fn [keyin]
                    (log/info "got key" keyin)
                    (let [new-state (try
                                      ((resolve 'robinson.update/update-state) @state keyin)
                                      (catch Throwable e
                                        (log/error e)
                                        @state))]
                      (reset!
                        state
                        (loop [new-state new-state]
                          (cond
                            (nil? new-state)
                               (System/exit 0)
                            (= (rw/current-state new-state) :sleep)
                              (recur ((get-tick-fn) new-state \,))
                            (contains? #{:loading :connecting} (rw/current-state state))
                              (recur ((get-tick-fn) new-state :advance))
                            :else
                              new-state))))))))))))))
