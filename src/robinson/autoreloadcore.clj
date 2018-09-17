(ns robinson.autoreloadcore
  (:use ns-tracker.core
        clojure.stacktrace)
  (:require [robinson.main :as main]
            [robinson.world :as rw]
            [zaffre.terminal :as zt]
            [zaffre.glterminal :as zgl]
            [zaffre.components :as zc]
            [zaffre.components.events :as zce]
            [zaffre.components.render :as zcr]
            [zaffre.tilesets :as ztiles]
            [zaffre.events :as zevents]
            [robinson.world :as rw]
            [robinson.render :as rr]
            [robinson.ui.updater :as ruu]
            [robinson.ui.components.robinson :as ruic]
            [clojure.core.async :as async :refer [go go-loop]]
            [clojure.tools.nrepl.server :as nreplserver]
            [taoensso.timbre :as log]))

(defonce server (nreplserver/start-server :port 7888))

(def default-setup-fn (constantly {}))
(defn default-tick-fn  [state] (do (println "default tick fn") (Thread/sleep 5000) state))
(defn default-render-fn [state last-dom] (println "default render fn"))

(def setup-fn (atom default-setup-fn))
(def tick-fn (atom default-tick-fn))
(def render-fn (atom default-render-fn))

(defn check-namespace-changes []
  (let [track (ns-tracker ["src/robinson"])]
    (while true
      (try
        (doseq [ns-sym (track)]
          (when (not (contains? #{"robinson.autoreloadcore"
                                  "robinson.main"
                                  "robinson-tools.worldgen"
                                  "robinson-tools.devtools"} (str ns-sym)))
            (log/info "Reloading namespace:" ns-sym)
            (require ns-sym :reload)
            (reset! render-fn (resolve 'robinson.render/render))
            (reset! setup-fn (resolve 'robinson.main/setup))
            (reset! tick-fn (resolve 'robinson.main/tick))
            (log/info "Done.")))
        (catch Throwable e (log/error e)))
      (Thread/sleep 500))))

(defn start-nstracker []
  (doto
    (Thread. check-namespace-changes)
    (.setDaemon true)
    (.start)))

;; Conveinience ref for accessing the last state when in repl.
(defonce state-ref (atom nil))
(defonce dom-ref (atom nil))

(defonce done-chan (async/chan))

(defn -main
  "Entry default point to application.

   Uses `setup` and `tick` function from `robinson.main`.

   `setup` returns the initial state of the application.

   `tick` takes the current state of the application and returns
   the next state after one iteration."
  []
  ; start with initial state from setup-fn
  (let []
    (reset! render-fn (resolve 'robinson.render/render))
    (reset! setup-fn (resolve 'robinson.main/setup))
    (reset! tick-fn (resolve 'robinson.main/tick))
    (start-nstracker)
    (@setup-fn
      (fn [state]
        (zgl/create-terminal
          (get state :terminal-groups)
          (get state :terminal-opts)
          (fn [terminal]
            (log/info "Create-terminal current-state" (rw/current-state state) (keys state) (get state :world))
            (add-watch state-ref :advance (fn [_ state-ref old-state new-state]
                                             (when (= (rw/current-state new-state) :loading)
                                               (reset! state-ref (@tick-fn new-state :advance)))))
                                               
            (reset! state-ref state)
            (zt/do-frame terminal 33
              (binding [zc/*updater* ruu/updater]
                (let [state (or @state-ref state)
                      _ (assert (not (nil? state)))
                      ui (@render-fn terminal state @dom-ref)]
                  (reset! dom-ref ui)
                  (assert (zc/element? ui))
                  ;; update component instance states
                  (log/info "---End Frame---"))))
              (zevents/add-event-listener terminal :keypress (fn [keyin]
                (if (nil? state)
                  (do
                    (log/info "Got nil state. Exiting.")
                    (async/>!! done-chan true)
                    (System/exit 0))
                  ; tick the old state through the tick-fn to get the new state
                  (let [state (try
                                (when (= keyin :exit)
                                  (System/exit 0))
                                (if keyin
                                  (do
                                    (log/info "Core current-state" (rw/current-state state))
                                    (log/info "Core got key" keyin)
                                    (let [new-state (@tick-fn @state-ref keyin)]
                                      (log/info "End of game loop")
                                      new-state))
                                  state)
                                 (catch Throwable ex
                                   (log/error ex)
                                   (print-stack-trace ex)
                                   state))]
                     (reset! state-ref state)))))
            (async/<!! done-chan)))))))
 
