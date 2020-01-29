(ns robinson.autoreloadcore
  (:use ns-tracker.core
        clojure.stacktrace)
  (:require [robinson.main :as main]
            [robinson.ui.mouse :as mouse]
            [robinson.world :as rw]
            [zaffre.terminal :as zt]
            [zaffre.glterminal :as zgl]
            [zaffre.components :as zc]
            [zaffre.components.events :as zce]
            [zaffre.components.render :as zcr]
            [zaffre.tilesets :as ztiles]
            [zaffre.events :as zevents]
            [robinson.events :as revents]
            [robinson.world :as rw]
            [robinson.render :as rr]
            [robinson.ui.updater :as ruu]
            [robinson.ui.components.robinson :as ruic]
            ;; Force loading of fx namespaces to eval defmethods
            [robinson.fx.airborn-item :as rfxai]
            [robinson.fx.blip :as rfxblip]
            [robinson.fx.whip-item :as rfxwhip]
            [robinson.fx.boomerang-item :as rfxboomerang]
            [clojure.core.async :as async :refer [go go-loop]]
            #_[clojure.tools.nrepl.server :as nreplserver]
            #_[clj-async-profiler.core :as prof]
            [dk.salza.liq.core :as liq-core]
            [dk.salza.liq.editor :as le]
            [dk.salza.liq.apps.findfileapp :as liq-findfileapp]
            [overtone.at-at :as atat]
            [taoensso.timbre :as log])
    (:gen-class))

;(defonce server (nreplserver/start-server :port 7888))
(def default-setup-fn (constantly {}))
(defn default-tick-fn  [state] (do (println "default tick fn") (Thread/sleep 5000) state))
(defn default-render-fn [state last-dom] (println "default render fn"))
(defn default-click-fn [col row last-dom] (println "default click fn"))
(defn default-move-fn [col row last-col last-row last-dom] (println "default move fn"))

(def setup-fn (atom default-setup-fn))
(def tick-fn (atom default-tick-fn))
(def render-fn (atom default-render-fn))
(def click-fn (atom default-click-fn))
(def move-fn (atom default-move-fn))

(def track (ns-tracker ["src/robinson"]))
(defn check-namespace-changes []
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
        (reset! click-fn (resolve 'robinson.ui.mouse/handle-click))
        (reset! move-fn (resolve 'robinson.ui.mouse/handle-mouse-move))
        (log/info "Done.")))
    (catch Throwable e (log/error e))))

(defn start-nstracker []
  (go-loop []
    (check-namespace-changes)
    (async/<! (async/timeout 500))
    (recur)))

;; Conveinience ref for accessing the last state when in repl.
(defonce state-ref (atom nil))
(defonce dom-ref (atom nil))
(defonce mouse-pos (atom [0 0]))

(defonce done-chan (async/chan))

(defn init-editor
  []
  (future
    (liq-core/-main "--minimal --no-init-file" "--no-threads" "--ghost" "--rows=20" "--columns=60")
    (le/add-rootfolder "src/robinson_tools/snippets")
    (le/add-searchpath "src/robinson_tools/snippets")
    (le/set-default-app liq-findfileapp/run)
    (le/remove-buffer "scratch")
    (le/new-buffer "-findfile-")
    (le/set-global-key "f5"
      (fn []
        (try
          (let [expr (le/get-content)
                prelude "(def ^:dynamic *state* nil)
                         (fn [state]
                           (binding [*state* state]"
                source (str prelude expr "))")
                _ (log/info source)
                f (load-string source)]
            (when f
              (swap! state-ref f)))
          (catch Throwable t
            (log/error t)))))
    (le/set-global-key "f4"
      (fn []
         (log/info "Exiting editor")
         (swap! state-ref rw/assoc-current-state :normal)))
    (le/updated)
    (doseq [i [" " "f" "f" "s" "r" "c" "down" "\n"]]
      (Thread/sleep 20)
      (le/handle-input i))))

(defn -main
  "Entry default point to application.

   Uses `setup` and `tick` function from `robinson.main`.

   `setup` returns the initial state of the application.

   `tick` takes the current state of the application and returns
   the next state after one iteration."
  []
  ; start with initial state from setup-fn
  (let [frames (atom 0)
        show-frames (atom true)
        mods (atom {:shift false
                    :alt false
                    :control false})]
    (reset! render-fn (resolve 'robinson.render/render))
    (reset! setup-fn (resolve 'robinson.main/setup))
    (reset! tick-fn (resolve 'robinson.main/tick))
    (reset! click-fn (resolve 'robinson.ui.mouse/handle-click))
    (reset! move-fn (resolve 'robinson.ui.mouse/handle-mouse-move))
    (start-nstracker)
    (init-editor)
    (@setup-fn
      (fn [state]
        (zgl/create-terminal
          (get state :terminal-groups)
          (get state :terminal-opts)
          (fn [terminal]
            (log/info "Create-terminal current-state" (rw/current-state state) (keys state) #_(get state :world))
            (let [groups (zt/groups terminal)
                  {:keys [columns
                          rows
                          character-width
                          character-height]} (get groups :app)
                          fps-fn (atat/every 1000
								   #(do
                                     (when  @show-frames
									   (log/info "frames " @frames))
									 (reset! frames 0))
								   zc/*pool*)]
              (log/info "Terminal groups" groups)
              (zt/set-window-size! terminal
                                   {:width (* columns character-width)
                                    :height (* rows character-height)}))
            (add-watch state-ref :advance (fn [_ state-ref old-state new-state]
                                            (when (= :quit (rw/current-state new-state))
                                                (log/info "Got quit state. Exiting.")
                                                (async/>!! done-chan true)
                                                (System/exit 0))
                                            (when (contains? #{:loading :connecting :sleep} (rw/current-state new-state))
                                              (reset! state-ref (@tick-fn new-state :advance @mods)))))
                                               
            (reset! state-ref state)
            (zt/do-frame terminal 0
              (binding [zc/*updater* ruu/updater]
                (when (zt/destroyed? terminal)
                  (log/info "terminal destroyed")
                  (System/exit 0))
                (let [state (or @state-ref state)
                      _ (assert (not (nil? state)))
                      ui (@render-fn terminal state @dom-ref)]
                  (reset! dom-ref ui)
                  (swap! frames inc)
                  (assert (zc/element? ui)))))
            (zevents/add-event-listener terminal :keypress (fn [keyin]
              ; tick the old state through the tick-fn to get the new state
              (try
                (when (= keyin :exit)
                  (System/exit 0))
                (when keyin
                  (log/info "Core current-state" (rw/current-state state))
                  (log/info "Core got key" keyin)
                  (let [new-state (@tick-fn (assoc @state-ref :screen terminal) keyin @mods)
                        state-stream (revents/stream new-state)]
                    (reset! show-frames true)
                    (doseq [state (revents/chan-seq state-stream)]
                      (log/info "got new state from stream")
                      (reset! state-ref state))
                    (log/info "got new state from stream")
                    (log/info "End of game loop")))
                 (catch Throwable ex
                   (log/error ex)
                   (print-stack-trace ex)
                   (reset! show-frames false)
                   state))))
			  (zevents/add-event-listener terminal :keydown
				(fn [new-key]
				  (println "keydown" new-key)
				  (when (contains? #{:lshift :rshift} new-key)
					(swap! mods merge {:shift true}))
				  (when (contains? #{:lcontrol :rcontrol} new-key)
					(swap! mods merge {:control true}))
				  (when (contains? #{:lalt :ralt} new-key)
					(swap! mods merge {:alt true}))))
			  (zevents/add-event-listener terminal :keyup
				(fn [new-key]
				(println "keyup" new-key)
				  (when (contains? #{:lshift :rshift} new-key)
					(swap! mods merge {:shift false}))
				  (when (contains? #{:lcontrol :rcontrol} new-key)
					(swap! mods merge {:control false}))
				  (when (contains? #{:lalt :ralt} new-key)
					(swap! mods merge {:alt false}))))
 
              (zevents/add-event-listener terminal :close (fn [keyin] (log/info "received :close") (System/exit 0)))
              (zevents/add-event-listener terminal :font-change (fn [{:keys [character-width
                                                                             character-height]}]
                                                                  (let [{:keys [columns rows]} (get (zt/groups terminal) :app)]
                                                                  (log/info "received :font-change")
                                                                  (log/info "terminal shape" columns rows)
                                                                  (log/info "font dims" character-width character-height)
                                                                  (zt/set-window-size! terminal
                                                                                       {:width (* columns character-width)
                                                                                        :height (* rows character-height)}))))
            (zevents/add-event-listener terminal :drag-and-drop
              (fn [{:keys [names]}]
                (log/info "received :drag-and-drop")
                (let [new-state (@tick-fn (assoc @state-ref :screen terminal) {:drag-and-drop names} @mods)]
                    (reset! state-ref new-state))))
            (zevents/add-event-listener terminal :click
              (fn [{:keys [button col row group-id]}]
                (log/info "received :click" button col row group-id)
                (try
                  (binding [zc/*updater* ruu/updater]
                    
                    (let [new-state (swap! state-ref @click-fn col row @dom-ref)
                          state-stream (revents/stream new-state)]
                      (doseq [state (revents/chan-seq state-stream)]
                        (log/info "got new state from stream")
                        (reset! state-ref state))
                        (log/info "got new state from stream")
                      (log/info "End of game loop")))
                  (catch Throwable t
                    (log/error t)))))
            (zevents/add-event-listener terminal :mouse-enter
              (fn [{:keys [col row]}]
                (log/trace "received :mouse-enter" col row)
                (let [[last-col last-row] @mouse-pos]
                  (try
                      (swap! state-ref
                        (fn [& more]
                          (binding [zc/*updater* ruu/updater]
                            (apply @move-fn more)))
                        col row last-col last-row @dom-ref)
                    (reset! mouse-pos [col row])
                    (catch Throwable t
                      (log/error t))))))
          (async/<!! done-chan)))))))

