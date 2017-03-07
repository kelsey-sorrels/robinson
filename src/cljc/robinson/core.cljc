;; ## A roguelike focused on immutability.##
;;
;(set! *warn-on-reflection* true)
(ns robinson.core
  (:require [robinson.setup :as setup]
            [zaffre.terminal :as zat]
            [zaffre.glterminal :as zgl]
            [zaffre.events :as zevents]
            [zaffre.tilesets :as ztiles]
            [zaffre.animation.wrapper :as zaw]
            [zaffre.util :as zutil]
            [robinson.world :as rw]
            [robinson.update :as rupdate]
            [robinson.render :as rrender]
            #?@(:clj (
                [clojure.stacktrace :as st]
                [clojure.core.async :as async :refer [go go-loop]]
                [clojure.stacktrace :refer [print-stack-trace]]
                [taoensso.timbre :as log]
                [clojure.java.io :as io]
                [taoensso.nippy :as nippy])
                :cljs (
                [cljs.core.async :as async]
                [taoensso.timbre :as log :include-macros true])))
  #?@(:clj (
     (:gen-class)
     (:import 
       [java.io DataInputStream DataOutputStream]))
     :cljs
     ((:require-macros [cljs.core.async.macros :refer [go go-loop]]))))

#?(
:clj
(log/merge-config!
  {:appenders {:spit (taoensso.timbre.appenders.core/spit-appender {:fname "/robinson.log"})}})
:cljs
(enable-console-print!))

(def font ztiles/pastiche-16x16)
 
;; Conveinience ref for accessing the last state when in repl.
(defonce state-ref (atom nil))

;(defonce done-chan (async/chan))
;#?@(:cljs (
;(cljs.reader/register-tag-parser! "robinson.monstergen.Monster" mg/map->Monster)
;(cljs.reader/register-tag-parser! "robinson.player.Player" mg/map->Player)
;(def world-storage (local-storage (atom nil) :world))))

(defonce key-chan (async/chan 1000))
;; Save thread
(defonce save-chan (async/chan (async/sliding-buffer 1)))

;; gamestates pushed onto render-chan will be rendered
(defonce render-chan (async/chan (async/sliding-buffer 1)))

(defn save-state [state]
  #?(:clj
     (async/>!! save-chan state)
     :cljs
     (go
       (async/>! save-chan state))))

(defn -main
  "Entry default point to application.

   Uses `setup` and `tick` function from `robinson.main`.

   `setup` returns the initial state of the application.
  
   `tick` takes the current state of the application and returns
   the next state after one iteration."
  []
  ; start with initial state from setup-fn
  (setup/setup
    (fn [state]
     (log/info "terminal-groups" (get state :terminal-groups))
     (log/info "terminal-opts" (get state :terminal-opts))
      (zgl/create-terminal ;zaw/create-animated-terminal
        ;zgl/create-terminal
        (get state :terminal-groups)
        (get state :terminal-opts)
        (fn [terminal]
          (reset! state-ref (dissoc state :terminal-groups :terminal-opts))
          (let [last-rendered-state (atom @state-ref) ;; last gamestate rendered
                last-renderstate    (atom
                                      (let [render-fn (resolve 'robinson.render/render)]
                                        (render-fn state)))]
            ;; render loop
            (zat/do-frame terminal 33
              ;; Draw strings
               (when-let [renderstate @last-renderstate]
                 (doseq [[layer-id characters] (get renderstate :layers)]
                   (zat/put-chars! terminal layer-id characters))))
            (go-loop [keyin (async/<! key-chan)]
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
                                    (let [new-state (rupdate/update-state state keyin)]
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
                  (log/info "sending state to render-chan")
                  (async/>!! render-chan state)
                  ;(save-state state)
                  state)))
              (recur (async/<! key-chan)))
            ;; render thread
            (go-loop [interrupted-state nil]
              (let [state (async/<! render-chan)]
                (log/info "Got state from render-chan" (test state))
                (when state
                  (reset! last-rendered-state state)
                  (log/info "Rendering world at time" (get-in state [:world :time]) (get-in state [:world :current-state]))
                  (try
                    ;(rm/log-time "render" (rrender/render state))
                    (let [now (System/currentTimeMillis)
                      render-fn (resolve 'robinson.render/render)]
                      (reset! last-renderstate (render-fn state))
                      (log/info "finished rendering in" (- (System/currentTimeMillis) now) "ms"))
                    #?(:clj
                       (catch Throwable e
                         (log/error "Error rendering" e)
                         (st/print-stack-trace e)
                         (st/print-cause-trace e))
                       :cljs
                       (catch js/Error e (log/error e)))))
                (recur nil)))
            ;; save thread
            (go []
              ;; wait for first element and then requeue it
              (async/>! save-chan (async/<! save-chan))
              (with-open [o (io/output-stream "save/world.edn")]
                (loop [state (async/<! save-chan)]
                  (log/info "World saved at time" (get-in state [:world :time]))
                  #?(:clj
                     (try
                       (nippy/freeze-to-out! (DataOutputStream. o) (get state :world))
                       (catch Throwable e (log/error "Error saving" e)))
                     :cljs
                     (reset! world-storage (get state :world)))
                  ;(as-> state state
                  ;  (get state :world)
                  ;  (pp/write state :stream nil)
                  ;  (spit "save/world.edn.out" state))
                  (recur (async/<! save-chan)))))
              (zevents/add-event-listener terminal :keypress
                (fn [keyin]
                  (log/info "Got key" keyin (type keyin))
                  (async/>!! key-chan keyin)))))))))

#?(:cljs
   (-main))

