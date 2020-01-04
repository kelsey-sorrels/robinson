;; ## A roguelike focused on immutability.##
;;
;(set! *warn-on-reflection* true)
(ns robinson.core
  (:require [robinson.main :as main]
            [zaffre.terminal :as zt]
            [zaffre.glterminal :as zgl]
            [zaffre.components :as zc]
            [zaffre.components.events :as zce]
            [zaffre.components.render :as zcr]
            [zaffre.tilesets :as ztiles]
            [robinson.ui.updater :as ruu]
            [robinson.ui.components.robinson :as ruic]
            [zaffre.events :as zevents]
            [robinson.world :as rw]
            [clojure.core.async :as async :refer [go go-loop]]
            [clojure.stacktrace :refer [print-stack-trace]]
            [taoensso.timbre :as log])
  (:gen-class))

(def width 55)
(def height 40)
;; Conveinience ref for accessing the last state when in repl.
(defonce state-ref (atom nil))

(defonce dom-ref (atom nil))

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
      (zgl/create-terminal
        (get state :terminal-groups)
        (get state :terminal-opts)
        (fn [terminal]
          (log/info "Create-terminal current-state" (rw/current-state state) (keys state) (get state :world))
          (reset! state-ref state)
          (zt/do-frame terminal 33
            (binding [zc/*updater* ruu/updater]
              (let [state (or @state-ref state)
                    _ (assert (not (nil? state)))
                    ui (zc/csx [ruic/Robinson {:game-state state}])
                    dom (zcr/render-into-container
                          terminal
                          @dom-ref
                          ui)]
                #_(log/info (zc/tree->str dom))
                ;(zt/refresh! terminal)
                (reset! dom-ref dom)
                (assert (zc/element? ui))
                ;; update component instance states
                (zc/update-state! zc/*updater*)
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
                                  (let [new-state (main/tick @state-ref keyin)]
                                    (log/info "End of game loop")
                                    new-state))
                                state)
                               (catch Throwable ex
                                 (log/error ex)
                                 (print-stack-trace ex)
                                 state))]
                   (reset! state-ref state)))))
         (async/<!! done-chan))))))
