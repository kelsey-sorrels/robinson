;; Functions for rendering state to screen
(ns robinson.render
  (:require 
            [robinson.random :as rr]
            [robinson.error :as re]
            [robinson.ui.updater :as ruu]
            [robinson.ui.components.robinson :as ruic]
            [taoensso.timbre :as log]
            [zaffre.terminal :as zt]
            [zaffre.components :as zc]
            [zaffre.components.events :as zce]
            [zaffre.components.render :as zcr]
            [clojure.stacktrace :as st]))

(defn render [terminal state last-dom]
  (binding [zc/*updater* ruu/updater
            zcr/*on-error* (fn [e] (re/log-exception e state))]
    (try
      (let [ui (zc/csx [ruic/Robinson {:game-state state}])
            dom (zcr/render-into-container
                  terminal
                  last-dom
                  ui)]
        (assert (zc/element? ui))
        ;; update component instance states
        (zc/update-state! zc/*updater*)
        dom)
       (catch Throwable t
         (re/log-exception t state)
         last-dom))))



