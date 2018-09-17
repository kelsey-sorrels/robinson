;; Functions for rendering state to screen
(ns robinson.render
  (:require 
            [taoensso.timbre :as log]
            [zaffre.terminal :as zt]
            [zaffre.components :as zc]
            [zaffre.components.events :as zce]
            [zaffre.components.render :as zcr]
            [robinson.ui.updater :as ruu]
            [robinson.ui.components.robinson :as ruic]
            [clojure.stacktrace :as st]))

(set! *warn-on-reflection* true)

(defn render [terminal state last-dom]
  (binding [zc/*updater* ruu/updater]
    (try
      (let [ui (zc/csx [ruic/Robinson {:game-state state}])
            dom (zcr/render-into-container
                  terminal
                  last-dom
                  ui)]
        #_(zt/refresh! terminal)
        (assert (zc/element? ui))
        ;; update component instance states
        (zc/update-state! zc/*updater*)
        dom)
       (catch Throwable t
         (do
           (log/error "Caught exception" t)
           (.printStackTrace t)
           (st/print-stack-trace t)
           (st/print-cause-trace t)
           last-dom)))))



