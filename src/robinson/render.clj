;; Functions for rendering state to screen
(ns robinson.render
  (:require 
            [taoensso.timbre :as log]
            [zaffre.terminal :as zt]
            [zaffre.components :as zc]
            [zaffre.components.events :as zce]
            [zaffre.components.render :as zcr]
            [robinson.ui.updater :as ruu]
            [robinson.ui.components.robinson :as ruic]))

(set! *warn-on-reflection* true)

(defn render [state last-dom]
  (binding [zc/*updater* ruu/updater]
    (let [ui (zc/csx [ruic/Robinson {:state state}])
          screen (get state :screen)
          dom (zcr/render-into-container
                screen
                last-dom
                ui)]
      (zt/refresh! screen)
      (assert (zc/element? ui))
      ;; update component instance states
      (zc/update-state! zc/*updater*)
      dom)))

