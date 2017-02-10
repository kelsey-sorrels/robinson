;; Functions for animating state to screen
(ns robinson.animation
  (:require 
            [taoensso.timbre :as log]
            [robinson.random :as rr]
            [robinson.math :as rmath]
            [robinson.common :as rc :refer [farther-than?
                                            wrap-line
                                            fill-missing
                                            xy->pos]]
            [robinson.override :as ro :refer [override]]
            [robinson.color :as rcolor]
            [robinson.viewport :as rv :refer [cells-in-viewport]]
            [robinson.world :as rw]
            [robinson.lineofsight :as rlos]
            [zaffre.terminal :as zat]
            [overtone.at-at :as atat]))


#?(:clj
(set! *warn-on-reflection* true))

(def frame-count (atom 0))
(def ^:dynamic *rain-rate* 0.96)

;; TODO: Turn render output into effect changes?
;;
