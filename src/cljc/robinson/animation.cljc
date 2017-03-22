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
            [clojure.core.async :as async :refer [go go-loop]]))

#?(:clj
(set! *warn-on-reflection* true))

(def frame-count (atom 0))
(def ^:dynamic *rain-rate* 0.96)

(defn animate [rstate state]
  "Takes a renderstate and a gamestate and produces a (possibly lazy) sequence of [dt rstate]"
  [[0 rstate]])

(defn stream [events]
  "Return a async chan that streams out states with appropriate delays between each successive state.
     When no more states are availalble the channel is closed and will only return nil."
  (let [rstate-chan (async/chan)]
    (go-loop [[dt rstate] (first events)
              xs         (next events)]
        (async/<! (async/timeout dt))
        (async/>! rstate-chan rstate)
        (if xs
          (recur (first xs) (next xs))
          (async/close! rstate-chan)))
    rstate-chan))

