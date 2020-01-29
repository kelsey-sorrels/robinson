;; Utility functions for manipulating events
(ns robinson.events
  (:require 
            [robinson.actors :as ractors]
            [robinson.error :as re]
            [robinson.update :as rupdate]
            [taoensso.timbre :as log]
            [clojure.core.async :as async :refer [go go-loop]]))

(defn stream [initial-state]
  "Return a async chan that streams out states with appropriate delays between each successive state.
   When no more states are availalble the channel is closed and will only return nil."
  (let [state-chan (async/chan)]
    ; make state immediately available
    (async/put! state-chan initial-state)
    (go-loop [state initial-state]
        (log/info "Actors present:" (not (ractors/empty-actors? state)))
        (if (ractors/empty-actors? state)
          ; no more actors 
          ; tick mobs/bookkeeping
          (if (get state :advance-time)
            (try
              (log/info "advancing time")
              ; advance time, and stream the results into state-chan
              (async/pipe (stream (rupdate/update-advance-time state)) state-chan)
              (catch Exception e
                (re/log-exception state e)))
            (do
              ; stop streaming
              (log/info "closing  state-chan")
              (async/close! state-chan)))
          ; actors to process, tick
          (do
            ; process actors at 60fps
            (async/<! (async/timeout (/ 1000 20)))
            (log/info "ticking actors")
            (let [new-state (ractors/tick-actors state)]
              (async/>! state-chan new-state)
              (recur new-state)))))
    state-chan))
 
(defn chan-seq [ch]
  (when-some [v (async/<!! ch)]
    (cons v (lazy-seq (chan-seq ch)))))

