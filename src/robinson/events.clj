;; Utility functions for manipulating events
(ns robinson.events
  (:require 
            [robinson.actors :as ractors]
            [robinson.update :as rupdate]
            [taoensso.timbre :as log]
            [clojure.core.async :as async :refer [go go-loop]]))

(defn stream [state]
  "Return a async chan that streams out states with appropriate delays between each successive state.
   When no more states are availalble the channel is closed and will only return nil."
  (let [state-chan (async/chan)]
    ; make state immediately available
    (async/put! state-chan state)
    (go-loop [state state]
        (if (ractors/empty-actors? state)
          ; no more actors, stop streaming
          (async/close! state-chan)
          ; actors to process, tick
          (do
            ; process actors at 60fps
            (async/<! (async/timeout (/ 1000 60)))
            (let [new-state (ractors/tick-actors state)]
              (async/>! state-chan new-state)
              (recur new-state)))))
    state-chan))
 
(defn chan-seq [ch]
  (when-some [v (async/<!! ch)]
    (cons v (lazy-seq (chan-seq ch)))))

