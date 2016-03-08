;; Utility functions for manipulating events
(ns robinson.events
  (:require 
            [taoensso.timbre :as log]
            [robinson.common :as rc]
            [robinson.math :as rmath]
            [clojure.core.async :as async :refer [go go-loop]]))

(defn assoc-event-time [state t]
  (assoc state :event-time t))

(defn conj-event [state dt f]
  (let [event-time (get state :event-time)]
    (update state :events (fn [events] (assoc events f (+ event-time dt))))))

(defn- empty-events? [state]
  (-> state
    :events
    empty?))

(defn- next-event [state]
  "Return a 2-vec of [next event, state with first event removed]."
  (when-not (empty-events? state)
    [(-> state
      :events
      peek)
     (update state :events (fn [events] (pop events)))]))

(defn stream [state]
  "Return a async chan that streams out states with appropriate delays between each successive state.
   When no more states are availalble the channel is closed and will only return nil."
  (let [state-chan (async/chan)]
    (async/put! state-chan state)
    (go-loop [state state
              elapsed 0]
        (if-let [[[f dt] state] (next-event state)]
          (do
            (async/<! (async/timeout dt))
            (let [new-state (-> state
                              (f)
                              (assoc :event-time elapsed))]
              (async/>! state-chan new-state)
              (recur new-state (+ elapsed dt))))
          (async/close! state-chan)))
    state-chan))
    
