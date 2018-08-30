;; Utility functions for manipulating events
(ns robinson.events
  (:require 
            [taoensso.timbre :as log]
            [robinson.fx :as rfx]
            [clojure.core.async :as async :refer [go go-loop]]))

;; For resetting event time when update-state is called
(defn assoc-event-time [state t]
  (assoc state :event-time t))

;; For adding new events to the end of the event stream
(defn conj-event [state dt f]
  (log/info "conj event")
  (let [event-time (get state :event-time)]
    (update state :events (fn [events] (assoc events f (+ event-time dt))))))

;; For adding new events at the start of the event stream without pushing back existing events
(defn merge-event [state dt f]
  (log/info "merge event")
  ; TODO: implement
  (let [event-time (get state :event-time)]
    (update state :events (fn [events] (assoc events f (+ event-time dt))))))

(defn transform->event-fn [transform]
  ; TODO: implement
  identity)

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
                              rfx/clear-fx
                              (f)
                              (assoc :event-time elapsed))]
              (async/>! state-chan new-state)
              (recur new-state (+ elapsed dt))))
          (async/close! state-chan)))
    state-chan))
    
