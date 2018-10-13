;; Utility functions for creating, adding, and removing actors
(ns robinson.actors
  (:require 
            [robinson.common :as rc]
            [taoensso.timbre :as log]
            [clojure.core.async :as async :refer [go go-loop]]))

(defn update-actor [state actor k f]
   (update-in state [:actors (get actor ::id) k] f))

;; For adding new events to the end of the event stream
(defn add-actor
  [state actor]
   (log/info "conj actor")
   (let [actor-id (.toString (java.util.UUID/randomUUID))]
     (update state :actors (fn [actors] (assoc actors actor-id actor)))))

;; For adding new events at the start of the event stream without pushing back existing events
(defn remove-actor [state actor]
  (log/info "merge event")
  (update state :actors (fn [actors] (dissoc actors (get actor ::id)))))

(defn empty-actors? [state]
  (-> state
    :actors
    empty?))

(defprotocol Actor
  (receive [this state]))

(defn fn->actor [f]
  (reify Actor
    (receive [this state] (f this state))))

(defn tick-actors [state]
  "Return a new state after all actors have been processed"
  (reduce (fn [state [actor-id actor]]
            (receive actor state))
          state
          (get state :actors)))

