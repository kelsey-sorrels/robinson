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

(defn tick-actors [state]
  "Return a new state after all actors have been processed"
  (reduce (fn [state [actor-id actor]]
            ((get actor :fn) actor state))
          state
          (get state :actors)))

(defn create-airborn-item-actor [item-pos-ks velocity ttl]
  {:velocity velocity
   :ttl ttl
   :fn (fn [self state]
         (-> state
           ; move item
           (update-in state item-pos-ks (partial rc/add-pos velocity))
           ; update ttl
           (update-actor self :ttl dec)))})

