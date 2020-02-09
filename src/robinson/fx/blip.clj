;; Utility functions for rendering state
(ns robinson.fx.blip
  (:require 
            [robinson.common :as rc]
            [robinson.math :as rmath]
            [robinson.color :as rcolor]
            [robinson.actors :as ractors]
            [robinson.actor-protocol :as rap]
            [robinson.fx :as rfx]
            [robinson.world :as rw]
            [taoensso.timbre :as log]))


(defrecord BlipActor [ttl fx-ks]
  rap/Actor
  (receive [this state]
    (let [ttl-zero (or (zero? ttl) (neg? ttl))]
      (letfn [(cleanup [state]
                (log/info "cleaning up BlipActor" ttl fx-ks)
                (-> state
                  (ractors/remove-actor this)
                  (rc/dissoc-in fx-ks)))]
        (if ttl-zero
          ; if done, cleaup
          (cleanup state)
          ; else, dec ttl
          (ractors/update-actor state this update :ttl dec))))))

(defmethod rfx/conj-effect :blip [state fx-type & [pos ch fg bg ttl]]
  (let [fx-id (rfx/fx-id)
        fg (or fg [255 255 255 255])
        bg (or bg [0 0 0 0])
        ttl (or ttl 1)
        actor (->BlipActor ttl (rfx/fx-ks fx-id))]
    (log/info "created BlipActor " ttl (rfx/fx-ks fx-id))
    (log/info (rfx/character-fx ch pos fg bg))
    (-> state
      ; create a character fx
      (rfx/conj-fx (rfx/character-fx ch pos fg bg) fx-id)
      ; create a corresponding actor that controls the fx
      (ractors/add-actor actor))))

