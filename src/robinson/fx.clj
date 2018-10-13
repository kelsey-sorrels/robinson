;; Utility functions for rendering state
(ns robinson.fx
  (:require 
            [robinson.common :as rc]
            [robinson.renderutil :as rutil]
            [robinson.math :as rmath]
            [robinson.color :as rcolor]
            [robinson.actors :as ractors]
            [robinson.world :as rw]
            [taoensso.timbre :as log]
            [zaffre.animation.wrapper :as zaw]))

(defn fx-id []
  (.toString (java.util.UUID/randomUUID)))

(defn fx-ks [id]
  [::fx id])

(defn conj-fx
  ([state fx]
   (conj-fx state fx (fx-id)))
  ([state fx id]
    (let [new-state (rc/conj-in state [::fx id] fx)]
      (log/info "conj-fx-transform" (keys new-state) (get new-state ::fx))
      new-state)))

(defn remove-fx [state fx-ks]
  (rc/dissoc-in state fx-ks))

(defn clear-fx
  [state]
  (assoc state ::fx {}))

(defn fx [state] (get state ::fx))

(defn character-fx [ch pos]
  {:type :character-fx
   :pos pos
   :ch ch})


(defrecord AirbornItemActor [velocity ttl fx-ks]
  ractors/Actor
  (receive [this state]
    (let [next-state
            (-> state
              ; move item
              (update-in state fx-ks (partial rc/add-pos velocity))
              ; update ttl
              (ractors/update-actor this :ttl dec))]
      (if (zero? ttl)
        (-> state
          (ractors/remove-actor this)
          (rc/dissoc-in fx-ks))
        next-state))))

(defn conj-fx-airborn-item [state item pos velocity ttl]
  (let [fx-id (fx-id)
        actor (->AirbornItemActor velocity ttl (fx-ks fx-id))]
    (-> state
        ; create a character fx
        (conj-fx (character-fx \- pos) fx-id)
        ; create a corresponding actor that controls the fx
        (ractors/add-actor actor))))


; FIXME: Implement
(defn conj-fx-blink [state] state)
 
