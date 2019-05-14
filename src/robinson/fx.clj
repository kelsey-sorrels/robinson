;; Utility functions for rendering state
(ns robinson.fx
  (:require 
            [robinson.common :as rc]
            [robinson.renderutil :as rutil]
            [robinson.math :as rmath]
            [robinson.color :as rcolor]
            [robinson.actors :as ractors]
            [robinson.world :as rw]
            [robinson.npc :as rnpc]
            [taoensso.timbre :as log]))

(defn fx-id []
  (.toString (java.util.UUID/randomUUID)))

(defn fx-ks [id]
  [::fx id])

(defn conj-fx
  ([state fx]
   (conj-fx state fx (fx-id)))
  ([state fx id]
    (let [new-state (assoc-in state [::fx id] fx)]
      (log/info "conj-fx-transform" (keys new-state) (get new-state ::fx))
      new-state)))

(defn remove-fx [state fx-ks]
  (rc/dissoc-in state fx-ks))

(defn clear-fx
  [state]
  (assoc state ::fx {}))

(defn fx [state] (get state ::fx))

(defn character-fx
 ([ch pos]
   (character-fx ch pos [255 255 255 255] [0 0 0 0]))
 ([ch pos fg bg]
   {:type :character-fx
    :pos pos
    :ch ch
    :color fg
    :background-color bg}))

(defn multi-character-fx
 ([ch-pos-list]
   "ch-pos is a list of {:ch :pos} objects."
   (multi-character-fx ch-pos-list [255 255 255 255] [0 0 0 0]))
 ([ch-pos-list fg bg]
   {:type :multi-character-fx
    :ch-pos ch-pos-list
    :color fg
    :background-color bg}))
 
(defmulti conj-effect (fn [state effect-type & more] effect-type))
(defmethod conj-effect :default [state effect-type & more]
  (log/warn (str "Unhandled effect-type in conj-effect " effect-type "vaLid effect-type: "  (methods conj-effect))))

