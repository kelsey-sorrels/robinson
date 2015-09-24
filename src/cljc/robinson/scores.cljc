;; Utility functions for reading and persisting scores
(ns robinson.common
  (:require 
            [taoensso.timbre :as log]
            [alandipert.enduro :as enduro]))

(defn get-scores
  [state]
  (-> state :scores deref))

(defn reset-scores!
  [state new-scores]
  (let [scores (get state :scores)]
    (enduro/reset! scores new-scores)))

(defn add-score!
  [state score]
  (reset-scores! state (conj (get-scores state) score)))

(defn state->score
  [state]
  {})

(defn persist-state-score!
  [state]
  (add-score! state (state->score state)))


