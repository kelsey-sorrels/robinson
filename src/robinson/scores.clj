;; Utility functions for reading and persisting scores
(ns robinson.scores
  (:require 
            [robinson.world :as rw]
            [robinson.player :as rp]
            [robinson.endgame :as rendgame]
            [taoensso.timbre :as log]
            [alandipert.enduro :as enduro]))

(defn state->points
  [state]
  (let [cur-state (rw/current-state state)]
    (int
      (* (+ (get-in state [:world :player :will-to-live])
            (rp/player-xp state)
            (rw/get-time state)
            (reduce-kv #(+ %1 %3) 0 (get-in state [:world :player :stats :num-items-harvested]))
            (reduce-kv #(+ %1 %3) 0 (get-in state [:world :player :stats :num-items-crafted])))
         (case cur-state
           :dead 1
           :rescued 2
           :game-over-dead 1
           :game-over-rescued 2)))))

(defn get-scores
  [state]
  (or (-> state :scores deref) []))

(defn reset-scores!
  [state new-scores]
  (let [scores (get state :scores)]
    (enduro/reset! scores new-scores)))

(defn add-score!
  [state score]
  (reset-scores! state (conj (get-scores state) score)))

(defn state->score
  [state]
  {:date        (java.util.Date.)
   :time        (rw/get-time state)
   :score       (state->points state)
   :end-message (rendgame/gen-end-madlib state)
   :player-name (rp/get-player-attribute state :name)})

(defn persist-state-score!
  [state]
  (add-score! state (state->score state)))


