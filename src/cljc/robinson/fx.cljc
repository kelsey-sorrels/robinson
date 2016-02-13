;; Utility functions for rendering state
(ns robinson.fx
  (:require 
            [taoensso.timbre :as log]
            [robinson.common :as rc]
            [robinson.renderutil :as rutil]
            [robinson.math :as rmath]
            [robinson.color :as rcolor]))


(defn conj-fx-transform
  [state from to item]
  {:pre [(vector? from)
         (vector? to)
         (= (count from) (count to) 2)]}
  (let [new-state (rc/conj-in state [:fx :transform] {:from-xy from
                                     :to-xy   to
                                     :ch      (rutil/item->char item)
                                     :fg      (rutil/item->fg item)})]
    (log/info "conj-fx-transform" (keys new-state) (get new-state :fx))
    new-state))

(defn clear-fx-transform
  [state]
  (assoc-in state [:fx :transform] []))


(defn conj-fx-blink [state xy]
  (rc/conj-in state [:fx :blink] {:xy xy}))

(defn clear-fx-blink
  [state]
  (assoc-in state [:fx :blink] []))

(defn conj-fx-blip [state xy instants]
  (rc/conj-in state [:fx :blip] {:xy xy :instants instants}))

(defn clear-fx-blip
  [state]
  (assoc-in state [:fx :blip] []))

(defn clear-fx
  [state]
  (assoc state :fx {}))

