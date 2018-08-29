;; Functions that manipulate state when showing popovers
(ns robinson.popover
  (:require
            [robinson.common :as rc]
            [robinson.random :as rr]
            [robinson.world :as rw]
            robinson.macros
            [robinson.macros :as rm]
            [taoensso.timbre :as log]))

(defn show-popover
  ([state message]
    (show-popover state message :normal))
  ([state message next-state]
  (-> state
    (rw/assoc-current-state :popover)
    (assoc-in [:world :popover-message] message)
    (assoc-in [:world :next-state] next-state))))

(defn clear-popover
  [state]
  (rc/dissoc-in state [:world :popover-message]))

(defn get-popover-message
  [state]
  (get-in state [:world :popover-message]))

(defn get-popover-next-state
  [state]
  (get-in state [:world :next-state]))

