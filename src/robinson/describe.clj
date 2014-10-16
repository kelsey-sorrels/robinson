;; Functions that help construct description messages.
(ns robinson.describe
  (:use
    [clojure.string :only [lower-case]]
    clojure.contrib.core
    robinson.common
    robinson.npc
    robinson.player)
  (:require
    [robinson.itemgen  :as ig]
    [robinson.monstergen :as mg]
    [taoensso.timbre :as timbre]))

(timbre/refer-timbre)

(defn describe-cell
  [cell]
  "")

(defn describe-npc
  [npc]
  "")

(defn describe-items
  [items]
  "")

(defn search
  "Search the Moore neighborhood for hidden things and make them visible."
  [state]
  state)

(defn extended-search
  "Search the Moore neighborhood for hidden things and make them visible. Describe interesting items in the log."
  [state]
  (let [directions       [["To the north-west" -1 -1] ["To the north" 0 -1] ["To the north-east" 1 -1]
                          ["To the west" -1 0]        ["At your feet" 0 0]  ["To the east" 1 0]
                          ["To the south-west" -1 1]  ["To the south" 0 1]  ["To the south-east" 1 1]]
        describe-cell-fn (fn [state direction cell]
                           (let [items     (get cell :items [])
                                 cell-type (get cell :type)]
                             (info "describing" direction items cell-type)
                             (cond
                               (and (empty? items)
                                    (= :tall-grass cell-type))
                                 (append-log state (format "%s is tall grass." direction))
                               (and (empty? items)
                                    (= :tree cell-type))
                                 (append-log state (format "%s is a tree." direction))
                               (empty? items)
                                 state
                               (and (= (count items) 1)
                                    (= :tall-grass cell-type))
                                 (append-log state (format "%s is a %s and tall grass." direction (get (first items) :name)))                       (and (= (count items) 1)
                                    (= :tree cell-type))
                                 (append-log state (format "%s is a %s and a tree." direction (get (first items) :name)))
                               (and (> (count items) 1)
                                    (= :tall-grass cell-type))
                                 (append-log state (format "%s there are %s, and tall grass."
                                                           direction (clojure.string/join ", "
                                                                                          (map (fn [[item-name item-count]]
                                                                                                 (format "%s x%d" item-name item-count))
                                                                                               (frequencies (map :name items))))))
                               (and (> (count items) 1)
                                    (= :tree cell-type))
                                 (append-log state (format "%s there are %s, and a tree."
                                                           direction (clojure.string/join ", "
                                                                                          (map (fn [[item-name item-count]]
                                                                                                 (format "%s x%d" item-name item-count))
                                                                                               (frequencies (map :name items))))))
                               (= (count items) 1)
                                 (append-log state (format "%s there is a %s."
                                                           direction (clojure.string/join ", "
                                                                                          (map (fn [[item-name item-count]]
                                                                                                 (format "%s x%d" item-name item-count))
                                                                                               (frequencies (map :name items))))))
                               (> (count items) 1)
                                 (append-log state (format "%s there are %s."
                                                           direction (clojure.string/join ", "
                                                                                          (map (fn [[item-name item-count]]
                                                                                                 (format "%s x%d" item-name item-count))
                                                                                               (frequencies (map :name items))))))
                                       :else
                                 state)))]
    (-> state
      (search)
      ((fn [state]
        (reduce
          (fn [state [direction dx dy]]
            (let [{x :x y :y} (get-in state [:world :player :pos])]
              (describe-cell-fn state direction (get-cell-at-current-place state (+ x dx) (+ y dy)))))
          state
          directions))))))

(defn describe-cell-at-xy
  "Add to the log, a message describing the scene at the cell indicated by the
   cursor's position."
  [state x y]
  (let [cell       (or (get-in (current-place state) [y x])
                       {:type :nil})
        npc        (first (filter (fn [npc] (and (= x
                                                    (-> npc :pos :x))
                                                 (= y
                                                    (-> npc :pos :y))))
                                  (npcs-at-current-place state)))
        message   (case (cell :type)
                    :floor "There is a floor here. You could put things on it if you wanted."
                    :vertical-wall "There is a wall here."
                    :horizontal-wall "There is a wall here."
                    :close-door "There is a closed door here."
                    :open-door "There is an open-door here."
                    :corridor "There is a passageway here."
                    "There is nothing here. Nothing I can tell you anyway.")]
    message))

