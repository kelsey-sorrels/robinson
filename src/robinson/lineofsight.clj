(ns robinson.lineofsight
  (:use [robinson.mapgen :exclude [-main]]
        robinson.common)
  (:require [clojure.contrib.math :as math]
            clojure.pprint))

;; The functions here can severely slow down the game if 
;; implemented poorly. Reflection can slow down evaluation,
;; so warn if it is occurring.
(set! *warn-on-reflection* true)

;; Memoized function that returns the points between
;; `[x1 y1]` and `[x2 y2]`
(def line-segment (memoize
  (fn
   [start end]
   (if (= start end)
      []
      (let [[x1 y1] start
            [x2 y2] end
            xdiff (- x2 x1)
            ydiff (- y2 y1)
            maxdiff (max (math/abs xdiff) (math/abs ydiff))
            dx (/ xdiff maxdiff)
            dy (/ ydiff maxdiff)]
        (map (fn [i] [(math/round (double (+ x1 (* i dx)))) (math/round (double (+ y1 (* i dy))))])
            (range (inc maxdiff))))))))

;; A fast version of `line-segment`. Internally, shift the values so that
;; `[x1 y1]` equals `[0 0]`, call `line-segment` and then shift everything back.
;; It's fast because `(line-segment-fast [0 0] [5 5])` is effectively the same
;; as `(line-segment [2 2] [7 7])` which plays nicely with memoization.
(def line-segment-fast (memoize
  (fn [start end]
    "(line-segment-fast [1 1] [5 4])"
    (let [[ox oy] start
          [dx dy] end]
      (map (fn [[x y]] [(+ ox x) (+ oy y)])
          (line-segment [0 0] [(- dx ox) (- dy oy)]))))))

;; A fast version of `line-segment`. Internally, shift the values so that
;; `[x1 y1]` equals `[0 0]`, call `line-segment` and then shift everything back.
;; It's fast because `(line-segment-fast [0 0] [5 5])` is effectively the same
;; as `(line-segment [2 2] [7 7])` which plays nicely with memoization.
(def line-segment-fast-without-endpoints (memoize
  (fn [start end]
    "(line-segment-fast [1 1] [5 4])"
    (let [[ox oy] start
          [dx dy] end]
      (rest
        (butlast
          (map (fn [[x y]] [(+ ox x) (+ oy y)])
               (line-segment [0 0] [(- dx ox) (- dy oy)]))))))))

(defn visible?
  [get-cell blocking? x1 y1 x2 y2]
  (not-any? blocking?
    (map (fn [[x y]] (get-cell x y))
    ;(map (fn [[x y]] ((fn [x y] (get-in grid [y x])) x y))
    ;(map (fn [[x y]] (get-in grid [y x]))
         (line-segment-fast-without-endpoints [x1 y1] [x2 y2]))))
 
(defn map-visibility
  ([origin blocking? grid]
   "Determine the visibility of a `grid` as a viewer at `origin`.

    `blocking?` is a predicate called like this `(blocking? cell)`
    for each cell in the grid.` It should return `true` if the cell
    block visibility. Eg: a wall, or closed door and `false` otherwise.

    (map-visibility [x y] blocking? (0,0) ------ x
                                      |  [[false false false]
                                      |   [false true  false]
                                      y   [false true  false]])
    (clojure.pprint/pprint (map-visibility [0 4] identity
                                   [[false false false false]
                                    [false false true  false]
                                    [false false true  false]
                                    [false false true  false]
                                    [false false true  false]]))
   "
   (map-visibility origin blocking? grid (fn [cell x y] (if (nil? cell) false))))
  ([origin blocking? grid exclude?]
   "Exclude? is a fn that takes a cell, its x pos, and y pos. If exclude? returns false,
    no visibility calculations will be performed on the cell and it will not be visible.
    Exclude? is useful for shortcutting the normal visibility procedure for known
    non-visible cells and speeding up calculations."
   (log-time "map-visibility"
     (let [[ox oy] origin
         width  (-> grid first count)
         height (count grid)
         get-cell (memoize (fn [x y] (get-in grid [y x])))
         blocking?-memo (memoize blocking?)]
     ;(println (with-xygrid grid))
     (vec (pmap (fn [line] (vec (map (fn [[cell x y]]
                                       (if (exclude? cell x y)
                                         false
                                         (visible? get-cell blocking?-memo ox oy x y)))
                                     line)))
                (with-xygrid grid)))))))

(defn cell-blocking?
  "Walls, closed doors, and `nil` cells block visibility."
  [cell]
  (if (nil? cell)
    true
    (contains? #{:vertical-wall
                 :horizontal-wall
                 :close-door
                 :tree
                 :palm-tree
                 :fruit-tree} (cell :type))))


(defn -main
  "Run speed tests for `map-visibility`."
  [& args]
  ;(println (= (line-segment [0 0] [22 40]) (line-segment-fast [0 0] [22 40])))
  ;(println (= (line-segment [10 10] [22 40]) (line-segment-fast [10 10] [22 40])))
  ;(println (= (line-segment [22 40] [10 10]) (line-segment-fast [22 40] [10 10])))
  
  (dotimes [i 10]
    (let [place (repeat 22 (repeat 40 false))]
     (time (map-visibility [(int (/ i 2)) 2] identity place)))))
  
