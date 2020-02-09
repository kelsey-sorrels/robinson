(ns robinson.lineofsight
  (:require [robinson.common :as rc]
            [robinson.player :as rp]
            [robinson.inventory :as ri]
            [taoensso.timbre :as log]
            [robinson.macros :as rm]
            [robinson.world :as rw]
            [robinson.math :as math]
            [robinson.world  :as rw]
            [clojure.walk :as w]
            [clojure.zip :as z]))

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


(defn square-points [x0 y0 radius]
  (letfn [(points [x y m]
            (let [x+   (+ x0 x)
                  x-   (- x0 x)
                  y+   (+ y0 y)
                  y-   (- y0 y)
                  x0y+ (+ x0 y)
                  x0y- (- x0 y)
                  xy0+ (+ y0 x)
                  xy0- (- y0 x)
                  xys  [[x+ y+]
                        [x+ y-]
                        [x- y+]
                        [x- y-]
                        [x0y+ xy0+]
                        [x0y+ xy0-]
                        [x0y- xy0+]
                        [x0y- xy0-]]
                  oy    y
                  om    m
                  [y m] (if (pos? m)
                            [(dec y) (- m (* 8 y))]
                            [y m])]
                (if (<= x y)
                 (concat xys
                         (points (if (pos? om)
                                   x
                                   (inc x))
                                 (if (pos? m)
                                   y
                                   oy)
                                 (+ m 4 (* 8 x))))
                 xys)))]
    (points 0 radius (- 5 (* 4 radius)))))


(defn remove-points-beyond-radius
  [x0 y0 r segment]
  (remove (fn [[x y]]
            (rc/farther-than? x0 y0 x y r))
          segment))

(defn paths->trie [segments]
  (reduce (fn [tree segment]
            (assoc-in tree segment {}))
          {}
          segments))

(defn trie->paths [trie]
  (reduce
    (fn [paths [k subtree]]
      (if (empty? subtree)
        (conj paths (list k))
        (concat
          paths
          (map (fn [path]
                 (cons k path))
                 (trie->paths subtree)))))
    (list)
    trie))

(defn replace-vals [kvs m]
  (reduce-kv (fn [r k v]
               (assoc r k (get kvs k v)))
             {}
             m))

(defn adj-xys
  [x y]
  [[(inc x) y]
   [(dec x) y]
   [x (inc y)]
   [x (dec y)]])

(defn assoc-adj [m [x y] exclude-subtree?]
  (reduce (fn [m k]
            (log/info "testing" k)
            (if (exclude-subtree? k)
              (do
                (log/info "adding post-process" k)
                (assoc m k {}))
              m))
          m
          (adj-xys x y)))

;; cull subtries with parent keys if `(exclude-subree? key)` is false.
(defn cull-trie [exclude-subtree? trie]
  (w/prewalk (fn [m] (if (and (map? m)
                              (not (empty? m)))
                       (reduce-kv (fn [m k v]
                                    (if (exclude-subtree? k)
                                      (assoc m k {})
                                      (-> m
                                        (assoc k v)
                                        (assoc-adj k exclude-subtree?))))
                                  {}
                                  m)
                       m))
               trie))

(defn trie->keys [trie]
  (loop [t  (z/zipper map? vals #(zipmap (keys %1) %2) trie)
         ks #{}]
    (cond
      (z/end? t) ks
      (empty? (z/node t)) (recur (z/next t) ks)
      (z/branch? t) (let [new-keys (set (keys (z/node t)))]
        (recur (z/next t) (clojure.set/union new-keys ks)))
      :leaf
      (log/info "got leaf" (z/node t)))))

(defn conj!-adj [children [x y] exclude-subtree?]
  (reduce (fn [children xy]
            (if (exclude-subtree? xy)
              (conj! children {xy {}})
              children))
          children
          (adj-xys x y)))

(defn cull-trie->keys [exclude-subtree? trie]
  (loop [t  (z/zipper ; branch?
                      map?
                      ; children
                      (fn [node]
                        (persistent!
                          (reduce-kv (fn [children xy subtree]
                                       (if (exclude-subtree? xy)
                                         children
                                         (-> children
                                           (conj! subtree)
                                           (conj!-adj xy exclude-subtree?))))
                                     (transient [])
                                     node)))
                      ; make node
                      #(zipmap (keys %1) %2)
                      ; root
                      trie)
         ks #{}]
    (cond
      (z/end? t) ks
      (empty? (z/node t)) (recur (z/next t) ks)
      (z/branch? t) (let [new-keys (set (keys (z/node t)))]
        (recur (z/next t) (clojure.set/union new-keys ks)))
      :leaf
      (log/info "got leaf" (z/node t)))))

(defn trie->leaves [trie]
  "Given a trie, returns the set of nodes without children."
  #{})

(def r->trie
  (reduce (fn [result r]
            (let [perimeter-points (square-points 0 0 r)
                  segments         (remove empty?
                                     (map #(->>
                                             (line-segment [0 0] %)
                                             (remove-points-beyond-radius 0 0 r))
                                          perimeter-points))
                  ;; truncate segments to radius
                  ;segments         (remove-points-beyond-radius 0 0 k segments)
                  ;_ (log/info "segments")
                  ;_ (log/info segments)
                  trie             (paths->trie segments)]
              (assoc result r trie)))
          {}
          (range 20)))

(defn trie->leaves [trie]
  "Given a trie, return a set containing all of the leaf nodes, ie: the keys
   that have empty children."
  (map last (trie->paths trie)))

(def r->perimeter-xys
  "Map radius to the set of `[x y]`'s that form the perimeter of the visibility trie."
  (into {}
        (map (fn [[r trie]]
               [r (trie->leaves trie)])
             r->trie)))

(defn perimeter-xys [center-x center-y r]
  (let [xys  (get r->perimeter-xys (int r))
        xys  (map (fn [[x y]] [(+ x center-x) (+ y center-y)])
                   xys)]
    xys))

(defn visible-xys
  [center-x center-y r xy-visible?]
  (let [trie (get r->trie (int r))
        ;_ (log/info "potential visibility trie" trie)
        trie (cull-trie (fn [[x y]] (xy-visible? [(+ x center-x) (+ y center-y)]))
                        trie)
        ;_ (log/info "visibility trie" trie)
        xys  (trie->keys trie)
        xys  (map (fn [[x y]] [(+ x center-x) (+ y center-y)])
                   xys)]
    xys))

(defn visible-xys
  [center-x center-y r xy-visible?]
  (let [trie (get r->trie (int r))
        ;_ (log/info "potential visibility trie" trie)
        ;_ (log/info "visibility trie" trie)
        xys  (cull-trie->keys (fn [[x y]] (xy-visible? [(+ x center-x) (+ y center-y)]))
                              trie)
        xys  (map (fn [[x y]] [(+ x center-x) (+ y center-y)])
                   xys)]
    xys))


(defn visible?
  [;get-cell
   visibility-cache
   ;blocking?
   x1 y1 x2 y2]
  (every? (fn [[x y]] (visibility-cache x y))
    ;(map (fn [[x y]] ((fn [x y] (get-in grid [y x])) x y))
    ;(map (fn [[x y]] (get-in grid [y x]))
         (line-segment-fast-without-endpoints [x1 y1] [x2 y2])))
 
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
   (rm/log-time "map-visibility"
     (let [[ox oy] origin
         width  (-> grid first count)
         height (count grid)
         get-cell (memoize (fn [x y] (get-in grid [y x])))
         blocking?-memo (memoize blocking?)]
     ;(println (with-xygrid grid))
     (vec (pmap
            (fn [line] (vec (map (fn [[cell x y]]
                                   (if (exclude? cell x y)
                                     false
                                     (visible? get-cell blocking?-memo ox oy x y)))
                                 line)))
              (rw/with-xygrid grid)))))))

(def blocking-cell-types
  #{:vertical-wall
    :horizontal-wall
    :close-door
    :palisade
    :tree
    :mountain
    :bamboo
    :palm-tree
    :fruit-tree
    ;; pirate cell types
    :bulkhead
    :bulkhead2
    :wooden-wall
    :mast
    :porthole
    ;; ruined temple types
    :empty
    :vertical-wall-alt
    :horizontal-wall-alt
    :upper-left-1
    :upper-right-1
    :bottom-left-1
    :bottom-right-1
    :upper-left-2
    :upper-right-2
    :bottom-left-2
    :bottom-right-2
    :vine
    :moss-vertical-wall 
    :moss-horizontal-wall
    :moss-vertical-wall-alt
    :moss-horizontal-wall-alt
    :moss-upper-left-1
    :moss-upper-right-1
    :moss-bottom-left-1
    :moss-bottom-right-1
    :moss-upper-left-2
    :moss-upper-right-2
    :moss-bottom-left-2
    :moss-bottom-right-2
    :white-vertical-wall
    :white-horizontal-wall
    :white-vertical-wall-alt
    :white-horizontal-wall-alt
    :white-upper-left-1
    :white-upper-right-1
    :white-bottom-left-1
    :white-bottom-right-1
    :white-upper-left-2
    :white-upper-right-2
    :white-bottom-left-2
    :white-bottom-right-2})

(defn cell-blocking?
  "Walls, closed doors, and `nil` cells block visibility."
  [cell]
  (if (nil? cell)
    true
    (contains? blocking-cell-types (cell :type))))


(defn sight-distance
  [state]
  (if-let [atmo   (get-in state [:data :atmo])]
    (let [frames (count atmo)
          t      (mod (get-in state [:world :time]) frames)
          frame  (nth atmo t)
          values (flatten frame)
          item   (ri/inventory-id->item state :lantern)
          on     (and item (= (get item :state) :on))
          #_#__      (log/info "sight-distance. lantern:" item "state:" on)
          values (map (fn [v] (if on (max v 100) v)) values)]
    (max 3.5 (+ 2.5 (* 18 (/ (reduce + values) (* 255 (count values)))))))
    5))

(defn target->cellsxy
  [state target-x target-y]
  (let [[start-x
         start-y] (rp/player-xy state)]
    (map (fn [[x y]] [(rw/get-cell state x y) x y])
         (line-segment-fast [start-x start-y] [target-x target-y]))))

(defn -main
  "Run speed tests for `map-visibility`."
  [& args]
  ;(println (= (line-segment [0 0] [22 40]) (line-segment-fast [0 0] [22 40])))
  ;(println (= (line-segment [10 10] [22 40]) (line-segment-fast [10 10] [22 40])))
  ;(println (= (line-segment [22 40] [10 10]) (line-segment-fast [22 40] [10 10])))
  
  (dotimes [i 10]
    (let [place (repeat 22 (repeat 40 false))]
     (time (map-visibility [(int (/ i 2)) 2] identity place)))))
  
