;; Functions for randomly generating maps.
(ns robinson.dungeons.temple
  (:require [robinson.common :as rc]
            [robinson.lineofsight :as rlos]
            [clojure.math.combinatorics :as combo]
            [algotools.algos.graph :as graph]
            [taoensso.timbre :as timbre]
            algotools.data.union-find
            clojure.set))

(timbre/refer-timbre)

(declare print-cells)

(defn range*
  [v1 v2]
  (range (min v1 v2) (inc (max v1 v2))))

(defn canvas
  "Create a blank grid using `nil` for cell values."
  [width height]
  (vec (repeat height (vec (repeat width nil)))))

(defn find-point-relation [[start-x start-y] [end-x end-y]]
  "  top
    \\    /
     \\  /
      \\/
  left/\\right
     /  \\
    /    \\
    bottom"
  (let [left-bottom  (> (- end-y start-y) (- end-x start-x))
        bottom-right (> (- end-y start-y) (* -1 (- end-x start-x)))]
    (cond
      (and left-bottom bottom-right)
        :bottom
      left-bottom
        :left
      bottom-right
        :right
      :else
        :top)))


(defn find-door
  "Takes a room and returns an [x y] of a door on the edge of the room."
  [[x-min y-min x-max y-max] start end]
  (let [side         (find-point-relation start end)]
    (println "find-door" start end side)
    (rand-nth (case side
                :left
                  (map (fn [i] [x-min i]) (range (inc y-min) y-max))
                :right
                  (map (fn [i] [x-max i]) (range (inc y-min) y-max))
                :top
                  (map (fn [i] [i y-min]) (range (inc x-min) x-max))
                :bottom
                  (map (fn [i] [i y-max]) (range (inc x-min) x-max))))))

(defn rooms-to-edges
  "Generate a random set of edges between rooms such that each room
   is traversable from every other room."
  [nodes]
  (let [_ (trace "all nodes" nodes)
        minimum-edges (map vector nodes (rest nodes))
        all-edges (combo/combinations nodes 2)
        _ (trace "all edges" all-edges)
        _ (trace "minimum-edges" minimum-edges)
        random-edges (map vec (take (/ (count nodes) 2) (shuffle all-edges)))
        union-edges  (distinct (concat minimum-edges random-edges))]
    (trace "union-edges" union-edges)
    union-edges))

(defn points-to-corridor
  "Convert a sequence of points `[x y]` into `[x y {:type :corridor}]`."
  [points]
  (map (fn [[x y]] [x y {:type :corridor}]) points))

(defn room-to-cellsxy
  "Convert the bounds of a room into a list of points that represent the room."
  [min-x min-y max-x max-y]
  (let [top    [(map (fn [x] [x min-y {:type :horizontal-wall}]) (range min-x (inc max-x)))]
        bottom [(map (fn [x] [x max-y {:type :horizontal-wall}]) (range min-x (inc max-x)))]
        middle-line (concat [[min-x {:type :vertical-wall}]]
                            (vec (map (fn [x] [x {:type :floor}]) (range (inc min-x) max-x)))
                            [[max-x {:type :vertical-wall}]])
        middle-lines (vec (map (fn [y] (map (fn [[x cell]] [x y cell]) middle-line)) (range (inc min-y) max-y)))
        lines (concat top middle-lines bottom)]
    (apply concat lines)))

(defn points-to-fully-connected-graph
  [points]
  (reduce (fn [m [from to]]
            (update m (str from) (fn [old]
                                   (let [d (rc/distance (apply rc/xy->pos from) (apply rc/xy->pos to))
                                         d (if (< d 3)
                                             (+ d (- 3 d))
                                             d)]
                                     (conj (or old (list))
                                           [(str to) d])))))
          {}
          (for [from  points
                to    points
                :when (not= from to)]
            [from to])))

(defn graph-to-minimum-spanning-tree
  [g]
  (map (fn [[from to]]
         [(read-string from) (read-string to)])
       (graph/MST-kruskal g true)))

(defn additional-edges
  "`room-centers` is a list of [x y]
   `g` is a list of xy point pairs eg: [[[x y] [x y]] [[x y] [x y]] ...]"
  [min-width min-height room-centers g]
  (let [edges
        (sort-by (fn [[[x1 y1] [x2 y2]]]
                   (+ (rand-int 2) (rc/chebyshev-distance (rc/xy->pos x1 y1) (rc/xy->pos x2 y2))))
                 (for [room-center-1 room-centers
                       room-center-2 room-centers
                       :when (let [[x1 y1] room-center-1
                                   [x2 y2] room-center-2]
                               (and ; rooms should not connect to themselves
                                    ;(not= room-center-1 room-center-2)
                                    ; rooms should connect only to rooms in the same row or column
                                    (or (< (Math/abs (- x1 x2)) (+ min-width 3))
                                        (< (Math/abs (- y1 y2)) (+ min-height 3)))
                                    ; only close together rooms should be connected, but it must be greater than 0
                                    ;  (ie exclude room from connecting to itself.
                                    (let [d (rc/chebyshev-distance (rc/xy->pos x1 y1) (rc/xy->pos x2 y2))]
                                      (< 0 d (+ min-width min-height 5)))
                                    ; always connect from the upper left to the lower right
                                    (< (+ (* x1 1000) y1) (+ (* x2 1000) y2))
                                    ;; don't connect any rooms that are already connected
                                    (not-any? (fn [room-center-pair]
                                                (println "room-center-pair" room-center-pair)
                                                (or (= room-center-pair
                                                       [room-center-1 room-center-2])
                                                    (= room-center-pair
                                                       [room-center-2 room-center-1])))
                                              g)))]
                   [room-center-1 room-center-2]))]
    (println "all extra edges" (reduce (fn [edges [[x1 y1] [x2 y2]]]
                                         (conj edges [[x1 y1] [x2 y2] (rc/chebyshev-distance (rc/xy->pos x1 y1) (rc/xy->pos x2 y2))]))
                                       []
                                       edges))
    (take 5 edges)))

(defn vertical-or-horizontally-aligned?
  [& xys]
  (some (partial apply =) (apply map vector xys)))

(defn horizontal-s-segment
  "x*****
        *
        ******x"
  [[x1 y1] [x2 y2]]
  (println "horizontal-s-segment")
  (let [mid-x (int (/ (+ x1 x2) 2))]
    (concat (map #(vector % y1)    (range* x1 mid-x))
            (map #(vector mid-x %) (range* y1 y2))
            (map #(vector % y2)    (range* mid-x x2)))))

(defn vertical-s-segment
  "    x
       *
       *
       ****
          *
          *
          x"
  [[x1 y1] [x2 y2]]
  (println "vertical-s-segment")
  (let [mid-y (int (/ (+ y1 y2) 2))]
    (concat (map #(vector x1 %)    (range* y1 mid-y))
            (map #(vector % mid-y) (range* x1 x2))
            (map #(vector x2 %)    (range* mid-y y2)))))

(defn vertical-corner-segment
  "  x
     *
     *
     *****x"
  [[x1 y1] [x2 y2]]
  (println "vertical-corner-segment")
  (concat (map #(vector x1 %) (range* y1 y2))
          (map #(vector % y2) (range* x1 x2))))

(defn horizontal-corner-segment
  "  x*****
          *
          *
          x"
  [[x1 y1] [x2 y2]]
  (println "horizontal-corner-segment")
  (concat (map #(vector x1 %) (range* y1 y2))
          (map #(vector % y2) (range* x1 x2))))

(defn wall-type
  [[min-x min-y max-x max-y] [x y]]
  (cond
    (= x min-x)
      :vertical
    (= x max-x)
      :vertical
    (= y min-y)
      :horizontal
    (= y max-y)
      :horizontal
    :else
      (assert false (format "Unknown wall type %d %d %d %d %d %d" min-x min-y max-x max-y x y))))

(defn multi-part-segment
  [from from-type to to-type]
  {:pre [(contains? #{:vertical :horizontal} from-type)
         (contains? #{:vertical :horizontal} to-type)]}
  (println "multi-part-segment" from-type to-type)
  (cond
    (= from-type to-type :horizontal)
      (vertical-s-segment from to)
    (= from-type to-type :vertical)
      (horizontal-s-segment from to)
    (= from-type :horizontal)
      (vertical-corner-segment from to)
    (= from-type :vertical)
      (horizontal-corner-segment from to)))

(defn make-corridor-points [cells room-centers-to-room-bounds start end]
  "Finds a list of `[x y]`s comprising a corridor from `start` to `end`.
   `room-centers-to-room-bounds` is a map with `[x y]` keys and `[min-x min-y max-x max-y]` values."
  (let [start-room-bounds (get room-centers-to-room-bounds start)
        end-room-bounds   (get room-centers-to-room-bounds end)
        door-xy-1         (find-door start-room-bounds start end)
        door-xy-2         (find-door end-room-bounds end start)]
    (println "start" start)
    (println "start-room-bounds" start-room-bounds)
    (println "end" end)
    (println "end-room-bounds"end-room-bounds)
    (println "door-1" door-xy-1)
    (println "door-2" door-xy-2)
    (cond
      (vertical-or-horizontally-aligned? door-xy-1 door-xy-2)
        (rlos/line-segment door-xy-1 door-xy-2)
      :else
        (multi-part-segment door-xy-1
                           (wall-type start-room-bounds door-xy-1)
                            door-xy-2
                           (wall-type end-room-bounds door-xy-2)))))

(defn cellsxy-to-cells
  "Convert a grid of `[x y cell]` to cell."
  [cells-xy]
  (let [lines-xy (map (fn [[_ v]] (first v)) (group-by second cells-xy))
        lines    (map (fn [line] (map last (group-by first line))) lines-xy)]
    lines))

(defn cellsxy-to-ascii [cells-xy]
  "Convert a grid of cells into a list of string that can be rendered."
  (let [contents (map (fn [line] (clojure.string/join
                                      (map (fn [[x y cell]]
                                        (if (nil? cell)
                                            \ 
                                            (case (cell :type)
                                              :floor \.
                                              :vertical-wall \|
                                              :horizontal-wall \-
                                              \?)))
                                       line))) cells-xy)]
    contents))

(defn merge-cells
  "When merging cells, given their types, determine
   the type of the resulting cell."
  [cell1 cell2]
  (case (cell1 :type)
    :floor (case (cell2 :type)
             :floor :floor
             :horizontal-wall :floor
             :vertical-wall :floor
             :corridor :close-door
             :close-door :floor
             :up-stairs :up-stairs
             :down-stairs :down-stairs
             :nil :floor)
    :horizontal-wall (case (cell2 :type)
             :floor :floor
             :horizontal-wall :horizontal-wall
             :vertical-wall :horizontal-wall
             :corridor :close-door
             :close-door :close-door
             :up-stairs :up-stairs
             :down-stairs :down-stairs
             :nil :horizontal-wall)
    :vertical-wall (case (cell2 :type)
             :floor :floor
             :horizontal-wall :horizontal-wall
             :vertical-wall :vertical-wall
             :corridor :close-door
             :close-door :close-door
             :up-stairs :up-stairs
             :down-stairs :down-stairs
             :nil :vertical-wall)
    :corridor (case (cell2 :type)
             :floor :floor
             :horizontal-wall :close-door
             :vertical-wall :close-door
             :corridor :corridor
             :close-door :close-door
             :up-stairs :up-stairs
             :down-stairs :down-stairs
             :nil :corridor)
    :close-door (case (cell2 :type)
             :floor :floor
             :horizontal-wall :close-door
             :vertical-wall :close-door
             :corridor :corridor
             :close-door :close-door
             :up-stairs :up-stairs
             :down-stairs :down-stairs
             :nil :close-door)
    :up-stairs (case (cell2 :type)
             :floor :up-stairs
             :horizontal-wall :up-stairs
             :vertical-wall :up-stairs
             :corridor :up-stairs
             :close-door :up-stairs
             :nil :up-stairs)
    :down-stairs (case (cell2 :type)
             :floor :down-stairs 
             :horizontal-wall :down-stairs 
             :vertical-wall :down-stairs 
             :corridor :down-stairs 
             :close-door :down-stairs 
             :nil :down-stairs)
    :nil (cell2 :type)))

(defn merge-with-canvas
  "Merge a grid of `[x y cell]` into an existing grid. The result is a merged grid
   following the rules of `merge-cells`."
  [canvas & cells-xy]
  (let [f (apply comp (map (fn [[x y cell]]
                             (fn [c] (update-in c [y x]
                               (fn [canvas-cell]
                                 {:type (merge-cells (if (nil? cell) {:type :nil} cell)
                                              (if (nil? canvas-cell) {:type :nil} canvas-cell))}))))
                           cells-xy))
        cells (f canvas)]
   ;(print-cells cells)
   cells))

(defn rand-int-range [min max]
  (+ (rand-int (- max min)) min))

(defn random-place
  "Create a grid of random rooms with corridors connecting them and doors
   where corridors connect to rooms."
  [width height]
  (let [num-rooms 19
        min-width 3
        min-height 3
        max-width 10
        max-height 8
        rows (int (/ height max-height))
        columns (int (/ width max-width))
        ;; list: [[x y width height] ...]
        room-bounds  (mapcat (fn [row] 
                               (map (fn [column]
                                      (let [width (rand-int-range min-width (dec max-width))
                                            height (rand-int-range min-height (dec max-height))
                                            x (rand-int-range (* column max-width) (+ (* column max-width) (dec max-width) (- width))) 
                                            y (rand-int-range (* row max-height) (+ (* row max-height) (dec max-height) (- height)))]
                                      [x y (+ x width) (+ y height)]))
                                    (range columns)))
                             (range rows))
        rooms        (mapcat #(apply room-to-cellsxy %) room-bounds)
        room-centers-to-room-bounds
                     (reduce (fn [m room-bounds]
                               (let [[x1 y1 x2 y2] room-bounds]
                                 (assoc m [(int (/ (+ x1 x2) 2))
                                           (int (/ (+ y1 y2) 2))]
                                          room-bounds)))
                             {}
                             room-bounds)
        room-centers (keys room-centers-to-room-bounds)
        fcg          (points-to-fully-connected-graph room-centers)
        ;; g is a list of [[x1 y1] [x2 y2]] elements. List of pairs of points.
        g            (graph-to-minimum-spanning-tree fcg)
        more-edges   (additional-edges min-width min-height room-centers g)
        g            (concat g more-edges)
        ;; reduce over g
        cells        (reduce (fn [cells [start goal]]
                               (let [points (make-corridor-points cells room-centers-to-room-bounds start goal)]
                                 (println "corridor points" points)
                                 (apply merge-with-canvas cells (points-to-corridor points))))
                             (apply merge-with-canvas (canvas width height) rooms)
                             g)
        upstairs     (conj [(first room-centers)] {:type :up-stairs})
        downstairs   (conj [(last room-centers)] {:type :down-stairs})]
    ;(println "fcg" fcg)
    (println "g" g)
    (println "more-edges" more-edges)
    ;(println (vec room-bounds))
    ;(println (vec rooms))
    {:place cells #_(apply merge-with-canvas (canvas width height)
                     (concat corridors rooms))
     :up-stairs upstairs
     :down-stairs downstairs}))

(defn place-to-ascii
  "Convert a grid of cells into a list of strings so that it can be rendered."
  [place]
  (let [contents (map (fn [line] (clojure.string/join
                                      (map (fn [cell]
                                        (if (nil? cell)
                                            \ 
                                            (case (cell :type)
                                              :floor \.
                                              :vertical-wall \|
                                              :horizontal-wall \-
                                              :close-door \+
                                              :corridor \#
                                              :up-stairs \<
                                              :down-stairs \>
                                              (str (cell :type)))))
                                       line))) place)]
    contents))

(defn print-cells
  [cells]
  (doall (map println (place-to-ascii cells))))
  

(defn -main
  "Generate a random grid and print it out."
  [& args]
  (println "generating...")
  (print-cells (get (random-place 80 25) :place)))

