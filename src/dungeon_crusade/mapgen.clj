(ns dungeon-crusade.mapgen
  (:require [clojure.math.combinatorics :as combo]
            [algotools.algos.graph :as graph]
            clojure.set))

(defn canvas [width height]
  (vec (repeat height (vec (repeat width nil)))))

(defn draw-ascii-place [ascii]
  (doall (map println ascii)))

(defn make-room [min-x min-y max-x max-y room-idx]
  {:id     (keyword (str room-idx))
   :x      (+ min-x (rand-int max-x))
   :y      (+ min-y (rand-int max-y))
   :width  (+ 2 (rand-int 5))
   :height (+ 2 (rand-int 5))})

(defn rooms-to-edges [nodes]
  (let [;_ (println "all nodes" nodes)
        minimum-edges (map #(vector %1 %2) nodes (rest nodes))
        all-edges (combo/combinations nodes 2)
        ;_ (println "all edges" all-edges)
        ;_ (println "minimum-edges" minimum-edges)
        random-edges (map vec (take (/ (count nodes) 2) (shuffle all-edges)))
        union-edges  (distinct (concat minimum-edges random-edges))]
    ;(println "union-edges" union-edges)
    union-edges))

(defn edge-to-points [start-x start-y end-x end-y orientation]
  (let [min-x (min start-x end-x)
        max-x (max start-x end-x)
        min-y (min start-y end-y)
        max-y (max start-y end-y)]
    (if orientation
      ;; horizontal first
      (let [horz [(conj (map (fn [x] [x start-y]) (range min-x max-x)) [max-x start-y])]
            vert (map (fn [y] [[end-x y]]) (range min-y max-y))
            points (distinct (concat horz vert))]
       points)
      ;; vertial first
      (let [vert (map (fn [y] [[start-x y]]) (range min-y max-y))
            horz [(conj (map (fn [x] [x end-y]) (range min-x max-x)) [start-x max-y])]
            points (distinct (concat vert horz))]
       points))))

(defn points-to-corridor [points]
  (map (fn [line] (map (fn [[x y]] [x y {:type :corridor}]) line)) points))

(defn room-to-cellsxy  [min-x min-y max-x max-y]
  (let [top    [(map (fn [x] [x min-y {:type :horizontal-wall}]) (range min-x (inc max-x)))]
        bottom [(map (fn [x] [x max-y {:type :horizontal-wall}]) (range min-x (inc max-x)))]
        middle-line (concat [[min-x {:type :vertical-wall}]]
                            (vec (map (fn [x] [x {:type :floor}]) (range (inc min-x) max-x)))
                            [[max-x {:type :vertical-wall}]])
        middle-lines (vec (map (fn [y] (map (fn [[x cell]] [x y cell]) middle-line)) (range (inc min-y) max-y)))
        lines (concat top middle-lines bottom)]
    lines))

(defn cellsxy-to-place [cells-xy]
  (let [lines-xy (map (fn [[_ v]] (first v)) (group-by second cells-xy))
        lines    (map (fn [line] (map last (group-by first line))) lines-xy)]
    lines))


(defn cellsxy-to-ascii [cells-xy]
  (let [contents (map (fn [line] (apply str
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

(def cell-type-order
  {:floor 0
   :horizontal-wall 1
   :vertical-wall 2
   :corridor 3
   :nil 4})
(defn merge-cells [cell1 cell2]
  (do ;(println "cell1" cell1 "cell2" cell2)
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
    :nil (cell2 :type))))

(defn merge-with-canvas [canvas & cells-xy]
  (let [f (apply comp (map (fn [[x y cell]]
                             (fn [c] (update-in c [y x]
                               (fn [canvas-cell]
                                 {:type (merge-cells (if (nil? cell) {:type :nil} cell)
                                              (if (nil? canvas-cell) {:type :nil} canvas-cell))}))))
                           (apply concat (apply concat cells-xy))))]
   (f canvas)))

(defn graph-to-map [graph]
  (let [rooms  (graph :rooms)
        edges  (graph :edges)]
        []))

(defn random-place [width height]
  (let [num-rooms 6
        min-width 3
        min-height 3
        max-width 10
        max-height 8
        room-bounds  (loop [xl (shuffle (range width))
                            yl (shuffle (range height))
                            result []]
                       (let [x1 (first (filter (partial > (- width max-width)) xl))
                             y1 (first (filter (partial > (- height max-height)) yl))
                             _ (println "xl" xl)
                             _ (println "yl" yl)
                             x2-potential (map (partial + x1) (shuffle (range min-width max-width)))
                             x2 (first (filter #(and (contains? (set xl) %)
                                                     (contains? (set xl) (int (/ (+ x1 %) 2))))
                                               x2-potential))
                             y2-potential (map (partial + y1) (shuffle (range min-height max-height)))
                             y2 (first (filter #(and (contains? (set yl) %)
                                                     (contains? (set yl) (int (/ (+ y1 %) 2))))
                                               y2-potential))
                             _ (println "x2-potential" x2-potential)
                             _ (println "y2-potential" y2-potential)
                             _ (println "x1" x1 "y1" y1 "x2" x2 "y2" y2)
                             xc (int (/ (+ x1 x2) 2))
                             yc (int (/ (+ y1 y2) 2))
                             _ (println "xc" xc "yc" yc)
                             result (conj result [x1 y1 x2 y2])]
                       (if (>= (count result) num-rooms)
                         result
                         (recur (clojure.set/difference (set xl) #{x1 xc x2})
                                (clojure.set/difference (set yl) #{y1 yc y2})
                                result))))

                     ;(map (fn [i] (let [
                     ;                   x1 (rand-int (- width max-width))
                     ;                   y1 (rand-int (- height max-height))
                     ;                   x2 (+ x1 min-width (rand-int (- max-width min-width)))
                     ;                   y2 (+ y1 min-height (rand-int (- max-height min-height)))]
                     ;               [x1 y1 x2 y2]))
                     ;     (range num-rooms))
        rooms        (map #(apply room-to-cellsxy %) room-bounds)
        room-centers (map (fn [[x1 y1 x2 y2]] [(int (/ (+ x1 x2) 2))
                                               (int (/ (+ y1 y2) 2))])
                          room-bounds)
        corridors    (map (fn [[[x1 y1] [x2 y2]]]
                            (points-to-corridor
                              (edge-to-points x1 y1 x2 y2 (< (rand-int 2) 1))))
                          (rooms-to-edges room-centers))
        upstairs     (conj [(first room-centers)] {:type :up-stairs})
        downstairs   (conj [(last room-centers)] {:type :down-stairs})]
    {:place (apply merge-with-canvas (canvas width height)
                   (concat corridors rooms))
     :up-stairs upstairs
     :down-stairs downstairs}))

(defn place-to-ascii [place]
  (let [contents (map (fn [line] (apply str
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

(defn -main [& args]
  (println "generating...")
  (doall (map println (place-to-ascii ((random-place 55 20) :place)))))

