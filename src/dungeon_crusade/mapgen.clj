(ns dungeon-crusade.mapgen
  (:require [clojure.math.combinatorics :as combo]
            [algotools.algos.graph :as graph]))

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

(defn rooms-to-graph [rooms]
  (let [all-edges (combo/combinations (map :id rooms) 2)
        minimum-edges (graph/MST-kruskal (apply conj (map (fn [[k v]] (hash-map k (map (fn [[_ id]] [id 1]) v))) (group-by first all-edges))) false)
        random-edges (map vec (take (/ (count rooms) 2) (shuffle all-edges)))
        union-edges  (distinct (concat minimum-edges random-edges))]
    (println minimum-edges)
    (println union-edges)
  {:rooms rooms
   :edges union-edges}))

(defn edge-to-points [start-x start-y end-x end-y orientation]
  (if orientation
    (let [horz (map-indexed (fn [i e] [(+ start-x i) start-y]) (range (min start-x end-x) (max start-x end-x)))
          vert (map-indexed (fn [i e] [end-x (+ start-y i)]) (range (min start-y end-y) (max start-y end-y)))
          points (distinct (concat horz vert [[(max start-x end-x) (max start-y end-y)]]))]
     points) 
    (let [horz (map-indexed (fn [i e] [(+ start-x i) end-y]) (range (min start-x end-x) (max start-x end-x)))
          vert (map-indexed (fn [i e] [start-x (+ start-y i)]) (range (min start-y end-y) (max start-y end-y)))
          points (distinct (concat vert horz [[(max start-x end-x) (max start-y end-y)]]))]
     points)))

(defn points-to-corridor [points]
  (map (fn [[x y]] [x y {:type :corridor}]) points))

(defn room-to-cellsxy [min-x min-y max-x max-y]
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
   :nil 3})

(defn merge-to-canvas [canvas & cells-xy]
  (let [f (apply comp (map (fn [[x y cell]]
                             (fn [c] (update-in c [y x]
                               (fn [canvas-cell]
                                 (first (sort-by #(if (nil? %)
                                                    (cell-type-order :nil)
                                                    (-> % :type cell-type-order))
                                                 [cell canvas-cell]))))))
                           (apply concat (apply concat cells-xy))))]
   (f canvas)))

(defn graph-to-map [graph]
  (let [rooms  (graph :rooms)
        edges  (graph :edges)]
        []))

(defn random-place [width height]
  (let [num-rooms 5
        rooms     (map (partial make-room 5 5 30 15) (range num-rooms))
        graph     (rooms-to-graph rooms)]
    graph))

(defn place-to-ascii [place]
  (let [contents (map (fn [line] (apply str
                                      (map (fn [cell]
                                        (if (nil? cell)
                                            \ 
                                            (case (cell :type)
                                              :floor \.
                                              :vertical-wall \|
                                              :horizontal-wall \-
                                              \?)))
                                       line))) place)]
    contents))

(defn -main [& args]
  (println "generating...")
  (doall (map println (place-to-ascii (merge-to-canvas (canvas 20 20)
                                                       (room-to-cellsxy 1 1 8 4)
                                                       (room-to-cellsxy 6 3 10 19)
                                                       (room-to-cellsxy 2 10 15 15)))))
  (println (random-place 30 20)))

