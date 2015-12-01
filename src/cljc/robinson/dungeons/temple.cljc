;; Functions for randomly generating maps.
(ns robinson.dungeons.temple
  (:require [robinson.common :as rc]
            [robinson.random :as rr]
            [robinson.noise :as rn]
            [robinson.lineofsight :as rlos]
            [robinson.world :as rw]
            [robinson.monstergen :as mg]
            [robinson.itemgen :as ig]
            [clojure.math.combinatorics :as combo]
            [algotools.algos.graph :as graph]
            [taoensso.timbre :as log]
            algotools.data.union-find
            clojure.set))

(declare print-cells)

(defn range*
  [v1 v2]
  (range (min v1 v2) (inc (max v1 v2))))

(defn canvas
  "Create a blank grid using `nil` for cell values."
  [width height]
  (vec (repeat height (vec (repeat width {:type :empty})))))

(defn find-point-relation [width height [start-x start-y] [end-x end-y]]
  "  top
    \\    /
     \\  /
      \\/
  left/\\right
     /  \\
    /    \\
    bottom"
  (let [left-bottom  (> (- end-y start-y) (* (/ height width) (- end-x start-x)))
        bottom-right (> (- end-y start-y) (* -1 (/ height width) (- end-x start-x)))]
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
  (let [side         (find-point-relation (- x-max x-min) (- y-max y-min) start end)]
    (log/info "find-door" start end side)
    (rr/rand-nth (case side
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
  (let [_ (log/trace "all nodes" nodes)
        minimum-edges (map vector nodes (rest nodes))
        all-edges (combo/combinations nodes 2)
        _ (log/trace "all edges" all-edges)
        _ (log/trace "minimum-edges" minimum-edges)
        random-edges (map vec (take (/ (count nodes) 2) (rr/rnd-shuffle all-edges)))
        union-edges  (distinct (concat minimum-edges random-edges))]
    (log/trace "union-edges" union-edges)
    union-edges))

(defn points-to-corridor
  "Convert a sequence of points `[x y]` into `[x y {:type :corridor}]`."
  [points]
  (let [moss-noise (rn/create-noise (rr/create-random 3))]
    (map (fn [[x y]] 
           (let [moss-probability (rn/noise moss-noise (/ x 10.0) (/ y 10.0))
                 cell-type        (cond
                                    (> moss-probability 0.7)
                                      :moss-corridor
                                    (< moss-probability 0.2)
                                      :white-corridor
                                    :else
                                      :corridor)]
             [x y {:type cell-type}]))
         points)))

(defn gen-chest-item
  [level]
  (let [item (ig/id->item (rr/rand-nth [:cup
                                        :bowl
                                        :jewlery
                                        :statue
                                        :human-skull
                                        :gong
                                        :stone-tablet
                                        :codex
                                        :rag]))]
    (log/debug "gen-chest item" item)
    (case (get item :type)
      :knife
        (assoc item :utility 10)
      item)))
 

(defn add-room-features
  [level cells-xy]
  (let [xy->cell      (into {} (map (fn [[x y cell]] [[x y] cell]) cells-xy))
        xy->cell-type (fn xy->cell-type [x y] (-> [x y] xy->cell (get :type)))
        seed          (rc/system-time-millis)
        vine-noise    (rn/create-noise (rr/create-random seed))
        water-noise   (rn/create-noise (rr/create-random (inc seed)))
        grass-noise   (rn/create-noise (rr/create-random (+ seed 2)))
        moss-noise    (rn/create-noise (rr/create-random (+ seed 2)))]
    (map (fn [[x y cell]]
           (let [moss-probability (rn/noise grass-noise (/ x 10.0) (/ y 10.0))]
             (case (get cell :type)
               :floor
                 (let [vine-probability  (rn/noise vine-noise (/ x 10.0) (/ y 10.0))
                       water-probability (rn/noise water-noise (/ x 10.0) (/ y 10.0))
                       grass-probability (rn/noise grass-noise (/ x 10.0) (/ y 10.0))
                       [max-type max-p]  (reduce (fn [[max-type max-p] [cell-type p]]
                                                   (if (> p max-p)
                                                     [cell-type p]
                                                     [max-type max-p]))
                                                 [:vine vine-probability]
                                                 [[:shallow-water water-probability]
                                                  [:tall-grass grass-probability]])]
                   (cond
                     (or (and (= max-type :vine) (> max-p 0.85))
                         (> max-p 0.8))
                       [x y (assoc cell :type max-type)]
                     (> (rr/uniform-double) 0.90)
                       [x y (rr/rand-nth [{:type :chest :items (vec (repeatedly (rr/uniform-int 3) (partial gen-chest-item level)))}
                                          {:type :altar}])]
                     :else
                       [x y cell]))
               (cond
                 (> moss-probability 0.7)
                   [x y (assoc cell :type (case (get cell :type)
                                            :vertical-wall       :moss-vertical-wall
                                            :horizontal-wall     :moss-horizontal-wall 
                                            :vertical-wall-alt   :moss-vertical-wall-alt 
                                            :horizontal-wall-alt :moss-horizontal-wall-alt 
                                            :upper-left-1        :moss-upper-left-1 
                                            :upper-right-1       :moss-upper-right-1 
                                            :bottom-left-1       :moss-bottom-left-1 
                                            :bottom-right-1      :moss-bottom-right-1 
                                            :upper-left-2        :moss-upper-left-2 
                                            :upper-right-2       :moss-upper-right-2 
                                            :bottom-left-2       :moss-bottom-left-2 
                                            :bottom-right-2      :moss-bottom-right-2
                                            (get cell :type)))]
                 (< moss-probability 0.2)
                   [x y (assoc cell :type (case (get cell :type)
                                            :vertical-wall       :white-vertical-wall
                                            :horizontal-wall     :white-horizontal-wall 
                                            :vertical-wall-alt   :white-vertical-wall-alt 
                                            :horizontal-wall-alt :white-horizontal-wall-alt 
                                            :upper-left-1        :white-upper-left-1 
                                            :upper-right-1       :white-upper-right-1 
                                            :bottom-left-1       :white-bottom-left-1 
                                            :bottom-right-1      :white-bottom-right-1 
                                            :upper-left-2        :white-upper-left-2 
                                            :upper-right-2       :white-upper-right-2 
                                            :bottom-left-2       :white-bottom-left-2 
                                            :bottom-right-2      :white-bottom-right-2
                                            (get cell :type)))]
                 :else
                   [x y cell]))))
         cells-xy)))

(defn room-to-cellsxy
  "Convert the bounds of a room into a list of points that represent the room."
  [level min-x min-y max-x max-y]
  (let [alternate-corners? (> 0.5 (rr/uniform-double))
        corners [[[min-x min-y {:type (if alternate-corners? :upper-left-2 :upper-left-1)}]
                  [max-x min-y {:type (if alternate-corners? :upper-right-2 :upper-right-1)}]
                  [min-x max-y {:type (if alternate-corners? :bottom-left-2 :bottom-left-1)}]
                  [max-x max-y {:type (if alternate-corners? :bottom-right-2 :bottom-right-1)}]]]
        wall-greeble? (< 0.7 (rr/uniform-double))
        [direction
         location] (if wall-greeble?
                     (if (> 0.5 (rr/uniform-double))
                       [:horizontal (set (take (inc (rr/uniform-int 2)) (shuffle (range (inc min-x) max-x))))]
                       [:vertical   (set (take (inc (rr/uniform-int 2)) (shuffle (range (inc min-y) max-y))))])
                     [nil nil])
        top    [(map (fn [x]
                       [x min-y {:type :horizontal-wall}])
                     (range (inc min-x) max-x))]
        bottom [(map (fn [x]
                       [x max-y {:type :horizontal-wall}])
                     (range (inc min-x) max-x))]
       ;; make one middle line template
        middle-line (concat [[min-x {:type :vertical-wall}]]
                            (vec (map (fn [x] [x {:type :floor}]) (range (inc min-x) max-x)))
                            [[max-x {:type :vertical-wall}]])
        ;; duplicate it
        middle-lines (vec (map (fn [y]
                                 (map (fn [[x cell]]
                                        [x y cell])
                                      middle-line))
                               (range (inc min-y) max-y)))
        cellsxy (apply concat (concat corners top middle-lines bottom))
        ;; add greeble
        cellsxy (map (fn [[x y cell]]
                       (if wall-greeble?
                         (cond
                           (and (= direction :horizontal)
                                (= (get cell :type) :horizontal-wall)
                                (contains? location x))
                             [x y (assoc cell :type :horizontal-wall-alt)]
                           (and (= direction :vertical)
                                (= (get cell :type) :vertical-wall)
                                (contains? location y))
                             [x y (assoc cell :type :vertical-wall-alt)]
                           :else
                           [x y cell])
                         [x y cell]))
                      cellsxy)
        cellsxy (add-room-features level cellsxy)]
   cellsxy))

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
                   (+ (rr/uniform-int 2) (rc/chebyshev-distance (rc/xy->pos x1 y1) (rc/xy->pos x2 y2))))
                 (for [room-center-1 room-centers
                       room-center-2 room-centers
                       :when (let [[x1 y1] room-center-1
                                   [x2 y2] room-center-2]
                               (and ; rooms should not connect to themselves
                                    ;(not= room-center-1 room-center-2)
                                    ; rooms should connect only to rooms in the same row or column
                                    (or (< (Math/abs (- x1 x2)) (+ min-width 0))
                                        (< (Math/abs (- y1 y2)) (+ min-height 0)))
                                    ; only close together rooms should be connected, but it must be greater than 0
                                    ;  (ie exclude room from connecting to itself.
                                    (let [d (rc/chebyshev-distance (rc/xy->pos x1 y1) (rc/xy->pos x2 y2))]
                                      (< 0 d (+ min-width min-height 5)))
                                    ; always connect from the upper left to the lower right
                                    (< (+ (* x1 1000) y1) (+ (* x2 1000) y2))
                                    ;; don't connect any rooms that are already connected
                                    (not-any? (fn [room-center-pair]
                                                (log/debug "room-center-pair" room-center-pair)
                                                (or (= room-center-pair
                                                       [room-center-1 room-center-2])
                                                    (= room-center-pair
                                                       [room-center-2 room-center-1])))
                                              g)))]
                   [room-center-1 room-center-2]))]
    (log/debug "all extra edges" (reduce (fn [edges [[x1 y1] [x2 y2]]]
                                           (conj edges [[x1 y1] [x2 y2] (rc/chebyshev-distance (rc/xy->pos x1 y1) (rc/xy->pos x2 y2))]))
                                         []
                                         edges))
    (take 5 edges)))

(defn find-junction-rooms [edges]
  "Return 2 junction room centers. Candidate junction rooms have atleast 3 edges connected to them."
  (let [counts (reduce (fn [m [from to]]
                         (-> m
                         (update from (fn [v] (inc (or v 0))))
                         (update from (fn [v] (inc (or v 0))))))
                       {}
                       edges)
       junctions (keys (into {} (filter (fn [[_ connections]] (> connections 2))
                                        counts)))]
    (log/debug "junctions" junctions)
    (set (take 2 junctions))))

(defn vertical-or-horizontally-aligned?
  [& xys]
  (some (partial apply =) (apply map vector xys)))

(defn horizontal-s-segment
  "x*****
        *
        ******x"
  [[x1 y1] [x2 y2]]
  (log/info "horizontal-s-segment")
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
  (log/info "vertical-s-segment")
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
  (log/info "vertical-corner-segment")
  (concat (map #(vector x1 %) (range* y1 y2))
          (map #(vector % y2) (range* x1 x2))))

(defn horizontal-corner-segment
  "  x*****
          *
          *
          x"
  [[x1 y1] [x2 y2]]
  (log/info "horizontal-corner-segment")
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
      (rr/rand-nth [:vertical :horizontal])
      #_(assert false (format "Unknown wall type %d %d %d %d %d %d" min-x min-y max-x max-y x y))))

(defn multi-part-segment
  [from from-type to to-type]
  {:pre [(contains? #{:vertical :horizontal} from-type)
         (contains? #{:vertical :horizontal} to-type)]}
  (log/debug "multi-part-segment" from-type to-type)
  (cond
    (= from-type to-type :horizontal)
      (vertical-s-segment from to)
    (= from-type to-type :vertical)
      (horizontal-s-segment from to)
    (= from-type :horizontal) ; to vertical
      (vertical-corner-segment from to)
    (= from-type :vertical) ; to horizontal
      (horizontal-corner-segment from to)))

(defn make-corridor-points [cells room-centers-to-room-bounds junction-room-centers start end]
  "Finds a list of `[x y]`s comprising a corridor from `start` to `end`.
   `room-centers-to-room-bounds` is a map with `[x y]` keys and `[min-x min-y max-x max-y]` values."
  (let [start-room-bounds (get room-centers-to-room-bounds start)
        end-room-bounds   (get room-centers-to-room-bounds end)
        door-xy-1         (if (contains? junction-room-centers start)
                            start
                            (find-door start-room-bounds start end))
        door-xy-2         (if (contains? junction-room-centers end)
                            end
                            (find-door end-room-bounds end start))]
    (log/info "start" start)
    (log/info "start-room-bounds" start-room-bounds)
    (log/info "end" end)
    (log/info "end-room-bounds"end-room-bounds)
    (log/info "door-1" door-xy-1)
    (log/info "door-2" door-xy-2)
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

(defn merge-cell-types
  "When merging cells, given their types, determine
   the type of the resulting cell."
  [cell1 cell2]
  (log/debug "merging cells" cell1 "," cell2)
  (let [cell1-type (get cell1 :type)
        cell2-type (get cell2 :type)]
    (cond
      (or (and (contains? #{:corridor :moss-corridor :white-corridor} cell1-type)
               (contains? #{:horizontal-wall :horizontal-wall-alt :vertical-wall :vertical-wall-alt :close-door :open-door
                            :moss-horizontal-wall :moss-horizontal-wall-alt :moss-vertical-wall :moss-vertical-wall-alt
                            :white-horizontal-wall :white-horizontal-wall-alt :white-vertical-wall :white-vertical-wall-alt}
                          cell2-type))
          (and (contains? #{:corridor :moss-corridor :white-corridor} cell2-type)
               (contains? #{:horizontal-wall :horizontal-wall-alt :vertical-wall :vertical-wall-alt :close-door :open-door
                            :moss-horizontal-wall :moss-horizontal-wall-alt :moss-vertical-wall :moss-vertical-wall-alt
                            :white-horizontal-wall :white-horizontal-wall-alt :white-vertical-wall :white-vertical-wall-altr}
                          cell2-type)))
        (rr/rand-nth [:close-door :open-door :corridor :moss-corridor :white-corridor])
      (every? #{:corridor :moss-corridor :white-corridor} [cell1-type cell2-type])
        cell1-type
      (= cell1-type :empty)
        cell2-type
      (= cell2-type :empty)
        cell1-type
      (= cell1-type cell2-type)
        cell1-type
      :else
        (assert false (format "Trying to merge %s %s" (str cell1-type) (str cell2-type))))))

(defn merge-with-canvas
  "Merge a grid of `[x y cell]` into an existing grid. The result is a merged grid
   following the rules of `merge-cell-types`."
  [canvas & cells-xy]
  (log/debug "before")
  ;(print-cells canvas)
  (let [f (apply comp (map (fn [[x y cell]]
                             (fn [c] (update-in c [y x]
                               (fn [canvas-cell]
                                 (let [cell-type (merge-cell-types (if (nil? cell) {:type :nil} cell)
                                                                   (if (nil? canvas-cell) {:type :nil} canvas-cell))
                                       items     (vec (concat (get cell :items []) (get canvas-cell :items [])))]
                                   (log/debug "cell" cell "canvas-cell" canvas-cell "items" items)
                                   (as-> {} new-cell
                                     (assoc new-cell :type cell-type)
                                     (if (seq items) 
                                       (assoc new-cell :items items)
                                       new-cell)))))))
                           cells-xy))
        cells (f canvas)]
  (log/debug "after")
  ;(print-cells canvas)
  cells))

(defn room-bounds-to-center
  [[x1 y1 x2 y2]]
  [(int (/ (+ x1 x2) 2))
   (int (/ (+ y1 y2) 2))])

(defn level->monster-probabilities
  [level]
  (get {1 [1 :spider
           1 :gecko]
        2 [1 :spider
           1 :gecko]
        3 [1 :mosquito
           1 :snake
           1 :spider]
        4 [1 :cobra
           1 :tarantula]
        5 [1 :cobra
           1 :tarantula]
        6 [1 :cobra
           1 :tarantula]
        7 [1 :monkey
           1 :centipede]
        8 [1 :monkey
           1 :centipede]}
       level))

(defn make-boss-npcs [cells boss-type]
  (reduce (fn [npcs [_ x y]]
            (conj npcs (assoc (mg/id->monster boss-type)
                              :pos (rc/xy->pos x y))))
          []
          (take 1
            (shuffle (filter (fn [[{cell-type :type} _ _]]
                               (= cell-type :shallow-water))
                             (rw/with-xy cells))))))


(defn make-npcs [cells level]
  (let [monster-probabilities (partition 2 (level->monster-probabilities level))]
  (log/info "monster-probabilities" monster-probabilities)
  (reduce (fn [npcs [_ x y]]
            (conj npcs (assoc (mg/id->monster (rr/rand-weighted-nth monster-probabilities))
                              :pos (rc/xy->pos x y))))
          (if (= level 8)
            (make-boss-npcs cells (rr/rand-nth [:giant-centipede :gorilla :giant-snake]))
            [])
          (take 20
            (shuffle (filter (fn [[{cell-type :type} _ _]]
                               (= cell-type :floor))
                             (rw/with-xy cells)))))))

  
(defn random-place
  "Create a grid of random rooms with corridors connecting them and doors
   where corridors connect to rooms."
  [level]
  (let [width 80
        height 23
        min-width 3
        min-height 3
        max-width 10
        max-height 7
        rows (int (/ height max-height))
        columns (int (/ width max-width))
        ;; list: [[x y width height] ...]
        room-bounds  (mapcat (fn [row] 
                               (map (fn [column]
                                      (let [width (rr/uniform-int min-width (dec max-width))
                                            height (rr/uniform-int min-height (dec max-height))
                                            x (rr/uniform-int (* column max-width) (+ (* column max-width) (dec max-width) (- width))) 
                                            y (rr/uniform-int (* row max-height) (+ (* row max-height) (dec max-height) (- height)))]
                                      [x y (+ x width) (+ y height)]))
                                    (range columns)))
                             (range rows))
        ;rooms        (mapcat #(apply room-to-cellsxy %) room-bounds)
        room-centers-to-room-bounds
                     (reduce (fn [m room-bounds]
                               (assoc m (room-bounds-to-center room-bounds)
                                        room-bounds))
                             {}
                             room-bounds)
        room-centers (keys room-centers-to-room-bounds)
        fcg          (points-to-fully-connected-graph room-centers)
        ;; g is a list of [[x1 y1] [x2 y2]] elements. List of pairs of points.
        g            (graph-to-minimum-spanning-tree fcg)
        more-edges   (additional-edges min-width min-height room-centers g)
        ;g            (concat g more-edges)
        ;; set of room centers that are junction rooms
        junction-rooms (find-junction-rooms g)
        ;; merge room cells to canvas, skipping junction rooms
        cells        (reduce (fn [cells room-bound]
                               (log/debug "room-bound" room-bound)
                               (if (contains? junction-rooms (room-bounds-to-center room-bound))
                                 cells
                                 (let [room-cellsxy (apply room-to-cellsxy level room-bound)]
                                   (log/debug "room-cellsxy" room-cellsxy)
                                   (apply merge-with-canvas cells room-cellsxy))))
                             (canvas width height)
                             room-bounds)
        ;; reduce over g, merging corridor cells to canvas
        cells        (reduce (fn [cells [start goal]]
                               (let [points (make-corridor-points cells room-centers-to-room-bounds junction-rooms start goal)]
                                 (log/debug "corridor points" points)
                                 (apply merge-with-canvas cells (points-to-corridor points))))
                             cells
                             g)
        [[upstairs-x upstairs-y]] (shuffle (clojure.set/difference (set room-centers) junction-rooms))
        [[downstairs-x downstairs-y]] (shuffle (filter (fn [[x y]]
                                                         (rc/farther-than?
                                                           (rc/xy->pos x y)
                                                           (rc/xy->pos upstairs-x upstairs-y)
                                                           (/ (min width height) 2.0)))
                                                       (clojure.set/difference (set room-centers) junction-rooms)))
        cells        (as-> cells cells
                       ;; only add up-stairs to levels > 0
                       ;; ie: temple merged into island should not have up-stairs.
                       (if (pos? level)
                         (assoc-in cells [upstairs-y upstairs-x :type] :up-stairs)
                         cells)
                       (update-in cells [downstairs-y downstairs-x] (fn [cell]
                                                                      (if (< level 8)
                                                                        (assoc cell :type :down-stairs
                                                                                    :dest-type :temple
                                                                                    :gen-args [(inc level)])
                                                                        (assoc cell :type :artifact-chest
                                                                                    :items (rr/rand-nth [[(ig/id->item :robe)]
                                                                                                         [(ig/id->item :ritual-knife)]
                                                                                                         [(ig/id->item :ancient-spear)]
                                                                                                         [(ig/id->item :blowgun)
                                                                                                          (ig/id->item :blowdart)
                                                                                                          (ig/id->item :blowdart)
                                                                                                          (ig/id->item :blowdart)
                                                                                                          (ig/id->item :blowdart)
                                                                                                          (ig/id->item :blowdart)
                                                                                                          (ig/id->item :blowdart)
                                                                                                          (ig/id->item :blowdart)
                                                                                                          (ig/id->item :blowdart)
                                                                                                          (ig/id->item :blowdart)
                                                                                                          (ig/id->item :blowdart)]
                                                                                                         [(ig/id->item :cure)
                                                                                                          (ig/id->item :cure)
                                                                                                          (ig/id->item :cure)]]))))))
        npcs         (make-npcs cells level)]
    ;(println "fcg" fcg)
    ;(log/debug "g" g)
    ;(log/debug "more-edges" more-edges)
    ;(println (vec room-bounds))
    ;(println (vec rooms))
    {:cells cells 
     :movement :fixed
     :npcs npcs
     #_#_:up-stairs upstairs
     #_#_:down-stairs downstairs}))

(defn place-to-ascii
  "Convert a grid of cells into a list of strings so that it can be rendered."
  [place]
  (let [contents (map (fn [line] (clojure.string/join
                                      (map (fn [cell]
                                        (if (nil? cell)
                                            \ 
                                            (case (cell :type)
                                              :empty " "
                                              :floor \·
                                              :corridor "\033[38;2;191;171;143m#\033[0m"
                                              ;:vertical-wall \║
                                              :vertical-wall "\033[38;2;191;171;143m║\033[0m"
                                              :horizontal-wall "\033[38;2;191;171;143m═\033[0m"
                                              :vertical-wall-alt \°
                                              :horizontal-wall-alt \°
                                              :upper-left-1 "\033[38;2;191;171;143m╔\033[0m"
                                              :upper-right-1 "\033[38;2;191;171;143m╗\033[0m"
                                              :bottom-left-1 "\033[38;2;191;171;143m╚\033[0m"
                                              :bottom-right-1 "\033[38;2;191;171;143m╝\033[0m"
                                              :upper-left-2 "\033[38;2;191;171;143m◙\033[0m"
                                              :upper-right-2 "\033[38;2;191;171;143m◙\033[0m"
                                              :bottom-left-2 "\033[38;2;191;171;143m◙\033[0m"
                                              :bottom-right-2 "\033[38;2;191;171;143m◙\033[0m"
                                              :open-door \'
                                              :close-door \+
                                              :altar \┬
                                              :chest "\033[38;2;124;97;45m■\033[0m"
                                              ;:shallow-water \~
                                              :shallow-water "\033[38;2;1;217;163m~\033[0m"
                                              :tall-grass "\033[38;2;1;140;1m\"\033[0m"
                                              ;:vine \⌠
                                              :vine "\033[38;2;1;140;1m⌠\033[0m"
                                              :up-stairs \<
                                              :down-stairs \>
                                              :moss-corridor "\033[38;2;1;140;1m#\033[0m"
                                              :moss-vertical-wall "\033[38;2;1;140;1m║\033[0m"
                                              :moss-horizontal-wall  "\033[38;2;1;140;1m═\033[0m"
                                              :moss-vertical-wall-alt  "\033[38;2;1;140;1m°\033[0m"
                                              :moss-horizontal-wall-alt  "\033[38;2;1;140;1m°\033[0m"
                                              :moss-upper-left-1  "\033[38;2;1;140;1m╔\033[0m"
                                              :moss-upper-right-1  "\033[38;2;1;140;1m╗\033[0m"
                                              :moss-bottom-left-1  "\033[38;2;1;140;1m╚\033[0m"
                                              :moss-bottom-right-1  "\033[38;2;1;140;1m╝\033[0m"
                                              :moss-upper-left-2  "\033[38;2;1;140;1m◙\033[0m"
                                              :moss-upper-right-2  "\033[38;2;1;140;1m◙\033[0m"
                                              :moss-bottom-left-2  "\033[38;2;1;140;1m◙\033[0m"
                                              :moss-bottom-right-2 "\033[38;2;1;140;1m◙\033[0m"
                                              :white-corridor "#"
                                              :white-vertical-wall "║"
                                              :white-horizontal-wall  "═"
                                              :white-vertical-wall-alt  "°"
                                              :white-horizontal-wall-alt  "°"
                                              :white-upper-left-1  "╔"
                                              :white-upper-right-1  "╗"
                                              :white-bottom-left-1  "╚"
                                              :white-bottom-right-1  "╝"
                                              :white-upper-left-2  "◙"
                                              :white-upper-right-2  "◙"
                                              :white-bottom-left-2  "◙"
                                              :white-bottom-right-2 "◙"
                                              (str (cell :type)))))
                                       line))) place)]
    contents))

(defn print-cells
  [cells]
  (doall (map println (place-to-ascii cells))))
  
(defn merge-cells
  [cells]
  (let [temple-place (random-place 0)]
    (mapv (fn [line temple-line y]
            (mapv (fn [cell temple-cell x]
                   (let [cell-type (get cell :type)
                         is-corridor (contains? #{:corridor :moss-corridor :white-corridor} (get temple-cell :type))]
                     (if is-corridor
                       {:type (case (get temple-cell :type)
                                :corridor
                                  :dirt
                                :moss-corridor
                                  :short-grass
                                :white-corridor
                                  :gravel)}
                       (if (= (get temple-cell :type) :empty)
                         cell
                         temple-cell))))
                  line
                  temple-line
                  (range)))
          cells
          (get temple-place :cells)
          (range))))

(defn -main
  "Generate a random grid and print it out."
  [& args]
  (println "generating...")
  (print-cells (get (random-place 0) :cells)))

