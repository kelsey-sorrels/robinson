(ns robinson.crafting.recipe-gen
  (:require clojure.set
            [clojure.core.matrix :as m]
            [rockpick.core :as rp]
            [loom.graph :as lg]
            [loom.alg :as la]
            [loom.alg-generic :as lag]
            [loom.label :as ll]
            [incanter.stats :as is]
            [taoensso.timbre :as log])
  (:import [java.lang Math]))

(defn std-dev [x]
  (case (count x)
    0 0
    1 0
    (let [mean (/ (reduce + x) (count x))]
      (Math/sqrt (/ (reduce + (map (fn [x] (* (- x mean) (- x mean))) x))
                  (dec (count x)))))))


(defn rand-weighted
  [m]
   "Randomly select a value from map of {value weight}."
   (cond
     (empty? m)
       nil
     (= 1 (count m))
       (-> m first second)
     :else
       (let [[wm a]     (reduce (fn [[m a] [v weight]]
                                  [(conj m [a v]) (+ weight a)])
                                [[] 0] m)
             n          (rand a)]
         (second (last (remove (fn [[wn _]] (> wn n)) wm))))))

(defrecord Graph [adj connections])
(defn g->loom-graph [g]
  (let [{:keys [adj connections]} g
        edges (for [[[col-min col-max] [row-min row-max]] connections
                    row (range row-min row-max)
                    col (range col-min col-max)
                    :let [e (m/mget adj row col)]
                    :when (pos? e)]
                [row col])]
    (apply lg/digraph edges)))

(defn fully-connected
  "Shape is a seq of odd-valued ints"
  [shape]
  (let [rows (count shape)
        nodes (reduce + shape)
        ; empty adjacency matrix
        adj (m/mutable (m/zero-matrix nodes nodes))
        connections (let [n (reductions + shape)
                          n-offs (map vector n (rest n))
                          m (reductions + (cons 0 shape))
                          m-offs (map vector m (rest m))
                          connections (map vector n-offs m-offs)]
                      connections)]
    (doseq [[[col-min col-max] [row-min row-max]] connections]
      (doseq [row (range row-min row-max)
              col (range col-min col-max)]
        (let [layer-len (/ (+ (- col-max col-min) (- row-max row-min)) 2)
              offset (- col row layer-len)]
          (when (<= -1 offset 1)
            (m/mset! adj row col 1)))))
    (let [g (->Graph (m/immutable adj) connections)]
      g)))
       
(defn prune-crossed-edges
  [g]
  (let [{:keys [adj connections]} g
        m-adj (m/mutable adj)]
    (doseq [[[col-min col-max] [row-min row-max]] connections]
      (doseq [row (range (inc row-min) row-max)
              col (range col-min (dec col-max))]
        (let [e (m/mget m-adj row col)
              dual (m/mget m-adj (dec row) (inc col))]
          (when (and (pos? e) (pos? dual))
            (if (= (rand-int 2) 1)
              (m/mset! m-adj row col 0)
              (m/mset! m-adj (dec row) (inc col) 0))))))
    (let [g (->Graph (m/immutable m-adj) connections)]
      g)))

(defn prior-edges-transposed [adj prev-row-max prev-row-min
                              prev-col-max prev-col-min
                              row-max row-min
                              col-max col-min]
  (if prev-row-max
    (let [prev-rows (- prev-row-max prev-row-min)
          prev-cols (- prev-col-max prev-col-min)
          rows (- row-max row-min)
          cols (- col-max col-min)]
      #_(log/info "=======")
      #_(log/info [prev-rows prev-cols] [rows cols])
      #_(log/info "=======")
      (cond
        (< prev-rows cols)
          ; expand prev-rows then transpose
          (let [padding-m (m/zero-matrix (int (/ (- cols prev-rows) 2))
                                         (- prev-col-max prev-col-min))]
            (m/transpose
              (m/join padding-m
                      (m/select adj (range prev-row-min prev-row-max)
                                    (range prev-col-min prev-col-max))
                      padding-m)))
        (> prev-rows cols)
          ; contract prev-rows then transpose
          (m/transpose
            (m/select adj
                      (range
                        (+ prev-row-min (int (/ (- prev-rows cols) 2)))
                        (- prev-row-max (int (/ (- prev-rows cols) 2))))
                      (range prev-col-min prev-col-max)))
        (= prev-cols rows)
          ; transpose
          (m/transpose
            (m/select adj (range prev-row-min prev-row-max)
                          (range prev-col-min prev-col-max)))
        :else
          (log/info "prior-edges-transposed :default" [prev-rows prev-cols] [rows cols])))
    (m/zero-matrix (- row-max row-min)
                   (- col-max col-min))))

(defn prune-random-edges
  [lambda-b0 lambda-b1 lambda-b2 bernoulli-b0 g]
  (let [{:keys [adj connections]} g
        m-adj (m/mutable adj)]
    (doseq [[[[col-min col-max]
              [row-min row-max]] 
             [[prev-col-min prev-col-max]
              [prev-row-min prev-row-max]]] (map vector connections
                                              (cons nil connections))]
      (let [prior-edges (prior-edges-transposed
                              m-adj prev-row-max prev-row-min
                              prev-col-max prev-col-min
                              row-max row-min
                              col-max col-min)
            #_ (log/info "prior-edges")
            #_ (m/pm prior-edges)
            #_ (log/info "--------")
            #_ (log/info (m/shape prior-edges))
            #_ (log/info [row-min row-max] [col-min col-max])
            conns (into {} (for [row (range row-min row-max)
                                 col (range col-min col-max)
                                 :let [prior-edge (m/mget prior-edges
                                                    (- row row-min)
                                                    (- col col-min))
                                      w (if (pos? prior-edge)
                                          bernoulli-b0
                                          (- 1 bernoulli-b0))
                                      #_ (log/info "w" w prior-edge [(- row row-min)
                                                                        (- col col-min)])]]
                             [[row col] (min 1 (max 0.01 w))]))
            width (- row-max row-min)
            rm-num (is/sample-poisson 1
                     ; l = b0 + w * b1 + w * w * b2
                     :lambda (+ lambda-b0
                                (* width
                                   lambda-b1)
                                (* width width lambda-b2)))
            ; create a matrix using the previous layer
            prior-edges (m/mutable (m/zero-matrix (- row-max row-min)
                                                  (- col-max col-min)))
            rm-conns (take rm-num (repeatedly (fn [] (rand-weighted conns))))]
      #_(log/info "removing" rm-num " from layer" (- row-max row-min))
      #_(log/info "weighted-cons" conns)
      #_(log/info "rm-cons" (vec rm-conns))
      (doseq [[row col] rm-conns]
        #_(log/info "removing" row col "from layer"(- row-max row-min)) 
        (m/mset! m-adj row col 0))))
    (let [g (->Graph (m/immutable m-adj) connections)]
      g)))

(defn retained [g]
  (let [g (g->loom-graph g)
        g-t (lg/transpose g)
        start 0
        end (dec (count (lg/nodes g)))
        reaches-start (la/bf-traverse g start)
        reaches-end (la/bf-traverse g-t end)
        ; if a node is reachable from start and end, then retain it
        bi-reachable (clojure.set/intersection
                       (set reaches-start)
                       (set reaches-end))]
    #_(log/info "retained from start" (set reaches-start))
    #_(log/info "retained from end" (set reaches-end))
    bi-reachable))
        
        
(defn prune-unreachable
  [g]
  (let [{:keys [adj connections]} g
        m-adj (m/mutable adj)
        retained (retained g)]
    #_(log/info "retained" retained)
    (doseq [n (remove retained (range (m/row-count adj)))]
      #_(log/info "removing node" n)
      (m/set-selection! m-adj n :all 0)
      (m/set-selection! m-adj :all n 0))
    (let [g (->Graph (m/immutable m-adj) connections)]
      ;(log/info g)
      g)))

(def db32 {
  :black [0 0 0]
  :valhalla [34 32 52]
  :loulou [69 40 60]
  :oiled-cedar [102 57 49]
  :rope [143 86 59]

  :tahiti-gold [223 113 38]
  :twine [217 160 102]
  :pancho [238 195 154]
  :golden-fizz [251 242 54]
  :atlantis [153 229 80]

  :christi [106 190 48]
  :elf-green [55 148 110]
  :dell [75 105 47]
  :verdigris [82 75 36]
  :opal [50 60 57]

  :deep-koamaru [63 63 116]
  :venice-blue [48 96 130]
  :royal-blue [91 110 225]
  :cornflower [99 155 255]
  :viking [95 205 228]

  :light-steel-blue [203 219 252]
  :white [255 255 255]
  :heather [155 173 183]
  :topaz [132 126 135]
  :dim-gray [105 106 106]

  :smokey-ash [89 86 82]
  :clairvoyant [118 66 138]
  :brown [172 50 50]
  :mandy [217 87 99]
  :plum [215 123 186]

  :rainforest [143 151 74]
  :stinger [138 111 48]})

(def black [3 5 5])

(def random-cell-type
  ; ?
  {:c \? :fg black :bg (db32 :topaz)})

(def complication-cell-type
  ; !
  {:c \! :fg black :bg (db32 :brown)})

(def remedy-cell-type
  ; +
  {:c \+ :fg black :bg (db32 :royal-blue)})

(def material-component-cell-type
  ; &
  {:c \& :fg black :bg (db32 :tahiti-gold)})

(def enhancement-cell-type
  ; ☼
  {:c \☼ :fg black :bg (db32 :christi)})

(def morale-cell-type
  ; p
  {:c \@ :fg black :bg (db32 :plum )})

(defn ch->cell-type [ch]
  (let [m {
            \? random-cell-type
            \! complication-cell-type
            \+ remedy-cell-type
            \& material-component-cell-type
            \☼ enhancement-cell-type
            \@ morale-cell-type}]
    (assert (contains? m ch) (str ch (int ch) "not found in " m))
    (get m ch)))

(defn cell-type-freqs
  [layers y]
  (if (< y (/ (count layers) 2))
    [complication-cell-type
     complication-cell-type
     material-component-cell-type
     material-component-cell-type
     remedy-cell-type
     random-cell-type]
    [remedy-cell-type
     remedy-cell-type
     material-component-cell-type
     enhancement-cell-type
     morale-cell-type
     random-cell-type]))

(defn node->row
  [layers n]
  (get
    (->> layers
      (map-indexed vector)
      (mapcat (fn [[i v]] (repeat v i)))
      (map-indexed vector)
      (into {}))
    n))

(defn node->col
  [layers n]
  (get
    (->> layers
      (mapcat range)
      (map-indexed vector)
      (into {}))
    n))

(defn node-y [layers n]
  (* 2 (node->row layers n)))

(defn node-x [layers n]
  (let [row (node->row layers n)
        col (node->col layers n)
        max-nodes (reduce max layers)
        row-num-nodes (get layers row)
        x (* 2
             (+ (/ (- max-nodes row-num-nodes) 2)
                col))]
    #_(log/info "row" row "max-nodes" max-nodes "row-num-nodes" row-num-nodes "x" x)
    (int x)))

(defn paths-contain-type?
  [f g source ch n]
  (f
    (fn [path]
      (contains? (set (map (fn [n] (get (ll/label g n) :type)) path)) ch))
    (lag/trace-paths
      (partial lg/predecessors g)
      n)))

(defn all-paths-contain-type?
  [g source ch n]
  (paths-contain-type? every?  g source ch n))

(defn any-paths-contain-type?
  [g source ch n]
  (paths-contain-type? some  g source ch n))

(defn not-any-paths-contain-type?
  [g source ch n]
  (paths-contain-type? not-any? g source ch n))

(defn draw
  [labeled-graph layers]
  (let [max-nodes (reduce max layers)
        width (dec (* 2 (reduce max layers)))
        height (dec (* 2 (count layers)))
        c (make-array Character/TYPE height width)]
    ;(log/info edges)
    (log/info labeled-graph)
    ;; fill canvas with empty chars
    (doseq [row (range height)
            col (range width)]
      (aset-char c row col \space))
    ;; fill in nodes
    (doseq [[idx n] (map-indexed vector (lg/nodes labeled-graph))
            :let [label (ll/label labeled-graph n)]]
      #_(log/info "fill node" n label layers (int (node-y layers n)) (int (node-x layers n)))
      (aset-char c
        (int (node-y layers n))
        (int (node-x layers n))
        (or (get label :type) \x)))

    ;; fill edges
    (doseq [[ni nf] (lg/edges labeled-graph)]
      (let [xi (node-x layers ni)
            yi (node-y layers ni)
            xf (node-x layers nf)
            yf (node-y layers nf)]
        ;(log/info xi yi xf yf)
        (cond
          (= xi xf)
            (aset-char c (/ (+ yi yf) 2) xi \|)
          (< xi xf)
            (aset-char c (/ (+ yi yf) 2) (/ (+ xi xf) 2) \\)
          (> xi xf)
            (aset-char c (/ (+ yi yf) 2) (/ (+ xi xf) 2) \/))))
  (for [row c]
    (for [ch row]
      (if (contains? #{\x \space \\ \| \/} ch)
        {:c ch :fg [255 255 255] :bg black}
        (ch->cell-type ch))))))

(defn log [layers]
  (doseq [row layers]
    (doseq [cell row]
      (log/info (get cell :c \o)))
    (log/info))
  layers)

(defn write-xp
  [layers path]
  (rp/write-xp (clojure.java.io/output-stream path) layers))

(defn interp [amount a b]
  (+ (* (- 1 amount) a) (* amount b)))

(defn semi-shuffle [amount xs]
  "Interpolate between sorted and shuffled. If amount = 0 then equivalent to (sort xs).
   If amount = 1 then equivalent to (shuffle xs). If amount = 0.5 then halfway between
   sorted and shuffled."
  ; k = x, v = index
  (let [sorted (into {} (map-indexed (comp vec reverse vector) (sort xs)))
        shuffled (into {} (map-indexed (comp vec reverse vector) (shuffle xs)))]
    (log/debug sorted)
    (log/debug shuffled)
    (map first
      (sort #(< (second %1) (second %2))
        (merge-with (partial interp amount) sorted shuffled)))))

(defn color-paths-once
  "Color nodes so that each path contains exactly one colored node.
   Return set of colored nodes."
  ([g] (color-paths-once
         (lag/trace-paths
           (partial lg/predecessors g)
           (reduce max 0 (lg/nodes g)))
         (clojure.set/difference
           (set (lg/nodes g))
           (set (cons (reduce max (lg/nodes g))
                      (take 4 (sort (lg/nodes g))))))
         #{}))
  ([remaining-paths potential-nodes colored-nodes]
    #_(log/info "remaining-paths" remaining-paths)
    (let [potential-nodes (semi-shuffle 0.3 (vec potential-nodes))]
      ;; any remaining paths?
      (if (seq remaining-paths)
        ; any remaining nodes?
        (if (seq potential-nodes)
          ;; pick a node
          (loop [selected-node (first potential-nodes)
                 remaining-nodes (next potential-nodes)]
                  ;; partition remining-paths into those that contain the new node
                  ;; and those that do not
            (when selected-node
              #_(log/info "selected-node" selected-node)
              ;; partition remaining paths into paths containing selected node
              ;; and paths not containing selected node
              (let [path-groups (group-by (fn [path] (contains? (set path) selected-node))
                                          remaining-paths)
                    paths-containing-node (get path-groups true)
                    paths-without-node (get path-groups false)
                    nodes-covered-by-new-paths (set (mapcat identity paths-containing-node))
                    next-potential-nodes (set (clojure.set/difference
                                            (set potential-nodes)
                                            nodes-covered-by-new-paths))
                    #_ (log/info "nodes-covered-by-new-paths" nodes-covered-by-new-paths)
                    #_ (log/info "next-potential-nodes" next-potential-nodes)
                    result (color-paths-once
                             paths-without-node
                             (semi-shuffle 0.2 (vec next-potential-nodes))
                             (conj colored-nodes selected-node))]
                    (if result
                      result
                      (recur (first remaining-nodes) (next remaining-nodes))))))
          ;; remaining paths but no potential nodes
          (do
            #_(log/info "backtracking")
            nil))
        ;; no remaining paths. return colored nodes
        colored-nodes))))


(defn next-type
  [types g n & more]
  (let [preds (lg/predecessors g n)
        parent-types (clojure.set/union
                       (set more)
                       ; parent labels
                       (set (map (partial ll/label g)
                                 preds)))]
    #_(log/info n "parents" preds)
    #_(log/info n "parent-labels" parent-types)
    (when (< (count parent-types) 4)
      (loop [c (first @types)]
        (swap! types rest)
        (if (contains? parent-types c)
          (recur (first @types))
          c)))))

(defn label-graph [layers g]
  (let [selected-nodes (color-paths-once g)
        min-node (reduce min (lg/nodes g))
        num-nodes (count (lg/nodes g))
        ; & is always first.
        ; have an even mix of types after that
        top-types (atom (cycle (mapcat shuffle (repeat (map :c (cell-type-freqs layers 0))))))
        bottom-types (atom (cycle (mapcat shuffle (repeat (map :c (cell-type-freqs layers 50))))))
        types (fn [y] (if (< y (/ (count layers) 2)) top-types bottom-types))
        labeled-graph (reduce (fn [g n]
                        #_(log/info n "selected-node?" (contains? selected-nodes n))
                        (let [x (node-x layers n)
                              y (node-y layers n)]
                        (cond
                          ; root, \&
                          (= n min-node)
                            (do
                              #_(log/info "labeling root node &")
                              (ll/add-label g n {:type \& :x x :y y}))
                          ; selected (all paths colored once)
                          (contains? selected-nodes n)
                            (do
                              #_(log/info "labeling selected-node" n)
                              (ll/add-label g n {:type \! :x x :y y}))
                          ; if paths to root contains \! but not \+
                          (and
                            (all-paths-contain-type? g min-node \! n)
                            (not-any-paths-contain-type? g min-node \+ n))
                            (do
                              #_(log/info "labeling ! descendant node" n)
                              (ll/add-label g n {:type (next-type (types y) g n) :x x :y y}))
                          :else
                            ; else, label, but not \+
                            (do
                              #_(log/info "labeling" n)
                              (ll/add-label g n {:type (next-type (types y) g n \+) :x x :y y})))))
                        g
                        (lg/nodes g))]
    (log/info "label-graph selected-nodes" selected-nodes)
    labeled-graph))


(defn gen-graph [layers [lambda-b0 lambda-b1 lambda-b2 bernoulli-b0] write-xp? progress i]
  (try
    #_(log/info (float (* 100 (/ progress (count parameters)))) "%")
    (let [g (-> layers
            fully-connected
            prune-crossed-edges
            ((partial prune-random-edges lambda-b0 lambda-b1 lambda-b2 bernoulli-b0))
            #_log
            prune-unreachable)]
      g)
      (catch Throwable e (log/error e))))


(defn gen-graph-curate [layers
                        [lambda-b0 lambda-b1 lambda-b2 bernoulli-b0 :as params]
                        write-xp? progress i]
  (log/info "gen-graph-curate")
  (letfn [(gg [] (gen-graph layers params false progress i))]
    (loop [g (gg) j 0 k 0]
      (let [loom-graph (g->loom-graph g)]
        ; select graph
        (if (and (< k 50)
                 (< (* 1.5 (count layers))
                    (count (lg/nodes loom-graph)) (* 0.8 (reduce + layers)))
                 ; make sure the last node has at least 2 connections
                 (< 1 (count (lg/predecessors loom-graph (reduce max (lg/nodes loom-graph)))) 4)
                 ; make sure there are enough decision points
                 (< 3 (reduce + (filter #(< 1 %) (map (comp count (partial lg/successors loom-graph))
                                                      (lg/nodes loom-graph))))))
          (let [labeled-graph (label-graph layers loom-graph)
                complication-nodes (filter (fn [n] (= (get (ll/label labeled-graph n) :type) \!))
                                       (lg/nodes labeled-graph))
                material-nodes (filter (fn [n] (= (get (ll/label labeled-graph n) :type) \&))
                                       (lg/nodes labeled-graph))
                node-labels (map (fn [n] (get (ll/label labeled-graph n) :type))
                                  (sort (vec (lg/nodes labeled-graph))))
                node-freqs (frequencies
                             node-labels)]

            (log/info "i" i "j" j "k" k)
            #_(log/info "question-nodes" question-nodes)
            #_(log/info "min-question" (reduce min question-nodes))
            #_(log/info "max-question" (reduce max question-nodes))
            #_(log/info "sum-question" (reduce + 0 question-nodes))
            #_(log/info "max-nodes" (* 0.8 (reduce + layers)))

            #_(log (draw labeled-graph layers))
            (log/info "or" (= 1 (count material-nodes))
                (< 1 (std-dev (map (partial node-y layers)
                                   (rest (sort material-nodes))))))
            ;; material node toward top
            (log/info "<" (/ (reduce + 0 (map (partial node-y layers) material-nodes))
                  (count material-nodes))
               (* 0.5 (count layers)))
            ;; complication nodes toward top
            (log/info "<" (/ (reduce + 0 (map (partial node-y layers) complication-nodes))
                  (inc (count complication-nodes)))
               (* 0.5 (count layers)))

            ;; select coloring
            (if (and
                  ;; no nil labels
                  (not-any? nil? node-labels)
                  ;; material node count and dispersion
                  (or (= 1 (count material-nodes))
                      (< 1 (std-dev (map (partial node-y layers)
                                         (rest (sort material-nodes))))))
                  ;; material node toward top
                  (< (/ (reduce + 0 (map (partial node-y layers) material-nodes))
                        (count material-nodes))
                     (* 0.75 (count layers)))
                  ;; complication nodes toward top
                  (< (/ (reduce + 0 (map (partial node-y layers) complication-nodes))
                        (inc (count complication-nodes)))
                     (* 0.75 (count layers)))
                  ;; at least four node types present
                  (< 4 (count (keys node-freqs)))
                  ;; not too many siblings of same type
                  (< (reduce +
                       ; count of duplicate children for each node
                       (mapcat (fn [n]
                                 ; how many duplicate children for node n
                                 (let [children-labels (cons (ll/label labeled-graph n)
                                                             (map (partial ll/label labeled-graph)
                                                               (lg/successors loom-graph n)))]
                                   (remove (partial = 1)
                                           (vals (frequencies (map :type children-labels))))))
                               (lg/nodes labeled-graph)))
                     (* 0.15 (count node-labels))))
              (let [path (str "data/params-"
                              (format "%02.2f" (float lambda-b0))
                              "-"
                              (format "%02.2f" (float lambda-b1))
                              "-"
                              (format "%02.2f" (float lambda-b2))
                              "-"
                              (format "%02.2f" (float bernoulli-b0))
                              "-"
                              i
                              "-"
                              j
                              "-"
                              k)]
                #_(-> labeled-graph
                  (draw layers)
                  (write-xp (str path ".xp")))
                {:graph labeled-graph
                 :current-node (reduce min (lg/nodes labeled-graph))
                 :layers layers
                 :img (draw labeled-graph layers)})
              (recur g j (inc k))))
          (recur (gg) (inc j) 0))))))



(defn gen-crafting-graph []
  (m/set-current-implementation :vectorz)
  (let [layers [1 3 5 5 5 3 1]]
    (gen-graph-curate layers [0.15 0.16 0.30 0.464] false 0 0)))


