;; Utility functions and functions for manipulating state
(ns robinson.common
  (:require 
            [robinson.math :as math]
            [taoensso.timbre :as log]
            [alandipert.enduro :as enduro]))

(defn log-io
  "Log function inputs and outputs by wrapping an function f."
  [msg f]
  (fn [& args]
  (let [result (apply f args)]
       (println (format "(%s %s)=>%s" msg (str args) (str result)))
    result)))

(defn noun->indefinite-article [noun] (if (contains? #{\a \e \i \o \u} (first noun))
                                        "an"
                                        "a"))

(defn has-keys? [m keys]
  (apply = (map count [keys (select-keys m keys)])))

(defn position?
  [pos]
  (= #{:x :y} (-> pos keys set)))

(defn add-pos [pos & poss]
  {:x (reduce + (:x pos) (map :x poss))
   :y (reduce + (:y pos) (map :y poss))})

(defn sub-pos [pos & poss]
  {:x (reduce - (:x pos) (map :x poss))
   :y (reduce - (:y pos) (map :y poss))})

(defn midpoint [pos & poss]
  {:x (/ (reduce + (:x pos) (map :x poss)) (inc (count poss)))
   :y (/ (reduce + (:y pos) (map :y poss)) (inc (count poss)))})

(defn cross
  [u v]
  {:x (- (* (:y u) (:z v)) (* (u :z) (v :y)))
   :y (- (* (:z u) (:x v)) (* (u :x) (v :z)))})

(defn tangent
  [pos]
  (cross (assoc pos :z 0)
         {:x 0 :y 0 :z 1}))

(defn scale
  [a pos]
  {:x (* a (pos :x))
   :y (* a (pos :y))})
  
(defn pos->xy
  [{x :x y :y}]
  [x y])

(defn xy->pos
  [x y]
  {:x x :y y})

(defn chebyshev-distance
  "Chebyshev/chessboard distance between 2 points"
  [p1 p2]
  (max (math/abs (long (- (p1 :x) (p2 :x))))
       (math/abs (long (- (p1 :y) (p2 :y))))))

(defn distance-sq
  [p1 p2]
  (let [sq (fn [x] (* x x))]
  (+ (sq (- (:x p1) (:x p2)))
     (sq (- (:y p1) (:y p2))))))

(defn distance
  "Euclidean distance between 2 points"
  [p1 p2]
  (math/sqrt (double (distance-sq p1 p2))))

(defn farther-than?
  "Are the two points farther in distance than l?"
  ([p1 p2 l]
  (> (distance-sq p1 p2) (* l l)))
  ([x1 y1 x2 y2 l]
  (if (or (> (math/abs (- x1 x2)) l)
          (> (math/abs (- y1 y2)) l))
    true
    (> (distance-sq (xy->pos x1 y1) (xy->pos x2 y2)) (* l l)))))
  
(defn nearest
  [x y [xy & xys]]
  (let [pos (xy->pos x y)]
    (loop [[tx ty :as xy] xy
            d (distance-sq
                pos
                (apply xy->pos xy))
            xys xys]
      (if-let [[nx ny :as nxy] (first xys)]
        (let [nd (distance-sq
                   pos
                   (xy->pos nx ny))]
          (if (< nd d)
            (recur nxy nd (next xys))
            (recur xy d (next xys))))
        [[tx ty] (Math/sqrt d)]))))

(defn xy-in-range?
  "Any of the xys close enough (< d) to x y?"
  [x y d xys]
  (let [pos (xy->pos x y)]
    (some (fn [[x y]] (not (farther-than? pos (xy->pos x y) d)))
      xys)))
 
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

(defn find-point-relation-ext
  [[start-x start-y] [end-x end-y]]
  (let [dx (- end-x start-x)
        dy (- end-y start-y)
        two-pi (* 2 Math/PI)
        pi-8 (/ Math/PI 8)
        dirs [
              :right
              :up-right
              :up
              :up-left
              :left
              :down-left
              :down
              :down-right
            ]
        theta (Math/atan2 (- dy) dx)
        theta (if (neg? theta)
                (+ two-pi theta)
                theta)
        idx (* (/ (+ theta pi-8) two-pi) (count dirs))]
    (log/info idx)
    (nth dirs idx)))

(defn bound [min-v v max-v]
  (min max-v (max min-v v)))

(def directions-ext
  #{:left :right :down :up :up-left :up-right :down-left :down-right :center})

(defn is-direction?
  [keyin]
  (contains? #{:left :down :up :right} keyin))

(defn is-direction-ext?
  [keyin]
  (or (is-direction? keyin)
      (contains? #{:up-left :up-right :down-left :down-right} keyin)))

(defn direction->offset-pos [direction]
  {:x
    (case direction
      (:left :up-left :down-left) -1
      (:right :up-right :down-right) 1
      0)
   :y
    (case direction
      (:up :up-left :up-right) -1
      (:down :down-left :down-right) 1
      0)})

(defn direction->path
  [start-pos direction length]
  (take length (iterate (fn [pos] (add-pos pos (direction->offset-pos direction))) start-pos)))

(defn fill-missing
  "For each item in coll for which (pred item) returns true, replace that
   element with the result of (f item vcoll-item) where vcoll-item
   starts with (first vcoll) and proceeds to the next element each
   time (pred item) evaluates to true. If the collection vcoll is exhausted,
   nil will fill the remaining values.

       user=> (fill-missing
                #(not (contains? % :val))
                #(assoc %1 :val %2)
                [1 2 3]
                [{:val :a}
                 {}
                 {:val :b}
                 {:val :c}
                 {}
                 {}
                 {}])
       ({:val :a}
        {:val 1}
        {:val :b}
        {:val :c}
        {:val 2}
        {:val 3}
        {:val nil})"
    [pred f vcoll coll]
    (if (empty? coll)
          coll
          (let [x  (first coll)
                xs (rest coll)
                y  (first vcoll)
                ys (rest vcoll)]
                  (if (pred x)
                            (cons (f x y) (if (empty? xs) [] (fill-missing pred f ys xs)))
                            (cons x (if (empty? xs) [] (fill-missing pred f vcoll xs)))))))

(defn fn-in
  "Applies a function to a value in a nested associative structure and an input value.
   ks sequence of keys and v is the second arguement to f. The nested value will be
   updated to (f o v) where o is the value that would be returned by (get-in m ks)."
  [f m ks v]
  (update-in m ks (fn [coll] (f coll v))))

(defn concat-in
  [m ks v]
  (fn-in concat m ks v))

(defn conj-in
  [m ks v]
  (fn-in conj m ks v))

(defn map-in
  [m ks f]
  (fn-in (fn [coll _] (if (vector? coll)
                        (mapv f coll)
                        (map f coll)))
         m ks nil))

(defn reduce-in
  "Reduce a value inside an associative datastructure. `more` can be either
  a reducing function, a reducting function and an initial value."
  ([m ks f]
   (update-in m ks (fn [coll] (reduce f coll))))
  ([m ks f v]
   (update-in m ks (fn [coll] (reduce f v coll)))))

(defn filter-in
  [m ks f]
  (fn-in (fn [coll _] (if (vector? coll)
                        (vec (filter f coll))
                        (filter f coll))) m ks nil))

(defn remove-in
  [m ks f]
  (fn-in (fn [coll _] (if (vector? coll)
                        (vec (remove f coll))
                        (remove f coll))) m ks nil))

(defn some-in
  [m ks f]
  (fn-in (fn [coll _] (if (vector? coll)
                        (vec (some f coll))
                        (some f coll))) m ks nil))

(defn update-in-matching
  [m ks p f & args]
  (if (fn? p)
    (map-in m ks (fn [e] (if (p e)
                           (apply f e args)
                           e)))
    (apply update-in-matching m ks (partial = p) f args)))


(defn dissoc-in
  [m ks]
  (update-in m (butlast ks) (fn [v] (dissoc v (last ks)))))

(defn map-indexed-in-p
  "Update in m the collection accessible by (get-in m ks)
   for which (p item) returns true for an item in the collection
   is replaced with the value of (f index element) where index
   starts at zero and is incremented with each call to f."
  [m ks p f]
  (update-in m ks (fn [coll]
    (loop [i 0
           j 0
           result []]
      (log/debug "loop i" i "j" j "count coll" (count coll))
      (if (>= j (count coll))
        result
        (let [item (nth coll j)]
          (log/debug "item" item)
          (if (p item)
            (recur (inc i) (inc j) (conj result (f i item)))
            (recur i (inc j) (conj result item)))))))))

(defn remove-first
  "Removes the first matching element from coll."
  [e coll]
  (if-not (fn? e)
    (remove-first (partial = e) coll)
    (let [[l1 l2] (split-with (complement e) coll)]
      (concat l1 (rest l2)))))

(defmacro fn-cond->
  "Like cond-> but threads values through test functions as well."
  [expr & clauses]
  (let [g (gensym)
        steps (map (fn [[test step]] `(if (~test ~g) (-> ~g ~step) ~g))
                   (partition 2 clauses))]
    `(let [~g ~expr
           ~@(interleave (repeat g) (butlast steps))]
       ~(if (empty? steps)
          g
          (last steps)))))

(comment
(t/defalias HasHotkey (t/HMap :mandatory {:hotkey Character}))
(t/defalias Item (t/HMap :mandatory {:id t/Kw :name String :name-plural String}
                  :optional  {:fuel Number :utility Number :attack t/Kw :hunger Number :thirst Number
                              :recoverable-item (t/Vec t/Kw) :type t/Kw}))
(t/defalias Cell (t/HMap :mandatory {:type t/Kw}
                         :optional {:items (t/Vec Item)}))
(t/defalias Place (t/Vec (t/Vec (t/U Cell nil))))
(t/defalias Npc   (t/HMap :mandatory {:race t/Kw :name String :name-plural String :energy Number :speed Number :strength Number :toughness Number
                              :attacks (t/Set t/Kw) :temperment t/Kw :movement-policy t/Kw :range-threshold-status (t/Set t/Kw)}))
(t/defalias Player (t/HMap :mandatory {:id t/Kw :name String :race t/Kw :class t/Kw :movement-policy t/Kw :in-party Boolean
                    :inventory (t/Vec (t/I Item HasHotkey)) :dexterity Number :speed Number :size Number
                    :strength Number :toughness Number :hp Number :max-hp Number :will-to-live Number
                    :max-will-to-live Number :money Number :xp Number :level Number :hunger Number
                    :max-hunger Number :thirst Number :max-thirst Number :pos Pos :starting-pos Pos
                    :place t/Kw :body-parts (t/Set t/Kw) :attacks (t/Set t/Kw) :status (t/Set t/Kw)
                    :stats (t/HMap :mandatory {:timeline (t/Vec t/Any) :num-animals-killed (t/Map t/Kw Number)
                                             :num-items-crafted (t/Map t/Kw Number)
                                             :num-items-harvested (t/Map t/Kw Number)
                                             :num-kills-by-attack-type (t/Map t/Kw Number)
                                             :num-items-eaten (t/Map t/Kw Number)})
                    :wounds (t/Map t/Kw Number)}))
(t/defalias World (t/HMap :mandatory {:seed Number :block-size (t/Map t/Kw Number) :width Number :height Number
                               :viewport (t/HMap :mandatory {:width Number :height Number :pos Pos})
                               :places (t/Map t/Kw Place)
                               :current-place t/Kw
                               :volcano-pos Pos
                               :lava-points (t/Vec Pos)
                               :time Integer
                               :current-state t/Kw
                               :selected-hotkeys (t/Set Character)
                               :remaining-hotkeys (t/Set Character)
                               :log (t/Vec t/Any)
                               :ui-hint (t/U String nil)
                               :dialog-log (t/Vec t/Any)
                               :player Player
                               :fruit (t/HMap :mandatory {:poisonous (t/Set t/Kw) :skin-identifiable (t/Set t/Kw) :tounge-identifiable (t/Set t/Kw) :identified (t/Set t/Kw)})
                               :frogs (t/HMap :mandatory {:poisonous (t/Set t/Kw)})
                               :quests (t/Map t/Kw t/Any)
                               :npcs (t/Vec Npc)}))
(t/defalias State (HMap :mandatory {:world World :screen t/Any})))

;; Indicate that the player action advanced time (this will include getting hungrier/thirstier, updating visibility, monsters attacking, updating cells, etc.
(defn inc-time
  [state]
  (log/info "inc-time=true")
  (assoc-in state [:world :inc-time] true))

(defn inc-time?
  [state]
  (get-in state [:world :inc-time] false))

(defn clear-inc-time
  [state]
  (log/info "inc-time = false")
  (dissoc-in state [:world :inc-time]))

(defn append-log
  "Append a message to the in-game log. The last five log messages are retained."
  ([state message]
   (append-log state message :gray))
  ([state message color]
   (assoc-in state
             [:world :log]
             (vec (take-last 23 (concat
                                  (-> state :world :log)
                                  [{:text message
                                    :time (+ (-> state :world :time)
                                             ;; if inc-time has been set, then time will advance but has not yet been set
                                             ;; add a 1 in this case so that the log message will reflect the correct time
                                             (if (inc-time? state)
                                               1
                                               0))
                                    :color color}]))))))

(defn ui-hint
  [state msg]
  (assoc-in state [:world :ui-hint] msg))

(defn clear-ui-hint
  [state]
  (ui-hint state nil))

(defn wrap-line [size text]
  (loop [left size
         line []
         lines []
         words (clojure.string/split text #"\s+")]
    (if-let [word (first words)]
      (let [wlen (count word)
            spacing (if (== left size) "" " ")
            alen (+ (count spacing) wlen)]
        (if (<= alen left)
          (recur (- left alen) (conj line spacing word) lines (next words))
          (recur (- size wlen) [word] (conj lines (clojure.string/join line)) (next words))))
      (when (seq line)
        (conj lines (clojure.string/join line))))))


(def hotkeys
  (vec (seq "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")))

(defn get-settings
  [state]
  (-> state :settings deref))

(defn reset-settings!
  [state new-settings]
  (let [settings (get state :settings)]
    (enduro/reset! settings new-settings)))

(defn system-time-millis []
  (System/currentTimeMillis))

(defn turns-to-time
  [t]
  (let [mins (* t 4.18)
        hours (quot mins 60)
        mins-rem (rem mins 60)]
    {:hours (int hours)
     :mins (int mins)
     :mins-rem (int mins-rem)}))
