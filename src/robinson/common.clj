;; Utility functions and functions for manipulating state
(ns robinson.common
  (:use     clojure.contrib.core)
  (:require [clojure.data.generators :as dg]
            [clojure.core.typed :as t]
            [taoensso.timbre :as timbre]
            [pallet.thread-expr :as tx])
  (:import  (clojure.lang ITransientCollection)))

(t/ann ^:no-check taoensso.timbre/refer-timbre [-> t/Any])
(timbre/refer-timbre)

(t/ann ^:no-check clojure.data.generators/uniform [Long Long -> Long])
(t/ann uniform-int (t/IFn [Long -> Long]
                          [Long Long -> Long]))
(defn uniform-int
 ([hi] {:pre [(pos? hi)]} (uniform-int 0 hi))
 ([lo hi] {:pre [(< lo hi)]} (dg/uniform lo hi)))

(defmacro log-time
  "Log the time it takes to execute body."
  [msg & body]
  `(time
     (let [result# (do ~@body)]
       (println ~msg)
       result#)))

(t/ann log-io (t/All [a x y ...]
                [String [(t/U t/EmptySeqable (t/HSeq (y ... y))) -> x] -> [(t/U t/EmptySeqable (t/HSeq (y ... y))) -> x]]))
(defn log-io
  "Log function inputs and outputs by wrapping an function f."
  [msg f]
  (fn [args]
  (let [result (apply f args)]
    (println (format "(%s %s)=>%s" msg (str args) (str result)))
    result)))

(letfn [(arg-when- [threader arg sym condition body]
  `(let [~sym ~arg
         arg# ~arg]
    (if ~condition
      (~threader arg# ~@body)
      arg#)))]
  (defmacro arg-when->
    "Lexically assign the threaded argument to the specified symbol
    and conditionally execute the body statements.
    (-> 3
      (arg-when-> [x] (> x 2) (* x)))
    (-> 2
      (arg-when-> [x] (> x 2) (* x)))
    => 2"
    {:indent 1}
    [arg [sym] condition & body]
    (arg-when- 'clojure.core/-> arg sym condition body)))

(letfn [(arg-if- [threader arg sym condition form else-form]
  `(let [~sym ~arg
         arg# ~arg]
    (if ~condition
      (~threader arg# ~form)
      (~threader arg# ~else-form))))]
  (defmacro arg-if->
    "Lexically assign the threaded argument to the specified symbol
    and conditionally execute the body statements.
    (-> 3
      (arg-when-> [x] (> x 2) (* x)))
    (-> 2
      (arg-when-> [x] (> x 2) (* x)))
    => 2"
    {:indent 1}
    [arg [sym] condition form else-form]
    (arg-if- 'clojure.core/-> arg sym condition form else-form)))

(t/ann ^:no-check vec-match? [(t/Vec t/Any) (t/Vec t/Any) -> Boolean])
(defn vec-match?
  [test-expr expr]
  (let [arg-match? (t/fn [[test-term term] :- (t/I (t/Vec t/Any) (t/ExactCount 2))] :- Boolean
    (cond
      (fn? test-term)  (test-term term)
      (= :* test-term) true
      (set? test-term) (contains? test-term term)
      :else       (= test-term term)))]
  (every? arg-match? (map vector test-expr expr))))

(defmacro first-vec-match
  [match & body]
  `(condp vec-match? ~match
     ~@body))

(t/ann noun->indefinite-article [String -> String])
(defn noun->indefinite-article [noun] (if (contains? #{\a \e \i \o \u} (first noun))
                                        "an"
                                        "a"))

(t/defalias Pos (t/HMap :mandatory {:x Integer :y Integer} :complete? true))

(t/ann pos->xy [Pos -> (t/I (t/Vec Integer)(t/ExactCount 2))])
(defn pos->xy
  [{x :x y :y}]
  [x y])

(t/ann xy->pos [Integer Integer -> Pos])
(defn xy->pos
  [x y]
  {:x x :y y})

(t/ann ^:no-check Math/abs (t/IFn [Double -> Double]
                                  [Float -> Float]
                                  [Integer -> Integer]
                                  [Long -> Long]))
(t/ann chebyshev-distance [Pos Pos -> Long])
(defn chebyshev-distance
  "Chebyshev/chessboard distance between 2 points"
  [p1 p2]
  (max (Math/abs (long (- (get p1 :x) (get p2 :x))))
       (Math/abs (long (- (get p1 :y) (get p2 :y))))))

(t/ann distance-sq [Pos Pos -> t/AnyInteger])
(defn distance-sq
  [p1 p2]
  (let [sq (t/fn [x :- t/AnyInteger] :- t/AnyInteger (* x x))]
  (+ (sq (- (:x p1) (:x p2)))
     (sq (- (:y p1) (:y p2))))))

(t/ann distance [Pos Pos -> Double])
(defn distance
  "Euclidean distance between 2 points"
  [p1 p2]
  (Math/sqrt (double (distance-sq p1 p2))))

(t/ann farther-than? [Pos Pos Number -> Boolean])
(defn farther-than?
  "Are the two points farther in distance than l?"
  [p1 p2 l]
  (> (distance-sq p1 p2) (* l l)))

(t/ann fill-missing (t/All [x y] [(t/Pred x)
                                   (t/IFn [x y -> x])
                                   (t/Seq y)
                                   (t/I t/EmptyCount (t/Seq x)) -> (t/Seq x)]))
                                   
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
                xs (next coll)
                y  (first vcoll)
                ys (next vcoll)]
                  (if (pred x)
                            (cons (f x y) (if (empty? xs) [] (fill-missing pred f ys xs)))
                            (cons x (if (empty? xs) [] (fill-missing pred f vcoll xs)))))))

(t/ann ^:no-check clojure.core/update-in (t/All [x [y :< (clojure.lang.Associative t/Any t/Any)]]
                                           [y (t/Seqable t/Any) [t/Any -> t/Any] -> y]))
(t/ann fn-in (t/All [x
                     [y :< (clojure.lang.Associative t/Any t/Any)]]
               [[t/Any x -> t/Any] y (t/Seqable t/Any) x -> y]))
(defn fn-in
  "Applies a function to a value in a nested associative structure and an input value.
   ks sequence of keys and v is the second arguement to f. The nested value will be
   updated to (f o v) where o is the value that would be returned by (get-in m ks)."
  [f m ks v]
  (update-in m ks (fn [coll] (f coll v))))

(t/ann concat-in (t/All [[x :< (clojure.lang.Associative t/Any t/Any)]]
                   [x (t/Seqable t/Any) (t/Seqable t/Any) -> x]))
(defn concat-in
  [m ks v]
  (fn-in concat m ks v))

(t/ann conj-in (t/All [x]
                 (t/IFn [x (clojure.lang.Associative t/Any t/Any) t/Any -> x])))
(defn conj-in
  [m ks v]
  (fn-in conj m ks v))

(t/ann map-in (t/All [x]
                [x (clojure.lang.Associative t/Any t/Any) [t/Any -> t/Any] -> x]))
(defn map-in
  [m ks f]
  (fn-in (fn [coll _] (if (vector? coll)
                        (vec (map f coll))
                        (map f coll)))
         m ks nil))

(t/ann reduce-in (t/All [x]
                   (t/IFn [x (clojure.lang.Associative t/Any t/Any) (t/IFn [t/Any -> t/Any]) -> x]
                          [x (clojure.lang.Associative t/Any t/Any) (t/IFn [t/Any -> t/Any]) t/Any -> x])))
(defn reduce-in
  "Reduce a value inside an associative datastructure. `more` can be either
  a reducing function, a reducting function and an initial value."
  ([m ks f]
   (update-in m ks (fn [coll] (reduce f coll))))
  ([m ks f v]
   (update-in m ks (fn [coll] (reduce f v coll)))))

(t/ann filter-in (t/All [x]
                   (t/IFn [x (clojure.lang.Associative t/Any t/Any) (t/IFn [t/Any -> t/Any]) -> x])))
(defn filter-in
  [m ks f]
  (fn-in (fn [coll _] (if (vector? coll)
                        (vec (filter f coll))
                        (filter f coll))) m ks nil))

(t/ann remove-in (t/All [x]
                   [x (clojure.lang.Associative t/Any t/Any) [t/Any -> t/Any] -> x]))
(defn remove-in
  [m ks f]
  (fn-in (fn [coll _] (if (vector? coll)
                        (vec (remove f coll))
                        (remove f coll))) m ks nil))

(t/ann some-in (t/All [x]
                 (t/IFn [x (clojure.lang.Associative t/Any t/Any) (t/IFn [t/Any -> t/Any]) -> x])))
(defn some-in
  [m ks f]
  (fn-in (fn [coll _] (if (vector? coll)
                        (vec (some f coll))
                        (some f coll))) m ks nil))

(t/ann update-in-matching (t/All [a b]
                            (t/IFn [a (clojure.lang.Associative t/Any t/Any) (t/IFn [b -> Boolean]) (t/IFn [b -> t/Any]) -> a])))
(defn update-in-matching
  [m ks p f]
  (if (fn? p)
    (map-in m ks (fn [e] (if (p e)
                           (f e)
                           e)))
    (update-in-matching m ks (partial = p) f)))

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
      (debug "loop i" i "j" j "count coll" (count coll))
      (if (>= j (count coll))
        (do (debug result)
        result)
        (let [item (nth coll j)]
          (debug "item" item)
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


(defn add-extras
  "Adds extras to a place like items, and special cell types 
   extras are in the format of `[[[x y] object] [[x y] object] &]` 
   objects are cells with a type and maybe items `{:type :floor :items []}`"
  [place extras]
  ;; create a list of functions that can be applied to assoc extras, then create a composition of
  ;; so that setting can pass through each fn in turn.
  #_(debug "add-extras" place extras)
  (reduce (fn [place [[x y] & r]]
           (let [args (concat [[y x]] r)]
             #_(debug "assoc-in place" args)
             (apply assoc-in place args))) place extras))

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
(t/defalias State (HMap :mandatory {:world World :screen t/Any}))

(t/ann append-log (t/IFn [State String -> State]
                        [State String t/Kw -> State]))
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
                                    :time (-> state :world :time)
                                    :color color}]))))))

(t/ann ui-hint (t/IFn [State String -> State]))
(defn ui-hint
  [state msg]
  (assoc-in state [:world :ui-hint] msg))

(t/ann clear-ui-hint (t/IFn [State String -> State]))
(defn clear-ui-hint
  [state]
  (ui-hint state nil))

(t/ann wrap-line (t/IFn [Integer String -> (t/Vec String)]))
(defn wrap-line [size text]
  (loop [left size line [] lines []
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

(defn make-gen-fns [name-space id->obj-map]
 (doseq [id (keys id->obj-map)]
   (let [sym (->> id name (str "gen-") symbol)]
   (intern name-space sym (fn [] (get id->obj-map id))))))

