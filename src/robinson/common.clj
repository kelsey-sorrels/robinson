;; Utility functions and functions for manipulating state
(ns robinson.common
  (:use     clojure.contrib.core)
  (:require [ clojure.data.generators :as dg]
            [taoensso.timbre :as timbre]
            [pallet.thread-expr :as tx]))

(timbre/refer-timbre)

(defn uniform-int
 ([hi] (uniform-int 0 hi))
 ([lo hi] (int (dg/uniform lo hi))))

(defmacro log-time
  "Log the time it takes to execute body."
  [msg & body]
  `(time
     (let [result# (do ~@body)]
       (println ~msg)
       result#)))

(defn log-io
  "Log function inputs and outputs by wrapping an function f."
  [msg f]
  (fn [& args]
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


(defn vec-match?
  [test-expr expr]
  (let [arg-match? (fn [[test-term term]]
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

(defn noun->indefinite-article [noun] (if (contains? #{\a \e \i \o \u} (first noun))
                                        "an"
                                        "a"))

(defn pos->xy
  [{x :x y :y}]
  [x y])

(defn xy->pos
  [x y]
  {:x x :y y})

(defn chebyshev-distance
  "Chebyshev/chessboard distance between 2 points"
  [p1 p2]
  (max (Math/abs (- (:x p1) (:x p2)))
       (Math/abs (- (:y p1) (:y p2)))))

(defn distance-sq
  [p1 p2]
  (let [sq (fn [x] (* x x))]
  (+ (sq (- (:x p1) (:x p2)))
     (sq (- (:y p1) (:y p2))))))

(defn distance
  "Euclidean distance between 2 points"
  [p1 p2]
  (Math/sqrt (distance-sq p1 p2)))

(defn farther-than?
  "Are the two points farther in distance than l?"
  [p1 p2 l]
  (> (distance-sq p1 p2) (* l l)))

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
                        (vec (map f coll))
                        (map f coll)))
         m ks nil))

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

(defn remove-in
  [m ks f]
  (fn-in (fn [coll _] (if (vector? coll)
                        (vec (remove f coll))
                        (remove f coll))) m ks nil))

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
  (debug "add-extras" place extras)
  (reduce (fn [place [[x y] & r]]
           (let [args (concat [[y x]] r)]
             #_(debug "assoc-in place" args)
             (apply assoc-in place args))) place extras))

(defn append-log
  "Append a message to the in-game log. The last five log messages are retained."
  ([state message]
   (append-log state message :gray))
  ([state message color]
   (assoc-in state
             [:world :log]
             (vec (take-last 23 (conj (-> state :world :log)
                                     {:text message
                                      :time (-> state :world :time)
                                      :color color}))))))

(defn ui-hint
  [state msg]
  (assoc-in state [:world :ui-hint] msg))

(defn clear-ui-hint
  [state]
  (ui-hint state nil))

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


