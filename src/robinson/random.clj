;; Utility functions and functions for manipulating state
(ns robinson.random
  (:require [clojure.string])
  (:refer-clojure :exclude [mask rand-nth]))


(defrecord RandomState [seed have-next-next-gaussian next-next-gaussian])

(defprotocol Random
  (next-int! [this]
             [this n])
  (next-float! [this])
  (next-long! [this])
  (next-boolean! [this])
  (next-double! [this])
  (next-gaussian! [this])
  (get-state [this]))


(def ^:private addend 0xB)
(def ^:private multiplier 0x5DEECE66D)
(def ^:private mask (dec (bit-shift-left 1 48)))


(defn create-random
  [seed-or-state]
  (let [seed                    (cond (number? seed-or-state)
                                      (atom seed-or-state)
                                      (instance? RandomState seed-or-state)
                                      (atom (get seed-or-state :seed))
                                      :else
                                      (throw "create-random requires either a number or RandomState"))
        next-next-gaussian      (cond (number? seed-or-state)
                                      (atom 0)
                                      (instance? RandomState seed-or-state)
                                      (atom (get seed-or-state :next-next-gaussian))
                                      :else
                                      (throw "create-random requires either a number or RandomState"))
        have-next-next-gaussian (cond (number? seed-or-state)
                                      (atom false)
                                      (instance? RandomState seed-or-state)
                                      (atom (get seed-or-state :havenext-next-gaussian))
                                      :else
                                      (throw "create-random requires either a number or RandomState"))
        next! (fn [bits]
                (let [new-seed (swap! seed (fn [s]
                                             (bit-and
                                               (unchecked-add
                                                 (unchecked-multiply (long s)
                                                                     (long multiplier))
                                                 addend)
                                                mask)))]
                (int (bit-and 0x7FFFFFFF
                              (unsigned-bit-shift-right
                                new-seed
                                (- 48 bits))))))]
    (reify Random
      (next-int! [this]
        (next! 32))
      (next-int! [this n]
        (if (= n (bit-and n (- n)))
          (int (bit-shift-right (unchecked-multiply n (long (next! 31)))
                                31))
          (loop [bits   (next! 31)
                 value  (mod bits n)]
            (if (neg? (- bits (+ value (dec n))))
              (let [bits  (next! 31)
                    value (mod bits n)]
                (recur bits value))
              value))))
      (next-long! [this]
        (+ (long (next! 32)) (next! 32)))
      (next-boolean! [this]
        (not= (next! 1) 0))
      (next-float! [this]
        (/ (next! 24) (float (bit-shift-left 1 24))))
      (next-double! [this]
        (/ (+ (bit-shift-left (long (next! 26)) 27)
              (next! 27))
           (double (bit-shift-left 1 53))))
      (next-gaussian! [this]
        (if @have-next-next-gaussian
          (do
            (reset! have-next-next-gaussian false)
            @next-next-gaussian)
          (loop [v1 (dec (* 2 (next-double! this)))
                 v2 (dec (* 2 (next-double! this)))
                 s  (+ (* v1 v1) (* v2 v2))]
            (if (or (>= s 1) (zero? s))
              (let [v1 (dec (* 2 (next-double! this)))
                    v2 (dec (* 2 (next-double! this)))
                    s  (+ (* v1 v1) (* v2 v2))]
                (recur v1 v2 s))
              (let [multiplier (StrictMath/sqrt (* -2 (/ (StrictMath/log s) s)))]
                (reset! next-next-gaussian (* v2 multiplier))
                (reset! have-next-next-gaussian true)
                (* v1 multiplier))))))
     (get-state [this]
       (RandomState. @seed @next-next-gaussian @have-next-next-gaussian)))))

(def ^:dynamic
  *rnd*
  (create-random 42))

(defn set-rnd! [rnd]
  (alter-var-root *rnd* (constantly rnd)))

(defn uniform-int
 ([] (next-int! *rnd* 0xFFFFFFFF))
 ([hi] {:pre [(pos? hi)]} (next-int! *rnd* (int hi)))
 ([lo hi] {:pre [(<= lo hi)]} (+ (int lo) (next-int! *rnd* (- (int hi) (int lo)))))
 ([rnd lo hi] {:pre [(<= lo hi)]} (+ (int lo) (next-int! rnd (- (int hi) (int lo))))))

(defn uniform-double
 ([] (uniform-double 1.0))
 ([hi] {:pre [(pos? hi)]} (* hi (next-double! *rnd*)))
 ([lo hi] {:pre [(< lo hi)]} (+ lo (* (next-double! *rnd*) (- hi lo))))
 ([rnd lo hi] {:pre [(< lo hi)]} (+ lo (* (next-double! rnd) (- hi lo)))))

(defn rand-bool
 ([] (rand-bool 0.5))
 ([threshold] (> (uniform-double) threshold)))

(defn- swap [v i1 i2]
  (assoc v i2 (nth v i1) i1 (nth v i2)))

(defn rnd-shuffle
  ([coll]
  (rnd-shuffle *rnd* coll))
  ([rnd coll]
  (let [coll (vec coll)]
    (loop [i (count coll)
           coll coll]
      (if (> i 1)
        (recur (dec i) (swap coll (dec i) (next-int! rnd i)))
        coll)))))

(defn rand-nth
  ([coll]
  (when (seq coll)
    (rand-nth *rnd* coll)))
  ([rnd coll]
  (nth coll (uniform-int rnd 0 (count coll)))))

(defn rand-weighted-nth
  ([m]
    (rand-weighted-nth *rnd* m))
  ([rnd m]
   "Randomly select a value from a list of [weight value]'ss."
   (cond
     (empty? m)
       nil
     (= 1 (count m))
       (-> m first second)
     :else
       (let [[wm a]     (reduce (fn [[m a] [weight v]]
                                  [(conj m [a v]) (+ weight a)])
                                [[] 0] m)
             n          (uniform-double rnd 0 a)]
         (second (last (remove (fn [[wn _]] (> wn n)) wm)))))))

(defn die
 [expr]
 (let [[n-str d-str] (clojure.string/split expr #"d")]
   (try
     (let [n (Integer/parseInt n-str)
           d (Integer/parseInt d-str)]
       (->> n
         (repeat 1)
         (map (fn [_] (inc (rand-nth (range d)))))
         (reduce +)))
     (catch Throwable t
       nil))))
   
