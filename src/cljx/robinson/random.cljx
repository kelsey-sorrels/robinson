;; Utility functions and functions for manipulating state
(ns robinson.random
  (:use     clojure.contrib.core)
  (:require [clojure.data.generators :as dg]
            [clojure.core.typed :as t]
            [taoensso.timbre :as timbre]
            [pallet.thread-expr :as tx]))

(timbre/refer-timbre)

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
                                      (ref 0)
                                      (instance? RandomState seed-or-state)
                                      (ref (get seed-or-state :next-next-gaussian))
                                      :else
                                      (throw "create-random requires either a number or RandomState"))
        have-next-next-gaussian (cond (number? seed-or-state)
                                      (ref false)
                                      (instance? RandomState seed-or-state)
                                      (ref (get seed-or-state :havenext-next-gaussian))
                                      :else
                                      (throw "create-random requires either a number or RandomState"))
        next! (fn [bits]
                (int (bit-and 0x7FFFFFFF
                              (unsigned-bit-shift-right
                                (swap! seed (fn [s]
                                              (bit-and
                                                (unchecked-add
                                                  (unchecked-multiply (long s)
                                                                      (long multiplier))
                                                  addend)
                                                 mask)))
                                (- 48 bits)))))]
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
        (dosync
          (if @have-next-next-gaussian
            (do
              (ref-set have-next-next-gaussian false)
              @next-next-gaussian)
            (loop [v1 (dec (* 2 (next-double! this)))
                   v2 (dec (* 2 (next-double! this)))
                   s  (+ (* v1 v1) (* v2 v2))]
              (if (or (>= s 1) (zero? s))
                (let [v1 (dec (* 2 (next-double! this)))
                      v2 (dec (* 2 (next-double! this)))
                      s  (+ (* v1 v1) (* v2 v2))]
                  (recur v1 v2 s))
                (let [multiplier #+clj (StrictMath/sqrt (* -2 (/ (StrictMath/log s) s)))
                                 #+cljs (.sqrt js/Math (* -2 (/ (.log js/Math s) s)))]
                  (ref-set next-next-gaussian (* v2 multiplier))
                  (ref-set have-next-next-gaussian true)
                  (* v1 multiplier)))))))
     (get-state [this]
       (RandomState. @seed @next-next-gaussian @have-next-next-gaussian)))))

(def ^:dynamic
  *rnd*
  (create-random 42))

(defn uniform-int
 ([hi] {:pre [(pos? hi)]} (next-int! *rnd* hi))
 ([lo hi] {:pre [(< lo hi)]} (+ lo (next-int! *rnd* (- hi lo)))))

(defn uniform-double
 ([hi] {:pre [(pos? hi)]} (* hi (next-double! *rnd*)))
 ([lo hi] {:pre [(< lo hi)]} (+ lo (* (next-double! *rnd*) (- hi lo)))))

(defn- swap [v i1 i2]
  (assoc v i2 (nth v i1) i1 (nth v i2)))

(defn rnd-shuffle
  ([coll]
  (rnd-shuffle *rnd* coll))
  ([rnd coll]
  (loop [i (count coll)
         coll coll]
    (if (> i 1)
      (recur (dec i) (swap coll (dec i) (next-int! rnd i)))
      coll))))


