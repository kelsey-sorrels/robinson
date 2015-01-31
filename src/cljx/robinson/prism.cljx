;; Utility functions and functions for manipulating vector and scalar fields
(ns robinson.prism
  (:use 
        robinson.common)
  (:require 
            [robinson.noise :as rn]
            [taoensso.timbre :as timbre]))

(defn invert
  [f]
  (fn [[x y]]
    (+ 1 (- (f [x y])))))

(defn wrap-constant
  [c]
  (fn [& _]
    (constantly c)))

(defn wrap-arg
  [xy-or-fn]
  (cond
    (vector? xy-or-fn)
      (wrap-constant xy-or-fn)
    (fn? xy-or-fn)
      xy-or-fn))

(defn coerce [f]
  (fn [x y]
    (f [x y])))

(defn noise [n]
  (fn [[x y]]
    (rn/noise n x y)))

(defn snoise [n]
  (fn [[x y]]
    (rn/snoise n x y)))

(defn offset
  [xy-or-fn f]
  (cond
    (vector? xy-or-fn)
      (let [[x y] xy-or-fn]
        (fn [[xi yi]]
          (f [(+ xi x) (+ yi y)])))
    (fn? xy-or-fn)
      (fn [[xi yi]]
        (let [[x y] (xy-or-fn [xi yi])]
          (f [(+ xi x) (+ yi y)])))))

(defn scale [s f]
  (fn [[x y]]
    (let [s (/ 1 s)]
      (f [(* s x) (* s y)]))))

(defn center [f]
  (fn [[x y]]
    (offset [-0.5 -0.5] (scale 0.5 f))))

(defn radius []
  (fn [[x y]]
    #+clj  (Math/sqrt (+ (* x x) (* y y)))
    #+cljs (.sqrt js/Math (+ (* x x) (* y y)))))

(defn center-radius []
  (fn [[x y]]
    (center (radius (xy->pos x y)))))

(defn vsnoise
  [fnoise]
  (fn [[x y]]
    (vec (fnoise x y) (fnoise (+ x 12.301) (+ y 70.261)))))

(defn vnoise
  [noise]
  (fn [[x y]]
    (vec (rn/noise noise x y) (rn/noise noise (+ x -78.678) (+ y 7.6789)))))

(defn v+
  [xy-or-fn-0 xy-or-fn-1]
  (cond
    (and (vector xy-or-fn-0)
         (vector xy-or-fn-1))
      (mapv + xy-or-fn-0 xy-or-fn-1)
    (and (vector? xy-or-fn-0)
         (fn? xy-or-fn-1))
      (fn [[xi yi]]
        (let [[x0 y0] xy-or-fn-0
              [x1 y1] (xy-or-fn-1 xi yi)]
          [(+ x0 x1) (+ y0 y1)]))
    (and (fn? xy-or-fn-0)
         (vector? xy-or-fn-1))
      (fn [[xi yi]]
        (let [[x0 y0] (xy-or-fn-0 xi yi)
              [x1 y1] xy-or-fn-1]
          [(+ x0 x1) (+ y0 y1)]))
    (and (fn? xy-or-fn-0)
         (fn? xy-or-fn-1))
      (fn [[xi yi]]
        (let [[x0 y0] (xy-or-fn-0 xi yi)
              [x1 y1] (xy-or-fn-1 xi yi)]
          [(+ x0 x1) (+ y0 y1)]))))

(defn v*
  [s xy-or-f]
  (case
    (vector? xy-or-f)
      (let [[x y] xy-or-f]
        [(* s x) (* s y)])
    (fn? xy-or-f)
      (fn [[xi yi]]
        [(* s (xy-or-f xi)) (* s (xy-or-f yi))])))

(defn v-
  [xy-or-fn-0 xy-or-fn-1]
  (case
    (and (vector xy-or-fn-0)
         (vector xy-or-fn-1))
      (mapv - xy-or-fn-0 xy-or-fn-1)
    :else
      (v+ xy-or-fn-0 (v* -1 xy-or-fn-1))))

(defn s+
  [s f]
  (fn [[x y]]
    (let [v (f [x y])]
      (+ s v))))

(defn sample-tree [n]
  (offset [-0.5 -0.5] (s+ -0.5 (scale 0.01 (noise n)))))

#_(defn sample-island
  [noise x y]
  (cliskf/vectorize
    (cliskf/vlet [c  ((invert (scale 0.43 (offset (vnoise noise) (radius)))) x y)
                  c1 ((offset [0.5 0.5] (s+ -0.5 (scale 0.06 noise))) x y)
                  c2 ((offset [-0.5 -0.5] (s+ -0.5 (scale 0.08 noise))) x y)]
      (cond
        ;; interior biomes
        (> -0.55  c)
          (cond
            (and (pos? c1) (pos? c2) (> c1 c2))
            ;; interior jungle
            jungle
            (and (pos? c1) (pos? c2))
            heavy-forest
            (and (pos? c1) (neg? c2) (> c1 c2))
            light-forest
            (and (pos? c1) (neg? c2))
            bamboo-grove
            (and (neg? c1) (pos? c2) (> c1 c2))
            meadow
            (and (neg? c1) (pos? c2))
            rocky
            (and (neg? c1) (neg? c2) (> c1 c2))
            swamp
            :else
            dirt)
        ;; shore/yellow
        (> -0.5  c)
            sand
        ;; surf/light blue
        (> -0.42 c)
          surf
        ;; else ocean
        :else
          ocean))))

(defn -main [& args]
  (let [n (rn/create-noise)]
    #_(rn/print-fn (fn [x y] (rn/noise n x y)) 180 180)
    (rn/print-fn (coerce (sample-tree n)) 180 180)
    #_(rn/print-fn (coerce (invert (offset (vsnoise (partial rn/noise n))  (scale 0.2 (radius))))) 180 180)))

