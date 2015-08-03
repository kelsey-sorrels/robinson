;; Utility functions and functions for manipulating vector and scalar fields
(ns robinson.prism
  (:require 
            [robinson.common :as rc]
            [robinson.noise :as rn]))

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
    #?(:clj  (Math/sqrt (+ (* x x) (* y y)))
       :cljs (.sqrt js/Math (+ (* x x) (* y y))))))

(defn center-radius []
  (fn [[x y]]
    (center (radius))))

(defn vnoise
  [n]
  (fn [[x y]]
    (vector (rn/noise n x y) (rn/noise n (+ x 12.301) (+ y 70.261)))))

(defn vsnoise
  [n]
  (fn [[x y]]
  (vector (rn/snoise n x y) (rn/snoise n (+ x 12.301) (+ y 70.261)))))

(defn v+
  [xy-or-fn-0 xy-or-fn-1]
  (cond
    (and (vector? xy-or-fn-0)
         (vector? xy-or-fn-1))
      (mapv + xy-or-fn-0 xy-or-fn-1)
    (and (vector? xy-or-fn-0)
         (fn? xy-or-fn-1))
      (fn [[xi yi]]
        (let [[x0 y0] xy-or-fn-0
              [x1 y1] (xy-or-fn-1 [xi yi])]
          [(+ x0 x1) (+ y0 y1)]))
    (and (fn? xy-or-fn-0)
         (vector? xy-or-fn-1))
      (fn [[xi yi]]
        (let [[x0 y0] (xy-or-fn-0 [xi yi])
              [x1 y1] xy-or-fn-1]
          [(+ x0 x1) (+ y0 y1)]))
    (and (fn? xy-or-fn-0)
         (fn? xy-or-fn-1))
      (fn [[xi yi]]
        (let [[x0 y0] (xy-or-fn-0 [xi yi])
              [x1 y1] (xy-or-fn-1 [xi yi])]
          [(+ x0 x1) (+ y0 y1)]))))

(defn v*
  [s xy-or-f]
  (cond
    (vector? xy-or-f)
      (let [[x y] xy-or-f]
        [(* s x) (* s y)])
    (fn? xy-or-f)
      (fn [[xi yi]]
        (let [[x1 y1] (xy-or-f [xi yi])]
          [(* s x1) (* s y1)]))))

(defn v-
  [xy-or-fn-0 xy-or-fn-1]
  (case
    (and (vector? xy-or-fn-0)
         (vector? xy-or-fn-1))
      (mapv - xy-or-fn-0 xy-or-fn-1)
    :else
      (v+ xy-or-fn-0 (v* -1 xy-or-fn-1))))

(defn s+
  [s f]
  (fn [[x y]]
    (let [v (f [x y])]
      (+ s v))))

(defn sample-tree [n]
  (offset [-0.5 -0.5] (scale 0.01 (snoise n))))

(defn sample-island
  [n x y]
  (let [c  ((coerce (scale 4.3 (offset (vnoise n) (radius)))) x y)
        c1 ((coerce (offset [0.5 0.5] (scale 0.6 (snoise n)))) x y)
        c2 ((coerce (offset [-110.5 -640.5] (scale 0.8 (snoise n)))) x y)
        cgt #?(:clj  (> (Math/abs c1) (Math/abs c2))
               :cljs (> (.abs js/Math c1) (.abs js/Math c2)))]
    (cond
      ;; interior biomes
      (> 0.55  c)
        (cond
          (and (pos? c1) (pos? c2) cgt)
          :jungle
          (and (pos? c1) (pos? c2))
          :heavy-forest
          (and (pos? c1) (neg? c2) cgt)
          :light-forest
          (and (pos? c1) (neg? c2))
          :bamboo-grove
          (and (neg? c1) (pos? c2) cgt)
          :meadow
          (and (neg? c1) (pos? c2))
          :rocky
          (and (neg? c1) (neg? c2) cgt)
          :swamp
          :else
          :dirt)
      ;; shore/yellow
      (> 0.6  c)
          :sand
      ;; surf/light blue
      (> 0.68 c)
        :surf
      ;; else ocean
      :else
        :ocean)))

(defn map-biome [k]
  (case k
    :jungle \j
    :heavy-forest \F
    :light-forest \f
    :bamboo-grove \b
    :meadow \m
    :rocky \^
    :swamp \s
    :dirt \.
    :sand \,
    :surf \=
    :ocean \~))

(defn -main [& args]
  (let [n (rn/create-noise)]
    #_(rn/print-fn (fn [x y] (rn/noise n x y)) 180 180)
    #_(rn/print-fn (coerce (sample-tree n)) 180 180)
    (rn/print-fn map-biome (fn [x y] (sample-island n x y)) 180 180)
    #_(rn/print-fn (coerce (invert (offset (vsnoise n)  (scale 0.2 (radius))))) 180 180)))

