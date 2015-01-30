;; Functions for making random noise
(ns robinson.noise
  (:use     clojure.contrib.core)
  (:require [robinson.random :as rr]
            [taoensso.timbre :as timbre]))

(timbre/refer-timbre)

(defprotocol Noise
  (snoise [this xin yin])
  (noise [this x y]))


(def ^:private grad3 [[ 1  1  0]
                      [-1  1  0]
                      [ 1 -1  0]
                      [-1 -1  0]
                      [ 1  0  1]
                      [-1  0  1]
                      [ 1  0 -1]
                      [-1  0 -1]
                      [ 0  1  1]
                      [ 0 -1  1]
                      [ 0  1 -1]
                      [ 0 -1 -1]])
(def ^:private grad4 [[ 0,  1,  1,  1]
                      [ 0,  1,  1, -1]
                      [ 0,  1, -1,  1]
                      [ 0,  1, -1, -1]
                      [ 0, -1,  1,  1]
                      [ 0, -1,  1, -1]
                      [ 0, -1, -1,  1]
                      [ 0, -1, -1, -1]
                      [ 1,  0,  1,  1]
                      [ 1,  0,  1, -1]
                      [ 1,  0, -1,  1]
                      [ 1,  0, -1, -1]
                      [-1,  0,  1,  1]
                      [-1,  0,  1, -1]
                      [-1,  0, -1,  1]
                      [-1,  0, -1, -1]
                      [ 1,  1,  0,  1]
                      [ 1,  1,  0, -1]
                      [ 1, -1,  0,  1]
                      [ 1, -1,  0, -1]
                      [-1,  1,  0,  1]
                      [-1,  1,  0, -1]
                      [-1, -1,  0,  1]
                      [-1, -1,  0, -1]
                      [ 1,  1,  1,  0]
                      [ 1,  1, -1,  0]
                      [ 1, -1,  1,  0]
                      [ 1, -1, -1,  0]
                      [-1,  1,  1,  0]
                      [-1,  1, -1,  0]
                      [-1, -1,  1,  0]
                      [-1, -1, -1,  0]])
(def ^:private f2 (* 0.5 (dec #+clj  (Math/sqrt 3.0)
                              #+cljs (.sqrt js/Math 3.0))))
(def ^:private g2 (/ (- 3.0
                        #+clj  (Math/sqrt 3.0)
                        #+cljs (.sqrt js/Math 3.0))
                     6.0))
(def ^:private f3 (/ 1.0 3.0))
(def ^:private g3 (/ 1.0 6.0))
(def ^:private f4 (/ (dec #+clj  (Math/sqrt 5.0)
                          #+cljs (.sqrt js/Math 5.0))
                     4.0))
(def ^:private g4 (/ (- 5.0 #+clj  (Math/sqrt 5.0)
                            #+cljs (.sqrt js/Math 5.0))
                     20.0))

(defn- fast-floor
  [x]
  (let [xi (int x)]
    (if (< x xi)
      (dec xi)
      xi)))

(defn- dot
  ([grad x y]
  (let [[gx gy] grad]
    (+ (* gx x) (* gy y))))
  ([grad x y z]
  (let [[gx gy gz] grad]
    (+ (* gx x) (* gy y) (* gz z))))
  ([grad x y z w]
  (let [[gx gy gz gw] grad]
    (+ (* gx x) (* gy y) (* gz z) (* gw w)))))


(defn create-noise
  ([]
  (create-noise rr/*rnd*))
  ([rnd]
  (let [p           (rr/rnd-shuffle rnd (vec (range 256)))
        perm        (vec (concat p p))
        perm-mod-12 (mapv (fn [n] (mod n 12)) perm)]
    (reify Noise
      (snoise [this xin yin]
        (let [s (* f2 (+ xin yin))
              i (fast-floor (+ xin s))
              j (fast-floor (+ yin s))
              t (* (+ i j) g2)
              X0 (- i t)
              Y0 (- j t)
              x0 (- xin X0)
              y0 (- yin Y0)
              [i1 j1] (if (> x0 y0)
                        [1 0]
                        [0 1])
              x1 (+ (- x0 i1) g2)
              y1 (+ (- y0 j1) g2)
              x2 (+ (dec x0) (* 2 g2))
              y2 (+ (dec y0) (* 2 g2))
              ii (bit-and i 0xFF)
              jj (bit-and j 0xFF)
              gi0 (nth perm-mod-12 (+ ii (nth perm jj)))
              gi1 (nth perm-mod-12 (+ ii i1 (nth perm (+ jj j1))))
              gi2 (nth perm-mod-12 (+ ii 1 (nth perm (inc jj))))
              t0  (- 0.5 (* x0 x0) (* y0 y0))
              n0  (if (neg? t0)
                    0.0
                    (* t0 t0 t0 t0 (dot (nth grad3 gi0) x0 y0)))
              t1  (- 0.5 (* x1 x1) (* y1 y1))
              n1  (if (neg? t1)
                    0.0
                    (* t1 t1 t1 t1 (dot (nth grad3 gi1) x1 y1)))
              t2  (- 0.5 (* x2 x2) (* y2 y2))
              n2  (if (neg? t2)
                    0.0
                    (* t2 t2 t2 t2 (dot (nth grad3 gi2) x2 y2)))]
          (* 70.0 (+ n0 n1 n2))))
      (noise
        [this x y]
        (+ 0.5 (* 0.5 (snoise this x y))))))))




;; REPL code for displaying noise as ascii art
;(def px (partition 200 (for [x (range -100 100) y (range -100 100)] (noise n (/ x 10) (/ y 10)))))

(def ^:private ascii-chars [\# \A \@ \$ \% \= \+ \* \: \, \. \space])

;;(def n (create-noise))

(defn print-fn
  [f width height]
  (let [px (partition height (for [x (range (- (/ width 2)) (/ width 2))
                                   y (range (- (/ height 2)) (/ height 2))]
                               (f (/ x 10) (/ y 10))))]
    (doseq [line px]
      (println
        (apply str (map (fn [c]
                          (let [idx (if (zero? c)
                                        (dec (count ascii-chars)) 
                                        (- (count ascii-chars)
                                           (* c (count ascii-chars))))]
                            (nth ascii-chars (min (max idx 0) (dec (count ascii-chars))))))
                        line))))))

