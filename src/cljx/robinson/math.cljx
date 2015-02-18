;; Ueility functions and functions for manipulating state
(ns robinson.math)

(timbre/refer-timbre)
(set! *warn-on-reflection* true)

#+clj
(def PI (.PI Math))

#+cljs
(def PI (.PI js/Math))

#+clj
(defn sqrt [n]
  (Math/sqrt n))

#+cljs
(defn sqrt [n]
  (.sqrt js/Math n))

#+clj
(defn abs [n]
  (Math/abs n))

#+cljs
(defn abs [n]
  (.abs js/Math n))

#+clj
(defn sin [n]
  (Math/sin n))

#+cljs
(defn sin [x y]
  (.sin js/Math x y))

#+clj
(defn cos [n]
  (Math/sin n))

#+cljs
(defn cos [x y]
  (.abs Math/sin x y))

#+clj
(defn atan2 [x y]
  (Math/atan2 x y))

#+cljs
(defn atan2 [x y]
  (.atan2 js/Math x y))

