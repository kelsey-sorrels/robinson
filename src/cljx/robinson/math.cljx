;; Ueility functions and functions for manipulating state
(ns robinson.math)

#+clj
(set! *warn-on-reflection* true)

#+clj
(def PI (Math/PI))

#+cljs
(def PI (.-PI js/Math))

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
(defn round [n]
  (Math/round n))

#+cljs
(defn round [n]
  (.round js/math n))

#+clj
(defn ceil [n]
  (Math/ceil n))

#+cljs
(defn ceil [n]
  (.ceil js/math n))

#+clj
(defn sin [n]
  (Math/sin n))

#+cljs
(defn sin [n]
  (.sin js/Math n))

#+clj
(defn cos [n]
  (Math/cos n))

#+cljs
(defn cos [n]
  (.cos js/Math n))

#+clj
(defn atan2 [x y]
  (Math/atan2 x y))

#+cljs
(defn atan2 [x y]
  (.atan2 js/Math x y))

