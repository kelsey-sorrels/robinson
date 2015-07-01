;; Ueility functions and functions for manipulating state
(ns robinson.math)

#?(
:clj
(set! *warn-on-reflection* true)

(def PI (Math/PI))

(defn sqrt [n]
  (Math/sqrt n))

(defn abs [^double n]
  (Math/abs n))

(defn round [^double n]
  (Math/round n))

(defn ceil [n]
  (Math/ceil n))

(defn sin [n]
  (Math/sin n))

(defn cos [n]
  (Math/cos n))

(defn atan2 [x y]
  (Math/atan2 x y))

:cljs
(def PI (.-PI js/Math))

(defn sqrt [n]
  (.sqrt js/Math n))

(defn abs [n]
  (.abs js/Math n))

(defn round [n]
  (.round js/Math n))

(defn ceil [n]
  (.ceil js/Math n))

(defn sin [n]
  (.sin js/Math n))

(defn cos [n]
  (.cos js/Math n))

(defn atan2 [x y]
  (.atan2 js/Math x y)))

