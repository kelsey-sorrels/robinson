;; Ueility functions and functions for manipulating state
(ns robinson.math)

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
