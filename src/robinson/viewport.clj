;; Utility functions and functions for manipulating state
(ns robinson.viewport
  (:use     clojure.contrib.core
            robinson.player)
  (:require [ clojure.data.generators :as dg]
            [taoensso.timbre :as timbre]
            [pallet.thread-expr :as tx]))

(timbre/refer-timbre)

(defn xy-in-rect?
  [x y rx ry rw rh]
  (and (<= rx x (+ -1 rx rw))
       (<= ry y (+ -1 ry rh))))

(defn xy-in-safe-zone?
  "Is `(x,y)` in the viewport bounds defined in the viewport in `state`?"
  [state x y]
  (let [{v-width     :width
         v-height    :height
         {v-x :x v-y :y} :pos}
        (get-in state [:world :viewport])
        ;; safe zone size
        s 5]
        
    (xy-in-rect? x y (+ v-x s) (+ v-y s)
                     (- v-width (* 2 s)) (- v-height (* 2 s)))))
(defn xy->place-id
  [state x y]
  (let [{v-width     :width
         v-height    :height}
        (get-in state [:world :viewport])]
    (keyword (format "%d_%d" (int (/ x v-width)) (int (/ y v-height))))))

(defn get-place
  [state x y]
  (get-in state [:world :places (xy->place-id state x y)]))

(defn player-place
  [state]
  (apply get-place state (player-xy state)))

(defn place-id->anchor-xy
  [state place-id]
  (let [{v-width     :width
         v-height    :height}
        (get-in state [:world :viewport])
        [px py]      (map read-string (clojure.string/split (name place-id) #"_"))]
    [(* px v-width) (* py v-height)]))

(defn +xy
  [[x1 y1] [x2 y2]]
  [(+ x1 x2) (+ y1 y2)])

(defn -xy
  [[x1 y1] [x2 y2]]
  [(- x1 x2) (- y1 y2)])


(defn visible-place-ids
  [state x y]
  (let [{v-width     :width
         v-height    :height
         {v-x :x v-y :y} :pos}
        (get-in state [:world :viewport])
        ;; upper left place
        ul-place-id (xy->place-id state v-x v-y)
        [px py]      (map read-string (clojure.string/split (name ul-place-id) #"_"))]
    [ul-place-id 
     (keyword (format "%d_%d" (inc px) py))
     (keyword (format "%d_%d" px       (inc py)))
     (keyword (format "%d_%d" (inc px) (inc py)))]))

(defn viewport-xys
  [state]
  (let [{v-width     :width
         v-height    :height
         {v-x :x v-y :y} :pos}
        (get-in state [:world :viewport])]
    (info "viewport-xys v-x" v-x "v-y" v-y)
    (for [x (range v-x (+ v-x v-width))
          y (range v-y (+ v-y v-height))]
      [x y])))

(defn viewport-world-xys
  "Return a collection of 4-tuples in the form of
  `[viewport-x viewport-y world-x world-y]`."
  [state]
  (let [{v-width     :width
         v-height    :height
         {v-x :x v-y :y} :pos}
        (get-in state [:world :viewport])]
    (info "viewport-xys v-x" v-x "v-y" v-y)
    (for [x (range v-width)
          y (range v-height)]
      [x y (+ x v-x) (+ y v-y)])))
