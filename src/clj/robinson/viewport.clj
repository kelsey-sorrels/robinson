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

(defn viewport-wh
  [state]
  (let [{v-width     :width
         v-height    :height}
        (get-in state [:world :viewport])]
    [v-width v-height]))

(defn xy-in-viewport?
  [state x y]
  (let [{v-width     :width
         v-height    :height
         {v-x :x v-y :y} :pos}
        (get-in state [:world :viewport])]
        
    (xy-in-rect? x y
                 v-x v-y
                 v-width v-height)))

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
    ;(info "xy->place-id")
    ;(info x)
    ;(info v-width)
    ;(info y)
    ;(info v-height)
    [(if (neg? x)
       (dec (int (/ (inc x) v-width)))
       (int (/ x v-width)))
     (if (neg? y)
       (dec (int (/ (inc y) v-height)))
       (int (/ y v-height)))]))

(defn get-place
  [state x y]
  (get-in state [:world :places (xy->place-id state x y)]))

(defn player-place
  [state]
  (apply get-place state (player-xy state)))

(defn place-id->anchor-xy
  "For a given place-id return the world coordinates of the upper-left-hand corner of the place.
    Here
      +--> +---------->
           |
           |   place cells
           /"
  [state place-id]
  (let [{v-width     :width
         v-height    :height}
        (get-in state [:world :viewport])
        [px py]      place-id]
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
        [px py]     ul-place-id]
    [ul-place-id 
     [(inc px) py]
     [px       (inc py)]
     [(inc px) (inc py)]]))

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
    (info "viewport-world-xys v-x" v-x "v-y" v-y "v-width" v-width "v-height" v-height)
    (for [x (range v-width)
          y (range v-height)]
      [x y (+ x v-x) (+ y v-y)])))

(defn cells-in-viewport
  "Return a collection of cells in the viewport as a two-dimensional array"
  [state]
  (let [{v-width     :width
         v-height    :height
         {v-x :x v-y :y} :pos}
                          (get-in state [:world :viewport])
        ;; upper left place
        ul-place-id       (xy->place-id state v-x v-y)
        [px py]           ul-place-id
        ur-place-id       [(inc px) py]
        ll-place-id       [px       (inc py)]
        lr-place-id       [(inc px) (inc py)]
        [ax ay] (place-id->anchor-xy state lr-place-id)
        _ (info "v-x" v-x "v-y" v-y)
        _ (info "v-width" v-width "v-height" v-height)
        _ (info "ax" ax "ay" ay)
        start-x            (- v-width (- ax v-x))
        start-y            (- v-height (- ay v-y))
        ;start-x           (mod v-x v-width)
        ;start-y           (mod v-y v-height)
        ;start-x           (if (pos? v-x)
        ;                    start-x
        ;                    (- v-width start-x))
        ;start-y           (if (pos? v-y)
        ;                    start-y
        ;                    (- v-height start-y))
        _ (info "start-x" start-x "start-y" start-y)
        ul-place          (get-in state [:world :places ul-place-id])
        ur-place          (get-in state [:world :places ur-place-id])
        ll-place          (get-in state [:world :places ll-place-id])
        lr-place          (get-in state [:world :places lr-place-id])
    cells (concat 
      (map (fn [line1 line2]
           (when-not (vector? line1)
              (info "line1 not vector" line1)
              (throw (Exception. (spit line1))))
           (when-not (vector? line2)
              (info "line2 not vector" line2)
              (throw (Exception. spit line2)))

             (concat (subvec line1 start-x)
                     (subvec line2 0 start-x)))
           
           (subvec ul-place start-y)
           (subvec ur-place start-y))
      (map (fn [line3 line4] (concat (subvec line3 start-x)
                                     (subvec line4 0 start-x)))
           (subvec ll-place 0 start-y)
           (subvec lr-place 0 start-y)))]
    (info "ul-place-id" ul-place-id)
    (info "ur-place-id" ur-place-id)
    (info "lr-place-id" lr-place-id)
    (info "ll-place-id" ll-place-id)
    ;(info "ul-place" ul-place)
    ;(info "\n\n")
    ;(info "ur-place" ur-place)
    ;(info "\n\n")
    ;(info "ll-place" ll-place)
    ;(info "\n\n")
    ;(info "lr-place" lr-place)
    ;(info "\n\n")
    ;(info "cells" cells)
    cells))

