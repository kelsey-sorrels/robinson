;; Utility functions and functions for manipulating state
(ns robinson.viewport
  (:require
            [taoensso.timbre :as log]
            [robinson.player :as rp]
            #?@(:cljs (
               [goog.string :as gstring]
               [goog.string.format]))))

(defn xy-in-rect?
  [x y rx ry rw rh]
  {:pre [(integer? x)
         (integer? y)
         (integer? rx)
         (integer? ry)
         (integer? rw)
         (integer? rh)]}
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
  {:pre [(some? state)
         (some? (get-in state [:world :viewport]))
         (integer? x)
         (integer? y)]}
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
  {:pre [(not (nil? state))
         (integer? x)
         (integer? y)]
   :post [(vector? %)]}
  (let [{v-width     :width
         v-height    :height}
        (get-in state [:world :viewport])]
    #_(log/info "xy->place-id")
    #_(log/info "world" (str (dissoc (get state :world) :places)))
    #_(log/info x)
    #_(log/info v-width)
    #_(log/info y)
    #_(log/info v-height)
    [(if (neg? x)
       (dec (int (/ (inc x) v-width)))
       (int (/ x v-width)))
     (if (neg? y)
       (dec (int (/ (inc y) v-height)))
       (int (/ y v-height)))]))

(defn get-place
  [state x y]
  (get-in state [:world :places (xy->place-id state x y)]))

(defn player-place-id
  [state]
  (apply xy->place-id state (rp/player-xy state)))

(defn player-place
  [state]
  (apply get-place state (rp/player-xy state)))

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
    (log/info "viewport-xys v-x" v-x "v-y" v-y)
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
    (log/info "viewport-world-xys v-x" v-x "v-y" v-y "v-width" v-width "v-height" v-height)
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
        ;_ (log/info "ul-place-id" (str ul-place-id))
        ;_ (log/info "ur-place-id" (str ur-place-id))
        ;_ (log/info "ll-place-id" (str ll-place-id))
        ;_ (log/info "lr-place-id" (str lr-place-id))
        [ax ay] (place-id->anchor-xy state lr-place-id)
        ;_ (log/info "v-x" v-x "v-y" v-y)
        ;_ (log/info "v-width" v-width "v-height" v-height)
        ;_ (log/info "ax" ax "ay" ay)
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
        ;_ (log/info "start-x" start-x "start-y" start-y)
        ;_ (log/info "place-ids" (str (keys (get-in state [:world :places]))))
        ul-cells          (get-in state [:world :places ul-place-id :cells])
        ur-cells          (get-in state [:world :places ur-place-id :cells])
        ll-cells          (get-in state [:world :places ll-place-id :cells])
        lr-cells          (get-in state [:world :places lr-place-id :cells])
        ;_ (log/info "ul-place-0" (str (type (get-in state [:world ]))))
        ;_ (log/info "ul-place-1" (str (type (get-in state [:world :places ]))))
        ;_ (log/info "ul-place-2" (str (type (get-in state [:world :places ul-place-id]))))
        ;_ (log/info "ur-place" (str ur-place))
        ;_ (log/info "ll-place" (str ll-place))
        ;_ (log/info "lr-place" (str lr-place))
    cells (concat 
      (map (fn [line1 line2]
           (when-not (vector? line1)
              (log/info "line1 not vector" line1)
              (throw #?(:clj
                        (Exception. (spit line2))
                        :cljs
                        (js/Error. (gstring/spit line1)))))
           (when-not (vector? line2)
              (log/info "line2 not vector" line2)
              (throw #?(:clj
                        (Exception. (spit line2))
                        :cljs
                        (js/Error. (gstring/spit line2)))))
           (concat (subvec line1 start-x)
                   (subvec line2 0 start-x)))
           
           (subvec ul-cells start-y)
           (subvec ur-cells start-y))
      (map (fn [line3 line4] (concat (subvec line3 start-x)
                                     (subvec line4 0 start-x)))
           (subvec ll-cells 0 start-y)
           (subvec lr-cells 0 start-y)))]
    ;(log/info "ul-place-id" ul-place-id)
    ;(log/info "ur-place-id" ur-place-id)
    ;(log/info "lr-place-id" lr-place-id)
    ;(log/info "ll-place-id" ll-place-id)
    ;(log/info "ul-place" ul-place)
    ;(log/info "\n\n")
    ;(log/info "ur-place" ur-place)
    ;(log/info "\n\n")
    ;(log/info "ll-place" ll-place)
    ;(log/info "\n\n")
    ;(log/info "lr-place" lr-place)
    ;(log/info "\n\n")
    ;(log/info "cells" cells)
    cells))

(defn cellsxy-in-viewport
  "Return a collection of cells in the viewport as an array.
  Each element in the array has this format
  `[cell viewport-x viewport-y world-x world-y]`."
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
        ;_ (log/info "ul-place-id" (str ul-place-id))
        ;_ (log/info "ur-place-id" (str ur-place-id))
        ;_ (log/info "ll-place-id" (str ll-place-id))
        ;_ (log/info "lr-place-id" (str lr-place-id))
        [ax ay] (place-id->anchor-xy state lr-place-id)
        ;_ (log/info "v-x" v-x "v-y" v-y)
        ;_ (log/info "v-width" v-width "v-height" v-height)
        ;_ (log/info "ax" ax "ay" ay)
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
        ;_ (log/info "start-x" start-x "start-y" start-y)
        ;_ (log/info "place-ids" (str (keys (get-in state [:world :places]))))
        ul-cells          (get-in state [:world :places ul-place-id :cells])
        ur-cells          (get-in state [:world :places ur-place-id :cells])
        ll-cells          (get-in state [:world :places ll-place-id :cells])
        lr-cells          (get-in state [:world :places lr-place-id :cells])
        ;_ (log/info "ul-place-0" (str (type (get-in state [:world ]))))
        ;_ (log/info "ul-place-1" (str (type (get-in state [:world :places ]))))
        ;_ (log/info "ul-place-2" (str (type (get-in state [:world :places ul-place-id]))))
        ;_ (log/info "ul-place" (str ul-place))
        ;_ (log/info "ur-place" (str ur-place))
        ;_ (log/info "ll-place" (str ll-place))
        ;_ (log/info "lr-place" (str lr-place))
        ;first-wx (drop (dec v-x) (range))
        ;first-vx (drop (dec start-x) (range))
        ;rest-wx  (drop (+ -1 v-x start-x) (range))
        dx       (- v-width start-x)
        dy       (- v-height start-y)
        r-x      (range 0 dx)
        r-y      (range 0 dy)
        r-xrest  (range dx v-width)
        r-yrest  (range dy v-height)
        ;_ (println "v-width" v-width)
        ;_ (println "v-height" v-height)
        ;_ (println "v-x" v-x)
        ;_ (println "v-y" v-y)
        ;_ (println "ax" ax)
        ;_ (println "ay" ay)
        ;_ (println "start-x" start-x)
        ;_ (println "start-y" start-y)
        ;_ (println "dx" dx)
        ;_ (println "dy" dy)
        ;_ (println "r-y" r-y)
        ;_ (println "r-yrest" r-yrest)
        ;_ (println "r-xrest" r-xrest)
        conj-cells (fn [cells place min-x min-y max-x max-y x-range y-range]
                     (reduce (fn [cells [line sy]]
                               (reduce (fn [cells [cell sx]]
                                         (conj! cells 
                                                [cell sx sy (+ sx v-x) (+ sy v-y)]))
                                       cells
                                       (map vector (subvec line min-x max-x)
                                                   x-range)))
                             cells
                             (map vector
                                  (subvec place min-y max-y)
                                  y-range)))
        cells (-> (transient [])
                    (conj-cells ul-cells start-x start-y v-width v-height r-x     r-y)
                    (conj-cells ur-cells 0       start-y start-x v-height r-xrest r-y)
                    (conj-cells ll-cells start-x 0       v-width start-y  r-x     r-yrest)
                    (conj-cells lr-cells 0       0       start-x start-y  r-xrest r-yrest)
                    persistent!)]
    ;(log/info "ul-place-id" ul-place-id)
    ;(log/info "ur-place-id" ur-place-id)
    ;(log/info "lr-place-id" lr-place-id)
    ;(log/info "ll-place-id" ll-place-id)
    ;(log/info "ul-place" ul-place)
    ;(log/info "\n\n")
    ;(log/info "ur-place" ur-place)
    ;(log/info "\n\n")
    ;(log/info "ll-place" ll-place)
    ;(log/info "\n\n")
    ;(log/info "lr-place" lr-place)
    ;(log/info "\n\n")
    ;(time (log/info "cells" cells))
    cells))
