(ns dungeon-crusade.lineofsight
  (:use [dungeon-crusade.mapgen :exclude [-main]]
        dungeon-crusade.common)
  (:require [clojure.contrib.math :as math]))

(defn line-segment [start end]
  "(line-segment [1 1] [5 4])"
  (let [[x1 y1] start
        [x2 y2] end
        xdiff (- x2 x1)
        ydiff (- y2 y1)
        maxdiff (+ 0.000001 (max (math/abs xdiff) (math/abs ydiff)))
        dx (/ xdiff maxdiff)
        dy (/ ydiff maxdiff)]
    (for [i (range (inc maxdiff))]
      [(math/round (float (+ x1 (* i dx)))) (math/round (float (+ y1 (* i dy))))])))

(defn map-visibility [origin blocking? place]
  "(map-visibility [x y] blocking? (0,0) ------ x
                                     |  [[false false false]
                                     |   [false true  false]
                                     y   [false true  false]])"
                                    
  (let [[ox oy] origin
        width  (-> place first count)
        height (count place)
        bounds-xy (filter (fn [[x y e]]
                            (or (= x 0)
                                (= x (dec width))
                                (= y 0)
                                (= y (dec height))))
                          (with-xy place))
        visible? (memoize
                   (fn [x1 y1 x2 y2]
                     (not-any? #(blocking? %)
                       (map (fn [[x y]] (get-in place [y x]))
                         (line-segment [x1 y1] [x2 y2])))))]
    (println (with-xygrid place))
    (map (fn [line] (map (fn [[_ x y]] (visible? ox oy x y)) line)) (with-xygrid place))))

(defn -main [& args]
  (println "generating...")
  (println (line-segment [0 0] [2 10]))
  (doall (map println (map-visibility [0 2] identity
                                       [[false false false]
                                        [false false  false]
                                        [false true  false]]))))
  
