(ns dungeon-crusade.lineofsight
  (:use [dungeon-crusade.mapgen :exclude [-main]]
        dungeon-crusade.common)
  (:require [clojure.contrib.math :as math]
            clojure.pprint))

(set! *warn-on-reflection* true)

(def line-segment (memoize
  (fn [start end]
    "(line-segment [1 1] [5 4])"
    (if (= start end)
      []
      (let [[x1 y1] start
            [x2 y2] end
            xdiff (- x2 x1)
            ydiff (- y2 y1)
            maxdiff (max (math/abs xdiff) (math/abs ydiff))
            dx (/ xdiff maxdiff)
            dy (/ ydiff maxdiff)]
        (map (fn [i] [(math/round (double (+ x1 (* i dx)))) (math/round (double (+ y1 (* i dy))))])
            (range (inc maxdiff))))))))

(def line-segment-fast (memoize
  (fn [start end]
    "(line-segment-fast [1 1] [5 4])"
    (let [[ox oy] start
          [dx dy] end]
      (map (fn [[x y]] [(+ ox x) (+ oy y)])
          (line-segment [0 0] [(- dx ox) (- dy oy)]))))))
 
(defn map-visibility [origin blocking? place]
  "(map-visibility [x y] blocking? (0,0) ------ x
                                     |  [[false false false]
                                     |   [false true  false]
                                     y   [false true  false]])
  (clojure.pprint/pprint (map-visibility [0 4] identity
                                  [[false false false false]
                                   [false false true  false]
                                   [false false true  false]
                                   [false false true  false]
                                   [false false true  false]]))
  "
                                    
  (let [[ox oy] origin
        width  (-> place first count)
        height (count place)
        bounds-xy (filter (fn [[x y e]]
                            (or (= x 0)
                                (= x (dec width))
                                (= y 0)
                                (= y (dec height))))
                          (with-xy place))
        get-cell (fn [place ks] (memoize (get-in place ks)))
        visible? (fn [x1 y1 x2 y2]
                   (not-any? #(blocking? %)
                     ;(map (fn [[x y]] (get-cell place [y x]))
                     (map (fn [[x y]] (get-in place [y x]))
                       (rest (butlast (line-segment-fast [x1 y1] [x2 y2]))))))]
    ;(println (with-xygrid place))
    (vec (pmap (fn [line] (vec (map (fn [[_ x y]] (visible? ox oy x y)) line))) (with-xygrid place)))))

(defn cell-blocking? [cell]
  (if (nil? cell)
    true
    (contains? #{:vertical-wall
                 :horizontal-wall
                 :close-door} (cell :type))))


(defn -main [& args]
  ;(println "generating...")
  ;(println (time (line-segment [0 0] [22 40])))
  ;(println (time (line-segment [0 0] [22 40])))
  ;(println (time (line-segment [0 0] [22 40])))
  ;(dotimes [i 10]
  ;  (println (- 10 i))
  ;  (Thread/sleep 1000))
  (println (= (line-segment [0 0] [22 40]) (line-segment-fast [0 0] [22 40])))
  (println (= (line-segment [10 10] [22 40]) (line-segment-fast [10 10] [22 40])))
  (println (= (line-segment [22 40] [10 10]) (line-segment-fast [22 40] [10 10])))
  
  (dotimes [i 10]
    (let [place (repeat 22 (repeat 40 false))]
     (time (map-visibility [(int (/ i 2)) 2] identity place)))))
  
