(ns dungeon-crusade.mapgen)

(defn canvas [width height]
  (vec (repeat height (str (vec (repeat width " "))))))

(defn draw-ascii-place [ascii]
  (doall (map println ascii)))
(defn make
(defn random-ascii-place [width height]
  (let [num-rooms 5
        make-room-fn (fn [] {:x (rand-int 30)
                             :y (rand-int 20)
                             :width (rand-int 2 6)
                             :height (rand-int 2 6)})
        rooms     (map make-room (range num-rooms))


