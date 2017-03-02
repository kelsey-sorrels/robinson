(ns robinson.image
  (require [zaffre.imageutil :as zimg]))

(def image-values
  "Return a sequence of {:x x :y y :fg [r g b (a)]} for an image."
  (memoize
    (fn [path]
      (let [image (zimg/load-image path)
            data  (for [i (range (.limit (get image :byte-buffer)))]
                    (.get byte-buffer))
            pixels (partition (get image :channels))
            xys    (for [y (range (get image :height))
                         x (range (get image :width))]
                     [x y])
            pixel-xys (map (fn [pixel [x y]] {:x x :y y :fg pixel}) pixels xys)]
        (zimg/close image)
        pixel-xys))))

(def mk-img-chars [x y path]
  (map (fn [pixel]
         (-> pixel
           (assoc :c \u2584)
           (update :x (partial + x))
           (update :y (partial + y))))
    (image-values path)))


