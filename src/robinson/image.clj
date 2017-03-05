(ns robinson.image
  (:require [zaffre.imageutil :as zimg]))

;; Return a sequence of {:x x :y y :fg [r g b (a)]} for an image.
(def image-values
  (memoize
    (fn [path]
      (let [image       (zimg/load-image path)
            byte-buffer (get image :byte-buffer)
            data        (for [i (range (.limit byte-buffer))]
                          (.get byte-buffer))
            pixels      (partition (get image :channels) data)
            xys         (for [y (range (get image :height))
                              x (range (get image :width))]
                          [x y])
            pixel-xys   (map (fn [pixel [x y]] {:x x :y y :fg (vec pixel)}) pixels xys)]
        (.close image)
        pixel-xys))))

(defn mk-img-chars [x y path]
  (map (fn [pixel]
         (-> pixel
           (assoc :c \u2584)
           (update :x (partial + x))
           (update :y (partial + y))))
    (image-values path)))


