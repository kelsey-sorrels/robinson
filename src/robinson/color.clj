(ns robinson.color
  (:require [zaffre.color :as zcolor]
            [tinter.core :as tinter]))

;; RBG color definitions.
;; It's easier to use names than numbers.
(def color-to-rgb-map
  {:brown             (zcolor/color 139 69 19)
   :dark-brown        (zcolor/color 70 44 9)
   :ship-brown        (zcolor/color 67 51 30)
   :ship-light-brown  (zcolor/color 131 88 32)
   :ship-dark-brown   (zcolor/color 33 25 15)
   ;;:black           (zcolor/color 0 0 0]
   :black             (zcolor/color 6 6 19)
   :white             (zcolor/color 255 255 255)
   :gray              (zcolor/color 128 128 128)
   :dark-gray         (zcolor/color 64 64 64)
   :darker-gray       (zcolor/color 32 32 32)
   :light-gray        (zcolor/color 192 192 192)
   :lighter-gray      (zcolor/color 225 225 225)
   :red               (zcolor/color 190 38 51);(vec (tinter/hex-str-to-dec "D31C00"))
   :dark-red          (zcolor/color 110 18 21);(vec (tinter/hex-str-to-dec "D31C00"))
   :orange            (zcolor/color 235 137 49);(vec (tinter/hex-str-to-dec "D36C00"))
   :yellow            (zcolor/color 247 226 107);(vec (tinter/hex-str-to-dec "D3B100"))
   ;:highlight         [229 165 8];(vec (tinter/hex-str-to-dec "D3B100"))
   :highlight         (zcolor/color 183 124 7);(vec (tinter/hex-str-to-dec "D3B100"))
   :background        (zcolor/color 6 8 12)
   :sea-foam          (zcolor/color 144 213 184)
   :light-green       (zcolor/color 163 206 39)
   :green             (zcolor/color 68 137 26);(vec (tinter/hex-str-to-dec "81D300"))
   :dark-green        (zcolor/color 64 105 00)
   :moss-green        (zcolor/color 1 140 1)
   :temple-beige      (zcolor/color 191 171 143)
   :blue-green        (zcolor/color 55 148 110);(vec (tinter/hex-str-to-dec "19B4D7"))
   :blue              (zcolor/color 0 87 132);(vec (tinter/hex-str-to-dec "00ACD3"))
   :brilliant-blue    (zcolor/color 50 137 172);(vec (tinter/hex-str-to-dec "00ACD3"))
   :light-blue        (zcolor/color 203 219 252);(vec (tinter/hex-str-to-dec "19B4D7"))
   :dark-blue         (zcolor/color 0 63 116)
   :purple            (zcolor/color 133 0 211)
   :fushia            (zcolor/color 211 0 148)
   :light-brown       (zcolor/color 216 196 116)
   :beige             (zcolor/color 200 180 100)
   :dark-beige        (zcolor/color 124 97 45)
   :transparent       (zcolor/with-alpha (zcolor/color 0 0 0) 0)})

(defn limit-channel
  ([v]
    (limit-channel 0 v 255))
  ([min-v v max-v]
    (unchecked-byte (min (max min-v v) max-v))))

(defn map-rgb [f rgba]
  (zcolor/color
    (limit-channel (f (zcolor/red rgba)))
    (limit-channel (f (zcolor/green rgba)))
    (limit-channel (f (zcolor/blue rgba)))
    (zcolor/alpha rgba)))

(defn map-rgba [f rgba]
  (zcolor/color
    (limit-channel (f (zcolor/red rgba)))
    (limit-channel (f (zcolor/green rgba)))
    (limit-channel (f (zcolor/blue rgba)))
    (limit-channel (f (zcolor/alpha rgba)))))
    
(defn map-colors [f rgba1 rgba2]
  (zcolor/color
    (f (zcolor/red rgba1) (zcolor/red rgba2))
    (f (zcolor/green rgba1) (zcolor/green rgba2))
    (f (zcolor/blue rgba1) (zcolor/blue rgba2))
    (f (zcolor/alpha rgba1) (zcolor/alpha rgba2))))

(defn min-color [rgba1 rgba2]
  (map-colors min rgba1 rgba2))

(defn max-color [rgba1 rgba2]
  (map-colors max rgba1 rgba2))

(defn min-rgb [rgba]
  (min (min (long (zcolor/red rgba))
            (long (zcolor/green rgba)))
       (long (zcolor/blue rgba))))

(defn max-rgb [rgba]
  (max (max (long (zcolor/red rgba))
            (long (zcolor/green rgba)))
        (long (zcolor/blue rgba))))

(def min-mono (zcolor/const-color 6 6 11))
(defn rgb->mono
  [rgba]
  (let [hi-rgb (long (max-rgb rgba))
        lo-rgb (long (min-rgb rgba))
        avg (long (bit-shift-right  (+ hi-rgb lo-rgb) 1))]
   (max-color (zcolor/color avg avg avg)
              min-mono)))

(defn color->rgb
  ([color]
    (color->rgb color 255))
  ([color alpha]
  {:post [(integer? %)]}
  (zcolor/with-alpha (color color-to-rgb-map) alpha)))

(def min-dark (zcolor/const-color 5 5 7))
(defn darken-rgb
  [rgb d]
  (assert (integer? rgb) (str "rgb not a color (integer)"))
  (max-color (map-rgba (partial * d) rgb) min-dark))

(defn add-rgb
  [rgb1 rgb2]
  {:post [(vector? %)]}
  (map-colors (comp limit-channel +) rgb1 rgb2))

(defn lerp [initial final n]
  (+ initial (* n (- final initial))))

(defn lerp-rgb [initial-rgb final-rgb n]
  (map-colors #(unchecked-byte (int (lerp %1 %2 n))) initial-rgb final-rgb))

(def night-rgb
  (zcolor/color
    (byte (/ 255 2))
    (byte (/ 255 2))
    (byte (/ 255 3))
    127))

(def day-rgb
  (zcolor/color 255 255 255 0)) 

(defn lighting
  [sight-distance]
  (cond
    (<= sight-distance 4)
      night-rgb
    (<= sight-distance 7)
      (lerp-rgb night-rgb day-rgb (/ (- sight-distance 4) (- 7 4)))
    :else
      day-rgb))

(defn night-tint
  [rgb d]
  (if (> d 4)
    rgb
    (zcolor/color
      (/ (zcolor/red rgb) 4)
      (/ (zcolor/green rgb) 3)
      (/ (max-rgb rgb) 2))))

;; returns [char fg bg]
(defn night-tint-npc
  [[s fg bg] d]
   [s (night-tint (color->rgb (or fg :white)) d) (color->rgb (or bg :black))])

(defn target-tint-npc
  [[s fg bg] targeted?]
   [s fg (if targeted? (:green color->rgb) bg)])

(defn color-bloodied-char
  [bloodied? fg]
  (if bloodied?
    :dark-red
    fg))

