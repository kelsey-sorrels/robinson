;; Functions for developing and debugging
(ns robinson-tools.devtools
  (:require [robinson.common :as rc]
            [robinson.player :as rp]
            [robinson.worldgen :as rwg]
            [robinson.random :as rr]
            [robinson.noise :as rn]
            [quil.core :as q]
            [taoensso.timbre :as log]
            quil.applet))

(defn biome->color
  [biome]
  (case biome
    :jungle        [0 0 128]
    :heavy-forest  [10 60 80]
    :light-forest  [10 80 90]
    :bamboo-grove  [10 210 10]
    :meadow        [128 10 10]
    :rocky         [128 128 128]
    :swamp         [50 90 110]
    :dirt          [90 80 20]
    :sand          [200 200 10]
    :surf          [50 90 210]
    :ocean         [20 70 150]
    [200 0 200]))

(def samples (atom []))
(def applet  (atom nil))

(defn show-world
  [state]
  (let [{width :width
         height :height} (get state :world)
        [player-x
         player-y]       (rp/player-xy state)
        [start-x
         start-y]        (rc/pos->xy (rp/player-starting-pos state))
        seed             (get-in state [:world :seed])
        n                (rn/create-noise (rr/create-random seed))]
    (when (or (not @applet)
              (.finished @applet)
              (not (q/current-graphics)))
      (when @applet
        (.dispose @applet))
      (reset! applet (letfn [(draw  [] (doseq [[x y w h c] @samples]
                                         (q/fill (apply q/color c))
                                         (q/rect x y w h)))
                             (setup [] (q/frame-rate 1)
                                       (q/background 0 0 0))]
                       (quil.applet/applet
                                    :title "Show World"
                                    :renderer :p2d
                                    :setup setup
                                    :draw draw
                                    :size [(* 2 width) (* 2 height)]))))
    (reset! samples (concat
                      (vec
                        (for [y (range (- height) height 2)
                              x (range (- width) width 2)]
                          [(+ x width) (+ y height) 2 2 (biome->color (rwg/sample-island n x y))]))
                       ;; mark player position red
                      [[(+ player-x width) (+ player-y height) 8 8 [255 0 0]]
                       ;; mark starting location pink
                       [(+ start-x width)  (+ start-y height)  8 8 [255 0 255]]]))
    (log/info "samples" samples)))
