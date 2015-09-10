;; Utility functions and functions for manipulating state
(ns robinson-tools.worldgen
  (:require 
            [robinson.common :as rc]
            [taoensso.timbre :as log]
            [robinson.random :as rr]
            [robinson.noise :as rn]
            [robinson.world :as rw]
            #?@(:clj (
                [quil.core :as q]
                quil.applet))))

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

(defn -main [& args]
  (let [width   400
        height  400
        seed    1
        n       (rn/create-noise (rr/create-random seed))
        samples (for [y (range (- height) height 2)
                      x (range (- width) width 2)]
                  [(+ x width) (+ y height) 2 2 (rwg/sample-island n x y)])]
    #_(letfn [(draw  [] (doseq [[x y w h b] samples]
                        (q/fill (apply q/color (biome->color
                                                 (case b
                                                   :ocean :ocean
                                                   :surf :ocean
                                                   :sand))))
                        (q/rect x y w h)))
            (setup [] (q/frame-rate 1)
                      (q/no-stroke)
                      (q/background 0 0 0))]
      (quil.applet/applet
                   :title "Ocean/Sand"
                   :renderer :p2d
                   :setup setup
                   :draw draw
                   :size [(* 2 width) (* 2 height)]))
    (letfn [(draw  [] (doseq [[x y w h b] samples]
                        (q/fill (apply q/color (biome->color
                                                 (case b
                                                   :ocean :ocean
                                                   :sand :sand
                                                   :surf :surf
                                                   :dirt))))
                        (q/rect x y w h)))
            (setup [] (q/frame-rate 1)
                      (q/no-stroke)
                      (q/background 0 0 0))]
      (quil.applet/applet
                   :title "Ocean/Surf/Sand/Interior"
                   :renderer :p2d
                   :setup setup
                   :draw draw
                   :size [(* 2 width) (* 2 height)]))
    #_(letfn [(draw  [] (doseq [[x y w h b] samples]
                        (q/fill (apply q/color (biome->color b)))
                        (q/rect x y w h)))
            (setup [] (q/frame-rate 1)
                      (q/no-stroke)
                      (q/background 0 0 0))]
      (quil.applet/applet
                   :title "Ocean/Surf/Sand/Interior"
                   :renderer :p2d
                   :setup setup
                   :draw draw
                   :size [(* 2 width) (* 2 height)]))))


