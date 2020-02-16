;; Functions for rendering state to screen
(ns robinson.fx.rain
  (:require
            [taoensso.timbre :as log]
            [zaffre.color :as zcolor]
            [robinson.random :as rr]
            [robinson.color :as rcolor]))
 
(defn rain-transition
  [advanced-cell old-cell]
  (cond
    ;; drop->nil
    ;; drop->drop
    (= advanced-cell :drop)
      (cond
        (> (rand) 0.95)
          nil
        (> (rand) 0.95)
          :splash1
        :else
          :drop)
    ;; drop->splash
    (= old-cell :splash1)
      :splash2
    (= old-cell :splash2)
      :splash3
    (= old-cell :splash3)
      :splash4
    (= old-cell :splash4)
      nil
    :else
    nil))

(def blue (rcolor/color->rgb :blue))
(def bg (zcolor/color 0 0 0 0))
(def drop
      {:c \|
       :fg blue
       :bg bg
       :blend-mode :screen})
(def splash1
      {:c  \*
       :fg blue
       :bg bg
       :blend-mode :screen})
(def splash2
      {:c  \o
       :fg blue
       :bg bg
       :blend-mode :screen})
(def splash3
      {:c  \°
       :fg blue
       :bg bg
       :blend-mode :screen})
(def splash4
      {:c  \
       :fg blue
       :bg bg
       :blend-mode :screen})

(defn render-rain-cell
  [cell] 
  (case cell
    :drop drop
    :splash1 splash1
    :splash2 splash2
    :splash3 splash3
    :splash4 splash4
    nil))

(defn step-rain
  [rain-state vw vh rain-rate]
  ;; update rain
  ;(println "------------------------------------------------------")
  (vec  (for [[y advanced-line old-line] (map vector (range)
                                                     (concat [(repeat vw nil)] (butlast rain-state))
                                                     rain-state)]
     (do
     ;(println (mapv (fn [v] (if v 1 0)) old-line))
     ;; prev-cell is the value above `cell`.
     (vec (for [[x advanced-cell old-cell] (map vector (range) advanced-line old-line)]
       (if (zero? y)
         (if (> (rand) rain-rate)
           :drop
           nil)
         (rain-transition advanced-cell old-cell))))))))

