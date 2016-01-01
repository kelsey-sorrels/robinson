;; Functions for animating state to screen
(ns robinson.animation
  (:require 
            [taoensso.timbre :as log]
            [robinson.random :as rr]
            [robinson.math :as rmath]
            [robinson.common :as rc :refer [farther-than?
                                            wrap-line
                                            fill-missing
                                            xy->pos]]
                                            
            [robinson.color :as rcolor]
            [robinson.viewport :as rv :refer [cells-in-viewport]]
            [robinson.aterminal :as rat]
            [robinson.aanimatedterminal :as raat]
            [overtone.at-at :as atat])
  #?(:clj
     (:import  robinson.aterminal.ATerminal
               robinson.aanimatedterminal.AAnimatedTerminal
               robinson.aanimatedterminal.AEffect
               robinson.aanimatedterminal.AMask
               robinson.aanimatedterminal.ACellOpts
               robinson.aanimatedterminal.APalette)))


#?(:clj
(set! *warn-on-reflection* true))

;(def rain-cache (atom nil))

(def ^:dynamic *rain-rate* 0.96)

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

(defn render-rain-cell
  [terminal x y cell] 
  (case cell
    :drop
      (do
        (rat/set-fx-char! terminal x y \|)
        (rat/set-fx-fg! terminal x y (rcolor/color->rgb :blue))
        (rat/set-fx-bg! terminal x y (rcolor/color->rgb :black)))
    :splash1
      (do
        (rat/set-fx-char! terminal x y \*)
        (rat/set-fx-fg! terminal x y (rcolor/color->rgb :blue))
        (rat/set-fx-bg! terminal x y (rcolor/color->rgb :black)))
    :splash2
      (do
        (rat/set-fx-char! terminal x y \o)
        (rat/set-fx-fg! terminal x y (rcolor/color->rgb :blue))
        (rat/set-fx-bg! terminal x y (rcolor/color->rgb :black)))
    :splash3
      (do
        (rat/set-fx-char! terminal x y \°)
        (rat/set-fx-fg! terminal x y (rcolor/color->rgb :blue))
        (rat/set-fx-bg! terminal x y (rcolor/color->rgb :black)))
    :splash4
      (do
        (rat/set-fx-char! terminal x y \·)
        (rat/set-fx-fg! terminal x y (rcolor/color->rgb :blue))
        (rat/set-fx-bg! terminal x y (rcolor/color->rgb :black)))
    ;; default
      (do
        (rat/set-fx-char! terminal x y nil)
        (rat/set-fx-fg! terminal x y nil)
        (rat/set-fx-bg! terminal x y nil))))

(defn step-rain!
  [rain-state vw vh]
  ;; create empty rain values if not present
  ;(swap! rain-state (fn [rain]
  ;                    (if (nil? rain)
  ;                      (repeat vh (vec (repeat vw nil)))
  ;                      rain)))
  ;; update rain
  ;(println "------------------------------------------------------")
  (swap! rain-state (fn [rain]
                     (vec  (for [[y advanced-line old-line] (map vector (range)
                                                                        (concat [(repeat vw nil)] (butlast rain))
                                                                        rain)]
                        (do
                        ;(println (mapv (fn [v] (if v 1 0)) line))
                        ;; prev-cell is the value above `cell`.
                        (vec (for [[x advanced-cell old-cell] (map vector (range) advanced-line old-line)]
                          (if (zero? y)
                            (if (> (rand) *rain-rate*)
                              :drop
                              nil)
                            (rain-transition advanced-cell old-cell))))))))))

(defn make-rand-fg-effect
  [terminal]
  (let [[vw vh] (rat/get-size terminal)
        mask    (atom (repeat vh (repeat vw true)))
        palette (atom {})]
    (reify AEffect
      (id=? [this id]
        (= :rand-fg id))
      (apply-effect! [this terminal]
        nil
        #_(doseq [[y line mask-line] (map vector (range) @rain-cache @mask)
                [x cell mask?] (map vector (range) line mask-line)
                :when mask?]
          (render-rain-cell terminal x y cell)))
      AMask
      (swap-mask! [this f]
        (swap! mask f)
        ;(println "=====================")
        ;(doseq [line @mask]
        ;  (println (mapv (fn [v] (if v 1 0)) line)))
        this)
      (reset-mask! [this new-mask]
        (reset! mask new-mask)
        this)
      ACellOpts
      (set-cell-opts! [this opts]
        ;; nop
        this))))
  
  
;(defn make-rain-effect
;  [terminal]
;  (let [[vw vh] (rat/get-size terminal)
;        mask    (atom (repeat vh (repeat vw true)))]
;    (reify AEffect
;      (id=? [this id]
;        (= :rain id))
;      (apply-effect! [this terminal]
;        (dosync
;          (step-rain! vw vh)
;          (doseq [[y line mask-line] (map vector (range) @rain-cache @mask)
;                  [x cell mask?] (map vector (range) line mask-line)
;                  :when mask?]
;            (render-rain-cell terminal x y cell))))
;      AMask
;      (swap-mask! [this f]
;        (swap! mask f)
;        ;(println "=====================")
;        ;(doseq [line @mask]
;        ;  (println (mapv (fn [v] (if v 1 0)) line)))
;        this)
;      (reset-mask! [this new-mask]
;        (reset! mask new-mask)
;        this))))

  
(defrecord RainEffect
  [terminal mask rain-state]
  AEffect
  (id=? [this id]
    (= :rain id))
  (apply-effect! [this terminal]
    (let [[vw vh]    (rat/get-size terminal)]
      (dosync
        (step-rain! rain-state vw vh)
        (doseq [[y line mask-line] (map vector (range) @rain-state @mask)
                [x cell mask?] (map vector (range) line mask-line)
                :when mask?]
          (render-rain-cell terminal x y cell)))))
  AMask
  (swap-mask! [this f]
    (swap! mask f)
    ;(println "=====================")
    ;(doseq [line @mask]
    ;  (println (mapv (fn [v] (if v 1 0)) line)))
    this)
  (reset-mask! [this new-mask]
    (reset! mask new-mask)
    this))

(defn make-rain-effect
  [terminal]
  (let [[vw vh]    (rat/get-size terminal)
        mask       (atom (repeat vh (repeat vw true)))
        rain-state (atom (repeat vh (vec (repeat vw nil))))]
    (RainEffect. terminal mask rain-state)))

(defn swap-rain-mask! [terminal f]
  (raat/swap-matching-effect! terminal
                              (fn [fx] (raat/id=? fx :rain))
                              (fn [rain-fx] (raat/swap-mask! rain-fx f))))

(defn reset-rain-mask! [terminal v]
  (raat/swap-matching-effect! terminal
                              (fn [fx] (raat/id=? fx :rain))
                              (fn [rain-fx]
                                (let [[columns rows] (rat/get-size terminal)]
                                  (raat/reset-mask! rain-fx (repeat rows (repeat columns v)))))))

(defn wrap-terminal
  ([terminal & effects-gen-fns]
    (let [started       (atom false)
          effects       (atom (mapv (fn [effect-gen-fn] (effect-gen-fn terminal)) effects-gen-fns))
          schedule-pool (atat/mk-pool)]
      (reify ATerminal
          ;; pass ATerminal methods through to terminal
          (get-size [this] (rat/get-size terminal))
          (put-string [this x y string] (rat/put-string terminal x y string))
          (put-string [this x y string fg bg] (rat/put-string terminal x y string fg bg))
          (put-string [this x y string fg bg style] (rat/put-string terminal x y string fg bg style))
          (put-chars [this characters] (rat/put-chars terminal characters))
          (set-fg [this x y fg] (rat/set-fg terminal x y fg))
          (set-bg [this x y bg] (rat/set-bg terminal x y bg))
          (get-key-chan [this] (rat/get-key-chan terminal))
          (apply-font [this windows-font else-font size antialias] (rat/apply-font terminal windows-font else-font size antialias))
          (set-cursor [this xy] (rat/set-cursor terminal xy))
          (clear [this] (rat/clear terminal))
          ;; override refresh
          (refresh [this] (doseq [effect @effects]
                            (raat/apply-effect! effect terminal))
                          (rat/refresh terminal))
          AAnimatedTerminal
          (swap-effect-seq! [this f] (swap! effects f))
          (swap-matching-effect! [this p f] (swap! effects (fn [effects]
                                                             (reduce (fn [effects effect]
                                                                       (conj effects (if (p effect)
                                                                                       (f effect)
                                                                                       effect)))
                                                                     []
                                                                     effects))))
          (start! [this fps]
            (dosync
              (when-not @started
                (reset! started true)
                (atat/every (/ 1000 fps)
                            (fn []
                              (doseq [effect @effects]
                                (raat/apply-effect! effect terminal))
                              (rat/refresh terminal))
                            schedule-pool))))))))

(defn set-mask! [terminal mask-ids xys v]                                                                           
  {:pre [(set? mask-ids)
         (coll? xys)
         (contains? #{true false} v)]}
  (doseq [mask-id mask-ids]
         (raat/swap-matching-effect! terminal
                                     (fn [fx]
                                              (log/info "fx type" (type fx))
                                              (log/info "fx" fx)
                                              (and (instance? AMask fx)
                                                   (raat/id=? fx mask-id)))
                                     (fn [effect]
                                       (raat/swap-mask! effect
                                                        (fn [mask]
                                                          (reduce (fn [mask [y xys]]
                                                                    (update mask y (fn [line]
                                                                                     (apply assoc (vec line) (interleave (map first xys)
                                                                                                                         (repeat v))))))
                                                                  (vec mask)
                                                                  (group-by second xys))))))))
                                                                             


