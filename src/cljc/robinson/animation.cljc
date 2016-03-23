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
            [robinson.world :as rw]
            [robinson.lineofsight :as rlos]
            [zaffre.aterminal :as zat]
            [robinson.aanimatedterminal :as raat]
            [overtone.at-at :as atat])
  #?(:clj
     (:import  zaffre.aterminal.ATerminal
               robinson.aanimatedterminal.AAnimatedTerminal
               robinson.aanimatedterminal.AId
               robinson.aanimatedterminal.AEffect
               robinson.aanimatedterminal.AFilter
               robinson.aanimatedterminal.AMask
               robinson.aanimatedterminal.ARequiresState
               robinson.aanimatedterminal.APalette)))


#?(:clj
(set! *warn-on-reflection* true))

(def frame-count (atom 0))
(def ^:dynamic *rain-rate* 0.96)

;; util/helper fns
(defn nil-grid [terminal]
  (let [[vw vh]    (zat/get-size terminal)]
    (repeat vh (vec (repeat vw nil)))))

(defn lantern-flicker [opts x y fg]
  {:post [(vector? %)]}
  (if (get opts :lantern-flicker)
    (let [v                   (int (* 50 (/ (Math/abs (hash @frame-count)) Integer/MAX_VALUE)))
          lantern-flicker-rgb [v v 0]]
      (rcolor/add-rgb fg lantern-flicker-rgb))
    fg))

(defn vignette [opts x y fg]
  (if-let [distance (get opts :distance-from-player)]
    (do
      (rcolor/darken-rgb fg
                         (min 1 (/ 2 (max 1 distance)))))
    fg))

(defn night-tint [opts x y fg]
  (if-let [day? (get opts :night-tint)]
    (rcolor/night-tint fg day?)
    fg))

(defn no-op-filter [_ _ _ bg] bg)


;;; Effects

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
        (zat/set-fx-char! terminal :fx x y \|)
        (zat/set-fx-fg! terminal :fx x y (rcolor/color->rgb :blue))
        (zat/set-fx-bg! terminal :fx x y (rcolor/color->rgb :black)))
    :splash1
      (do
        (zat/set-fx-char! terminal :fx x y \*)
        (zat/set-fx-fg! terminal :fx x y (rcolor/color->rgb :blue))
        (zat/set-fx-bg! terminal :fx x y (rcolor/color->rgb :black)))
    :splash2
      (do
        (zat/set-fx-char! terminal :fx x y \o)
        (zat/set-fx-fg! terminal :fx x y (rcolor/color->rgb :blue))
        (zat/set-fx-bg! terminal :fx x y (rcolor/color->rgb :black)))
    :splash3
      (do
        (zat/set-fx-char! terminal :fx x y \°)
        (zat/set-fx-fg! terminal :fx x y (rcolor/color->rgb :blue))
        (zat/set-fx-bg! terminal :fx x y (rcolor/color->rgb :black)))
    :splash4
      (do
        (zat/set-fx-char! terminal :fx x y \·)
        (zat/set-fx-fg! terminal :fx x y (rcolor/color->rgb :blue))
        (zat/set-fx-bg! terminal :fx x y (rcolor/color->rgb :black)))
    nil))

(defn step-rain!
  [rain-state vw vh]
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

(defn render-rand-fg
  [terminal layer-id x y cell-opts cell-palette palette]
  (let [color-ids (get @palette cell-palette)
        i         (int (/ @frame-count 10))
        n         (mod (Math/abs (hash (+ x (* 13 y) (* 17 i)))) (count color-ids))
        color-id  (nth color-ids n)
        fg-rgb    (->> (rcolor/color->rgb color-id)
                    (lantern-flicker cell-opts x y)
                    (vignette cell-opts x y)
                    (night-tint cell-opts x y))]
    (zat/set-fx-fg! terminal layer-id x y fg-rgb)))

(defrecord RandFgEffect
  [terminal mask opts palette]
  AId
  (id [this]
    :rand-fg)
  AEffect
  (apply-effect! [this terminal]
    (let [[vw vh]    (zat/get-size terminal)]
      (doseq [[y line mask-line] (map vector (range) @opts @mask)
              [x cell-opts mask?] (map vector (range) line mask-line)
              :let               [cell-palette  (get cell-opts :palette)]
              :when              cell-palette]
        (render-rand-fg terminal :map x y cell-opts cell-palette palette))))
  APalette
  (update-palette! [this f]
    (swap! palette f)
    this))

(defn make-rand-fg-effect
  [terminal opts]
  (let [[vw vh]   (zat/get-size terminal)
        mask      (atom (repeat vh (repeat vw true)))
        palette   (atom {})]
    (RandFgEffect. terminal mask opts palette)))

(defrecord RainEffect
  [terminal mask rain-state]
  AId
  (id [this]
    :rain)
  AEffect
  (apply-effect! [this terminal]
    (let [[vw vh]    (zat/get-size terminal)]
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
  [terminal cell-opts]
  (let [[vw vh]    (zat/get-size terminal)
        mask       (atom (repeat vh (repeat vw true)))
        rain-state (atom (repeat vh (vec (repeat vw nil))))]
    (RainEffect. terminal mask rain-state)))

(defn swap-rain-mask! [terminal f]
  (raat/swap-matching-effect-or-filter! terminal
                              (fn [fx] (= (raat/id fx) :rain))
                              (fn [rain-fx] (raat/swap-mask! rain-fx f))))

(defn reset-rain-mask! [terminal v]
  (raat/swap-matching-effect-or-filter! terminal
                              (fn [fx] (= (raat/id fx) :rain))
                              (fn [rain-fx]
                                (let [[columns rows] (zat/get-size terminal)]
                                  (raat/reset-mask! rain-fx (repeat rows (repeat columns v)))))))

(defn transform [terminal mask state transforms]
  #_(log/info (format "drawing %d transforms" (count transforms)))
  (doseq [transform transforms]
    (let [path (rlos/line-segment-fast-without-endpoints (get transform :from-xy) (get transform :to-xy))
          n    (int (/ @frame-count 1))]
      (when (< n (count path))
        (log/info "n" n "count path" (count path))
        (let [[vx vy] (rv/world-xy->screen-xy state (nth path n))]
          (when (or true (get-in mask [vy vx]))
            (log/info "drawing * @ " vx vy)
            (zat/set-fx-char! terminal :fx vx vy \*)
            (zat/set-fx-fg! terminal :fx vx vy (rcolor/color->rgb :white))
            (zat/set-fx-bg! terminal :fx vx vy (rcolor/color->rgb :black))))))))
  
(defrecord TransformEffect
  [terminal mask state]
  AId
  (id [this]
    :transform)
  AEffect
  (apply-effect! [this terminal]
    (dosync 
      (when-let [state @state]
        (transform terminal @mask state (get-in state [:fx :transform])))))
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
  ARequiresState
  (reset-state! [this new-state]
    (reset! state new-state)
    this)
  Object
  (toString [this]
    (format "TransformEffect terminal, mask, state")))

(defn make-transform-effect
  [terminal cell-opts]
  (let [[vw vh]    (zat/get-size terminal)
        mask       (atom (repeat vh (repeat vw true)))]
    (TransformEffect. terminal mask (atom nil))))

(defn blink [terminal state blinks]
  ;(log/info (format "drawing %d blinks" (count blinks)))
  (doseq [blink blinks]
    (log/info @frame-count (mod @frame-count 15))
    (when (< (mod @frame-count 15) 7)
      (let [[vx vy] (get blink :xy)]
          (log/info "drawing * @ " vx vy)
          (zat/set-fx-char! terminal :fx vx vy \space)))))

(defrecord BlinkEffect
  [terminal state]
  AId
  (id [this]
    :blink)
  AEffect
  (apply-effect! [this terminal]
    (dosync 
      (when-let [state @state]
        (blink terminal state (get-in state [:fx :blink])))))
  ARequiresState
  (reset-state! [this new-state]
    (reset! state new-state)
    this)
  Object
  (toString [this]
    (format "BlinkEffect terminal, state")))

(defn make-blink-effect
  [terminal cell-opts]
  (let [[vw vh]    (zat/get-size terminal)]
    (BlinkEffect. terminal (atom nil))))

(defn blip [terminal state blips]
  (doseq [blip blips]
    (let [[vx vy] (rv/world-xy->screen-xy state (get blip :xy))]
      ;(log/info "drawing blip" blip "@" vx vy "frame-count" @frame-count)
      ;; get the last instant before @frame-count
      (when-let [instant (->> (get blip :instants)
                              (filter (fn [instant]
                                        (< (get instant :time) @frame-count)))
                              (sort-by (fn [instant]
                                         (get instant :time)))
                              last)]
        ;(log/info "drawing instant" instant)
        (when-let [ch (get instant :ch)]
          (zat/set-fx-char! terminal :fx vx vy ch))
        (when-let [fg (get instant :fg)]
          (zat/set-fx-fg! terminal :fx vx vy fg))
        (when-let [bg (get instant :bg)]
          (zat/set-fx-bg! terminal :fx vx vy bg))))))
    
(defrecord BlipEffect
  [terminal state]
  AId
  (id [this]
    :blip)
  AEffect
  (apply-effect! [this terminal]
    (dosync 
      (when-let [state @state]
        (blip terminal state (get-in state [:fx :blip])))))
  ARequiresState
  (reset-state! [this new-state]
    (reset! state new-state)
    this)
  Object
  (toString [this]
    (format "BlipEffect terminal, state")))

(defn make-blip-effect
  [terminal cell-opts]
  (let [[vw vh]    (zat/get-size terminal)]
    (BlipEffect. terminal (atom nil))))

;;; Filters

(defrecord FunctionFilter
  [filter-id fg-f bg-f fx-fg-f fx-bg-f mask]
  AId
  (id [this]
    filter-id)
  AFilter
  (transform-fg [this cell-opts x y fg]
    (fg-f cell-opts x y fg))
  (transform-bg [this cell-opts x y bg]
    (bg-f cell-opts x y bg))
  (transform-fx-fg [this cell-opts x y fx-fg]
    (fx-fg-f cell-opts x y fx-fg))
  (transform-fx-bg [this cell-opts x y fx-bg]
    (fx-bg-f cell-opts x y fx-bg)))

(defn make-vignette-filter [terminal]
  (FunctionFilter. :vignette-filter
                   vignette
                   no-op-filter
                   vignette
                   no-op-filter
                   (atom (nil-grid terminal))))

(defn make-night-tint-filter [terminal]
  (FunctionFilter. :night-tint-filter
                   night-tint
                   no-op-filter
                   night-tint
                   no-op-filter
                   (atom (nil-grid terminal))))

(defn make-lantern-filter [terminal]
  (FunctionFilter. :lantern-flicker-filter
                   lantern-flicker
                   no-op-filter
                   lantern-flicker
                   no-op-filter
                   (atom (nil-grid terminal))))

(defn apply-filters [cell-opts ch fx-filters]
  (reduce (fn [ch fx-filter]
            ;;(log/info "applying filter" (raat/id fx-filter) "ch" ch)
            (let [{x :x y :y} ch]
              (-> ch
                (update :fg (partial raat/transform-fg fx-filter cell-opts x y))
                (update :bg (partial raat/transform-bg fx-filter cell-opts x y))
                #_((fn [ch] (log/info "ch" ch) ch)))))
          ch
          fx-filters))

;;; Animated Terminal

(defrecord AnimatedTerminal
  [terminal started opts effects filters schedule-pool]
  ATerminal
    ;; pass ATerminal methods through to terminal
    (get-size [this] (zat/get-size terminal))
    (put-chars! [this layer-id characters]
      (swap! opts
             (fn [opts]
               (reduce (fn [opts [y y-characters]]
                         (update opts y (fn [line]
                                               (apply assoc (vec line) (interleave (map :x y-characters)
                                                                                   (map :opts y-characters))))))
                       (vec opts)
                       (group-by :y characters))))
      (let [characters (mapv (fn [ch] (apply-filters (get ch :opts {})  ch @filters)) characters)]
        (zat/put-chars! terminal layer-id characters)))
    (get-key-chan [this] (zat/get-key-chan terminal))
    (set-fg! [this layer-id x y fg] (zat/set-fg! terminal layer-id x y fg))
    (set-bg! [this layer-id x y bg] (zat/set-bg! terminal layer-id x y bg))
    (apply-font! [this windows-font else-font]
       (zat/apply-font! terminal windows-font else-font))
    (set-cursor! [this x y] (zat/set-cursor! terminal x y))
    (clear! [this]
      (reset! opts (nil-grid terminal))
      (zat/clear! terminal))
    (refresh! [this]
      (reset! frame-count 0)
      (zat/clear-fx! terminal)
      (doseq [effect @effects]
        (raat/apply-effect! effect terminal))
      (log/info "done applying effects")
      (zat/refresh! terminal))
    AAnimatedTerminal
    (swap-effect-seq! [this f] (swap! effects f))
    (swap-matching-effect-or-filter!
      [this p f]
      (swap! effects (fn [effects]
                       (reduce (fn [effects effect]
                                 (conj effects (if (p effect)
                                                 (f effect)
                                                 effect)))
                               []
                               effects)))
      (swap! filters (fn [filters]
                       (reduce (fn [filters fx-filter]
                                 (conj filters (if (p fx-filter)
                                                 (f fx-filter)
                                                 fx-filter)))
                               []
                               filters))))
    (start! [this fps]
      (dosync
        (when-not @started
          (reset! started true)
          (atat/every (/ 1000 fps)
                      (fn []
                        (dosync
                          (swap! frame-count inc)
                          (zat/clear-fx! terminal)
                          (doseq [effect @effects]
                            (raat/apply-effect! effect terminal))
                          (zat/assoc-fx-uniform! terminal "time" (swap! frame-count inc))
                          (zat/refresh! terminal)))
                      schedule-pool)))))

(defn wrap-terminal
  [terminal effects-gen-fns filters]
  (let [started       (atom false)
        ;; grid of cell-opts
        opts          (atom (nil-grid terminal))
        effects       (atom (mapv (fn [effect-gen-fn] (effect-gen-fn terminal opts)) effects-gen-fns))
        filters       (atom filters)
        schedule-pool (atat/mk-pool)]
    (AnimatedTerminal. terminal started opts effects filters schedule-pool)))

(defn set-mask! [terminal mask-ids xys v] 
  {:pre [(set? mask-ids)
         (coll? xys)
         (contains? #{true false} v)]}
  (raat/swap-matching-effect-or-filter!
    terminal
    (fn [fx] (and (instance? AMask fx)
                  (contains? mask-ids (raat/id fx))))
    (fn [effect]
      (raat/swap-mask! effect
                       (fn [mask]
                         (reduce (fn [mask [y xys]]
                                   (update mask y (fn [line]
                                                    (apply assoc (vec line) (interleave (map first xys)
                                                                                        (repeat v))))))
                                 (vec mask)
                                        (group-by second xys)))))))
                                                                             

(defn set-palette! [terminal new-palette]
  {:pre [(map? new-palette)]}
  (raat/swap-matching-effect-or-filter!
    terminal
    (fn [fx] (instance? APalette fx))
    (fn [effect]
      (raat/update-palette! effect (fn [palette] new-palette)))))

(defn reset-state! [terminal new-state]
  ;{:pre [(get new-state :fx)]}
  (raat/swap-matching-effect-or-filter!
    terminal
    (fn [fx] (instance? ARequiresState fx))
    (fn [effect]
      (raat/reset-state! effect new-state))))
  
  
