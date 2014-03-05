(ns dungeon-crusade.main
  (require [lanterna.screen :as s]))

(defmacro defsource
  "Similar to clojure.core/defn, but saves the function's definition in the var's
   :source meta-data."
  {:arglists (:arglists (meta (var defn)))}
  [fn-name & defn-stuff]
  `(do (defn ~fn-name ~@defn-stuff)
       (alter-meta! (var ~fn-name) assoc :source (quote ~&form))
       (var ~fn-name)))

(defn ascii-to-place [ascii]
  (let [char-to-cell (fn [c]
    (case c
      \x {:type :wall}
      nil))]
    (map (fn [line] (map char-to-cell line)) ascii)))

(defn init-place-0 []
  (ascii-to-place
    ["xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
     "x                               x"
     "x                               x"
     "x                               x"
     "x                               x"
     "x                               x"
     "x                               x"
     "x                               x"
     "x                               x"
     "x                               x"
     "x                               x"
     "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"]))

(defn init-world []
  {:places {:0 (init-place-0)}
   :current-place :0
   :player {:hp 10
            :sym "@"
            :pos {:x 5 :y 5}}})

(defn current-place [state]
  (let [current-place (-> state :world :current-place)]
    (-> state :world :places current-place)))

(defn with-xy [f place]
  (dorun 
    (map (fn [e] (apply f e)) 
         (mapcat concat (map-indexed (fn [y line] (map-indexed (fn [x cell] [cell x y]) line)) place)))))

(defn get-xy [x y place]
  (when-first [cell (filter (fn [cell cx cy] (and (= x cx) (= y cy))) (with-xy concat place))]
    cell))

(defn collide? [x y place]
  (if-let [cell (get-xy x y place)]
    (= (cell :type) :wall)
    false))

(defn move-left [state]
  (let [x (-> state :world :player :pos :x)
        y (-> state :world :player :pos :y)]
    (when-not (collide? (- x 1) y (current-place state))
      (assoc-in state [:world :player :pos :x] (- x 1)))))
  
(defn move-right [state]
  (let [x (-> state :world :player :pos :x)
        y (-> state :world :player :pos :y)]
    (when-not (collide? (+ x 1) y (current-place state))
      (assoc-in state [:world :player :pos :x] (+ x 1)))))
  
(defn move-up [state]
  (let [x (-> state :world :player :pos :x)
        y (-> state :world :player :pos :y)]
    (when-not (collide? x (- y 1) (current-place state))
      (assoc-in state [:world :player :pos :y] (- y 1)))))
  
(defn move-down [state]
  (let [x (-> state :world :player :pos :x)
        y (-> state :world :player :pos :y)]
    (println "move-down")
    (when-not (collide? x (+ y 1) (current-place state))
      (assoc-in state [:world :player :pos :y] (+ y 1)))))
  
(defn update-state [state keyin]
  (case keyin
    \h (move-left state)
    \j (move-down state)
    \k (move-up state)
    \l (move-right state)
    state))

(defn render [state]
  (do
    (s/clear (state :screen))
    ;; draw map
    (with-xy
      (fn [cell x y]
        (when cell
          (let [outchar (case (cell :type)
            :wall "x"
            "?")]
            (s/put-string (state :screen) x y outchar))))
      (current-place state))
    ;; draw character
    (println (-> state :world :player))
    (s/put-string
      (state :screen)
      (-> state :world :player :pos :x)
      (-> state :world :player :pos :y)
      (-> state :world :player :sym)
      {:fg :green})
    (s/redraw (state :screen))))

;; Example setup and tick fns
(defsource setup []
  (let [screen (s/get-screen :swing)]
    (s/start screen)
    {:world (init-world) :screen screen}))

(defn tick [state]
  (let [keyin  (s/get-key-blocking (state :screen))]
    (println "got " keyin " type " (type keyin))
    (let [newstate (update-state state keyin)]
      (render newstate)
      newstate)))

