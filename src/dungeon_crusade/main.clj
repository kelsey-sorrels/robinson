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
      \| {:type :vertical-wall}
      \- {:type :horizontal-wall}
      \. {:type :floor}
      \+ {:type :door}
      \# {:type :corridor}
      nil))]
    (map (fn [line] (map char-to-cell line)) ascii)))

(defn init-place-0 []
  (ascii-to-place
    ["-------------          ----------"
     "|...........|          |........|"
     "|...........|          |........|"
     "|...........|          |........|"
     "|...........|          |........|"
     "|...........|          |........|"
     "|...........+######    |........|"
     "|...........|     #    |........|"
     "|...........|     #####+........|"
     "|...........|          |........|"
     "|...........|          |........|"
     "-------------          ----------"]))

(defn init-world []
  {:places {:0 (init-place-0)}
   :current-place :0
   :player {:hp 10
            :sym "@"
            :pos {:x 5 :y 5}}})

(defn current-place [state]
  (let [current-place (-> state :world :current-place)]
    (-> state :world :places current-place)))

(defn with-xy [place]
  (mapcat concat (map-indexed (fn [y line] (map-indexed (fn [x cell] [cell x y]) line)) place)))

(defn map-with-xy [f place]
  (dorun 
    (map (fn [e] (apply f e)) (with-xy place))))

(defn get-xy [x y place]
  (when-first [cell (filter (fn [[cell cx cy]] (and (= x cx) (= y cy))) (with-xy place))]
    cell))

(defn collide? [x y place]
  (let [cellxy (get-xy x y place)]
    (println "check cell " cellxy)
    (let [cell (first cellxy)]
      (if (-> cell nil? not)
        (some (fn [collision-type] (= (cell :type) collision-type)) [:vertical-wall :horizontal-wall])
        true))))

(defn move-left [state]
  (let [x (-> state :world :player :pos :x)
        y (-> state :world :player :pos :y)]
    (if-not (collide? (- x 1) y (current-place state))
      (assoc-in state [:world :player :pos :x] (- x 1))
      state)))
  
(defn move-right [state]
  (let [x (-> state :world :player :pos :x)
        y (-> state :world :player :pos :y)]
    (if-not (collide? (+ x 1) y (current-place state))
      (assoc-in state [:world :player :pos :x] (+ x 1))
      state)))
  
(defn move-up [state]
  (let [x (-> state :world :player :pos :x)
        y (-> state :world :player :pos :y)]
    (if-not (collide? x (- y 1) (current-place state))
      (assoc-in state [:world :player :pos :y] (- y 1))
      state)))
  
(defn move-down [state]
  (let [x (-> state :world :player :pos :x)
        y (-> state :world :player :pos :y)]
    (println "move-down")
    (if-not (collide? x (+ y 1) (current-place state))
      (assoc-in state [:world :player :pos :y] (+ y 1))
      state)))
  
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
    (map-with-xy
      (fn [cell x y]
        (when cell
          (let [outchar (case (cell :type)
            :vertical-wall   "|"
            :horizontal-wall "-"
            :floor           "."
            :door            "+"
            :corridor        "#"
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
    ;; draw status bar
    (s/put-string (state :screen) 0  23
      (format " %s $%d HP:%d(%d) Pw:%d Amr:%d XP:%d/%d T%d                             "
        "location-detail" 0 10 0 0 0 0 100 0) {:fg :black :bg :white})
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

