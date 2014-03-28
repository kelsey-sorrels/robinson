(ns dungeon-crusade.main
  (use     dungeon-crusade.common
           dungeon-crusade.update
           dungeon-crusade.render)
  (require [lanterna.screen :as s]))

;; Example setup and tick fns
(defsource setup []
  (let [screen (s/get-screen :swing)
        terminal (.getTerminal screen)]
    (s/start screen)
    {:world (init-world) :screen screen :terminal terminal :time 0}))

(def render-count (atom 0))
(defn tick [state]
  (do
    (println "before-render")
    (swap! render-count inc)
    (when (> @render-count 1) (throw nil))
    (render state)
    (println "after-render")
    (swap! render-count dec)
    (let [keyin  (s/get-key-blocking (state :screen))]
      (println "got " keyin " type " (type keyin))
      (let [newstate (update-state state keyin)]
        (if-not (nil? newstate)
          (update-in newstate [:time] (fn [t] (inc t)))
          nil)))))

