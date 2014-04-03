(ns dungeon-crusade.main
  (use     dungeon-crusade.common
           dungeon-crusade.update
           dungeon-crusade.render)
  (require [lanterna.screen :as s]))

;; Example setup and tick fns
(defn setup []
  (let [screen (s/get-screen :swing)
        terminal (.getTerminal screen)
        world (if (.exists (clojure.java.io/file "world.save"))
                (read-string (slurp "world.save"))
                (init-world))]
    (s/start screen)
    {:world world :screen screen :terminal terminal}))

(def render-count (atom 0))
(defn tick [state]
  (do
    (swap! render-count inc)
    (when (> @render-count 1) (throw nil))
    (render state)
    (swap! render-count dec)
    (let [keyin  (s/get-key-blocking (state :screen))]
      (println "got " keyin " type " (type keyin))
      (let [newstate (update-state state keyin)]
        (spit "world.save" (state :world))
        newstate))))

