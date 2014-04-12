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
                (init-world))
         ;; load quests
         _ (doall (map #(load-file (.getPath %))
                     (filter (fn [file] (.endsWith (.getPath file) ".clj"))
                             (.listFiles (clojure.java.io/file "src/dungeon_crusade/quests")))))

         ;; get a list of all the quests that have been loaded
         quests (map deref (flatten (map #(-> % ns-publics vals)
                                         (filter #(.contains (-> % ns-name str)
                                                             "dungeon-crusade.quests")
                                                 (all-ns)))))
        _ (doall (map #(println "Loaded quest" (% :name)) quests))]

    (s/start screen)
    ;; tick once using the rest (.) command to update visibility
    (update-state {:world world :screen screen :terminal terminal :quests quests} \.)))

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

