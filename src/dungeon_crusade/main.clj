(ns dungeon-crusade.main
  (:use    clojure.pprint
           dungeon-crusade.worldgen
           dungeon-crusade.dialog
           dungeon-crusade.update
           dungeon-crusade.render)
  (:require [lanterna.screen :as s]
            [taoensso.timbre :as timbre]))

(timbre/refer-timbre)

;; Example setup and tick fns
(defn setup
  "Create the intial `state` value.

   `state` contains
    
   * a `:world` that contains places, npcs, a player

   * a `:screen` to render the world

   * `quests` that are loaded dynamically on startup."
  []
  (let [screen (s/get-screen :swing)
        world (if (.exists (clojure.java.io/file "save/world.clj"))
                (read-string (slurp "save/world.clj"))
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
        quest-map (apply hash-map (mapcat (fn [i] [(i :id) i]) quests))
        _ (doall (map #(info "Loaded quest" (% :name)) quests))
        _ (info "dialogs" (apply merge (map :dialog quests)))
        dialog (apply merge (map (fn [[k v]]
                                   {k (dialog->fsm v)})
                                 (apply merge (map :dialog quests))))]

    (s/start screen)
    ;; tick once using the rest (.) command to update visibility
    (update-state {:world world :screen screen :quests quest-map :dialog dialog} \.)))

;; Sometimes rendering can loop and freeze the game. Keep track of how many times
;; `render` has been called in each tick and throw if render-coutn > 1.
(def render-count (atom 0))

(defn tick
  "The game loop.

   Take the current state, render it, wait for player input, then update
   the state using the player's input and return the new state. Save the
   world too, in case the  game is interrupted. Then we can load it next
   time we start up."
  [state]
  (do
    (swap! render-count inc)
    (when (> @render-count 1) (throw nil))
    (render state)
    (swap! render-count dec)
    (let [keyin  (s/get-key-blocking (state :screen))]
      (info "got " keyin " type " (type keyin))
      (let [newstate (update-state state keyin)]
        (spit "save/world.clj" (with-out-str (pprint (state :world))))
        newstate))))

