(ns dungeon-crusade.main
  (:use    clojure.pprint
           dungeon-crusade.common
           dungeon-crusade.worldgen
           dungeon-crusade.dialog
           dungeon-crusade.update
           dungeon-crusade.render)
  (:require [lanterna.screen :as s]
            [taoensso.timbre :as timbre]
            [clojure.core.async :as async]))

(timbre/refer-timbre)

(defn tick
  "The game loop.

   Take the current state, render it, wait for player input, then update
   the state using the player's input and return the new state. Save the
   world too, in case the  game is interrupted. Then we can load it next
   time we start up."
  ([state]
   (let [keyin  (s/get-key-blocking (state :screen))]
     (tick state keyin)))
  ([state keyin]
    (do
      (info "got " keyin " type " (type keyin))
      (log-time "tick"
        (let [new-state (log-time "update-state" (update-state state keyin))]
          (async/thread (spit "save/world.clj" (with-out-str (pprint (new-state :world)))))
          (log-time "render" (render new-state))
          new-state)))))

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
    (tick {:world world :screen screen :quests quest-map :dialog dialog} \.)))

