(ns dungeon-crusade.main
  (:use    clojure.pprint
           dungeon-crusade.common
           [dungeon-crusade.worldgen :exclude [-main]]
           dungeon-crusade.dialog
           dungeon-crusade.npc
           dungeon-crusade.update
           [dungeon-crusade.monstergen :exclude [-main]]
           dungeon-crusade.render)
  (:require 
            [dungeon-crusade.swingterminal :as swingterminal]
            clojure.edn
            ;[lanterna.screen :as s]
            [taoensso.timbre :as timbre]
            [clojure.core.async :as async]))


(timbre/refer-timbre)

(timbre/set-config! [] (read-string (slurp "config/timbre.clj")))

(defn tick
  "The game loop.

   Take the current state, render it, wait for player input, then update
   the state using the player's input and return the new state. Save the
   world too, in case the  game is interrupted. Then we can load it next
   time we start up."
  ([state]
   (let [keyin (swingterminal/wait-for-key (state :screen))]
     (if keyin
       (tick state keyin)
       state)))
  ([state keyin]
    (do
      (info "got " (str keyin) " type " (type keyin))
      (log-time "tick"
        (let [new-state (log-time "update-state" (update-state state keyin))]
          (log-time "render" (render new-state))
          (async/thread (spit "save/world.edn" (prn-str (new-state :world))))
          ;(async/thread (spit "save/world.edn" (with-out-str (pprint (new-state :world)))))
          new-state)))))

;; Example setup and tick fns
(defn setup
  "Create the intial `state` value.

   `state` contains
    
   * a `:world` that contains places, npcs, a player

   * a `:screen` to render the world

   * `quests` that are loaded dynamically on startup."
  []
  (let [terminal  (swingterminal/make-terminal 80 24)
        world (if (.exists (clojure.java.io/file "save/world.edn"))
                (->> (slurp "save/world.edn")
                     (clojure.edn/read-string {:readers {'dungeon_crusade.monstergen.Monster map->Monster}}))
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
                                 (apply merge (map :dialog quests))))
        state {:world world :screen terminal :quests quest-map :dialog dialog}
        state (reduce (fn [state _] (add-npcs state 1)) state (range 5))]

    ;; tick once using the rest (.) command to update visibility
    (tick state \.)))

