(ns robinson.main
  (:use    clojure.pprint
           clojure.stacktrace
           robinson.common
           [robinson.worldgen :exclude [-main]]
           robinson.dialog
           robinson.npc
           robinson.update
           [robinson.monstergen :exclude [-main]]
           robinson.render)
  (:require 
            [clojure.data.generators :as dg]
            [robinson.swingterminal :as swingterminal]
            clojure.edn
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
    (try
      (info "got " (str keyin) " type " (type keyin))
      (log-time "tick"
        (let [new-state (log-time "update-state" (update-state state keyin))]
          (when new-state
            (log-time "render" (render new-state))
            (async/thread (spit "save/world.edn" (prn-str (new-state :world)))))
          ;(async/thread (spit "save/world.edn" (with-out-str (pprint (new-state :world)))))
          new-state))
      (catch Exception e
        (do
          (error "Caught exception" e)
          (error (.getCause e))
          (.printStackTrace e)
          (print-stack-trace e)
          state)))))

;; Example setup and tick fns
(defn setup
  "Create the intial `state` value.

   `state` contains
    
   * a `:world` that contains places, npcs, a player

   * a `:screen` to render the world

   * a `:data` the contents of the `/data` folder with the filename (excluding
   extention) as the key and the contents as the value.

   * a `:settings` the contents of `/config/settings.edn`.

   * `quests` that are loaded dynamically on startup."
  ([] (setup nil))
  ([screen]
  (let [data  (apply hash-map
                (mapcat (fn [file]
                          [(keyword (.getName file))
                           (->> (.getPath file)
                             (slurp)
                             (clojure.edn/read-string))])
                        (.listFiles (clojure.java.io/file "data"))))
        _     (when (get data :seed)
                (alter-var-root #'dg/*rnd* (constantly (java.util.Random. (get data :seed)))))
        world (if (.exists (clojure.java.io/file "save/world.edn"))
                (->> (slurp "save/world.edn")
                     (clojure.edn/read-string {:readers {'robinson.monstergen.Monster map->Monster}}))
                (init-world (dg/long)))
        ;; load quests
        _ (doall (map #(load-file (.getPath %))
                       (filter (fn [file] (.endsWith (.getPath file) ".clj"))
                               (.listFiles (clojure.java.io/file "quests")))))

        ;; get a list of all the quests that have been loaded
        quests (map deref (flatten (map #(-> % ns-publics vals)
                                         (filter #(.contains (-> % ns-name str)
                                                             "robinson.quests")
                                                  (all-ns)))))
        quest-map (apply hash-map (mapcat (fn [i] [(i :id) i]) quests))
        _ (doall (map #(info "Loaded quest" (% :name)) quests))
        _ (info "dialogs" (apply merge (map :dialog quests)))
        dialog (apply merge (map (fn [[k v]]
                                   {k (dialog->fsm v)})
                                 (apply merge (map :dialog quests))))
        _ (debug "loaded data" data)
        settings (->> (slurp "config/settings.edn")
                      (clojure.edn/read-string))
        terminal  (or screen
                      (swingterminal/make-terminal 80 24 [255 255 255] [0 0 0] nil
                                               (get settings :windows-font)
                                               (get settings :else-font)
                                               (get settings :font-size)))
        state {:world world :screen terminal :quests quest-map :dialog dialog :data data :settings settings}
        state (reduce (fn [state _] (add-npcs state 1)) state (range 5))]
    ;; tick once to render frame
    (tick state \.))))

