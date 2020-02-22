(ns robinson.main
  (:require 
            [robinson.common :as rc]
            [robinson.random :as rr]
            [robinson.error :as re]
            [robinson.world :as rw]
            [robinson.worldgen :as rwgen]
            [robinson.dialog :as rdiag]
            [robinson.npc :as rnpc]
            [robinson.update :as ru]
            [robinson.monstergen :as mg]
            [robinson.render :as rrender]
            [robinson.fs :as rfs]
            [robinson.font :as rfont]
            [zaffre.tilesets :as ztiles]
            [robinson.events :as revents]
            clojure.data.priority-map
            [taoensso.timbre :as log]
            [zaffre.glterminal :as glterminal]
            [zaffre.tilesets :as ztiles]
            [robinson.macros :as rm]
            [rockpick.core :as rpc]
            [clojure.stacktrace :as st]
            [clojure.core.async :as async :refer [go go-loop]]
            [clojure.java.io :as io]
            clojure.edn
            [alandipert.enduro :as enduro]
            [taoensso.nippy :as nippy]
            [clojure.pprint :refer :all])
     (:import [java.io DataInputStream DataOutputStream]))


;TODO: refer timbre to set this.
;#+clj
;(log/set-config! [] (read-string (slurp "config/timbre.clj"))(log/set-log-level! :error)

(log/merge-config!
  {:ns-blacklist ["robinson.render"]})

; TODO fix?
#_(cljs.reader/register-tag-parser! "robinson.monstergen.Monster" mg/map->Monster)
#_(cljs.reader/register-tag-parser! "robinson.player.Player" mg/map->Player)
#_(def world-storage (local-storage (atom nil) :world))

(def save-chan (async/chan (async/sliding-buffer 1)))

(def render-chan (async/chan (async/sliding-buffer 1)))
(defonce last-rendered-state (atom nil))
(defonce last-rendered-dom (atom nil))

;(def world-storage (atom nil))

(go-loop []
  (let [state (async/<! save-chan)
        tmp-file (rfs/cwd-file "save/world.edn.tmp")
        target-file(rfs/cwd-file "save/world.edn")]
    (if-not (contains? #{:quit} (rw/current-state state))
      (do
        (log/info "World saved at time" (get-in state [:world :time]) tmp-file target-file)
        (try
          (with-open [o (io/output-stream tmp-file)]
            (nippy/freeze-to-out! (DataOutputStream. o) (get state :world)))
          (catch Throwable e (log/error "Error saving" e)))

        ;; copy tmp file to permenent store atomically
        (java.nio.file.Files/move (.toPath tmp-file)
                                  (.toPath target-file)
                                  (into-array java.nio.file.CopyOption
                                            [(java.nio.file.StandardCopyOption/ATOMIC_MOVE)
                                             (java.nio.file.StandardCopyOption/REPLACE_EXISTING)]))

        ;(as-> state state
        ;  (get state :world)
        ;  (pp/write state :stream nil)
        ;  (spit "save/world.edn.out" state))
        (recur))
      (recur))))

(defn save-state [state]
  ; Don't save main menu updates
  (when-not (contains? #{:start
                         :configure
                         :configure-font
                         :create-font
                         :create-font-name
                         :connecting
                         :connection-failed
                         :loading
                         :share-score
                         :quit?}
                       (rw/current-state state))
    (async/put! save-chan state)))
  
(defn render-state [state]
  (async/>!! render-chan state))

(defn tick
  "The game loop.

   Take the current state, render it, wait for player input, then update
   the state using the player's input and return the new state. Save the
   world too, in case the  game is interrupted. Then we can load it next
   time we start up."
  [state keyin keymods]
  {:pre  [(or (char? keyin)
              (keyword? keyin)
              (map? keyin))
          (empty? (get state :events))]
   :post [(= (type state) (type %))
          (empty? (get % :events))]}
    (try
      (log/info "got " (str keyin) " type " (type keyin))
      (rm/log-time "tick"
        (let [update-state-fn (resolve 'robinson.update/update-state)
              _ (log/info "update-state-fn" update-state-fn)
              new-state       (rm/log-time "update-state" (update-state-fn state keyin keymods))]
          (when new-state
            (save-state new-state))
          ;(async/thread (spit "save/world.edn" (with-out-str (pprint (new-state :world)))))
          new-state))
         (catch Exception e
           (do
             (re/log-exception state e)
             (log/error "Caught exception" e)
             (.printStackTrace e)
             (st/print-stack-trace e)
             (st/print-cause-trace e)
             state))))

;; Example setup and tick fns
(defn setup
  "Create the intial `state` value.

   `state` contains
    
   * a `:world` that contains places, npcs, a player

   * a `:screen` to render the world

   * a `:data` the contents of the `/data` folder with the filename (excluding
   extention) as the key and the contents as the value.

   * a `:settings` an atom containing the contents of `/config/settings.edn`.
   
   * a `:fonts` a sequence of the contents of files in /config/fonts/*.edn

   * `quests` that are loaded dynamically on startup."
  ([f] (setup nil f))
  ([screen f]
    (when (.exists (rfs/cwd-file "config/.feedbackparticipant"))
      (when (not (.exists (rfs/cwd-file "config/.userid")))
        (spit "config/.userid" (doto (java.util.UUID/randomUUID)
                                            (.toString)))))
    (let [data (apply hash-map
                    (mapcat (fn [file]
                              (let [file-name (.getName file)
                                    map-key   (if (re-find #".xp$" file-name)
                                                (keyword (clojure.string/replace-first file-name #".xp$" ""))
                                                (keyword file-name))
                                    map-value (if (re-find #".xp$" file-name)
                                                 (rpc/read-xp (clojure.java.io/input-stream (.getPath file)))
                                                 (->> (.getPath file)
                                                      (slurp)
                                                      (clojure.edn/read-string)))]
                              [map-key
                               map-value]))
                            (filter #(not (.isDirectory %))
                                    (.listFiles (rfs/cwd-file "data")))))
          feedback-participant (.exists (rfs/cwd-file "config/.feedbackparticipant"))
          version              (if (.exists (rfs/cwd-file "VERSION"))
                                 (slurp (rfs/cwd-path "VERSION"))
                                 "SNAPSHOT")
          user-id              (if (.exists (rfs/cwd-file "config/.userid"))
                                 (slurp (rfs/cwd-path "config/.userid"))
                                 "unknown")
          _                    (when (get data :seed)
                                 (rr/set-rnd! (rr/create-random (get data :seed))))
          world                (update
                                      {:current-state :start
                                       :time 0
                                       :continue (.exists (rfs/cwd-file "save/world.edn"))
                                       :player {
                                         :name ""}}
                                    :current-state
                                    (fn [cur-state]
                                      (if (= cur-state :share-score)
                                        :start
                                        cur-state)))
          ;; load quests
          ;_ (doall (map #(load-file (.getPath %))
          ;               (filter (fn [file] (.endsWith (.getPath file) ".clj"))
          ;                       (.listFiles (clojure.java.io/file "quests")))))

          ;; get a list of all the quests that have been loaded
          ;quests (map deref (flatten (map #(-> % ns-publics vals)
          ;                                 (filter #(.contains (-> % ns-name str)
          ;                                                     "robinson.quests")
          ;                                          (all-ns)))))
          ;quest-map (apply hash-map (mapcat (fn [i] [(i :id) i]) quests))
          ;_ (doall (map #(log/info "Loaded quest" (% :name)) quests))
          ;_ (log/info "dialogs" (apply merge (map :dialog quests)))
          ;dialog (apply merge (map (fn [[k v]]
          ;                           {k (dialog->fsm v)})
          ;                         (apply merge (map :dialog quests))))
          ;;_ (debug "loaded data" data)
          settings (clojure.edn/read-string
                     (slurp (rfs/cwd-path "config/settings.edn")))
          fonts     (rfont/read-font-configs)
          font      (get fonts (get settings :font))
          fx-shader (get settings :fx-shader)
          _ (log/info "Using font settings" font)
          terminal-groups [{:id :app
                            :layers [
                              :map
                              :features
                              :ceiling
                              #_:fx
                              :shading
                              :ui]
                            :columns 80
                            :rows 24
                            :pos [0 0]
                            :font (partial rfont/make-font-fn font)}]
          terminal-opts {:title (format "Robinson - %s@%s" user-id version)
                         :screen-width (* 80 17)
                         :screen-height (* 24 24)
                         :default-fg-color [255 255 255 255]
                         :default-bg-color [5 5 8 128]
                         #_#_:fx-shader {:name     "retro.fs"
                                     :uniforms [["time" 0.0]
                                                ["noise" (or (get fx-shader :noise) 0.0016)]
                                                ["colorShift" (or (get fx-shader :color-shift) 0.00001)]
                                                ["scanlineDepth" (or (get fx-shader :scanline-depth) 0.94)]
                                                ["brightness" (or (get fx-shader :brightness) 0.68)]
                                                ["contrast" (or (get fx-shader :contrast) 2.46)]]}}]
      (log/info "Creating terminal in thread:" (.getName (Thread/currentThread)))
      (log/info "Loaded data:" (keys data))
      (log/debug "World: " world)
      ;; set log level
      (log/set-level! (get settings :log-level))
      ;; tick once to render frame
      (f {:world world
          :quests {} #_quest-map
          :dialog {} #_dialog
          :terminal-groups terminal-groups
          :terminal-opts terminal-opts
          :feedback-participant feedback-participant
          :version version
          :user-id user-id
          :data data
          :settings (enduro/file-atom {} "config/settings.edn")
          :scores   (enduro/file-atom [] "scores/scores.edn")
          :fonts fonts
          :events (clojure.data.priority-map/priority-map)}))))

