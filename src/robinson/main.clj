(ns robinson.main
  (:require 
            [robinson.common :as rc]
            [robinson.random :as rr]
            [robinson.world :as rw]
            [robinson.worldgen :as rwgen]
            [robinson.dialog :as rdiag]
            [robinson.npc :as rnpc]
            [robinson.update :as ru]
            [robinson.monstergen :as mg]
            [robinson.render :as rrender]
            [zaffre.tilesets :as ztiles]
            [robinson.events :as revents]
            clojure.data.priority-map
            [taoensso.timbre :as log]
            [zaffre.glterminal :as glterminal]
            [zaffre.font :as zfont]
            [robinson.macros :as rm]
            [rockpick.core :as rpc]
            [clojure.stacktrace :as st]
            [clojure.core.async :as async :refer [go go-loop]]
            [clojure.java.io :as io]
            clojure.edn
            [alandipert.enduro :as enduro]
            [taoensso.nippy :as nippy]
            [clojure.pprint :refer :all])
     (:import [java.io DataInputStream DataOutputStream]
              [zaffre.font TTFFont]))


;TODO: refer timbre to set this.
;#+clj
;(log/set-config! [] (read-string (slurp "config/timbre.clj"))(log/set-log-level! :error)

(log/merge-config!
  {:ns-blacklist ["robinson.render"]})

(def tile-font ztiles/pastiche-16x16)

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
  (let [state (async/<! save-chan)]
    (log/info "World saved at time" (get-in state [:world :time]))
       (try
         (with-open [o (io/output-stream "save/world.edn")]
           (nippy/freeze-to-out! (DataOutputStream. o) (get state :world)))
         (catch Throwable e (log/error "Error saving" e)))
    ;(as-> state state
    ;  (get state :world)
    ;  (pp/write state :stream nil)
    ;  (spit "save/world.edn.out" state))
    (recur)))

#_(go-loop [interrupted-state nil]
  (if-let [state (or interrupted-state
                     (async/alt!
                       render-chan ([v] v)
                       :default @last-rendered-state))]
    (do
      (reset! last-rendered-state state)
      (log/info "Rendering world at time" (get-in state [:world :time]))
      (try
        ;(rm/log-time "render" (rrender/render state))
        (let [render-fn (resolve 'robinson.render/render)
              dom (render-fn state @last-rendered-dom)]
          (reset! last-rendered-dom dom))
           (catch Throwable e
             (log/error "Error rendering" e)
             (st/print-stack-trace e)
             (st/print-cause-trace e)))
      (recur (async/<! render-chan) #_(async/alt!
               (async/timeout 600) nil
               render-chan ([v] v))))
    (recur (async/<! render-chan) #_(async/alt!
             (async/timeout 600) nil
             render-chan ([v] v)))))

(defn save-state [state]
  (async/>!! save-chan state))
  
(defn render-state [state]
  (async/>!! render-chan state))

(defn tick
  "The game loop.

   Take the current state, render it, wait for player input, then update
   the state using the player's input and return the new state. Save the
   world too, in case the  game is interrupted. Then we can load it next
   time we start up."
  [state keyin]
  {:pre  [(or (char? keyin)
              (keyword? keyin))
          (empty? (get state :events))]
   :post [(= (type state) (type %))
          (empty? (get % :events))]}
    (try
      (log/info "got " (str keyin) " type " (type keyin))
      (rm/log-time "tick"
        (let [update-state-fn (resolve 'robinson.update/update-state)
              _ (log/info "update-state-fn" update-state-fn)
              new-state       (rm/log-time "update-state" (update-state-fn state keyin))]
          (when new-state
            #_(render-state new-state)
            (save-state new-state))))
          ;(async/thread (spit "save/world.edn" (with-out-str (pprint (new-state :world)))))
          ;new-state))
         (catch Exception e
           (do
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
    (when (.exists (io/file "config/.feedbackparticipant"))
      (when (not (.exists (io/file "config/.userid")))
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
                                    (.listFiles (clojure.java.io/file "data")))))
          feedback-participant (.exists (io/file "config/.feedbackparticipant"))
          version              (if (.exists (io/file "VERSION"))
                                 (slurp "VERSION")
                                 "SNAPSHOT")
          user-id              (if (.exists (io/file "config/.userid"))
                                 (slurp "config/.userid")
                                 "unknown")
          _                    (when (get data :seed)
                                 (rr/set-rnd! (rr/create-random (get data :seed))))
          world                (update
                                    (if (.exists (clojure.java.io/file "save/world.edn"))
                                      (with-open [o (io/input-stream "save/world.edn")]
                                        (nippy/thaw-from-in! (DataInputStream. o)))
                                      {:current-state :start
                                       :time 0})
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
                     (slurp "config/settings.edn"))
          fonts       (apply sorted-map
                        (mapcat (fn [file]
                                  (let [file-name (.getName file)
                                        map-key   (if (re-find #".edn$" file-name)
                                                    (keyword (clojure.string/replace-first file-name #".edn$" ""))
                                                    (keyword file-name))
                                        map-value (->> (.getPath file)
                                                    (slurp)
                                                    (clojure.edn/read-string))]
                                  [map-key
                                   map-value]))
                                (.listFiles (clojure.java.io/file "config/fonts"))))
          font      (get fonts (get settings :font))
          fx-shader (get settings :fx-shader)
          _ (log/info "Using font settings" font)
          terminal-groups [{:id :app
                            :layers [
                              #_:map
                              #_:features
                              #_:fx
                              :ui]
                            :columns 80
                            :rows 24
                            :pos [0 0]
                            :font (constantly tile-font)
                            #_#_:font (fn [platform]
                                    (zfont/->TTFFont
                                      (get font (case platform
                                                  :linux   :linux-font
                                                  :macosx  :macosx-font
                                                  :windows :windows-font))
                                      (get font :font-size)
                                      false #_(get font :transparent)))}]
          terminal-opts {:title (format "Robinson - %s@%s" user-id version)
                         :screen-width (* 80 12)
                         :screen-height (* 24 16)
                         #_#_:default-fg-color [255 255 255 255]
                         #_#_:default-bg-color [5 5 8 128]
                         #_#_:fx-shader {:name     "retro.fs"
                                     :uniforms [["time" 0.0]
                                                ["noise" (or (get fx-shader :noise) 0.0016)]
                                                ["colorShift" (or (get fx-shader :color-shift) 0.00001)]
                                                ["scanlineDepth" (or (get fx-shader :scanline-depth) 0.94)]
                                                ["brightness" (or (get fx-shader :brightness) 0.68)]
                                                ["contrast" (or (get fx-shader :contrast) 2.46)]]}}]
      (log/info "Creating terminal in thread:" (.getName (Thread/currentThread)))
      (log/info "Loaded data:" (keys data))
      (log/info "World: " world)
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

