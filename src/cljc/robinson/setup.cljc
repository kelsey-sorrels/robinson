(ns robinson.setup
  (:require 
            [robinson.random :as rr]
            [robinson.monstergen :as mg]
            [robinson.events :as revents]
            clojure.data.priority-map
            #?@(:clj (
                [taoensso.timbre :as log]
                [zaffre.glterminal :as glterminal]
                [zaffre.lwjglutil :as zlwjglutil]
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
                :cljs (
                [robinson.webglterminal :as webglterminal]
                [robinson.macros :as rm :include-macros true]
                [taoensso.timbre :as log :include-macros true]
                [cljs.core.async :as async]
                [cljs-promises.core :as p]
                [cljs-promises.async :refer-macros [<?]]
                [alandipert.storage-atom :refer [local-storage]]
                [goog.net.XhrIo :as xhr]
                [cljs-promises.core :as p]
                [cljs-promises.async :refer-macros [<?]])))

  #?@(:clj (
     (:import [java.io DataInputStream DataOutputStream]
              [zaffre.font TTFFont]))
     :cljs (
     (:import [goog Uri])
     (:require-macros [cljs.core.async.macros :refer [go go-loop]]
                      [robinson.macros :as rm]))))


;TODO: refer timbre to set this.
;#+clj
;(log/set-config! [] (read-string (slurp "config/timbre.clj"))(log/set-log-level! :error)

(log/merge-config!
  {:ns-blacklist ["robinson.render"]})

#?(:cljs
(defn get-resource [path]
  (-> (p/promise (fn [resolve reject]
                   (xhr/send
                     path
                     (fn [e]
                       (log/info e)
                       (if (.isSuccess (.-target e))
                         (resolve (.getResponseText (.-target e)))
                         (reject (.-target e)))))))
      (p/then (fn [e]
                (log/info "Got response" e)
                (cljs.reader/read-string e))
              (fn [e] (log/error (str e))))
      (p/catch (fn [e] (log/error (str e)))))))

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
    (let [data                 #?(:clj
                                  (apply hash-map
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
                                  :cljs
                                  (p/all [(get-resource "data/atmo")
                                          (get-resource "data/help")]))
          feedback-participant (.exists (io/file "config/.feedbackparticipant"))
          version              (if (.exists (io/file "VERSION"))
                                 (slurp "VERSION")
                                 "SNAPSHOT")
          user-id              (if (.exists (io/file "config/.userid"))
                                 (slurp "config/.userid")
                                 "unknown")
          _                    (when (get data :seed)
                                 (rr/set-rnd! (rr/create-random (get data :seed))))
          world                #?(:clj
                                  (update
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
                                  ;; Read from local storage
                                  :cljs
                                  (if-let [world @world-storage]
                                    world
                                    {:current-state :start
                                     :time 0}))
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
          settings #?(:clj
                      (clojure.edn/read-string
                        (slurp "config/settings.edn"))
                      ;TODO: read from config page in cljs
                      :cljs
                      {:windows-font "Courier New"
                       :else-font    "Monospaced"
                       :size         18})
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
          state #?(:clj
                   {:world world
                    :quests {} #_quest-map
                    :dialog {} #_dialog
                    :feedback-participant feedback-participant
                    :version version
                    :user-id user-id
                    :data data
                    :settings (enduro/file-atom {} "config/settings.edn")
                    :scores   (enduro/file-atom [] "scores/scores.edn")
                    :fonts fonts
                    :events (clojure.data.priority-map/priority-map)}
                   :cljs
                   (p/then data (fn [[atmo help]]
                     {:world world
                      :quests {} #_quest-map
                      :dialog {} #_dialog
                      :data {:atmo atmo :help help}
                      :settings settings})))
          _ (log/info "Using font settings" font)
          terminal-groups [{:id :app
                            :layers [
                              :map
                              :mapfx
                              :features
                              :fx
                              :ui
                              :uifx]
                            :columns 80
                            :rows 24
                            :pos [0 0]
                            :font (constantly
                                    (zfont/->TTFFont
                                      (get font (case (zlwjglutil/platform)
                                                  :linux   :linux-font
                                                  :macosx  :macosx-font
                                                  :windows :windows-font))
                                      (get font :font-size)
                                      (get font :transparent)))}]
          terminal-opts {:title (format "Robinson - %s@%s" user-id version)
                         :screen-width (* 80 18)
                         :screen-height (* 24 22)
                         :default-fg-color [255 255 255]
                         :default-bg-color [5 5 8]
                         #_#_:fx-shader {:name     "retro.fs"
                                     :uniforms [["time" 0.0]
                                                ["noise" (or (get fx-shader :noise) 0.0016)]
                                                ["colorShift" (or (get fx-shader :color-shift) 0.00001)]
                                                ["scanlineDepth" (or (get fx-shader :scanline-depth) 0.94)]
                                                ["brightness" (or (get fx-shader :brightness) 0.68)]
                                                ["contrast" (or (get fx-shader :contrast) 2.46)]]}}]
      (log/info "Creating terminal in thread:" (.getName (Thread/currentThread)))
      (log/info "Loaded data:" (keys data))
      ;; set log level
      (log/set-level! (get settings :log-level))
      ;; tick once to render frame
      #?(:clj
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
             :events (clojure.data.priority-map/priority-map)})
         :cljs
         (p/then data (fn [[atmo help]]
           (let [state {:world world
                        :quests {} #_quest-map
                        :dialog {} #_dialog
                        :data {:atmo atmo :help help}
                        :settings settings}]
             (f state))))))))

