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
            #+clj  [robinson.swingterminal :as swingterminal]
            #+cljs [robinson.webglterminal :as webglterminal]
            #+clj
            [robinson.macros :as rm]
            #+cljs
            [robinson.macros :as rm :include-macros true]
            #+clj
            [clojure.stacktrace :as st]
            #+clj
            [clojure.core.async :as async :refer [go go-loop]]
            #+cljs
            [cljs.core.async :as async]
            #+cljs
            [cljs-promises.core :as p]
            #+cljs
            [cljs-promises.async :refer-macros [<?]]
            [robinson.aterminal :as aterminal]
            #+clj
            [clojure.java.io :as io]
            #+clj
            clojure.edn
            #+clj
            [taoensso.nippy :as nippy]
            #+clj
            [clojure.pprint :refer :all]
            #+clj
            [taoensso.timbre :as log]
            #+cljs
            [shodan.console :as log :include-macros true]
            #+cljs
            [goog.net.XhrIo :as xhr]
            #+cljs
            [cljs-promises.core :as p]
            #+cljs
            [cljs-promises.async :refer-macros [<?]])

  #+clj
  (:import [java.io DataInputStream DataOutputStream])
  #+cljs
  (:import [goog Uri])
  #+cljs
  (:require-macros [cljs.core.async.macros :refer [go go-loop]]
                   [robinson.macros :as rm]))


#+clj
(log/set-config! [] (read-string (slurp "config/timbre.clj")))

#+cljs
#_(cljs-promises.async/extend-promises-as-pair-channels!)

(def save-chan (async/chan (async/sliding-buffer 1)))

(def render-chan (async/chan (async/sliding-buffer 1)))

(go-loop []
  (let [state (async/<! save-chan)]
    (log/info "World saved at time" (get-in state [:world :time]))
    #+clj
    (try
      (with-open [o (io/output-stream "save/world.edn")]
        (nippy/freeze-to-out! (DataOutputStream. o) (get state :world)))
      (catch Throwable e (log/error e)))
    ;(as-> state state
    ;  (get state :world)
    ;  (pp/write state :stream nil)
    ;  (spit "save/world.edn.out" state))
    (recur)))

(go-loop []
  (let [state (async/<! render-chan)]
    (log/info "Rendering world at time" (get-in state [:world :time]))
    (try
      (rm/log-time "render" (rrender/render state))
      #+clj
      (catch Throwable e (log/error e))
      #+cljs
      (catch js/Error e (log/error e)))
    (recur)))

#+clj
(defn save-state [state]
  (async/>!! save-chan state))

#+cljs
(defn save-state [state]
  (go
    (async/>! save-chan state)))

#+clj
(defn render-state [state]
  (async/>!! render-chan state))
  
#+cljs
(defn render-state [state]
  (go
    (async/>! render-chan state)))
  
(defn tick
  "The game loop.

   Take the current state, render it, wait for player input, then update
   the state using the player's input and return the new state. Save the
   world too, in case the  game is interrupted. Then we can load it next
   time we start up."
  [state keyin]
    (try
      (log/info "got " (str keyin) " type " (type keyin))
      (rm/log-time "tick"
        (let [new-state (rm/log-time "update-state" (ru/update-state state keyin))]
          (when new-state
            (do
            (render-state new-state)
            (save-state new-state)))
          ;(async/thread (spit "save/world.edn" (with-out-str (pprint (new-state :world)))))
          new-state))
      #+clj
      (catch Exception e
        (do
          (log/error "Caught exception" e)
          (.printStackTrace e)
          (st/print-stack-trace e)
          (st/print-cause-trace e)
          state))
      #+cljs
      (catch js/Error e
        (log/error e)
        state)))
#+cljs
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
      (p/catch (fn [e] (log/error (str e))))))

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
  ([f] (setup nil f))
  ([screen f]
  (let [data  #+clj
              (apply hash-map
                (mapcat (fn [file]
                          [(keyword (.getName file))
                           (->> (.getPath file)
                             (slurp)
                             (clojure.edn/read-string))])
                        (.listFiles (clojure.java.io/file "data"))))
              ;TODO: read from js var
              #+cljs
              (p/all [(get-resource "data/atmo")
                      (get-resource "data/help")])
        _     (when (get data :seed)
                (rr/set-rnd! (rr/create-random (get data :seed))))
        world #+clj
              (if (.exists (clojure.java.io/file "save/world.edn"))
                (with-open [o (io/input-stream "save/world.edn")]
                  (nippy/thaw-from-in! (DataInputStream. o)))
      
                {:current-state :start
                 :time 0})
              ;TODO: read from local storage
              #+cljs
              {:current-state :start
               :time 0}
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
        settings #+clj
                 (clojure.edn/read-string
                   (slurp "config/settings.edn"))
                 ;TODO: read from config page in cljs
                 #+cljs
                 {:windows-font "Courier New"
                  :else-font    "Monospaced"
                  :font-size 18}

        terminal  (or screen
                      #+clj
                      (swingterminal/make-terminal 80 24 [255 255 255] [0 0 0] nil
                                               (get settings :windows-font)
                                               (get settings :else-font)
                                               (get settings :font-size))
                      #+cljs
                      (webglterminal/make-terminal 80 24 [255 255 255] [0 0 0] nil
                                               (get settings :windows-font)
                                               (get settings :else-font)
                                               (get settings :font-size)))]
    ;; tick once to render frame
    #+clj
    (let [state {:world world :screen terminal :quests {} #_quest-map :dialog {} #_dialog :data data :settings settings}]
      (f (tick state \.)))
    #+cljs
    (p/then data (fn [[atmo help]]
      (let [state {:world world :screen terminal :quests {} #_quest-map :dialog {} #_dialog :data {:atmo atmo :help help}  :settings settings}]
        (f (tick state \.))))))))

