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
            [robinson.aterminal :as aterminal]
            #?@(:clj (
                [taoensso.timbre :as log]
                [robinson.swingterminal :as swingterminal]
                [robinson.macros :as rm]
                [clojure.stacktrace :as st]
                [clojure.core.async :as async :refer [go go-loop]]
                [clojure.java.io :as io]
                clojure.edn
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
     (:import [java.io DataInputStream DataOutputStream]))
     :cljs (
     (:import [goog Uri])
     (:require-macros [cljs.core.async.macros :refer [go go-loop]]
                      [robinson.macros :as rm]))))


;TODO: refer timbre to set this.
;#+clj
;(log/set-config! [] (read-string (slurp "config/timbre.clj"))(log/set-log-level! :error)

;(log/set-level! :debug)
(log/set-level! :info)
;(log/set-level! :error)

#?@(:cljs (
(cljs.reader/register-tag-parser! "robinson.monstergen.Monster" mg/map->Monster)
(cljs.reader/register-tag-parser! "robinson.player.Player" mg/map->Player)
(def world-storage (local-storage (atom nil) :world))))

(def save-chan (async/chan (async/sliding-buffer 1)))

(def render-chan (async/chan (async/sliding-buffer 1)))

;(def world-storage (atom nil))

(go-loop []
  (let [state (async/<! save-chan)]
    (log/info "World saved at time" (get-in state [:world :time]))
    #?(:clj
       (try
         (with-open [o (io/output-stream "save/world.edn")]
           (nippy/freeze-to-out! (DataOutputStream. o) (get state :world)))
         (catch Throwable e (log/error e)))
       :cljs
       (reset! world-storage (get state :world)))
    ;(as-> state state
    ;  (get state :world)
    ;  (pp/write state :stream nil)
    ;  (spit "save/world.edn.out" state))
    (recur)))

(defonce last-rendered-state (atom nil))
(go-loop [interrupted-state nil]
  (if-let [state (or interrupted-state
                     (async/alt!
                       render-chan ([v] v)
                       :default @last-rendered-state))]
    (do
      (reset! last-rendered-state state)
      (log/info "Rendering world at time" (get-in state [:world :time]))
      (try
        (rm/log-time "render" (rrender/render state))
        #?(:clj
           (catch Throwable e (log/error e))
           :cljs
           (catch js/Error e (log/error e))))
      (recur (async/alt!
               (async/timeout 600) nil
               render-chan ([v] v))))
    (recur (async/alt!
             (async/timeout 600) nil
             render-chan ([v] v)))))

(defn save-state [state]
  #?(:clj
     (async/>!! save-chan state)
     :cljs
     (go
       (async/>! save-chan state))))
  
(defn render-state [state]
  #?(:clj
     (async/>!! render-chan state)
     :cljs
     (go
       (async/>! render-chan state))))

(defn tick
  "The game loop.

   Take the current state, render it, wait for player input, then update
   the state using the player's input and return the new state. Save the
   world too, in case the  game is interrupted. Then we can load it next
   time we start up."
  [state keyin]
  {:pre  [(or (char? keyin)
              (keyword? keyin))]
   :post [(= (type state) (type %))]}
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
      #?(:clj
         (catch Exception e
           (do
             (log/error "Caught exception" e)
             (.printStackTrace e)
             (st/print-stack-trace e)
             (st/print-cause-trace e)
             state))
         :cljs
         (catch js/Error e
           (log/error e)
           state))))
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

   * a `:settings` the contents of `/config/settings.edn`.

   * `quests` that are loaded dynamically on startup."
  ([f] (setup nil f))
  ([screen f]
  (let [data  #?(:clj
                 (apply hash-map
                   (mapcat (fn [file]
                             [(keyword (.getName file))
                              (->> (.getPath file)
                                (slurp)
                                (clojure.edn/read-string))])
                           (.listFiles (clojure.java.io/file "data"))))
                 :cljs
                 (p/all [(get-resource "data/atmo")
                         (get-resource "data/help")]))
        _     (when (get data :seed)
                (rr/set-rnd! (rr/create-random (get data :seed))))
        world #?(:clj
                 (if (.exists (clojure.java.io/file "save/world.edn"))
                   (with-open [o (io/input-stream "save/world.edn")]
                     (nippy/thaw-from-in! (DataInputStream. o)))
      
                   {:current-state :start
                    :time 0})
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
                     :font-size 18})

        terminal  (or screen
                      #?(:clj
                         (swingterminal/make-terminal 80 24 [255 255 255] [0 0 0] nil
                                                  (get settings :windows-font)
                                                  (get settings :else-font)
                                                  (get settings :font-size))
                         :cljs
                         (webglterminal/make-terminal 80 24 [255 255 255] [0 0 0] nil
                                                  (get settings :windows-font)
                                                  (get settings :else-font)
                                                  (get settings :font-size))))]
    ;; tick once to render frame
    #?(:clj
       (let [state {:world world :screen terminal :quests {} #_quest-map :dialog {} #_dialog :data data :settings settings}]
         (f (tick state \.)))
       :cljs
       (p/then data (fn [[atmo help]]
         (let [state {:world world :screen terminal :quests {} #_quest-map :dialog {} #_dialog :data {:atmo atmo :help help}  :settings settings}]
           (f (tick state \.)))))))))

