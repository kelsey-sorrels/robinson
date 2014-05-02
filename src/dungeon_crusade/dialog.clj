;; Functions for drawing dialog trees as finite state machines
(ns dungeon-crusade.dialog
  (:use dungeon-crusade.common
        dorothy.core)
  (:require [lanterna.screen :as s]))

;; State Machine Functions
;; =======================
(defn state-machine [transition-table intial-state]
  (with-meta transition-table {:current-state (ref intial-state)}))

(defn fsm-current-state
  "Get the current state of the fsm."
  [fsm]
  (-> fsm meta :current-state deref))

(defn step-fsm
  "Updates state using the finite state machine and input."
  [state fsm input]
  (let [;_ (println "input:" input)
        fsm-state (fsm-current-state fsm)
        _ (println "current-state:" fsm-state)
        options (get fsm fsm-state)]
    (if (some (partial = input) (map first options))
      (let [
            _ (println "options:" options)
            [_ transition-fn next-state] (first (filter #(= input (first %)) options))
            _ (println "trans-fn:" transition-fn)
            _ (println "next-state:" next-state)
            _ (dosync (ref-set (-> fsm meta :current-state) next-state))]
        (transition-fn state))
      state)))

(defn get-valid-input
  "Returns a list of values valid for use as input to step-fsm."
  [fsm]
  (let [fsm-state (-> fsm meta :current-state deref)]
        ;_ (doall (map #(println first %) (get fsm fsm-state)))]
    (map first (get fsm fsm-state))))

;; Dorothy Drawing Functions
;; =========================

(defn- couplet-to-edge
  "Transform a dialog couplet to a dorothy edge."
  [start-node couplet]
  [start-node
   (nth couplet 3)
   {:label (apply str (mapcat str (interpose "\n" (map (fn [e] (if (nil? e)
                                                                "nil"
                                                                e))
                                                         (butlast couplet)))))}])

(defn- transform-to-graph
  "Transforms a dialog tree into a dorothy graph."
  [dialog]
  (digraph (concat [(node-attrs {:style :filled})]
                   (mapcat (fn [[start-node couplets]]
                             (map (fn [couplet]
                                    (couplet-to-edge start-node couplet))
                                  couplets))
                           (dialog :m)))))

(defn draw-dialog
  "Draw a dialog tree using dorothy."
  [dialog]
  (.start (Thread. (fn [] (-> dialog transform-to-graph dot show!)))))

;; Functions for converting dialog to finite state machine
;; =======================================================

(defn dialog->transition-matrix
  "Convery a dialog tree into a transition matrix suitable for a finite state machine."
  [dialog]
  (apply hash-map (mapcat (fn [[k v]]
                            [k (map (fn [[q r f t]]
                                      [q (fn [state]
                                           (-> state
                                               (append-log r)
                                               (f)))
                                       t])
                                    v)])
                            (dialog :m))))

(defn dialog->fsm
  [dialog]
  "Convert a dialog into a finite state machine."
  (state-machine (dialog->transition-matrix dialog)
                 (dialog :initial-state)))

;; Functions for running through npc dialog in a lanterna screen.
;; ==============================================================

(defn run-dialog
  "Runs through a dialog indefinitely."
  [dialog]
  (let [fsm (dialog->fsm dialog)
        screen (s/get-screen :swing)]
    (s/in-screen screen
      (loop [state {:log []}]
        (let [valid-input (get-valid-input fsm)
              options (zipmap (take (count valid-input) [\a \b \c \d \e \f \g \h \i]) valid-input)
              _ (s/clear screen)
              _ (s/put-string screen 0 0 (or (some-> state :world :log last :text) ""))
              _ (doall (map-indexed (fn [i [k v]] (s/put-string screen 0 (+ i 5) (format "%c - %s" k (if (nil? v) "nil" v)))) options))
              _ (s/redraw screen)
              key-in (s/get-key-blocking screen)
              input (get options key-in)]
        (recur (step-fsm state fsm input)))))))

(defn -main
  "Draw a sample dialog tree and then run through it using a lanterna screen."
  [& args]
  (let [dialog {:initial-state :start-not-given
                :m {:salutation-not-given
                    [["Yo what's that?"
                      "Oh it's just this thing I found. Do you want it?"
                      identity
                      :asked]]
                    :asked
                    [["Yes"
                      "Ok here you go."
                      (fn [state] state)
                      :yes-response]
                     ["No"
                      "Ok Maybe you will want it later."
                      identity
                      :no-response]]
                    :yes-response
                    [["Bye"
                      nil
                      identity
                      ;'stop-talking
                      :start-given]]
                    :no-response
                    [["Bye"
                      nil
                      identity
                      ;'stop-talking
                      :start-not-given]]
                    :start-given
                    [[nil
                      "Hi again. I hope that thing worked for you."
                      identity
                      :salutation-given]]
                    :salutation-given
                    [["It did -- I think"
                      "Good bye."
                      identity
                      :start-given]]
                    :start-not-given
                    [[nil
                      "Hey."
                      identity
                      :salutation-not-given]]}}]
  (do
    (draw-dialog dialog)
    (run-dialog dialog))))

