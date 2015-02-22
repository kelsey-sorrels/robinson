;; Functions for drawing dialog trees as finite state machines
(ns robinson.dialog
  (:use robinson.common
        dorothy.core)
  (:require ;[lanterna.screen :as s]
            [taoensso.timbre :as timbre]))

(timbre/refer-timbre)

;; State Machine Functions
;; =======================
(defn state-machine [transition-table initial-state]
  {:pre  [
          ;; Make sure all destination states are valid states
          ;; and that there are no unused states. Ie destination
          ;; states and state sets are identical.
          (let [states (map first transition-table)
                dest-states (map last (reduce concat (vals transition-table)))]
            (= (set states) (set dest-states)))
          ;; Make sure that initial-state is a valid state.
          (contains? (set (map first transition-table)) initial-state)]}
  (with-meta transition-table {:current-state (ref initial-state)}))

(defn fsm-current-state
  "Get the current state of the fsm."
  [fsm]
  (-> fsm meta :current-state deref))

(defn step-fsm
  "Updates state using the finite state machine and input."
  [state fsm input]
  (let [_ (trace "input:" input)
        fsm-state (fsm-current-state fsm)
        _ (debug "current-state:" fsm-state)
        options (get fsm fsm-state)]
    (if (some (partial = input) (map first options))
      (let [
            _ (debug "options:" options)
            [_ transition-fn next-state] (first (filter #(= input (first %)) options))
            _ (debug "trans-fn:" transition-fn)
            _ (debug "next-state:" next-state)
            _ (dosync (ref-set (-> fsm meta :current-state) next-state))]
        (transition-fn state))
      state)))

(defn get-valid-input
  "Returns a list of values valid for use as input to step-fsm."
  [fsm]
  (let [fsm-state (-> fsm meta :current-state deref)]
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
                                               (append-log :dialog-log r)
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

#_(defn run-dialog
  "Runs through a dialog indefinitely."
  [dialog]
  (let [fsm (dialog->fsm dialog)
        terminal (SwingTerminal.)
        screen (Screen. terminal)]
    (.startScreen screen)
    (loop [state {:dialog-log []}]
      (let [valid-input (get-valid-input fsm)
            options (zipmap (take (count valid-input) [\a \b \c \d \e \f \g \h \i]) valid-input)
            ;_ (.clear screen)
            _ (.clearScreen terminal)
            ;_ (s/clear screen)
            _ (.moveCursor terminal 0 0)
            _ (.putString terminal (or (some-> state :world :dialog-log last :text) ""))
            _ (doall (map-indexed (fn [i [k v]] (do
                                                  (.moveCursor terminal 0 (+ i 5))
                                                  (.putString terminal (format "%c - %s" k (if (nil? v) "nil" v))))) options))
            _ (.refresh screen)
            key-in (.getCharacter (.readInputBlocking terminal))
            input (get options key-in)]
        (recur (step-fsm state fsm input))))
     (.stopScreen screen)))

#_(defn -main
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
    (draw-dialog dialog)
    (run-dialog dialog)))

