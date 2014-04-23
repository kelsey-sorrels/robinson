;; Functions for drawing dialog trees as finite state machines
(ns dungeon-crusade.quest
  (:use dorothy.core))

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
  (-> dialog transform-to-graph dot show!))

(defn -main
  "Draw a sample dialog tree."
  [& args]
  (draw-dialog
               {:initial-state :start-not-given
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
                      'stop-talking
                      :start-given]]
                    :no-response
                    [["Bye"
                      nil
                      'stop-talking
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
                      :salutation-not-given]]}}))

