(ns dungeon-crusade.quests.test-quest
  (:use 
        dungeon-crusade.common
        dungeon-crusade.quest
        dungeon-crusade.npc
        dungeon-crusade.update
        dorothy.core))


;; A quest where you have to talk to a guy and
;; he gives you the MacGuffin.
(def q (quest :tq-0 {
  :name "Get MacGuffin"
  :data {:state :start}
  :dialog {:tq0-the-dude
           {:initial-state :start-not-given
            :m {:salutation-not-given
                [["Yo what's that?"
                  "Oh it's just this thing I found. Do you want it?"
                  identity
                  :asked]]
                 :asked
                 [["Yes"
                   "Ok here you go."
                   (fn [state]
                     (transfer-items-from-npc-to-player
                       state
                       :tq0-the-dude
                       (fn [item] true)))
                   :yes-response]
                   ["No"
                    "Ok maybe you will want it later."
                    identity
                    :no-response]]
                :yes-response
                [["Bye."
                  nil
                  stop-talking
                  :start-given]]
                :no-response
                [["Bye."
                  nil
                  stop-talking
                  :start-not-given]]
                :start-given
                [[nil
                  "Hi again. I hope that thing worked for you."
                  identity
                  :salutation-given]]
                :salutation-given
                [["It did -- I think."
                  "Goodbye."
                  stop-talking
                  :start-given]]
                :start-not-given
                [[nil
                  "Hey."
                  identity
                  :salutation-not-given]]}}}
  :stages {
    :0 {
      :name "Start"
      :pred (fn [state] true)
      :update (fn [state]
                (let [[_ x y] (first
                                (shuffle
                                  (filter #(= (-> % first :type) :floor)
                                          (with-xy (current-place state)))))]
                (add-npc state :0
                         {:id :tq0-the-dude
                          :name "The Dude"
                          :type :human
                          :movement-policy :constant
                          :inventory [{:type :ring
                                       :name "MacGuffin"}]} x y)))
      :nextstagefn (fn [state] :10)}
    :10 {
      :pred (fn [state] false)
      :update (fn [state] state)
      :nextstagefn (fn [state] nil)}}}))

