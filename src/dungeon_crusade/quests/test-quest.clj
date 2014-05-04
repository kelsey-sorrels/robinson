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
                  :salutation-not-given]]}}
           :tq0-friend-dude
           {:initial-state :start-not-given
            :m {:start
                [[nil
                  "Hi. You have the MacGuffing?"
                  identity
                  :asked]]
                :asked
                [["Yes"
                  "Thanks. I'll take it!"
                  (fn [state]
                    (-> state
                        (transfer-items-from-npc-to-player
                          :tq0-friend-dude
                          (fn [item] (= (item :id) :tq0-macguffin)))
                        stop-talking))
                    :given]
                 ["No"
                  "Maybe you will give it to me later..."
                  stop-talking
                  :start]]
                :given
                [[nil
                  "Thanks for that item. I love it!"
                  stop-talking
                  :given]]}}}
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
                                       :id :tq0-macguffin
                                       :name "MacGuffin"}]} x y)))
      :nextstagefn (fn [state] :10)}
    :10 {
      :pred (fn [state] (and (> (count (filter #(= (% :id) :tq0-macguffin) (get-in state [:world :player :inventory]))) 0)
                             (contains? (get-in state [:world :places]) :1)))
      :update (fn [state] 
                (let [[_ x y] (first
                                (shuffle
                                  (filter #(= (-> % first :type) :floor)
                                          (with-xy (current-place state)))))]
                (add-npc state :1
                         {:id :tq0-friend-dude
                          :name "Some guy"
                          :type :human
                          :movement-policy :constant
                          :inventory []} x y)))
      :nextstagefn (fn [state] nil)}}}))

