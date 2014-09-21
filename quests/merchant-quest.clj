(ns robinson.quests.merchant-quest
  (:use 
        robinson.common
        robinson.quest
        robinson.npc
        robinson.update
        dorothy.core))


;; A quest where you have to talk to a guy and
;; he gives you the MacGuffin.
(def q (quest :merchant-quest {
  :name "Merchant Quest"
  :data {:state :start}
  :buy-fn (fn [item] (case (item :type)
                         :ring (+ (rand-int 10) 10)
                         nil))
  :dialog {:mq-merchant
           {:initial-state :start
            :m {:start
                [[nil
                  "Heyt. Sho you wanna to buy summin'?"
                  identity
                  :asked]]
                 :asked
                 [["Yes"
                   "Check it out"
                   (fn [state]
                     (start-shopping state (npc-by-id state :mq-merchant)))
                   :start]
                   ["No"
                    "You come back when you want some of this."
                    identity
                    :start]]}}}
  :stages {
    :0 {
      :name "Start"
      :pred (fn [state] (contains? (get-in state [:world :places]) :0))
      :update (fn [state]
                (let [[_ x y] (first
                                (shuffle
                                  (filter #(= (-> % first :type) :floor)
                                          (with-xy (current-place state)))))]
                (add-npc state :0
                         {:id :mq-merchant
                          :name "Merchant Man"
                          :image-path "./images/npc-2.png"
                          :type :human
                          :movement-policy :constant
                          :inventory [{:type :ring
                                       :name "Gaudy Ring"
                                       :price 100}
                                      {:type :food
                                       :name "Lyrium Yogurt"
                                       :hunger 100
                                       :price 20}]}
                        x y
                        [:quests :merchant-quest :buy-fn])))
      :nextstagefn (fn [_] :1)}
    :1 {
      :name "End"
      :pred (fn [_] false)
      :update identity
      :nextstagefn (fn [_] nil)}}}))

