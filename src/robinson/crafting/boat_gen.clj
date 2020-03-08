(ns robinson.crafting.boat-gen
  (:require [robinson.common :as rc]
            [robinson.error :as re]
            [robinson.specs :as rspec]
            [robinson.random :as rr]
            [robinson.world :as rw]
            [robinson.player :as rp]
            [robinson.inventory :as ri]
            [robinson.storylet :as rs]
            [robinson.crafting :as rcrafting]
            [robinson.crafting.effects :as rce]
            [robinson.crafting.mod-protocol :as rcmp]
            [robinson.itemgen :as rig]
            [taoensso.timbre :as log])
  (:gen-class))

(defn item-id->countable-name
  [item-id & [cardinality]]
  (case item-id
    :rope (rand-nth ["piece" "length" "span"])
    "bit of"))


(def continue-choice
  {:name "add next item"
   :hotkey :space
   :source :boat-gen-continue-choice
   :choice/events [:next-item]})

(defmacro ns-bind
  [sym]
  `(symbol (var ~sym)))

(defn pred-use-log
  [state]
  (ri/inventory-contains? state :log))

(defn pred-use-rope
  [state]
  (ri/inventory-contains? state :rope))

(defn pred-use-stick
  [state]
  (ri/inventory-contains? state :stick))

(defn pred-use-ointment
  [state]
  (ri/inventory-contains? state :ointment))
  
(defn pred-use-herbs
  [state]
  (ri/inventory-contains? state :herbs))

(defn raft-choices
  [show-all num-logs num-ropes num-sticks]
  (let [log-choice {:name (str "use log (" num-logs ")")
                    :hotkey \a
                    :pred (ns-bind pred-use-log)
                    :effects [(rce/mod-dec-inventory-by-item-id :log)]
                    :$/items '(fn [items] (merge-with + (or items {}) {(rig/id->item :log) 1}))
                    :choice/events [:use-log]}
        rope-choice {:name (str "use rope (" num-ropes ")")
                     :hotkey \b
                     :pred (ns-bind pred-use-rope)
                     :effects [(rce/mod-dec-inventory-by-item-id :rope)]
                     :$/items '(fn [items] (merge-with + (or items {}) {(rig/id->item :rope) 1}))
                     :choice/events [:use-rope]}
        stick-choice {:name (str "use stick (" num-sticks ")")
                      :hotkey \c
                      :pred (ns-bind pred-use-stick)
                      :effects [(rce/mod-dec-inventory-by-item-id :stick)]
                      :$/items '(fn [items] (merge-with + (or items {}) {(rig/id->item :stick) 1}))
                      :choice/events [:use-stick]}]
    (cons log-choice
      (if show-all
        [rope-choice stick-choice]
        []))))

(defn start
  [state recipe]
  (if (-> state rw/player-cellxy first :type rw/type->water?)
    {:event/id :start
     :description ["The sea calls to you. You lay out your items and start crafting."]
     :event/choices (raft-choices
                      false
                      (ri/inventory-id->count state :log)
                      (ri/inventory-id->count state :rope)
                      (ri/inventory-id->count state :stick))}
    {:description ["This is going to work a lot better in water. Try moving to water first."]
     :event/choices [
       {:name "back"
        :effects [(rce/assoc-current-state-immediate :normal)]
        :abort true}]}))

(defn next-item
  [state recipe]
  {:event/id :next-item
   :description ["You look for the next item to use for the raft."]
   :event/choices (raft-choices true
                    (ri/inventory-id->count state :log)
                    (ri/inventory-id->count state :rope)
                    (ri/inventory-id->count state :stick))})

(rs/def-storylet headache
  #{:any}
  {:event/id :headache
   :description ["It all feels like too much. Your temples begin to throb as a headache rolls through your skull."]
   :event/choices [
     {:name "Take a break"
      :choice/events [
        {:event/id :headache-break
         :description "You pause for a moment."
         :event/choices [
           {:name "take a breath"
            :hotkey :space
            :choice/events [
              :break-hungry
              :break-thirsty]}]}]}
     {:name "Drink water"
      :choice/events [
        {:event/id :headache-water
         :description ["You take a drink of fresh water. It feels cooler in your head and you start to feel better."]
         :event/choices [continue-choice]}]}
     {:name "Take herbs"
      :pred (ns-bind pred-use-herbs)
      :choice/events [
         {:event/id :headache-medicine
          :description [" You search your items for some herbs"]
          :event/choices [
            {:name "continue"
             :hotkey :space
             :effects [(rce/mod-dec-inventory-by-item-id :herb)]
             :choice/events [
               {:event/id :headache-medicine2
                :description ["It works quickly and you go back to work."]
                :event/choices [continue-choice]}]}]}]}
     {:name "Ignore"
      :choice/events [
        {:event/id :headache-ignore
         :description "You try to ignore the throbbing pain."
         :effects [(rce/mod-hp -5 -1)]
         :event/choices [continue-choice]}]}]})

(rs/def-storylet sinks
  #{:after-log}
  {:event/id :sinks
    :description ["A wave crashes into you and your boat begins to bob under the water. It's sinking."]
    :event/choices [
      {:name "Pull it out"
       :choice/events [
         {:event/id :sinks-pull-out
          :description ["You make a grab for the raft but feel your legs give way as the mud slips under your feet."]
          :event/choices [
            {:name "Continue"
             :choice/events [
               {:event/id :sinks-pull-out-2
                :description ["Glurp! You sink up to your knees in the mud."]
                :event/choices [
                  {:name "Continue"
                   :choice/events [
                     {:event/id :sinks-immobile
                      :description ["You're not going anywhere stuck like this and it doesn't look like it's coming back up anytime soon. You'll have to start over."]
                      :event/choices [continue-choice]}
                     {:event/id :sinks-lose-item
                      ; FIXME
                      ;(set: _parts to  (find: where its value &gt; 0, ...(dataentries: $materials)))
                      ;(set: _part to (either: ..._parts))
                      ;(set: _partname to _part&#39;s name)
                      ;(set: $materials&#39;s _partname to it - 1)
                      :description ["You thrash about so you don't get sucked under with the boat. You lose a _partname in the process."]
                      :event/choices [continue-choice]}
                     {:event/id :sinks-sharp-rock
                      :description ["In the struggle to free yourself you step on a jagged rock. You've hurt your foot and can see blood staining the water. It looks bad."]
                      :event/choices [continue-choice]}
                     {:event/id :sinks-strain-back
                      :description ["It's stuck in the mud but you manage to yank it free."]
                      :event/choices [
                        {:name "Pull hard"
                         :effects [(rce/mod-hp -10 -5)]
                         :choice/events [
                          {:event/id :sinks-strains-back2
                          :description ["You pull hard but immediately realize you've hurt yourself. You can barely stand up as a deep pain radiates through your back."]
                          :event/choices [continue-choice]}]}]}]}]}]}]}]}
       {:name "Salvage"
        :choice/events [
          {:event/id :sinks-salvage
           :description ["You grab what you can before the waves wash it away. You'll have to start over."]
           :event/choices [continue-choice]}]}]})

(rs/def-storylet insect
  #{:after-log :stick}
  (let [bug (rand-nth [:spider :centipede :tarantula])
        bugs (get {:spider "spiders" :centipede "centipedes" :tarantula "tarantulas"} bug)]
    {:event/id :insect
      :description [(str "You roll over a log and " bugs " pour out of the wood. They're running toward you fast.")]
      :event/choices [
        {:name "fight"
         :choice/events [
           {:event/id :insect-fight
            :description [(str "You're not abandoning your raft because of some " bugs ". You stand your ground.")]
            :event/choices [
              {:name "fight"
               :effects [(rce/spawn-npc-immediate bug)
                         (rce/assoc-current-state-immediate :normal)]
               :choice/events [:next-item]}]}]}
        {:name "shake them off"
         :choice/events [
           {:event/id :insect-shake
            :description ["You freak out and shake them off but end up damaging the log."]
            :event/choices [continue-choice]}]}
        {:name "wait them out"
         :choice/events [:break-bungry :break-thirsty]}]}))

(rs/def-storylet rope-breaks
  #{:after-rope}
  (let [exclaimation (rand-nth ["SNAP" "THWAP"])]
  {:event/id :rope-breaks
   :description [(str exclaimation "! Your rope breaks in two!")]
   :event/choices [
     {:name "Fix it with a knot"
      :choice/events [
        {:event/id :rope-breaks-knot
         :description [" You attempt to knot the two lengths of rope together and salavge your work."]
         :event/choices [
           {:name "continue"
            :hotkey :space
            :choice/events [
              :rope-burn
              {:event/id :rope-breaks-knot-success
               :description ["It works! You tie the two pieces together and have enough material to continue on."]
               :event/choices [continue-choice]}]}]}]}
     {:name "Replace it"
      :choice/events [
        {:event/id :sope-breaks-replace
         :description ["You'll have to use some more rope to fix the broken part."]
         :event/choices [
           {:name "use rope"
            :choice/events [:use-rope]}]}]}]}))

(rs/def-storylet tired
  #{:any}
  {:event/id :tired
   :description ["Your eyes are heavy and you fumble with the materials. You're feeling much too tired to think straight."]
    :event/choices [
      {:name "Sleep"
        :choice/events [
        {:event/id :tired-sleep
         :description [" You rest, better safe than sorry."]
         :event/choices [
           {:name "sleep"
            :effects [(rce/mod-wtl 5 10)
                      (rce/assoc-current-state-immediate :sleep)]
            :choice/events [:next-item]}]}]}
      {:name "Ignore"
       :choice/events [
         {:event/id :tired-ignore
          :description ["You press on, ignoring the pain for now."]
          :event/choices [continue-choice]}]}]})

(rs/def-storylet rope-burn
  #{:after-rope}
  (let [exclaimation(rand-nth ["Swish" " Zip" " Zing" "Ouch"])
        bodypart (rand-nth ["hand" "wrist" "arm" "leg"])]
    {:event/id :rope-burn
     :$bodypart bodypart
     :description [(str exclaimation "! The rope rips against your " bodypart " giving you a friction burn.")]
     :event/choices [
       {:name "use bandage"
        :effects [(rce/mod-hp -10 -5)]
        :choice/events [
          {:event/id :rope-burn-bandage
           :description ["You pull out a bandage and wrap it as well as you can. You have to keep moving."]
           :event/choices [continue-choice]}]}
       {:name "apply ointment"
        :pred (ns-bind pred-use-ointment)
        :effects [(rce/mod-hp -10 -5)
                  (rce/mod-dec-inventory-by-item-id :ointment)]
        :choice/events [
          {:event/id :rope-burn-ointment
           :description ["You apply some ointment to the burn and push on."]
           :event/choices [continue-choice]}]}
       {:name "slow down"
        :effects [(rce/mod-hp -10 -5)]
        :choice/events [:break-hungry :break-thirsty]}
       {:name "ignore"
        :effects [(rce/mod-hp -10 -5)]
        :choice/events [
          {:event/id :rope-burn-ignore
           :description ["It's hard but you try to push through the pain."]
           :event/choices [continue-choice]}]}]}))

(rs/def-storylet fungus
  #{:after-log :after-stick}
  (let [break (rand-nth ["splinter" "crack" "shatter"])
        quality (rand-nth ["completely rotted" "covered in fungus" "rotten to the core" "dry-rotted"])]
    {:event/id :fungus
     :description [(str "You pull out some wood but it starts to " break " when you grab it. You look it over and see that its " quality ".")]
     :event/choices [
       {:name "cut it out"
        :choice/events [
          {:event/id :fungus-cut-out1
           :description ["Carefully, you work your way around the damaged area and remove the fungus."]
           :event/choices [continue-choice]}
          {:event/id :fungus-cut-out2
           :description ["There was just too much damaged area. You had to remove so much wood that the raft is unstable."]
           :event/choices [
             {:name "replace the part"
              :choice/events [:fungus-replacement]}]}]}
       {:name "find a replacement"
        :choice/events [:fungus-replacement]}
       {:name "reinforce it"
        :choice/events [:fungus-reinforce]}]}))

(defn fungus-replacement
  [state recipe]
  {:event/id :fungus-replacement
   :description ["You replace it with some other wood. Hopefully this piece is better."]
   :event/choices [
     {:name "continue"
      :hotkey :space
      :choice/events [:use-log :use-stick]}]})

(defn fungus-reinforce
  [state recipe]
  {:event/id :fungus-reinforce
   :description ["You don't have time to find something else or start over. You try to reinforce it with extra rope. That's going to have to do."]
   :event/choices [
     {:name "continue"
      :hotkey :space
      :choice/events [:use-rope]}]})

(defn splinter
  [state recipe]
  (let [bodypart (rand-nth ["hand" "wrist" "finger" "thumb"])
        exclaimation (rand-nth ["Zing" "Ouch"])]
    {:event/id :splinter
     :description [(str exclaimation "! You got a pretty serious splinter from the wood. It's driven deep in your " bodypart ".")]
     :event/choices [
       {:name "try and take it out"
        :effects [(rce/mod-hp -10 -5)]
        :choice/events [
          {:event/id :spinter-take-out
           :description [(str "You frantically rip open your " bodypart " trying to get it out. It hurts so bad and you think you might have made it worse.")]
           :event/choices [continue-choice]}]}
       {:name "apply a bandage"
        :effects [(rce/mod-hp -10 -5)]
        :choice/events [
          {:event/id :splinter-bandage
           :description [(str "You apply a bandage to your " bodypart " and hope for the best. Maybe it'll work itself out?")]
           :event/choices [continue-choice]}]}
       {:name "ignore"
        :effects [(rce/mod-hp -10 -5)]
        :choice/events [
          {:event/id :splinter-ignore
           :description ["You decide to wait and see what happens. After all, it's just a splinter..."]
           :event/choices [continue-choice]}]}]}))

(defn clothes-choices
  [state]
  (->> state
    ri/player-inventory
    (filter rig/is-wearable?)
    (map (fn [item]
           (if (rig/is-clothes? item)
             {:name (str "use " (get item :name))
              :choice/events [
               {:description [(str "You put on the " (get item :name))]
                :event/choices [
                  {:name "continue"
                   :effects [(rce/mod-wear-clothes item)]
                   :choice/events [
                     {:description ["You look pretty good in this and it's working to block the sun."
                                    "You take a look at yourself. Pretty good. You think."]
                      :event/choices [continue-choice]}]}]}]}
             {:name (str "use " (get item :name))
             :choice/events [
               {:description [(str "You wrap yourself in " (rand-nth ["some" "a bit of"]) " " (get item :name) " to protect yourself from the sun.")]
                :event/choices [
                  {:name "continue"
                   :effects [(rce/mod-wear-clothes item)]
                   :choice/events [
                     {:description [(str "It's " (rand-nth ["clumsy" "unflattering" "not the most flattering garment"]) " and you look strange, but it's working.")]
                      :event/choices [continue-choice]}]}]}]})))))
(defn sunburn
  [state recipe]
  (log/info (-> state rp/get-player))
  (if-let [worn-item (rp/worn-item state)]
    {:event/id :sunburn-protection
     :description [(str "The sun's rays fail to penetrate the " (get worn-item :name) " you are wearing.")]
     :event/choices [continue-choice]}
    {:event/id :sunburn
     :description ["The suns harsh rays beat down on your back and shoulders. You'll sunburn soon."]
     :event/choices [
       {:name "Find something to cover up"
        :effects [(rce/mod-hp -10 -5)]
        :choice/events [
        {:event/id :sunburn-cover
          :description ["What would you like to use to cover yourself?"]
          :event/choices
             (conj (vec (clothes-choices state))
               {:choice/id :sunburn-ignore
                :name "pass"
                :choice/events [
                  {:description ["On second thought, let's pass"]
                   :event/choices [continue-choice]}]})}]}
       {:choice/id :sunburn-ignore
        :name "Ignore"
        :effects [(rce/mod-hp -10 -5)]
        :choice/events [
          {:event/id :sunburn-ignore
           :description ["You can't stop now! You decide to ignore the burn and continue working."]
           :event/choices [continue-choice]}]}]}))

(defn break-hungry
  [state recipe]
  {:event/id :break-hungry
  :description ["You take a break."]
   :event/choices [
     {:name "Continue"
      :effects [(rce/mod-hunger -10 -5)]
      :choice/events [
        {:event/id :break-hungry2
         :description ["Rumble rumble. Your stomach starts to churn. You get hungry after some time."]
         :event/choices [continue-choice]}]}]})

(defn break-thirsty
  [state recipe]
  {:event/id :break-thirsty
   :description ["You decide to take a break."]
   :event/choices [
     {:name "continue"
      :effects [(rce/mod-thirst -10 -5)]
      :choice/events [
        {:event/id :break-thirst2
         :description ["You're getting thirsty waiting around."]
         :event/choices [continue-choice]}]}]})

(rs/def-storylet tired-ignore
  #{:tired}
  (let [material (rand-nth (get recipe :$/items))
        quality (rand-nth ["forgot to" "didn't"])
        travel (rand-nth ["float" "drift"])]
    (log/info "material" material)
    {:event/id :tired-ignore-2
     :description [(str "Shoot! You scramble when you realize you " quality " secure the raft parts well enough. You see a " material " " travel " away.")]
     :event/choices [
       {:name "reach for it"
        ; Remove material
        :$/items (update-in recipe [:$/items (get material :item/id)] dec)
         :choice/events [
           {:description ["It's too far away to get!"]
            :event/choices [continue-choice]}]}]}))

(rs/def-storylet rope-burn-ignore-cont
  #{:rope-burn :once}
  {:event/id :rope-burn-ignore-cont
   :description "On second thought maybe you shouldn't have ignored the rope burn."
   :event/choices [
     {:name "continue"
      :hotkey :space
      :effects [(rce/mod-hp -15 -10)
                (rce/mod-infected)]
      :choice/events [
        {:event/id :rope-burn-ignore3
         :description ["That rope burn looks bad. You hope it doesn't get infected."]
         :event/choices [continue-choice]}]}]})

(rs/def-storylet splinter-ignore-cont
  #{:splinter :once}
  {:event/id :splinter-ignore-cont
   :description ["Your $bodypart is throbbing. Its starting to look infected."]
   :event/choices [
     {:name "continue"
      :hotkey :space
      :effects [(rce/mod-hp -15 -10)
                (rce/mod-infected)]
      :choice/events [
        {:event/id :splinter-ignore3
         :description ["That splinter looks bad. You hope it doesn't get infected."]
         :event/choices [continue-choice]}]}]})

(defn finished
  [state recipe]
  (log/info "finished" (get recipe :type))
  (let [valid-recipe (rcrafting/get-recipe :log-raft)
        dominate-item (rcrafting/dominate-item
                        (filter #(rcrafting/item-satisfies-any-clause? %1 valid-recipe)
                                (->> recipe
                                  :$/items
                                  (map (fn [[item n]] (assoc item :count n))))))
        _ (log/info "finished valid recipe" valid-recipe)
        recipe-name (rcrafting/recipe-name (-> valid-recipe
                                             (merge recipe)
                                             (assoc :recipe/dominate-item dominate-item
                                                    :items (get recipe :$/items)))
                                           false)]
 
    (assert dominate-item)
    {:event/id :finished
     :description ["The raft is shaping up nicely."]
     :event/choices [
       {:name "continue"
        :hotkey :space
        :choice/events [
         {:description ["It looks properly seaworthly."]
         :event/choices [
           {:name "finish"
            :hotkey :space
            :recipe/name recipe-name
            :recipe/types (get valid-recipe :recipe/types)
            :recipe/dominate-item dominate-item
            :choice/events [
            {:description ["sdfsf"]
             :event/choices [
               {:name "finish"
                :done true}]}]}]}]}]}))

(defn any-in?
  [s1 s2]
  (log/info s1 s2 (not-empty (clojure.set/intersection s1 s2)))
  (not-empty (clojure.set/intersection s1 s2)))

(defn raft-complete?
  [recipe]
  (let [items (->> recipe
                :$/items
                (map (fn [[item n]] (assoc item :count n)))
                vec)
        blueprint (rcrafting/get-recipe :log-raft)
        r (rcrafting/valid-recipe?
            items
            blueprint)]
    (log/info "raft-complete?" r items blueprint)
    r))

(def boat-gen-ns *ns*)

(defn next-event
  [state recipe after]
  {:pre [(re/validate ::rspec/state state)
         (keyword? after)]}
  ((if (raft-complete? recipe)
    finished
    (rs/rand-storylet state recipe (rs/ns-storylets boat-gen-ns) after))
   state recipe))

(defn next-choice
  [state recipe after]
  {:name "continue"
   :hotkey :space
   :choice/events [(next-event state recipe after)]})

(defn use-rope
  [state recipe]
  (let [countable-name (item-id->countable-name :rope)]
    {:event/id :use-rope
     :description (str "You pick a " countable-name " of rope and start attaching it to the raft.")
     :event/choices [(next-choice state recipe :after-rope)]}))

(defn use-stick
  [state recipe]
  (let [add-verb (rand-nth ["add it" "incorporate it into"])]
    {:event/id :use-stick
     :description (str "You pick a stick and " add-verb " the raft.")
     :event/choices [(next-choice state recipe :after-stick)]}))

(defn use-log
  [state recipe]
  (let [carry-verb (rand-nth ["heft" "lug" "haul"])]
    {:event/id :use-log
     :description (str "You " carry-verb " a log and add it to the raft.")
     :event/choices [(next-choice state recipe :after-log)]}))

(defn gen-random
  [state recipe]
  (start state recipe))

