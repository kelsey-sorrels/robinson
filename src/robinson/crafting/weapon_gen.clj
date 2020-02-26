(ns robinson.crafting.weapon-gen
  (:require [robinson.common :as rc]
            [robinson.random :as rr]
            [robinson.world :as rw]
            [robinson.player :as rp]
            [robinson.inventory :as ri]
            [robinson.crafting :as rcrafting]
            [robinson.crafting.mod-protocol :as rcmp]
            [robinson.crafting.effects :as rce]
            [robinson.itemgen :as rig]
            [taoensso.timbre :as log]))

(defn recipe-requirements [recipe]
  "")

(defn recipe-output [recipe]
  "")

(def contact-types
  #{:blunt :edged :piercing :flexible})

(def wield-types
  #{:melee :thrown :ranged})

(defn contact-type?
  [t]
  (contains? contact-types t))

(defn wield-type?
  [t]
  (contains? wield-types t))

(defn other-contact-type
  [t]
  {:pre [(contact-type? t)]}
  (rand-nth (vec (disj contact-types t))))

(defn other-wield-type
  [t]
  {:pre [(wield-type? t)]}
  (rand-nth (vec (disj wield-types t))))

(defn short-name [name]
  (get {"damage" "dmg"
        "accuracy" "acc"
        "speed" "spd"
        "durability" "drb"
        "hunger" "hng"
        "thirst" "thr"}
       name name))

(defn replace-contact
  [old-types new-types]
  (-> old-types
    (clojure.set/difference contact-types)
    (clojure.set/union new-types)))

(defn replace-wield
  [old-types new-types]
  (-> old-types
    (clojure.set/difference wield-types)
    (clojure.set/union new-types)))

(defn merge-contact
  [contact-type]
  (with-meta #{contact-type} {:merge 'replace-contact}))

(defn merge-wield
  [wield-type]
  (with-meta #{wield-type} {:merge 'replace-wield}))

(defn replace
  [old-x new-x]
  new-x)

(defn merge-replace
  [x]
  (with-meta x {:merge 'replace}))
  

(def contact-choices
  {:blunt
     {:name "Blunt"
      :hotkey \a
      :recipe/types (merge-contact :blunt)}
   :edged
     {:name "Edged"
      :hotkey \b
      :recipe/types (merge-contact :edged)}
   :piercing
     {:name "Piercing"
      :hotkey \c
      :recipe/types (merge-contact :piercing)}
   :flexible
     {:name "Flexible"
      :hotkey \d
      :recipe/types (merge-contact :flexible)}})

(def wield-choices
  {:melee
    {:name "Melee"
     :hotkey \a
     :recipe/types (merge-wield :melee)}
   :thrown
     {:name "Thrown"
      :hotkey \b
      :recipe/types (merge-wield :thrown)}
   :ranged
     {:name "Ranged"
      :hotkey \c
      :recipe/types (merge-wield :ranged)}})

; Weapon questions are two-tiered :contact-type and :wield-type
; :contact-type is one of
;   :blunt
;   :edged
;   :piercing
;   :flexible
; :wield-type is one of
;   :melee
;   :thrown
;   :ranged
(defn gen-question-contact [state]
  (rcrafting/assoc-current-recipe state :current-stage {
    :event/id :event.id/contact-type
    :gen #{:contact-type}
    :title "Choose weapon type"
    :event/choices (vals contact-choices)}))

(defn gen-question-wield [state]
  (rcrafting/assoc-current-recipe state :current-stage {
    :event/id :event.id/wield-type
    :gen #{:wield-type}
    :title "Choose weapon type"
    :event/choices (vals wield-choices)}))

(defn gen-question [state recipe]
  {:pre [(not (nil? state))]
   :post [(not (nil? %))]}
  "Add a question to the current recipe"
  (cond
    (contains? (get recipe :gen) :contact-type)
      (gen-question-wield state)
    (contains? (get recipe :gen) :wield-type)
      (gen-question-contact state)
    :else
      ((rand-nth [gen-question-contact gen-question-wield]) state)))

(defn rand-unused-item
  [recipe ns]
  (let [id (keyword ns "id")]
    (log/info "rand-unused-item" (get recipe :items) (get recipe id))
    (let [unused-items (-> recipe
                        :items
                        (->>
                          (remove (fn [{id :item/id}] (contains? (get recipe id #{}) id)))))]
      (when (seq unused-items)
        (rand-nth unused-items)))))

;; :requirements is a set of any
;;  :event/id
;;  :choice/id
;;  :recipe/flags
;;  :recipe/types
;;  :recipe/id
;;  :recipe/components
;;  Any matches between the recipe and the complication will mark the complication
;;  for inclusion. Any complication with empty or nil :requirements will always
;;  be marked for inclusion.
(defn weapon-complications
  [recipe]
  (if-let [item (rand-unused-item recipe "event.complication.item")]
    [
      ; Material complications
      {:event/id :material
       :description ["You can't find the materials you need."
                     (fn [recipe]
                       (let [item (-> recipe :items vec rand-nth)]
                         (log/info "material complication" item recipe)
                         (format "The %s you are using are is too %s"
                           (get item :name)
                           (rcrafting/extreme-word item))))]
       :event/choices [
         {:hotkey \a
          :name "Search for something better"
          :event.complication.item/id (get item :item/id)
          :choice/events [
            {:description "You give up after spending hours looking for the right materials."
              :choice/one-of [(rce/mod-thirst -10 -5)
                              (rce/mod-hunger -10 -5)
                              (rce/mod-wtl -10 -5)]}
            {:description "You spend hours looking for the right material."
             :choice/one-of [(rce/mod-wtl 5 7)
                             (rce/mod-thirst -10 -5)
                             (rce/mod-hunger -10 -5)]}
            {:description "After hours of searching, you find the right material."
             :choice/one-of [(rce/mod-wtl 5 7)
                             (rce/mod-thirst -10 -5)
                             (rce/mod-hunger -10 -5)]}]}
         {:hotkey \b
          :name "Give up"
          :event.complication.item/id (get item :item/id)
          :choice/events [
            {:description "You learn to make do without the right parts."
             :choice/one-of [
               (rce/mod-durability -3 -1)
               (rce/mod-wtl -10 -5)]}
            {:description "You stop and take a break. After a few minutes inspiration strikes."
             :choice/one-of [
               (rce/mod-durability 1 3)
               (rce/mod-wtl 1 5)]}]}]}

      ; Bodily Limit
      #_{:event/id :body-constraint
       :description "You are overexerted. Slow down survivor. Take your time."
       :event/choices [{
         :hotkey :space
         :name "continue"
         :choice/one-of [
           (rce/mod-thirst 11 21)]}]}

      ; Edged
      #_{:event/id :edged
       :description ["The edge keeps warping when it should be straight."
                     "The edge should be smooth, not full of chips."
                     "The edge you were woking with has rolled causing your to start over."]
       :requirements #{:edged}
       :event/choices [{
         :hotkey :space
         :name "continue"
         :choice/one-of [
           (rce/mod-damage -7 -3)]}]}

      ; Too much or too little in size, shape, weight
      #_{:event/id :physical-property
       :description ["Too heavy. The balance is all off on this. It really should be lighter."
                     "Too light. You need something heavier to work with. This is too light."
                     "Too long!"
                     "Too short!"
                     "Not tapered enough!"]
       :requirements #{:low-weight :high-weight}
       :event/choices [{
         :hotkey :space
         :name "continue"
         :choice/one-of [
           (rce/mod-thirst 11 21)]}]}

      ; Handled weapon complications
      #_{:event/id :handle
       :description "You can hardly hold onto this. You need to find something easier to hold."
       :requirements #{:handle}
       :event/choices [{
         :hotkey :space
         :name "continue"
         :choice/one-of [
           (rce/mod-thirst 11 21)]}]}

      ; Flexible weapon complications
      #_{:event/id :flexibe
       :description "There should be a little more give to this. You know it's going to break."
       :requirements #{:flexible}
       :event/choices [{
         :hotkey :space
         :name "continue"
         :choice/one-of [
           (rce/mod-thirst 11 21)]}]}

      ; Wooden
      {:event/id :event.id/wooden
       :description ["The wood you have is bent. It's just not going to work."
                     "The wood you have is green. It should be more seasoned."
                     "The wood is full of holes caused by unending seasons of wind and rain."
                     "The wood you have is cracked."
                     "A crack is forming in the wood you are using."
                     "The wood you have is full of pin knots."
                     "The wood you have is weatherworn."]
       :requirements #{:recipe.item.property/wooden}
       :event/choices [{
         :hotkey :space
         :name "continue"
         :choice/one-of [
           (rce/mod-durability -3 -1)
           (rce/mod-damage -3 -1)
           (rce/mod-accuracy -3 -1)]
         :choice/events [{
           :description "You try to find a way to fix it."
           :event/choices [{
             :choice/id :choice.id/wooden-find-replacement
             :hotkey \a
             :name "find replacement"
            }{
             :choice/id :choice.id/wooden-change-design
             :hotkey \b
             :name "change design"
            }{
             :choice/id :choice.id/wooden-use-anyway
             :hotkey \c
             :name "use anyway"}]}]}]}

      ; Wooden - find replacement - complication
      {:event/id :event.id/wooden-find-replacement-complication
       :description ["Your search for a replacement fails. You cannot find the part you need."]
       :requirements #{:choice.id/wooden-find-replacement}
       :event/choices [{
         :hotkey :space
         :name "continue"
         :choice/one-of [
           (rce/mod-hunger -10 -5)
           (rce/mod-thirst -10 -5)]
        }]}

      ; Dimensional
      {:event/id :event.id/dimensional
       :description [(format "The %s you have is too big. You can't get it to work without finding something smaller."
                       (get item :name))
                     (format "The %s you have is too wide It should be more narrow."
                       (get item :name))
                     (format "The  you are trying to use is too thick. Something flatter would work better."
                       (get item :name))]
       :requirements #{}
       :event/choices [{
         :hotkey :space
         :name "continue"
         :choice/one-of [
           (rce/mod-durability -3 -1 item)
           (rce/mod-damage -3 -1 item )
           (rce/mod-accuracy -3 -1 item)]
         :choice/events [{
           :description "How do you want to try to to fix it?"
           :event/choices [{
             :choice/id :choice.id/dimensional-find-replacement
             :hotkey \a
             :name "find replacement"
            }{
             :choice/id :choice.id/dimensional-change-design
             :hotkey \b
             :name "change design"
            }{
             :choice/id :choice.id/dimensional-use-anyway
             :hotkey \c
             :name "use anyway"}]}]}]}

      ; Dimensional - find replacement - complication
      {:event/id :event.id/dimensional-find-replacement-complication
       :description ["Your search for a replacement fails. You cannot find the part you need."]
       :requirements #{:choice.id/dimensional-find-replacement}
       :event/choices [{
         :hotkey :space
         :name "continue"
         :choice/one-of [
           (rce/mod-hunger -10 -5)
           (rce/mod-thirst -10 -5)]
        }]}

      ; Edged
      {:event/id :event.id/edged
       :description ["The edge you are working with is chipped. You need a clean edge to move on."
                     "The edge you have is dull. It needs to be sharp."]
       :requirements #{:edged}
       :event/choices [{
         :hotkey :space
         :name "continue"
         :choice/one-of [
           (rce/mod-damage -5 -3)]
         :choice/events [{
           :description "How do you want to try to to fix it?"
           :event/choices [{
             :choice/id :choice.id/edged-find-replacement
             :hotkey \a
             :name "find replacement"
            }{
             :choice/id :choice.id/edged-change-design
             :hotkey \b
             :name "change design"
            }{
             :choice/id :choice.id/edged-use-anyway
             :hotkey \c
             :name "use anyway"}]}]}]}

      ; Edged - find replacement - complication
      {:event/id :event.id/edged-find-replacement-complication
       :description ["Your search for a replacement edge fails. You cannot find the edged item you need."]
       :requirements #{:choice.id/edged-find-replacement}
       :event/choices [{
         :hotkey :space
         :name "continue"
         :choice/one-of [
           (rce/mod-hunger -10 -5)
           (rce/mod-thirst -10 -5)]
        }]}

      ; Flexible
      {:event/id :event.id/flexible
       :description ["The part you are working with is too ridged You need a more supple part to move on."
                     "The part you are using is not strong enough under tension. It needs to be stronger."]
       :requirements #{:flexible}
       :event/choices [{
         :hotkey :space
         :name "continue"
         :choice/one-of [
           (rce/mod-damage -5 -3)]
         :choice/events [{
           :description "How do you want to try to to fix it?"
           :event/choices [{
             :choice/id :choice.id/flexible-find-replacement
             :hotkey \a
             :name "find replacement"
            }{
             :choice/id :choice.id/flexible-change-design
             :hotkey \b
             :name "change design"
            }{
             :choice/id :choice.id/flexible-use-anyway
             :hotkey \c
             :name "use anyway"}]}]}]}

      ; Flexible - find replacement - complication
      {:event/id :event.id/flexible-find-replacement-complication
       :description ["Your search for a replacement flexible item fails. You cannot find the flexible item you need."]
       :requirements #{:choice.id/flexible-find-replacement}
       :event/choices [{
         :hotkey :space
         :name "continue"
         :choice/one-of [
           (rce/mod-hunger -10 -5)
           (rce/mod-thirst -10 -5)]
        }]}

      ; Handled
      {:event/id :event.id/handled
       :description ["The handle you are designing keeps coming off. You need to find a way to secure it."
                     "The handle you are making has burns. You need to fix it."]
       :requirements #{:handled}
       :event/choices [{
         :hotkey :space
         :name "continue"
         :choice/one-of [
           (rce/mod-damage -5 -3)]
         :choice/events [{
           :description "How do you want to try to to fix it?"
           :event/choices [{
             :choice/id :choice.id/handled-find-replacement
             :hotkey \a
             :name "find replacement"
            }{
             :choice/id :choice.id/handled-change-design
             :hotkey \b
             :name "change design"
            }{
             :choice/id :choice.id/handled-use-anyway
             :hotkey \c
             :name "use anyway"}]}]}]}

      ; Handled - find replacement - complication
      {:event/id :event.id/handled-find-replacement-complication
       :description ["Your search for a replacement handled item fails. You cannot find the handled item you need."]
       :requirements #{:choice.id/handled-find-replacement}
       :event/choices [{
         :hotkey :space
         :name "continue"
         :choice/one-of [
           (rce/mod-hunger -10 -5)
           (rce/mod-thirst -10 -5)]
        }]}

      ; Ranged
      {:event/id :event.id/ranged
       :description ["The prototype isn't accurate enough. You keep missing the target."
                     "The prototype you are making doesn't have enough range .You need to make it reach farther."]
       :requirements #{:ranged}
       :event/choices [{
         :hotkey :space
         :name "continue"
         :choice/one-of [
           (rce/mod-damage -5 -3)]
         :choice/events [{
           :description "How do you want to try to to fix it?"
           :event/choices [{
             :choice/id :choice.id/ranged-find-replacement
             :hotkey \a
             :name "find replacement"
            }{
             :choice/id :choice.id/ranged-change-design
             :hotkey \b
             :name "change design"
            }{
             :choice/id :choice.id/ranged-use-anyway
             :hotkey \c
             :name "use anyway"}]}]}]}

      ; Ranged - find replacement - complication
      {:event/id :event.id/ranged-find-replacement-complication
       :description ["Your search for a replacement ranged item fails. You cannot find the ranged item you need."]
       :requirements #{:choice.id/ranged-find-replacement}
       :event/choices [{
         :hotkey :space
         :name "continue"
         :choice/one-of [
           (rce/mod-hunger -10 -5)
           (rce/mod-thirst -10 -5)]
        }]}

      ; Change Type
      {:event/id :event.id/change-type
       :description ["The design change took a wrong turn. You find yourself making something completely different."]
       :requirements #{:choice.id/wooden-change-design
                       :choice.id/edged-change-design}
       :event/choices (let [types (get recipe :recipe/types)]
         (log/info "change-type" types)
         (map (fn [recipe-type]
           (log/info "recipe-type" recipe-type)
           (cond
             (contact-type? recipe-type)
               (let [contact-type (other-contact-type recipe-type)]
                 (get contact-choices contact-type))
             (wield-type? recipe-type)
               (let [wield-type (other-wield-type recipe-type)]
                 (get wield-choices wield-type))))
           types))}

      ; Wooden handle complications
      #_{:event/id :wooden-handle
       :description ["Handle burns!"
                     "Handle uneven!"
                     "Handle popping out!"]
       :requirements #{:handle :wooden}
       :event/choices [{
         :hotkey :space
         :name "continue"
         :choice/one-of [
           (rce/mod-thirst 11 21)]}]}

      ; Special Boomerange Complications
      #_{:event/id :boomerang
       :description "Not bent enough!"
       :requirements #{:boomerang}
       :event/choices [{
         :hotkey :space
         :name "continue"
         :choice/one-of [
           (rce/mod-thirst 11 21)]}]}

      ; Splinter
      #_{:event/id :splinter
       :description ["Ouch! What was that? It's a splinter in your hand."
                     "Oof! A splinter wedged its way into your finger."]
       :requirements #{:wooden}
       :event/choices [{
         :hotkey :space
         :name "continue"
         :choice/one-of [
           (rce/mod-thirst 11 21)]}]}

      ; Wooden joints
      #_{:event/id :weak-joinery
       :description "Weak joinery!"
       :requirements #{:wooden}
       :event/choices [{
         :hotkey :space
         :name "continue"
         :choice/one-of [
           (rce/mod-thirst 11 21)]}]}

      ; Poisonous material
      #_{:event/id :allergic
       :description ["Poisonous material!"
                     "Allergic to material!"]
       :requirements #{:wooden}
       :event/choices [{
         :hotkey :space
         :name "continue"
         :choice/one-of [
           (rce/mod-thirst 11 21)]}]}

      ; Hunger
      {:event/id :hunger
       :description ["You feel so hungry you can't think straight."
                     "Your rumbling stomach distracts you from your task."]
       :requirements #{:player.state/hungry}
       :event/choices [{
         :hotkey :space
         :name "continue"
         :choice/one-of [
           (rce/mod-wtl -10 -5)]}]}

      ; Thirst
      #_{:event/id :thirst
       :description "Distracted by thirst!"
       :event/choices [{
         :hotkey :space
         :name "continue"
         :choice/one-of [
           (rce/mod-thirst 11 21)]}]}

      ; Low light
      #_{:event/id :low-light
       :description "Low light!"
       :event/choices [{
         :hotkey :space
         :name "continue"
         :choice/one-of [
           (rce/mod-thirst 11 21)]}]}

      ; Sound
      #_{:event/id :sound
       :description ["Interrupted by sound!"
                     "Loud work environment!"]
       :event/choices [{
         :hotkey :space
         :name "continue"
         :choice/one-of [
           (rce/mod-thirst 11 21)]}]}

      ; Empty prototype -----------------
      #_{:title ""
       :description ""
       :event/choices [{
         :hotkey :space
         :name "continue"
         :choice/one-of [
           (rce/mod-thirst 11 21)]}]}

      ; Spider
      #_{:event/id :spider
       :description "You notice a spider on your leg."
       :event/choices [
         {:hotkey \a
          :name "slap it"
          :choice/events [
            {:description "You pull you hand back and the spider runs up your leg."
             :event/choices [
               {:hotkey :space
                :name "attack"
                :choice/events [
                  {:description "You hit the spider and your thigh."
                   :choice/one-of [(rce/mod-hp 5 7)]}]}]}
             {:description "Before you can hit it the spider sinks its fangs into your leg."
              :choice/one-of [(rce/mod-hp 5 7)]}
             {:description "You slap the spider. There is not enough meat to eat."}]}
         {:hotkey \b
          :name "brush it off"
          :choice/events [
            {:description "You brush the spider off and it disappears into the ground."}
            {:description "As you brush off the spider thousands of spider babies pour onto your leg"
             :event/choices [
               {:hotkey \a
                :name "just give up and die"
                :choice/one-of [(rce/mod-hp 10 50)]}
               {:hotkey \b
                :name "slap all of them"
                :choice/one-of [(rce/mod-hp 5 7)]}
               {:hotkey \c
                :name "roll on the ground"}]}]}]}]
    []))


;;  :event/id
;;  :choice/id
;;  :recipe/id
;;  :recipe/categoy
;;  :recipe/types
;;  :recipe/flags
;;  :recipe/components
(defn gen-event [state
                 {:recipe/keys [category
                                types
                                flags
                                components
                                example-item-properties]
                  recipe-id :recipe/id
                  past-event-ids :past-event-ids
                  choice-id :choice/id
                  :as recipe}
                 event-type
                 events]
  (let [recipe-pool (set (clojure.set/union (when recipe-id #{recipe-id})
                                            choice-id
                                            (when category #{category})
                                            past-event-ids
                                            types
                                            components
                                            example-item-properties
                                            (when (< (rp/player-hunger state) 20) #{:player.state/hungry})))
        _ (log/info "past events" past-event-ids)
        _ (log/info "recipe-pool" recipe-pool)
        ; candidate events
        events (->> events
                 (remove nil?)
                 (filter
                   (fn [event]
                     (let [requirements (get event :requirements #{})
                           requirements-match (not-empty (clojure.set/intersection recipe-pool requirements))
                           no-requirements (empty? requirements)
                           no-past-events (not (contains? (set past-event-ids) (get event :event/id)))]
                       (log/info "event-info" (get event :event/id) (get event :description) past-event-ids)
                       (log/info "requirements" requirements)
                       (log/info requirements-match)
                       (log/info no-requirements)
                       (log/info no-past-events)
                       (and
                         ; event meets requirements
                         (or requirements-match
                             no-requirements)
                          ; not already encountered
                         no-past-events)))))

        _ (assert (not-empty events) "events empty")
        invoke-fn (fn invoke-fn [f]
                    (log/info "invoke-fn" f)
                    (f (rcrafting/current-recipe state)))
        blueprint (rr/rand-weighted-nth (map (fn [event] [(inc (count (get event :requirements #{}))) event])
                                                 events))
        _ (log/info "desc1" (-> blueprint
                                            :description))

        description (-> blueprint
                        :description
                        (rc/fn-cond->
                          sequential? (-> vec rand-nth)
                          fn? (apply [(rcrafting/current-recipe state)])))
        _ (log/info "desc1" description)
        _ (assert (not (fn? description)) (str "description is fn" description))
        next-stage (-> blueprint
                      (assoc :description description
                             :event/type event-type)
                      (update 
                        :event/choices (partial map (fn assoc-choice-effects [choice]
                                              (if-let [effect (rand-nth (get choice :choice/one-of))]
                                                (do (log/info "gen-event choice" choice "effect" effect)
                                                (assoc choice
                                                   :effects [effect]))
                                                choice)))))]
    (log/info "gen-event next-stage" next-stage)
    (when-not (get next-stage :title)
      (log/error next-stage "missing title"))
    (rcrafting/assoc-current-recipe state
       :current-stage next-stage
       :past-event-ids (if-let [next-event-id (get next-stage :event/id)]
                         (conj (or past-event-ids #{}) next-event-id)
                         past-event-ids))))

(defn gen-complication [state recipe]
    (gen-event state recipe
      :event-type/complication
      (weapon-complications recipe)))

(defn negative-effect?
  [effect]
  (and (rcmp/normative? effect)
       (neg? (rcmp/utility effect))))

(defn remedies
  [state recipe]
  ; Find debuffs which have not been remedied
  (let [debuffs (->> recipe
                  :effects
                  (filter negative-effect?)
                  (remove
                    (fn [effect]
                      (and
                        (satisfies? rcmp/ModCause effect)
                        (contains?
                          (-> recipe :event.complication.item/id)
                          (-> effect rcmp/cause :item/id))))))]
    (log/info "remedies debuffs" debuffs)
    ; slowdown
    ; push through
    (if (seq debuffs)
      (if-let [debuff (when (seq debuffs) (rand-nth debuffs))]
        [{:title "Remedy"
          :description "You think you can fix a problem with this item." 
          :event/choices [
            {:hotkey \a
             :name (str "fix " (rcmp/full-name debuff))
             :choice/one-of [(rce/mod-remove-effect debuff)]}
            {:name "skip"
             :hotkey :space}]}]
        [])
      [])))

(defn gen-remedy [state recipe]
  (gen-event state recipe :event-type/remedy (remedies state recipe)))

(defn gen-material [state recipe]
  (log/info (get recipe :recipe/types))
  (let [example-items (rcrafting/get-example-items-by-types (get recipe :recipe/types))
        _ (log/info "example-items" (vec example-items))
        items (take 2 (shuffle example-items))
        item-id (or (first items) :stick)
        amount (+ 1 (rand-int 4))
        item-name (or (if (< 1 amount)
                        (rig/id->name-plural item-id)
                        (rig/id->name item-id))
                      "unknown")
        _ (log/info "items:" items "item-id:" item-id "item-name:" item-name)
        buff-mod (first (shuffle [
                     (rce/mod-accuracy 1 5)
                     (rce/mod-damage 1 4)
                     (rce/mod-durability 1 5)
                     (rce/mod-stunning 0.1 0.5)
                     (rce/mod-dismembering 0.1 0.5)
                     (rce/mod-bleeding 0.1 0.5)
                     (rce/mod-knockback 0.1 0.5)]))
        debuff-mod (first (shuffle [
                     (rce/mod-accuracy -5 -1)
                     (rce/mod-damage -6 -2)
                     (rce/mod-durability -6 -2)]))
        _ (log/info "buff-mod" buff-mod)
        choices (map (fn [item]
                  (let [choice {:hotkey (get item :hotkey)
                                :name (get item :name)
                                :choice/one-of [(rce/mod-dec-inventory (get item :hotkey))]
                                :items #{item}}]
                    (if-let [valid-recipes (not-empty (rcrafting/valid-recipes (conj (get recipe :items) item)
                                                                    (rcrafting/get-recipes-by-category :weapon)))]
                      (assoc choice :choice/events [{
                        :description "You feel like the design is starting to take shape. Commit to a design?"
                        :event/type :event-type/material
                        :event/choices 
                          (concat
                            (map (fn [valid-recipe hotkey]
                                   (log/info "recipe choice" valid-recipe)
                                   (log/info "recipe items" (conj (get recipe :items #{}) item))
                                   (let [choice-name (rcrafting/recipe-name valid-recipe false)
                                         dominate-item (rcrafting/dominate-item
                                                         (filter #(rcrafting/item-satisfies-any-clause? %1 valid-recipe)
                                                                 (conj (get recipe :items #{}) item)))
                                         recipe-name (rcrafting/recipe-name (-> valid-recipe
                                                                              (merge recipe)
                                                                              (assoc :recipe/dominate-item dominate-item)
                                                                              (update :items (conj item)))
                                                                             false)]
                                     (log/info "dominate item" dominate-item)
                                     {:hotkey hotkey
                                      :name choice-name
                                      :recipe/name recipe-name
                                      :recipe/types (get valid-recipe :recipe/types)
                                      :recipe/dominate-item dominate-item
                                      :choice/events [{
                                        :description (str "You wrap up the " recipe-name ".")}]}))
                                   valid-recipes
                                   [\a \b \c \d \e \f \g])
                            (if (< (->> recipe
                                     :events
                                     (map :event/type)
                                     (filter (partial = :event-type/material))
                                     count) 6)
                              [{:name "keep working"
                                :hotkey :space}]
                              []))}])
                    choice)))
                (rcrafting/inventory-crafting-components state))
        events (if (empty? choices)
                 [{:description "You seem to have run out of items. Try again with more different items next time."
                   :event/choices [
                     {:name "no item - skip"
                      :hotkey :space
                      :done true}]}]
                 [{:description "Use an item from your inventory as a crafting component."
                   :event/choices
                    (concat
                      choices
                      [{:name "no item - skip"
                        :hotkey :space}])}])]
        events       (gen-event state recipe :event-type/material events)))

(defn enhancements
  [state recipe]
  (let [debuff-mod (first (shuffle [
                     (rce/mod-accuracy -5 -1)
                     (rce/mod-damage -6 -2)
                     (rce/mod-durability -6 -2)]))]
    (if-let [item (rand-unused-item recipe "event.enhancement.item")]
      [
        {:title "Increased Accuracy"
         :description [(format "The %s you %s was %s. This weapon is going to be a lot more accurate."
                         (:name item)
                         (rand-nth ["found" "used"])
                         (rand-nth ["great" "perfect" "amazing" "awesome"]))]
         :event/choices [{
           :hotkey :space
           :name "continue"
           :event.complication.item/id (get item :item/id)
           :choice/one-of [(rce/mod-accuracy 1 2)]}]}

          {:title "Increased Damage"
           :description [(format "The %s you %s was %s. This weapon is going to do a lot more damage."
                           (:name item)
                           (rand-nth ["found" "used"])
                           (rand-nth ["great" "perfect" "amazing" "awesome"]))]
           :event/choices [{
             :hotkey :space
             :name "continue"
             :event.complication.item/id (get item :item/id)
             :choice/one-of [(rce/mod-damage 1 2)]}]}

        ; wooden - find replacement
        {:description "In a flash of insight, you decide you can cannibalize another item for parts."
         :requirements #{:choice.id/wooden-find-replacement
                         :choice.id/edged-find-replacement}
         :event/choices
           (let [wooden-inventory (take 2 (shuffle (filter rcrafting/wooden (ri/player-inventory state))))
                 choices (map (fn [item]
                                {:hotkey (get item :hotkey)
                                 :name (get item :name)
                                 :choice/one-of [(rce/mod-dec-inventory (get item :hotkey))]
                                 :choice/events [{
                                   :description "Select a buff to bring along."
                                   :event/choices (let [choices (map (fn [buff hotkey]
                                                                       {:hotkey hotkey
                                                                        :name (rcmp/full-name buff)
                                                                        :choice/one-of [buff]})
                                                                   (get item :effects))]
                                                    (concat choices [{:name "no buffs - skip"
                                                                      :hotkey :space}]))}]})
                              wooden-inventory)]
             (if (empty? choices)
               [{:name "no items - skip"
                 :hotkey :space
                 :effects [debuff-mod]}]
               choices))}

        ; wooden - find-replacement 
        {:description "You can turn this into a fire hardened weapon if you find a fire."
         :requirements #{:choice.id/wooden-find-replacement}
         :event/choices [
             {:name "Use fire"
              :hotkey :enter
              :requirements {:adjacent-to-fire true}
              :effects [(rce/mod-firehardened)]}
             {:name "skip"
              :hotkey :space
              :effects [debuff-mod]}]}

        ; wooden - change design
        {:description "You mull the design in your mind."
         :requirements #{:choice.id/wooden-change-design}
         :event/choices [
             {:name "continue"
              :hotkey :space
              :choice/events [{
                :description "Yes, you can change this to include a piercing component."
                :event/choices [{
                  :name "continue"
                  :hotkey :space
                  :effects [(rce/mod-piercing 0.1 0.3)]}]}]}]}

        ; dimensional - find-replacement 
        #_{:description "You can use parts from another item to fix this."
         :requirements #{:choice.id/dimensional-find-replacement}
         :event/choices
           (let [inventory (take 2 (shuffle (ri/player-inventory state)))
                 choices (map (fn [item]
                                {:hotkey (get item :hotkey)
                                 :name (get item :name)
                                 :choice/one-of [(rce/mod-dec-inventory (get item :hotkey))]
                                 :choice/events [{
                                   :description "Select an effect to copy"
                                   :event/choices (let [choices (map (fn [effect hotkey]
                                                                       {:hotkey hotkey
                                                                        :name (rcmp/full-name effect)
                                                                        :choice/one-of [effect]})
                                                                   (get item :effects))]
                                                    (concat choices [{:name "no buffs - skip"
                                                                      :hotkey :space}]))}]})
                              inventory)]
             (if (empty? choices)
               [{:name "no applicable items - skip"
                 :hotkey :space
                 :effects [debuff-mod]}]
               choices))}

        ; dimensional - change design
        {:description "You mull the design in your mind. Examining shapes and interconnections."
         :requirements #{:choice.id/dimensional-change-design}
         :event/choices [
             {:name "continue"
              :hotkey :space
              :choice/events [{
                :description "Yes, you can change this to make this weapon much more durable."
                :event/choices [{
                  :name "continue"
                  :hotkey :space
                  :effects [(rce/mod-durability 5 10)]}]}]}]}

        ; edged - find-replacement - cannibalize
        #_{:description "You can use parts from another edged item to fix this."
         :requirements #{:choice.id/edged-find-replacement}
         :event/choices
           (let [inventory (take 2 (shuffle (filter rcrafting/edged (ri/player-inventory state))))
                 choices (map (fn [item]
                                {:hotkey (get item :hotkey)
                                 :name (get item :name)
                                 :choice/one-of [(rce/mod-dec-inventory (get item :hotkey))]
                                 :choice/events [{
                                   :description "Select an effect to copy"
                                   :event/choices (let [choices (map (fn [effect hotkey]
                                                                       {:hotkey hotkey
                                                                        :name (rcmp/full-name effect)
                                                                        :choice/one-of [effect]})
                                                                   (get item :effects))]
                                                    (concat choices [{:name "no buffs - skip"
                                                                      :hotkey :space}]))}]})
                              inventory)]
             (if (empty? choices)
               [{:name "no applicable items - skip"
                 :hotkey :space
                 :effects [debuff-mod]}]
               choices))}

        ; edged - change design - dismembering
        {:description "You think about how to make the edge sharper."
         :requirements #{:choice.id/edged-change-design}
         :event/choices [
             {:name "continue"
              :hotkey :space
              :choice/events [{
                :description "In a flash of insight you understand how to make the edge so sharp it can dismember limbs."
                :event/choices [{
                  :name "continue"
                  :hotkey :space
                  :effects [(rce/mod-dismembering 0.1 0.5)]}]}]}]}

        ; flexible - find-replacement - cannibalize
        #_{:description "You can use parts from another flexible item to fix this."
         :requirements #{:choice.id/flexible-find-replacement}
         :event/choices
           (let [inventory (take 2 (shuffle (filter rcrafting/flexible (ri/player-inventory state))))
                 choices (map (fn [item]
                                {:hotkey (get item :hotkey)
                                 :name (get item :name)
                                 :choice/one-of [(rce/mod-dec-inventory (get item :hotkey))]
                                 :choice/events [{
                                   :description "Select an effect to copy"
                                   :event/choices (let [choices (map (fn [effect hotkey]
                                                                       {:hotkey hotkey
                                                                        :name (rcmp/full-name effect)
                                                                        :choice/one-of [effect]})
                                                                   (get item :effects))]
                                                    (concat choices [{:name "no buffs - skip"
                                                                      :hotkey :space}]))}]})
                              inventory)]
             (if (empty? choices)
               [{:name "no applicable items - skip"
                 :hotkey :space
                 :effects [debuff-mod]}]
               choices))}

        ; flexible - change design - stunning
        {:description "You think about how to make the flex flexier"
         :requirements #{:choice.id/flexible-change-design}
         :event/choices [
             {:name "continue"
              :hotkey :space
              :choice/events [{
                :description "In a flash of insight you understand how to make it so flexible it can stun opponents."
                :event/choices [{
                  :name "continue"
                  :hotkey :space
                  :effects [(rce/mod-stunning 0.1 0.5)]}]}]}]}

        ; handled - find-replacement - cannibalize
        #_{:description "You can use parts from another handled item to fix this."
         :requirements #{:choice.id/handled-find-replacement}
         :event/choices
           (let [inventory (take 2 (shuffle (filter rcrafting/handled? (ri/player-inventory state))))
                 choices (map (fn [item]
                                {:hotkey (get item :hotkey)
                                 :name (get item :name)
                                 :choice/one-of [(rce/mod-dec-inventory (get item :hotkey))]
                                 :choice/events [{
                                   :description "Select an effect to copy"
                                   :event/choices (let [choices (map (fn [effect hotkey]
                                                                       {:hotkey hotkey
                                                                        :name (rcmp/full-name effect)
                                                                        :choice/one-of [effect]})
                                                                   (get item :effects))]
                                                    (concat choices [{:name "no buffs - skip"
                                                                      :hotkey :space}]))}]})
                              inventory)]
             (if (empty? choices)
               [{:name "no applicable items - skip"
                 :hotkey :space
                 :effects [debuff-mod]}]
               choices))}

        ; handled - change design - knockback
        {:description "You think about how to improve the handle."
         :requirements #{:choice.id/handled-change-design}
         :event/choices [
             {:name "continue"
              :hotkey :space
              :choice/events [{
                :description "In a flash of insight you understand how to make the handle easier to grip."
                :event/choices [{
                  :name "continue"
                  :hotkey :space
                  :effects [(rce/mod-knockback 0.1 0.5)]}]}]}]}

        ; mental - perseverence
        {:description "You think about how to much you've struggeled so far and how far you have come."
         :requirements #{:choice.id/handled-change-design}
         :event/choices [
             {:name "continue"
              :hotkey :space
              :choice/events [{
                :description "You start to feel better about your chances of survival."
                :event/choices [{
                  :name "continue"
                  :hotkey :space
                  :effects [(rce/mod-wtl 1 5)]}]}]}]}]
       [])))

(defn gen-enhancement [state recipe]
  (gen-event state recipe
    :event-type/enhancement
    (enhancements state recipe)))

(defn gen-player [state recipe]
  (let [player-events [
    ; Mental complications
    {:event/id :mental
     :title "Emotional Challenge"
     :description ["You tried to make the item work, but in the end it was too frustrating."
                   "As you conintue crafting you start feeling down."
                   "You're feeling depressed. None of this is working out right."]
     :requirements #{}
     :event/choices [{
       :hotkey :space
       :name "continue"
       :choice/one-of [
         (rce/mod-wtl -10 -5)]
       :choice/events [{
         :description "You try to find a way to deal with these feelings."
         :event/choices [{
           :choice/id :choice.id/mental-persevere
           :hotkey \a
           :name "persevere"
          }{
           :choice/id :choice.id/mental-optimism
           :hotkey \b
           :name "be optimistic"
          }{
           :choice/id :choice.id/mental-self-control
           :hotkey \c
           :name "practice self control"}]}]}]}

    ; Mental - persevere - complication
    {:event/id :event.id/mental-perseverecomplication
     :description ["You tried to persevere, but you're starting to fray at the edges. You feel burntout."]
     :requirements #{:choice.id/mental-persevere}
     :event/choices [{
       :hotkey :space
       :name "continue"
       :choice/one-of [
         (rce/mod-wtl -10 -5)
      ]}]}

    ; Mental - optimism - complication
    {:event/id :event.id/mental-optimism-complication
     :description ["You tried to be optimistic, but setback after setback leaves you feeling depressed."]
     :requirements #{:choice.id/mental-persevere}
     :event/choices [{
       :hotkey :space
       :name "continue"
       :choice/one-of [
         (rce/mod-wtl -10 -5)
      ]}]}

    ; Mental - self-control - complication
    {:event/id :event.id/mental-self-control-complication
     :description ["You tried to keep it together, but you're starting to panic. You feel like the world is closing in."]
     :requirements #{:choice.id/mental-self-control}
     :event/choices [{
       :hotkey :space
       :name "continue"
       :choice/one-of [
         (rce/mod-thirst -10 -5)
      ]}]}

    ; Mental - persevere - complication
    {:event/id :event.id/mental-perseverecomplication
     :description ["You tried to persevere, but you're starting to fray at the edges. You feel burntout."]
     :requirements #{:choice.id/mental-persevere}
     :event/choices [{
       :hotkey :space
       :name "continue"
       :choice/one-of [
         (rce/mod-wtl -10 -5)
      ]}]}]]
  (gen-event state recipe
    :event-type/player
    player-events)))

(defn gen-random [state recipe]
  ; first event? Do start event
  (if (empty? (get recipe :events))
    {
      :description (str "You begin crafting a " (name (get recipe :type)) ". You'll need to start with an item.")
      :event/choices [{
        :hotkey :space
        :name "continue"
        :choice/events [:gen-material]}]}
    ((rand-nth [gen-complication
                gen-material
                gen-remedy
                gen-enhancement
                gen-player])
      state recipe)))
