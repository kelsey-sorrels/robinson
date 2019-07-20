(ns robinson.crafting.weapon-gen
  (:require [robinson.random :as rr]
            [robinson.world :as rw]
            [robinson.crafting :as rcrafting]
            [robinson.crafting.mod :as rcmod]
            [robinson.crafting.mod-protocol :as rcmp]
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

(def contact-choices
  {:blunt
     {:name "Blunt"
      :hotkey \a
      :types (with-meta #{:blunt} {:merge 'replace-contact})}
   :edged
     {:name "Edged"
      :hotkey \b
      :types (with-meta #{:edged} {:merge 'replace-contact})}
   :piercing
     {:name "Piercing"
      :hotkey \c
      :types (with-meta #{:piercing} {:merge 'replace-contact})}
   :flexible
     {:name "Flexible"
      :hotkey \d
      :types (with-meta #{:flexible} {:merge 'replace-contact})}})

(def wield-choices
  {:melee
    {:name "Melee"
     :hotkey \a
     :types (with-meta #{:melee} {:merge 'replace-wield})}
   :thrown
     {:name "Thrown"
      :hotkey \b
      :types (with-meta #{:thrown} {:merge 'replace-wield})}
   :ranged
     {:name "Ranged"
      :hotkey \c
      :types (with-meta #{:ranged} {:merge 'replace-wield})}})

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
    :event/id :contact-type
    :gen #{:contact-type}
    :title "Choose weapon type"
    :event/choices (vals contact-choices)}))

(defn gen-question-wield [state]
  (rcrafting/assoc-current-recipe state :current-stage {
    :event/id :wield-type
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

;; Weapon mods
(defn mod-accuracy
  [low high]
  (let [n (rr/uniform-int low high)]
    (rcmod/adj-item-on-create "accuracy" "acc" :accuracy n)))

(defn mod-damage
  [low high]
  (let [n (rr/uniform-int low high)]
    (rcmod/adj-item-on-create "damage" "dmg" :damage n)))

(defn mod-durability
  [low high]
  (let [n (rr/uniform-int low high)]
    (rcmod/adj-item-on-create "durability" "dbl" :durability n)))

; Reduces opponent toughness
(defn mod-piercing
  [low high]
  (let [n (rr/uniform-int low high)]
    (rcmod/adj-defender-on-attack-temp "piercing" "prc" :toughness n)))

; Increase attackers speed
(defn mod-haste
  [low high]
  (let [n (rr/uniform-int low high)]
    (rcmod/adj-attacker-on-attack-temp "haste" :speed n)))

; chance of scaring monster
(defn mod-scare-monster
  [low high]
  (let [n (rr/uniform-double low high)]
    (rcmod/tag-defender-on-attack "scare" "scr" :scared n)))

; chance of stunning monster
(defn mod-stunning
  [low high]
  (let [n (rr/uniform-double low high)]
    (rcmod/tag-defender-on-attack "stun" "stn" :stunned n)))

; chance of dismembering monster
(defn mod-dismembering
  [low high]
  (let [n (rr/uniform-double low high)]
    (rcmod/tag-defender-on-attack "dismember" "dsm" :dismembered n)))

; chance of wounding monster
(defn mod-bleeding
  [low high]
  (let [n (rr/uniform-double low high)]
    (rcmod/tag-defender-on-attack "bleeding" "bld" :bleeding n)))

; chance of knockback
(defn mod-knockback
  [low high]
  (let [n (rr/uniform-double low high)]
    (rcmod/tag-defender-on-attack "knockback" "knb" :knockbacked n)))

; condition mod based on opponent attributes
#_(defn mod-conditioned-heirarchy
  [id mod]
    (->ModWeapon "conditioned" :conditioned {:id id :mod mod}))

;; Creation mods
(defn mod-hp [low high]
  (let [n (rr/uniform-int low high)]
    (rcmod/adj-player-on-create "health" "hp" :hp n)))

(defn mod-hunger [low high]
  (let [n (rr/uniform-int low high)]
    (rcmod/adj-player-on-create "hunger" "hun" :hunger n)))

(defn mod-thirst [low high]
  (let [n (rr/uniform-int low high)]
    (rcmod/adj-player-on-create "thirst" "thr" :thirst n)))

(defn mod-wtl [low high]
  (let [n (rr/uniform-int low high)]
    (rcmod/adj-player-on-create "wtl" "wtl" :wtl n)))

(defn mod-wound [dmg-low dmg-high time-low time-high]
  (let [dmg (rr/uniform-int dmg-low dmg-high)
        t (rr/uniform-int time-low time-high)]
    (rcmod/conj-player-immediate "wound" "wnd" :wounds {:dmg dmg :time t})))

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
  [
    ; Mental complications
    #_{:event/id :mental
     :description ["You catch yourself rushing again."
                   "What was that? Oh right, making weapons. You catch yourself getting distracted."
                   "Sidetracked!"]
     :event/choices [{
       :hotkey :space
       :name "continue"
       :choice/one-of [
         `(mod-hunger 10 20)
         `(mod-accuracy -5 -1)
         `(mod-damage -6 -2)]}]}

    ; Material complications
    #_{:event/id :material
     :description ["You can't find the materials you need."
                   "The materials you are using are too brittle."
                   "The materials you are using are too soft."]
     :event/choices [
       {:hotkey \a
        :name "Search for something better"
        :choice/events [
          {:description "You give up after spending hours looking for the right materials."
            :choice/one-of [`(mod-thirst -5 -10)
                     `(mod-hunger -5 -10)
                     `(mod-wtl -5 -10)]}
          {:description ["You spend hours looking for the right material."
                         "After hours of searching, you find the right material."]
           :choice/one-of [`(mod-wtl 5 7)
                    `(mod-thirst -5 -10)
                    `(mod-hunger -5 -10)]}]}
       {:hotkey \b
        :name "Give up"
        :choice/events [
          {:description "You learn to make do without the right parts."
           :choice/one-of [
             `(mod-durability -3 -1)
             `(mod-wtl -5 -10)]}
          {:description "You stop and take a break. After a few minutes inspiration strikes."
           :choice/one-of [
             `(mod-durability 1 3)
             `(mod-wtl 1 5)]}]}]}

    ; Bodily Limit
    #_{:event/id :body-constraint
     :description "You are overexerted. Slow down survivor. Take your time."
     :event/choices [{
       :hotkey :space
       :name "continue"
       :choice/one-of [
         `(mod-thirst 11 21)]}]}

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
         `(mod-damage -7 -3)]}]}

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
         `(mod-thirst 11 21)]}]}

    ; Handled weapon complications
    #_{:event/id :handle
     :description "You can hardly hold onto this. You need to find something easier to hold."
     :requirements #{:handle}
     :event/choices [{
       :hotkey :space
       :name "continue"
       :choice/one-of [
         `(mod-thirst 11 21)]}]}

    ; Flexible weapon complications
    #_{:event/id :flexibe
     :description "There should be a little more give to this. You know it's going to break."
     :requirements #{:flexible}
     :event/choices [{
       :hotkey :space
       :name "continue"
       :choice/one-of [
         `(mod-thirst 11 21)]}]}

    ; Wooden weapon complications
    {:event/id :wooden
     :description ["This wood is really bent. It's just not going to work."
                   "This wood is green. It should be more seasoned."
                   "This wood is full of holes caused by unending seasons of wind and rain."
                   "Cracked wood"
                   "Crack forming"
                   "Pin knots"
                   "Weatherworn wood"]
     :requirements #{:wooden}
     :event/choices [{
       :choice/id :wooden-find-replacement
       :hotkey \a
       :name "find replacement"
       :choice/one-of [
         `(mod-hunger 11 21)
         `(mod-thirst 11 21)]
      }{
       :choice/id :wooden-change-design
       :hotkey \b
       :name "change design"
       :choice/one-of [
         `(mod-durability -3 -1)
         `(mod-damage -3 -1)
         `(mod-accuracy -3 -1)]
      }{
       :choice/id :wooden-use-anyway
       :hotkey \c
       :name "use anyway"
       :choice/one-of [
         `(mod-durability -5 -1)
         `(mod-damage -5 -1)
         `(mod-accuracy -5 -1)]}]}

    ; Change Type
    {:event/id :change-type
     :description ["The design change took a wrong turn. You find yourself making something completely different."]
     :requirements #{:wooden-change-design
                     :edged-change-design}
     ; TODO choose between types or get assigned a type?
     :event/choices (let [types (get recipe :types)]
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
         `(mod-thirst 11 21)]}]}

    ; Special Boomerange Complications
    #_{:event/id :boomerang
     :description "Not bent enough!"
     :requirements #{:boomerang}
     :event/choices [{
       :hotkey :space
       :name "continue"
       :choice/one-of [
         `(mod-thirst 11 21)]}]}

    ; Splinter
    #_{:event/id :splinter
     :description ["Ouch! What was that? It's a splinter in your hand."
                   "Oof! A splinter wedged its way into your finger."]
     :requirements #{:wooden}
     :event/choices [{
       :hotkey :space
       :name "continue"
       :choice/one-of [
         `(mod-thirst 11 21)]}]}

    ; Wooden joints
    #_{:event/id :weak-joinery
     :description "Weak joinery!"
     :requirements #{:wooden}
     :event/choices [{
       :hotkey :space
       :name "continue"
       :choice/one-of [
         `(mod-thirst 11 21)]}]}

    ; Poisonous material
    #_{:event/id :allergic
     :description ["Poisonous material!"
                   "Allergic to material!"]
     :requirements #{:wooden}
     :event/choices [{
       :hotkey :space
       :name "continue"
       :choice/one-of [
         `(mod-thirst 11 21)]}]}

    ; Hunger
    #_{:event/id :hunger
     :description "Distracted by hunger!"
     :event/choices [{
       :hotkey :space
       :name "continue"
       :choice/one-of [
         `(mod-thirst 11 21)]}]}

    ; Thirst
    #_{:event/id :thirst
     :description "Distracted by thirst!"
     :event/choices [{
       :hotkey :space
       :name "continue"
       :choice/one-of [
         `(mod-thirst 11 21)]}]}

    ; Low light
    #_{:event/id :low-light
     :description "Low light!"
     :event/choices [{
       :hotkey :space
       :name "continue"
       :choice/one-of [
         `(mod-thirst 11 21)]}]}

    ; Sound
    #_{:event/id :sound
     :description ["Interrupted by sound!"
                   "Loud work environment!"]
     :event/choices [{
       :hotkey :space
       :name "continue"
       :choice/one-of [
         `(mod-thirst 11 21)]}]}

    ; Empty prototype -----------------
    #_{:title ""
     :description ""
     :event/choices [{
       :hotkey :space
       :name "continue"
       :choice/one-of [
         `(mod-thirst 11 21)]}]}

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
                 :choice/one-of [`(mod-hp 5 7)]}]}]}
           {:description "Before you can hit it the spider sinks its fangs into your leg."
            :choice/one-of [`(mod-hp 5 7)]}
           {:description "You slap the spider. There is not enough meat to eat."}]}
       {:hotkey \b
        :name "brush it off"
        :choice/events [
          {:description "You brush the spider off and it disappears into the ground."}
          {:description "As you brush off the spider thousands of spider babies pour onto your leg"
           :event/choices [
             {:hotkey \a
              :name "just give up and die"
              :choice/one-of [`(mod-hp 10 50)]}
             {:hotkey \b
              :name "slap all of them"
              :choice/one-of [`(mod-hp 5 7)]}
             {:hotkey \c
              :name "roll on the ground"}]}]}]}])

 
(defn eval-effect [effect]
  (apply (resolve (first effect)) (rest effect)))

(defn gen-event [state recipe events]
  (log/info (dissoc recipe :graph :img))
  (log/info (get recipe :past-events #{}))
  (let [blueprint (->> events
                    (remove (fn [event]
                      (when-let [event-id (get event :event/id)]
                        (contains? (get recipe :past-events #{}) event-id))))
                    rand-nth)
        next-stage (-> blueprint
                      (assoc :description (let [description (:description blueprint)]
                                            (if (sequential? description)
                                              (rand-nth description)
                                              description)))
                      (update 
                        :event/choices (partial map (fn assoc-choice-effects [choice]
                                              (if-let [effect (rand-nth (get choice :choice/one-of))]
                                                (do (log/info "gen-event choice" choice "effect" effect)
                                                (assoc choice
                                                   :effects [(eval-effect effect)]))
                                                choice)))))]
    (log/info "gen-event next-stage" next-stage)
    (rcrafting/assoc-current-recipe state
       :current-stage next-stage
       :past-events (conj (get recipe :past-events #{}) (get next-stage :event/id)))))

;;  :event/id
;;  :choice/id
;;  :recipe/id
;;  :recipe/categoy
;;  :recipe/types
;;  :recipe/flags
;;  :recipe/components
(defn gen-complication [state {:recipe/keys [category
                                             types
                                             flags
                                             components
                                             example-item-flags]
                              recipe-id :recipe/id
                              event-id :event/id
                              choice-id :choice/id
                              :as recipe}]
  (log/info recipe-id event-id choice-id category types components example-item-flags)
  (log/info (dissoc recipe :graph :img))
  (let [recipe-pool (clojure.set/union (when recipe-id #{recipe-id})
                                       (when event-id #{event-id})
                                       (when choice-id #{choice-id})
                                       (when category #{category})
                                       types
                                       components
                                       example-item-flags)]
    (log/info "recipe-pool" recipe-pool)
    (gen-event state recipe
      (filter
        (fn [complication]
          (let [requirements (get complication :requirements #{})]
            (or (some? (clojure.set/intersection recipe-pool requirements))
                (empty? requirements))))
        (weapon-complications recipe)))))

(def remedies [{
    :title "Remedies"
    :event/choices [
      {:name "Slow down"
       :hotkey \a
       :remedies [:slow-down]}
      {:name "Push through"
       :hotkey \b
       :remedies [:push-through]}]}])

(defn gen-remedy [state recipe]
  (gen-event state recipe remedies))

(defn gen-material [state recipe]
  (log/info (get recipe :types))
  (let [example-items (rcrafting/get-example-items-by-types (get recipe :types))
        _ (log/info "example-items" (vec example-items))
        items (take 2 (shuffle example-items))
        item-id (or (first items) :stick)
        item-name (or (rig/id->name item-id) "unknown1")
        _ (log/info "items:" items "item-id:" item-id "item-name:" item-name)
        amount (+ 1 (rand-int 4))
        buff-mod (eval-effect (first (shuffle [
                     `(mod-accuracy 1 5)
                     `(mod-damage 1 4)
                     `(mod-durability 1 5)
                     `(mod-stunning 0.1 0.5)
                     `(mod-dismembering 0.1 0.5)
                     `(mod-bleeding 0.1 0.5)
                     `(mod-knockback 0.1 0.5)])))
        debuff-mod (eval-effect (first (shuffle [
                   `(mod-accuracy -5 -1)
                   `(mod-damage -6 -2)
                   `(mod-durability -6 -2)])))
        _ (log/info "buff-mod" buff-mod)
        events [
        {:title "Material requirements"
         :description (rr/rand-nth [
           (format "You need %d %s to improve this recipe." amount item-name)])
         :event/choices [
             {:name item-name
              :hotkey :enter
              :material {:id item-id :amount amount}
              :effects [buff-mod]}
             {:name "skip"
              :hotkey :space
              :effects [debuff-mod]}]}]]
      (gen-event state recipe events)))

(def enhancements [
  {:title "Increased Accuracy"
   :description "A truly fortuitous event!. You find a way to make your weapon more accurate."
   :event/choices [{
     :hotkey :space
     :name "continue"
     :choice/one-of [`(mod-accuracy 1 2)]}]}
  {:title "Increased Damage"
   :description "With great care, you make the weapon much more powerful"
   :event/choices [{
     :hotkey :space
     :name "continue"
     :choice/one-of [`(mod-damage 1 2)]}]}])

(defn gen-enhancement [state recipe]
  (gen-event state recipe enhancements))
