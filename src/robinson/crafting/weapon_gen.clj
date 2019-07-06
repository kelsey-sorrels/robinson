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

(defn short-name [name]
  (get {"damage" "dmg"
        "accuracy" "acc"
        "speed" "spd"
        "durability" "drb"
        "hunger" "hng"
        "thirst" "thr"}
       name name))

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
    :gen #{:contact-type}
    :title "Choose weapon type"
    :choices [
      {:name "Melee"
       :hotkey \a
       :types #{:melee}}
      {:name "Thrown"
       :hotkey \b
       :types #{:thrown}}
      {:name "Ranged"
       :hotkey \c
       :types #{:ranged}}]}))

(defn gen-question-wield [state]
  (rcrafting/assoc-current-recipe state :current-stage {
    :gen #{:wield-type}
    :title "Choose weapon type"
    :choices [
      {:name "Blunt"
       :hotkey \a
       :types #{:blunt}}
      {:name "Edged"
       :hotkey \b
       :types #{:edged}}
      {:name "Piercing"
       :hotkey \c
       :types #{:piercing}}
      {:name "Flexible"
       :hotkey \d
       :types #{:flexible}}]}))

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

;; :requirements is a set of any
;;  :recipe/flags
;;  :recipe/types
;;  :recipe/id
;;  :recipe/components
;;  Any matches between the recipe and the complication will mark the complication
;;  for inclusion. Any complication with empty or nil :requirements will always
;;  be marked for inclusion.
(def weapon-complications [
  {:title "Rushing!"
   :description "You've been rushing again."
   :choices [{
     :hotkey :space
     :name "continue"
     :one-of [
       `(mod-accuracy -5 -1)
       `(mod-damage -6 -2)]}]}
  {:title "Poor materials!"
   :description "You can't find the materials you need."
   :choices [
     {:hotkey \a
      :name "Keep searching"
      :one-of [`(mod-thirst -5 -10)
               `(mod-hunger -5 -10)]
      :events [
        {:description "You spend hours looking for the right materials."
          :one-of [`(mod-wtl -5 -10)
                   `(mod-hunger -5 -10)]}
        {:description "After hours of searching, you find the right material."
         :one-of [`(mod-wtl 5 7)]}]}
     {:hotkey \b
      :name "Do without"
      :events [
        {:description "You learn to make do without the right parts."
         :one-of [
           `(mod-durability -3 -1)]}
        {:description "There is no way around it. It is just too hard. "
         :one-of [
           `(mod-durability -7 -5)]}]}]}
  {:title "Distracted!"
   :description "What was that? Oh right, making weapons."
   :choices [{
     :hotkey :space
     :name "continue"
     :one-of [
       `(mod-hunger 10 20)]}]}
  {:title "Overexertion!"
   :description "Slow down survivor. Take your time."
   :choices [{
     :hotkey :space
     :name "continue"
     :one-of [
       `(mod-thirst 11 21)]}]}
  {:title "Brittle materials!"
   :description "The materials you are using are too brittle."
   :choices [
     {:hotkey \a
      :name "Search for something better"
      :events [
        {:description "You give up after spending hours looking for the right materials."
          :one-of [`(mod-thirst -5 -10)
                   `(mod-hunger -5 -10)
                   `(mod-wtl -5 -10)]}
        {:description "After hours of searching, you find the right material."
         :one-of [`(mod-wtl 5 7)
                  `(mod-thirst -5 -10)
                  `(mod-hunger -5 -10)]}]}
     {:hotkey \b
      :name "Give up"
      :events [
        {:description "You learn to make do without the right parts."
         :one-of [
           `(mod-durability -3 -1)
           `(mod-wtl -5 -10)]}
        {:description "You stop and take a break. After a few minutes inspiration strikes."
         :one-of [
           `(mod-durability 1 3)
           `(mod-wtl 1 5)]}]}]}
  {:title "Soft materials!"
   :description "The materials you are using are too soft."
   :choices [{
     :hotkey :space
     :name "continue"
     :one-of [
       `(mod-durability -7 -3)]}]}
  {:title "Warped edge!"
   :description "The edge keeps warping when it should be straight."
   :requirements #{:edged}
   :choices [{
     :hotkey :space
     :name "continue"
     :one-of [
       `(mod-damage -7 -3)]}]}
  {:title "Chipped edge!"
   :description "The edge should be smooth, not full of chips."
   :requirements #{:edged}
   :choices [{
     :hotkey :space
     :name "continue"
     :one-of [
       `(mod-damage -7 -3)]}]}
  {:title "Rolled edge!"
   :description "The edge you were woking with has rolled causing your to start over."
   :requirements #{:edged :blade}
   :choices [{
     :hotkey :space
     :name "continue"
     :one-of [
       `(mod-damage 11 21)]}]}
  {:title "Too heavy!"
   :description "The balance is all off on this. It really should be lighter."
   :requirements #{:low-weight :high-weight}
   :choices [{
     :hotkey :space
     :name "continue"
     :one-of [
       `(mod-thirst 11 21)]}]}
  {:title "Too light!"
   :description "You need something heavier to work with. This is too light."
   :requirements #{:low-weight :high-weight}
   :choices [{
     :hotkey :space
     :name "continue"
     :one-of [
       `(mod-thirst 11 21)]}]}
  {:title "Hard to grasp!"
   :description "You can hardly hold onto this. You need to find something easier to hold."
   :requirements #{:handle}
   :choices [{
     :hotkey :space
     :name "continue"
     :one-of [
       `(mod-thirst 11 21)]}]}
  {:title "Too stiff!"
   :description "There should be a little more give to this. You know it's going to break."
   :requirements #{:flexible}
   :choices [{
     :hotkey :space
     :name "continue"
     :one-of [
       `(mod-thirst 11 21)]}]}
  {:title "Bent wood!"
   :description "This wood is really bent. It's just not going to work."
   :requirements #{:wooden}
   :choices [{
     :hotkey :space
     :name "continue"
     :one-of [
       `(mod-thirst 11 21)]}]}
  {:title "Green wood!"
   :description "This wood is green. It should be more seasoned."
   :requirements #{:wooden}
   :choices [{
     :hotkey :space
     :name "continue"
     :one-of [
       `(mod-thirst 11 21)]}]}
  {:title "Rotted wood!"
   :description "This wood is full of holes caused by unending seasons of wind and rain."
   :requirements #{:wooden}
   :choices [{
     :hotkey :space
     :name "continue"
     :one-of [
       `(mod-thirst 11 21)]}]}
  {:title "Too long!"
   :description ""
   :choices [{
     :hotkey :space
     :name "continue"
     :one-of [
       `(mod-thirst 11 21)]}]}
  {:title "Too short!"
   :description ""
   :choices [{
     :hotkey :space
     :name "continue"
     :one-of [
       `(mod-thirst 11 21)]}]}
  #_{:title "Over tempered!"
   :description ""
   :choices [{
     :hotkey :space
     :name "continue"
     :one-of [
       `(mod-thirst 11 21)]}]}
  #_{:title "Under tempered!"
   :description ""
   :choices [{
     :hotkey :space
     :name "continue"
     :one-of [
       `(mod-thirst 11 21)]}]}
  {:title "Handle burns!"
   :description ""
   :requirements #{:handle :wooden}
   :choices [{
     :hotkey :space
     :name "continue"
     :one-of [
       `(mod-thirst 11 21)]}]}
  {:title "Handle uneven!"
   :description ""
   :requirements #{:handle :wooden}
   :choices [{
     :hotkey :space
     :name "continue"
     :one-of [
       `(mod-thirst 11 21)]}]}
  {:title "Handle popping out!"
   :description ""
   :requirements #{:handle}
   :choices [{
     :hotkey :space
     :name "continue"
     :one-of [
       `(mod-thirst 11 21)]}]}
  {:title "Not bent enough!"
   :description ""
   :requirements #{:boomerang}
   :choices [{
     :hotkey :space
     :name "continue"
     :one-of [
       `(mod-thirst 11 21)]}]}
  {:title "Cracked wood!"
   :description ""
   :requirements #{:wooden}
   :choices [{
     :hotkey :space
     :name "continue"
     :one-of [
       `(mod-thirst 11 21)]}]}
  {:title "Cracked forming!"
   :description ""
   :requirements #{:wooden}
   :choices [{
     :hotkey :space
     :name "continue"
     :one-of [
       `(mod-thirst 11 21)]}]}
  {:title "Pin knots!"
   :description ""
   :requirements #{:wooden}
   :choices [{
     :hotkey :space
     :name "continue"
     :one-of [
       `(mod-thirst 11 21)]}]}
  {:title "Splinters!"
   :description ""
   :requirements #{:wooden}
   :choices [{
     :hotkey :space
     :name "continue"
     :one-of [
       `(mod-thirst 11 21)]}]}
  {:title "Weather worn wood!"
   :description ""
   :requirements #{:wooden}
   :choices [{
     :hotkey :space
     :name "continue"
     :one-of [
       `(mod-thirst 11 21)]}]}
  {:title "Not tapered enough!"
   :description ""
   :choices [{
     :hotkey :space
     :name "continue"
     :one-of [
       `(mod-thirst 11 21)]}]}
  {:title "Knotted!"
   :description ""
   :requirements #{:wooden}
   :choices [{
     :hotkey :space
     :name "continue"
     :one-of [
       `(mod-thirst 11 21)]}]}
  {:title "Weak joinery!"
   :description ""
   :requirements #{:wooden}
   :choices [{
     :hotkey :space
     :name "continue"
     :one-of [
       `(mod-thirst 11 21)]}]}
  {:title "Poisonous material!"
   :description ""
   :requirements #{:wooden}
   :choices [{
     :hotkey :space
     :name "continue"
     :one-of [
       `(mod-thirst 11 21)]}]}
  {:title "Allergic to material!"
   :description ""
   :requirements #{:wooden}
   :choices [{
     :hotkey :space
     :name "continue"
     :one-of [
       `(mod-thirst 11 21)]}]}
  {:title "Distract by hunger!"
   :description ""
   :choices [{
     :hotkey :space
     :name "continue"
     :one-of [
       `(mod-thirst 11 21)]}]}
  {:title "Distracted by thirst!"
   :description ""
   :choices [{
     :hotkey :space
     :name "continue"
     :one-of [
       `(mod-thirst 11 21)]}]}
  {:title "Low light!"
   :description ""
   :choices [{
     :hotkey :space
     :name "continue"
     :one-of [
       `(mod-thirst 11 21)]}]}
  {:title "Interrupted by sound!"
   :description ""
   :choices [{
     :hotkey :space
     :name "continue"
     :one-of [
       `(mod-thirst 11 21)]}]}
  {:title "Loud work environment!"
   :description ""
   :choices [{
     :hotkey :space
     :name "continue"
     :one-of [
       `(mod-thirst 11 21)]}]}
  {:title "Overworked!"
   :description ""
   :choices [{
     :hotkey :space
     :name "continue"
     :one-of [
       `(mod-thirst 11 21)]}]}
  {:title "Sidetracked!"
   :description ""
   :choices [{
     :hotkey :space
     :name "continue"
     :one-of [
       `(mod-thirst 11 21)]}]}
  {:title "Frustrated!"
   :description ""
   :choices [{
     :hotkey :space
     :name "continue"
     :one-of [
       `(mod-thirst 11 21)]}]}
  ;-----------------
  #_{:title ""
   :description ""
   :choices [{
     :hotkey :space
     :name "continue"
     :one-of [
       `(mod-thirst 11 21)]}]}
  ; Spider
  {:title ""
   :description "You notice a spider on your leg."
   :choices [
     {:hotkey \a
      :name "slap it"
      :events [
        {:description "You pull you hand back and the spider runs up your leg."
         :choices [
           {:hotkey :space
            :name "attack"
            :events [
              {:description "You hit the spider and your thigh."
               :one-of [`(mod-hp 5 7)]}]}]}
         {:description "Before you can hit it the spider sinks its fangs into your leg."
          :one-of [`(mod-hp 5 7)]}
         {:description "You slap the spider. There is not enough meat to eat."}]}
     {:hotkey \b
      :name "brush it off"
      :events [
        {:description "You brush the spider off and it disappears into the ground."}
        {:description "As you brush off the spider thousands of spider babies pour onto your leg"
         :choices [
           {:hotkey \a
            :name "just give up and die"
            :one-of [`(mod-hp 10 50)]}
           {:hotkey \b
            :name "slap all of them"
            :one-of [`(mod-hp 5 7)]}
           {:hotkey \c
            :name "roll on the ground"}]}]}]}])

 
(defn eval-effect [effect]
  (apply (resolve (first effect)) (rest effect)))

(defn gen-event [state recipe events]
  (let [blueprint (rand-nth events)
        next-stage (update blueprint
                      :choices (partial map (fn [choice]
                                            (if-let [effect (rand-nth (get choice :one-of))]
                                              (do (log/info "gen-event choice" choice "effect" effect)
                                              (assoc choice
                                                 :effects [(eval-effect effect)]))
                                              choice))))]
    (log/info "gen-event next-stage" next-stage)
    (rcrafting/assoc-current-recipe state :current-stage next-stage)))

(defn gen-complication [state {:keys [recipe/id
                                      recipe/category
                                      recipe/types
                                      recipe/components
                                      recipe/example-item-flags] :as recipe}]
  (let [recipe-pool (clojure.set/union (when id #{id})
                                       (when category #{category})
                                       types
                                       components
                                       example-item-flags)]
    (log/info "recipe-pool" recipe-pool)
    (gen-event state recipe
      (filter
        (fn [complication]
          (let [requirements (get complication :requirements #{})]
            (or (clojure.set/intersection recipe-pool requirements)
                (empty? requirements))))
        weapon-complications))))

(def remedies [{
    :title "Remedies"
    :choices [
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
         :choices [
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
   :choices [{
     :hotkey :space
     :name "continue"
     :one-of [`(mod-accuracy 1 2)]}]}
  {:title "Increased Damage"
   :description "With great care, you make the weapon much more powerful"
   :choices [{
     :hotkey :space
     :name "continue"
     :one-of [`(mod-damage 1 2)]}]}])

(defn gen-enhancement [state recipe]
  (gen-event state recipe enhancements))
