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

#_(defrecord ModWeapon [s k amount]
  rcmp/Mod
  (mod-name [this]
    (if (pos? amount)
       (str s " +" amount)
       (str s " " amount)))
  (mod-short-name [this]
    (if (pos? amount)
       (str (short-name s) "+" amount)
       (str (short-name s) "" amount)))
  (mod-type [this] :item-on-create)
  (mod-apply [this item]
    (update item k (fn [v] (+ (or v 0) amount)))))

#_(defrecord ModPlayerOnCreate [s k amount]
  rcmp/Mod
  (mod-name [this]
    (if (pos? amount)
       (str s " +" amount)
       (str s amount)))
  (mod-short-name [this]
    (if (pos? amount)
       (str (short-name s) " +" amount)
       (str (short-name s) " " amount)))
  (mod-type [this] :player-on-create)
  (mod-apply [this player]
    (update player k + amount)))

;; Weapon mods
(defn mod-accuracy
  [low high]
  (let [n (rr/uniform-int low high)]
    (rcmod/update-item-on-create :accuracy n)))

(defn mod-damage
  [low high]
  (let [n (rr/uniform-int low high)]
    (rcmod/update-item-on-create :damage n)))

(defn mod-durability
  [low high]
  (let [n (rr/uniform-int low high)]
    (rcmod/update-item-on-create :durability n)))

; Reduces opponent toughness
#_(defn mod-piercing
  []
  (->ModWeapon "piercing" :piercing true))

; Reduces opponent speed
#_(defn mod-haste
  [low high]
  (let [n (rr/uniform-int low high)]
    (->ModWeapon "durability" :speed n)))

; chance of scaring monster
#_(defn mod-scare-monster
  []
  (let [n (rr/uniform-int low high)]
    (->ModWeapon "durability" :scare-monster n)))

; chance of stunning monster
#_(defn mod-stunning
  (let [n (rr/uniform-int low high)]
    (->ModWeapon "stunning" :stunning n)))

; chance of dismembering monster
#_(defn mod-dismembering
  [low high]
  (let [n (rr/uniform-int low high)]
    (->ModWeapon "dismembering" :dismembering n)))

; chance of wounding monster
#_(defn mod-bleeding
  [low high]
  (let [n (rr/uniform-int low high)]
    (->ModWeapon "bleeding" :bleeding n)))

; chance of knockback
#_(defn mod-knockback
  [low high]
  (let [n (rr/uniform-int low high)]
    (->ModWeapon "knockback" :knockback n)))

; condition mod based on opponent attributes
#_(defn mod-conditioned-heirarchy
  [id mod]
    (->ModWeapon "conditioned" :conditioned {:id id :mod mod}))

;; Creation mods
(defn mod-hp [low high]
  (let [n (rr/uniform-int low high)]
    (rcmod/update-player-on-create :hp n)))

(defn mod-hunger [low high]
  (let [n (rr/uniform-int low high)]
    (rcmod/update-player-on-create :hunger n)))

(defn mod-thirst [low high]
  (let [n (rr/uniform-int low high)]
    (rcmod/update-player-on-create :thirst n)))

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
   :description "Poor materials make poor weapons."
   :choices [{
     :hotkey :space
     :name "continue"
     :one-of [
       `(mod-durability -7 -3)]}]}
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
   :choices [{
     :hotkey :space
     :name "continue"
     :one-of [
       `(mod-durability -7 -3)]}]}
  {:title "Soft materials!"
   :description "The materials you are using are too soft."
   :choices [{
     :hotkey :space
     :name "continue"
     :one-of [
       `(mod-durability -7 -3)]}]}
  {:title "Warped edge!"
   :description "The edge keeps warping when it should be straight."
   :choices [{
     :hotkey :space
     :name "continue"
     :one-of [
       `(mod-damage -7 -3)]}]}
  {:title "Chipped edge!"
   :description "The edge should be smooth, not full of chips."
   :choices [{
     :hotkey :space
     :name "continue"
     :one-of [
       `(mod-damage -7 -3)]}]}
  {:title "Rolled edge!"
   :description ""
   :choices [{
     :hotkey :space
     :name "continue"
     :one-of [
       `(mod-damage 11 21)]}]}
  {:title "Too heavy!"
   :description ""
   :choices [{
     :hotkey :space
     :name "continue"
     :one-of [
       `(mod-thirst 11 21)]}]}
  {:title "Too light!"
   :description ""
   :choices [{
     :hotkey :space
     :name "continue"
     :one-of [
       `(mod-thirst 11 21)]}]}
  {:title "Hard to grasp!"
   :description ""
   :choices [{
     :hotkey :space
     :name "continue"
     :one-of [
       `(mod-thirst 11 21)]}]}
  {:title "Too stiff!"
   :description ""
   :choices [{
     :hotkey :space
     :name "continue"
     :one-of [
       `(mod-thirst 11 21)]}]}
  {:title "Bent wood!"
   :description ""
   :choices [{
     :hotkey :space
     :name "continue"
     :one-of [
       `(mod-thirst 11 21)]}]}
  {:title "Green wood!"
   :description ""
   :choices [{
     :hotkey :space
     :name "continue"
     :one-of [
       `(mod-thirst 11 21)]}]}
  {:title "Rotten wood!"
   :description ""
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
   :choices [{
     :hotkey :space
     :name "continue"
     :one-of [
       `(mod-thirst 11 21)]}]}
  {:title "Handle uneven!"
   :description ""
   :choices [{
     :hotkey :space
     :name "continue"
     :one-of [
       `(mod-thirst 11 21)]}]}
  {:title "Handle popping out!"
   :description ""
   :choices [{
     :hotkey :space
     :name "continue"
     :one-of [
       `(mod-thirst 11 21)]}]}
  {:title "Not bent enough!"
   :description ""
   :choices [{
     :hotkey :space
     :name "continue"
     :one-of [
       `(mod-thirst 11 21)]}]}
  {:title "Cracked wood!"
   :description ""
   :choices [{
     :hotkey :space
     :name "continue"
     :one-of [
       `(mod-thirst 11 21)]}]}
  {:title "Cracked forming!"
   :description ""
   :choices [{
     :hotkey :space
     :name "continue"
     :one-of [
       `(mod-thirst 11 21)]}]}
  {:title "Pin knots!"
   :description ""
   :choices [{
     :hotkey :space
     :name "continue"
     :one-of [
       `(mod-thirst 11 21)]}]}
  {:title "Splinters!"
   :description ""
   :choices [{
     :hotkey :space
     :name "continue"
     :one-of [
       `(mod-thirst 11 21)]}]}
  {:title "Weather worn wood!"
   :description ""
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
   :choices [{
     :hotkey :space
     :name "continue"
     :one-of [
       `(mod-thirst 11 21)]}]}
  {:title "Weak joinery!"
   :description ""
   :choices [{
     :hotkey :space
     :name "continue"
     :one-of [
       `(mod-thirst 11 21)]}]}
  {:title "Poisonous material!"
   :description ""
   :choices [{
     :hotkey :space
     :name "continue"
     :one-of [
       `(mod-thirst 11 21)]}]}
  {:title "Allergic to material!"
   :description ""
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

(defn gen-complication [state recipe]
  (gen-event state recipe weapon-complications))

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
        buff-mod (eval-effect (first (shuffle [
                     `(mod-accuracy 1 5)
                     `(mod-damage 1 4)
                     `(mod-durability 1 5)])))
        debuff-mod (eval-effect (first (shuffle [
                   `(mod-accuracy -5 -1)
                   `(mod-damage -6 -2)
                   `(mod-durability -6 -2)])))
        _ (log/info "buff-mod" buff-mod)
        events [
        {:title "Material requirements"
         :description "New inventions require raw materials. This is no exception."
         :choices [
           (rcmp/item-on-create buff-mod
             {:name item-name
              :hotkey \a
              :material {:id item-id :amount (+ 1 (rand-int 4))}
              :effects [buff-mod]})
           (rcmp/item-on-create debuff-mod
             {:name "skip"
              :hotkey \b
              :effects [debuff-mod]})]}]]
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
