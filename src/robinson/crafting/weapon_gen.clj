(ns robinson.crafting.weapon-gen
  (:require [robinson.random :as rr]
            [robinson.world :as rw]
            [robinson.crafting :as rcrafting]
            [robinson.crafting.mod :as rcmod]
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

(defrecord ModWeapon [s k amount]
  rcmod/Mod
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

(defrecord ModPlayerOnCreate [s k amount]
  rcmod/Mod
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

(defn mod-accuracy [low high]
  (let [n (rr/uniform-int low high)]
    (->ModWeapon "accuracy" :accuracy n)))

(defn mod-damage [low high]
  (let [n (rr/uniform-int low high)]
    (->ModWeapon "damage" :damage n)))

(defn mod-durability [low high]
  (let [n (rr/uniform-int low high)]
    (->ModWeapon "durability" :durability n)))

(defn mod-hp [low high]
  (let [n (rr/uniform-int low high)]
    (->ModPlayerOnCreate "hp" :hp n)))

(defn mod-hunger [low high]
  (let [n (rr/uniform-int low high)]
    (->ModPlayerOnCreate "hunger" :hunger n)))

(defn mod-thirst [low high]
  (let [n (rr/uniform-int low high)]
    (->ModPlayerOnCreate "thirst" :thirst n)))

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
  #_{:title "Distracted!"
   :description "What was that? Oh right, making weapons."
   :choices [{
     :hotkey :space
     :name "continue"
     :one-of [
       `(mod-hunger 10 20)]}]}
  #_{:title "Overexertion!"
   :description "Slow down survivor. Take your time."
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

 
(defn gen-event [state recipe events]
  (let [blueprint (rand-nth events)
        next-stage (update blueprint
                      :choices (partial map (fn [choice]
                                            (if-let [effect (rand-nth (get choice :one-of))]
                                              (do (log/info "gen-event choice" choice "effect" effect)
                                              (assoc choice
                                                 :effects [(apply (resolve (first effect)) (rest effect))]))
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
  (rcrafting/assoc-current-recipe state :current-stage {
    :title "Material requirements"
    :description "New inventions require raw materials. This is no exception."
    :choices [
      {:name "Sticks"
       :hotkey \a
       :materials [{:id :stick :amount 10}]}
      {:name "Rocks"
       :hotkey \b
       :materials [{:id :rock :amount 10}]}]}))

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
