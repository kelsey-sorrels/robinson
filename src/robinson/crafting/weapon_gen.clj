(ns robinson.crafting.weapon-gen
  (:require [robinson.random :as rr]
            [taoensso.timbre :as log]
            [loom.graph :as lg]
            [loom.label :as ll]))

(defn current-recipe [state]
  (let [recipe-type (get-in state [:world :in-progress-recipe-type])]
    (get-in state [:world :in-progress-recipes recipe-type])))

(defn assoc-current-recipe [state & kvs]
  {:pre [(not (nil? state))]
   :post [(not (nil? %))]}
  (let [recipe-type (get-in state [:world :in-progress-recipe-type])]
    (update-in state [:world :in-progress-recipes recipe-type]
      (fn [recipe] (apply assoc recipe kvs)))))

(defn update-current-recipe [state f & xs]
  {:pre [(not (nil? state))]
   :post [(not (nil? %))]}
  (let [recipe-type (get-in state [:world :in-progress-recipe-type])]
    (update-in state [:world :in-progress-recipes recipe-type]
      (fn [recipe] (apply f recipe xs)))))

(defn recipe-requirements [recipe]
  "")

(defn recipe-output [recipe]
  "")

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
  (assoc-current-recipe state :current-stage {
    :gen :contact-type
    :title "Choose weapon type"
    :choices [
      {:name "Melee"
       :hotkey \a
       :types [:melee]}
      {:name "Thrown"
       :hotkey \b
       :types [:thrown]}
      {:name "Ranged"
       :hotkey \c
       :types [:ranged]}]}))

(defn gen-question-wield [state]
  (assoc-current-recipe state :current-stage {
    :gen :contact-type
    :title "Choose weapon type"
    :choices [
      {:name "Blunt"
       :hotkey \a
       :types [:blunt]}
      {:name "Edged"
       :hotkey \b
       :types [:edged]}
      {:name "Piercing"
       :hotkey \c
       :types [:piercing]}
      {:name "Flexible"
       :hotkey \d
       :types [:flexible]}]}))

(defn gen-question [state recipe]
  {:pre [(not (nil? state))]
   :post [(not (nil? %))]}
  "Add a question to the current recipe"
  (cond
    (contains? (set (get recipe :types)) :contact-type)
      (gen-question-wield state)
    (contains? (set (get recipe :types)) :wield-type)
      (gen-question-contact state)
    :else
      ((rand-nth [gen-question-contact gen-question-wield]) state)))

(defprotocol Mod
  (mod-name [this])
  (mod-type [this])
  (mod-apply [this item]))

(defrecord ModWeapon [s k amount]
  Mod
  (mod-name [this]
    (if (pos? amount)
       (str s " +" amount)
       (str s " " amount)))
  (mod-type [this] :weapon)
  (mod-apply [this item]
    (update item k inc amount)))

(defrecord ModPlayerOnCreate [s k amount]
  Mod
  (mod-name [this]
    (if (pos? amount)
       (str s " +" amount)
       (str s amount)))
  (mod-type [this] :player-on-create)
  (mod-apply [this player]
    (update player k inc amount)))

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
  #_{:title "Rushing!"
   :description "You've been rushing again."
   :choices [{
     :hotkey :space
     :name "continue"
     :one-of [
       `(mod-accuracy -5 -1)
       `(mod-damage -6 -2)]}]}
  #_{:title "Poor materials!"
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
  {:title ""
   :description "You notice a spider on your leg."
   :choices [{
     :hotkey \a
     :name "slap it"
     :one-of [
       `(mod-hp 1 2)
       nil]
     :next-stage {
       :title ""
       :description "You slap the spider"
       :choices [{
         :hotkey :space
         :name "continue"
         :one-of [
           `(mod-thirst 11 21)]}]}}]}])

 
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
    (assoc-current-recipe state :current-stage next-stage)))

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
  (assoc-current-recipe state :current-stage {
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

(defn next-node-items [recipe]
  (let [graph (get recipe :graph)
        current-node (get recipe :current-node)
        current-node-x (-> graph
                         (ll/label current-node)
                         :x)
        next-nodes (lg/successors graph current-node)]
    (map (fn [n]
           (let [x (-> graph (ll/label n) :x)]
             (cond
               (< x current-node-x)
                 {:name "left"
                  :hotkey \l
                  :next-node n}
               (= x current-node-x)
                 {:name "down"
                  :hotkey \m
                  :next-node n}
               (> x current-node-x)
                 {:name "right"
                  :hotkey \r
                  :next-node n})))
           next-nodes)))

;; Input handlers
(defn resolve-choice [state recipe keyin]
  (let [current-stage (get recipe :current-stage)
        ; find selected choice
        choice (->> (get current-stage :choices)
                 (filter #(= (get % :hotkey) keyin))
                 first)
        results (select-keys choice [:types :effects :materials])
        ; merge results into current recipe
        state-with-results (update-current-recipe state (partial merge-with into) results)
        ; if choice has a next-stage, use it,
        ; else create a stage that has items with a next stage
        next-stage (let [next-node-items (next-node-items recipe)]
                     (cond
                       (contains? choice :next-stage)
                         (get choice :next-stage)
                       ; one choice to next node? auto-increment
                       (= 1 (count next-node-items))
                         {:skip-to-node (-> next-node-items first :next-node)}
                       ;; else create next stage
                       :else
                         (get choice :next-stage
                           {:title "Choose path"
                            :choices next-node-items})))
        ; if the selected choice has a next-node use it,
        ; else use the current node
        next-node (get next-stage :skip-to-node (get choice :next-node (get recipe :current-node)))
        ; update the current recipe with the current stage and current node
        updated-state (assoc-current-recipe
                        state-with-results
                        :current-stage next-stage
                        :current-node next-node)]

    (log/info "current-stage" current-stage)
    (log/info "keyin" keyin)
    (log/info "choice" choice)
    (log/info "results" results)
    (log/info "next-stage" next-stage)
    (log/info "next-node" next-node)
    (if (= next-node (get recipe :current-node))
      updated-state
      ; gen stage for next node
      (let [next-node-type (get (ll/label (get recipe :graph) next-node) :type)]
        ((case next-node-type
          \? gen-question
          \! gen-complication
          \+ gen-remedy
          \& gen-material
          \☼ gen-enhancement
          (assert false (str "next node type unknown " next-node next-node-type)))
          updated-state (current-recipe updated-state))))))

(defn update [state keyin]
  (let [recipe-type (get-in state [:world :in-progress-recipe-type])
        recipe (get-in state [:world :in-progress-recipes recipe-type])]
    (resolve-choice state recipe keyin)))

(defn init [state recipe]
  {:pre [(not (nil? state))]
   :post [(not (nil? %))]}
  (let [n (get recipe :current-node)]
    (log/info "current node label" (ll/label (get recipe :graph) n))
    ((case (get (ll/label (get recipe :graph) n) :type)
        \? gen-question
        (assert false (str "current node not question" n (get recipe :graph))))
       state recipe)))
 
