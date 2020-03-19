;; Functions that manipulate state to do what the user commands.))
(ns robinson.combat
  (:require [robinson.common :as rc]
            [robinson.error :as re]
            [robinson.random :as rr]
            [robinson.world :as rw]
            [robinson.player :as rp]
            [robinson.inventory :as ri]
            [robinson.npc :as rnpc]
            [robinson.itemgen :as ig]
            [robinson.crafting.mod-protocol :as rcmp]
            [robinson.monstergen :as mg]
            [robinson.math :as rmath]
            [robinson.characterevents :as ce]
            [robinson.dynamiccharacterproperties :as dcp]
            [robinson.color :as rcolor]
            [robinson.fx :as rfx]
            [taoensso.timbre :as log]
            [robinson.macros :as rm]
            [clojure.stacktrace :refer [print-stack-trace]]
            [clojure.pprint :refer [pprint]])
   (:import robinson.dynamiccharacterproperties.DynamicCharacterProperties))

(def attack-mods {

})

(defn sharp-weapon?
  [attack]
  (contains? #{:spear :axe :knife :cutlass :ritual-knife :ancient-spear} attack))

(defn ranged-weapon?
  [attack]
  (contains? #{:flint :rock :coconut :bow :jack-o-lantern :pistol :ancient-spear :blowgun :whip} attack))

(defn format [s & args]
  (apply clojure.core/format s args))

(defn blood-splatter [state defender-path pos]
  (let [t (rw/get-time state)]
    (-> state
      (assoc-in (conj defender-path :bloodied) (+ 20 t))
      (rw/assoc-cells (zipmap (rw/adjacent-xys-ext pos) (repeatedly (fn [] {:bloodied (+ (rr/uniform-int 5 20) t)})))))))

(defn- gen-attack-message
  "Logs an attack message to the global state.
   `attack` is one of :bite :claws :punch.
   `damage-type` is one of :miss :hit :dead"
  [attacker defender attack defender-body-part damage-type]
  (let [attacker-race        (get attacker :race :human)
        defender-race        (get defender :race :human)
        attacker-name        (get attacker :name)
        defender-name        (get defender :name)
        rand-punch-verb      (fn [] (rr/rand-nth ["wack" "punch" "hit" "pummel" "batter"
                                                  "pound" "beat" "strike" "slug"]))
        rand-axe-verb        (fn [] (rr/rand-nth ["hit" "strike" "slash" "tear into" "cleave" "cut"]))
        rand-ranged-hit-verb (fn [] (rr/rand-nth ["hit" "strike"]))
        _                  (log/debug "gen-attack-messsage first-vec-match" attacker-race defender-race attack defender-body-part damage-type)
        msg (rm/first-vec-match [attacker-race defender-race attack defender-body-part damage-type]
              [:human :*       :punch         :*        :miss] (format "You punch the %s but miss." defender-name)
              [:human :*       :punch         :*        :hit]  (rr/rand-nth [(format "You %s the %s %s %s the %s."
                                                                              (rand-punch-verb)
                                                                              defender-name
                                                                              (rr/rand-nth ["solidly" "swiftly" "repeatedly"
                                                                                         "perfectly" "competently"])
                                                                              (rr/rand-nth ["on" "in" "across"])
                                                                              (name defender-body-part))
                                                                            (format "You %s the %s %s the %s."
                                                                              (rand-punch-verb)
                                                                              defender-name
                                                                              (rr/rand-nth ["on" "in" "across"])
                                                                              (name defender-body-part))
                                                                            (format "You %s the %s."
                                                                              (rand-punch-verb)
                                                                              defender-name)])
              [:human :*       :punch         :head     :dead] (format "You %s the %s in the head. Brains fly everywhere and it dies." (rand-punch-verb) defender-name)
              [:human :*       :punch         :neck     :dead] (format "You %s the %s in the neck snapping it and it dies." (rand-punch-verb) defender-name)
              [:human :*       :punch         :body     :dead] (format "You %s the %s in the body damaging internal organs. It dies." (rand-punch-verb) defender-name)
              [:human :*       :punch         :leg      :dead] (format "You %s the %s in the leg severing it and it dies." (rand-punch-verb) defender-name)
              [:human :*       :punch         :face     :dead] (format "You %s the %s in the face. Pieces of face fly everywhere and it dies." (rand-punch-verb) defender-name)
              [:human :*       :punch         :abdomen  :dead] (format "You %s the %s in the abdomen. Internal organs fly everywhere and it dies." (rand-punch-verb) defender-name)
              [:human :*       :punch         :claw     :dead] (format "You %s the %s in the claw and it dies." (rand-punch-verb) defender-name)
              [:human :*       :punch         :tail     :dead] (format "You %s the %s in the tail causing massive injuries and it dies." (rand-punch-verb) defender-name)
              [:human :*       :punch         :wing     :dead] (format "You %s the %s in the wing ripping it clean off and it dies." (rand-punch-verb) defender-name)
              [:human :*       :punch         :eye      :dead] (format "You %s the %s in the eye exploding it upon impact and it dies." (rand-punch-verb) defender-name)
              [:human :*       :punch         :snout    :dead] (format "You %s the %s in the snout crushing it and it dies." (rand-punch-verb) defender-name)
              [:human :*       :punch         :arm      :dead] (format "You %s the %s in the arm crushing bones and it dies." (rand-punch-verb) defender-name)
              [:human :*       :punch         :beak     :dead] (format "You %s the %s in the beak ripping it from its face and it dies." (rand-punch-verb) defender-name)
              [:human :*       :punch         :shell    :dead] (format "You %s the %s in the shell ripping to peices and it dies." (rand-punch-verb) defender-name)
              [:human :*       :punch         :tentacle :dead] (format "You %s the %s in the tentacle shredding it and it dies." (rand-punch-verb) defender-name)
              [:human :*       :punch         :*        :dead] (format "You %s the %s causing massive injuries and it dies." (rand-punch-verb) defender-name)
              [:human :*       sharp-weapon?  :*        :miss] (format "You swing at the %s but miss." defender-name)
              [:human :*       sharp-weapon?  :*        :hit]  (rr/rand-nth [(format "You %s the %s %s %s the %s."
                                                                       (rand-axe-verb)
                                                                       defender-name
                                                                       (rr/rand-nth ["solidly" "swiftly"
                                                                                  "perfectly" "competently"])
                                                                       (rr/rand-nth ["on" "in" "across"])
                                                                       (name defender-body-part))
                                                                     (format "You %s the %s %s the %s."
                                                                       (rand-axe-verb)
                                                                       defender-name
                                                                       (rr/rand-nth ["on" "in" "across"])
                                                                       (name defender-body-part))
                                                                     (format "You %s the %s."
                                                                       (rand-axe-verb)
                                                                       defender-name)])
              [:human :*       sharp-weapon?  :head     :dead] (format "You %s the %s in the head. Brains fly everywhere and it dies." (rand-axe-verb) defender-name)
              [:human :*       sharp-weapon?  :neck     :dead] (format "You %s the %s in the neck snapping it and it dies." (rand-axe-verb) defender-name)
              [:human :*       sharp-weapon?  :body     :dead] (format "You %s the %s in the body damaging internal organs. It dies." (rand-axe-verb) defender-name)
              [:human :*       sharp-weapon?  :leg      :dead] (format "You %s the %s in the leg severing it and it dies." (rand-axe-verb) defender-name)
              [:human :*       sharp-weapon?  :face     :dead] (format "You %s the %s in the face. Pieces of face fly everywhere and it dies." (rand-axe-verb) defender-name)
              [:human :*       sharp-weapon?  :abdomen  :dead] (format "You %s the %s in the abdomen. Internal organs fly everywhere and it dies." (rand-axe-verb) defender-name)
              [:human :*       sharp-weapon?  :claw     :dead] (format "You %s the %s in the claw and it dies." (rand-axe-verb) defender-name)
              [:human :*       sharp-weapon?  :tail     :dead] (format "You %s the %s in the tail causing massive injuries and it dies." (rand-axe-verb) defender-name)
              [:human :*       sharp-weapon?  :wing     :dead] (format "You %s the %s in the wing ripping it clean off and it dies." (rand-axe-verb) defender-name)
              [:human :*       sharp-weapon?  :eye      :dead] (format "You %s the %s in the eye exploding it upon impact and it dies." (rand-axe-verb) defender-name)
              [:human :*       sharp-weapon?  :snout    :dead] (format "You %s the %s in the snout crushing it and it dies." (rand-axe-verb) defender-name)
              [:human :*       sharp-weapon?  :arm      :dead] (format "You %s the %s in the arm crushing bones it and it dies." (rand-axe-verb) defender-name)
              [:human :*       sharp-weapon?  :beak     :dead] (format "You %s the %s in the beak ripping it from its face and it dies." (rand-axe-verb) defender-name)
              [:human :*       sharp-weapon?  :shell    :dead] (format "You %s the %s in the shell ripping to peices and it dies." (rand-axe-verb) defender-name)
              [:human :*       sharp-weapon?  :tentacle :dead] (format "You %s the %s in the tentacle shredding it and it dies." (rand-axe-verb) defender-name)
              [:human :*       sharp-weapon?  :*        :dead] (format "You %s the %s causing massive injuries and it dies." (rand-axe-verb) defender-name)
              [:human :*       ranged-weapon? :*        :miss] (format "You aim for the %s but miss." defender-name)
              [:human :*       ranged-weapon? :*        :hit]  (rr/rand-nth [(format "You %s the %s %s %s the %s."
                                                                      (rand-ranged-hit-verb)
                                                                      defender-name
                                                                      (rr/rand-nth ["solidly" "swiftly"
                                                                                 "perfectly" "competently"])
                                                                      (rr/rand-nth ["on" "in" "across"])
                                                                      (name defender-body-part))
                                                                    (format "You %s the %s %s the %s."
                                                                      (rand-ranged-hit-verb)
                                                                      defender-name
                                                                      (rr/rand-nth ["on" "in" "across"])
                                                                      (name defender-body-part))
                                                                    (format "You %s the %s."
                                                                      (rand-ranged-hit-verb)
                                                                      defender-name)])
              [:human :*       ranged-weapon? :head     :dead] (format "You %s the %s in the head. Brains fly everywhere and it dies." (rand-ranged-hit-verb) defender-name)
              [:human :*       ranged-weapon? :neck     :dead] (format "You %s the %s in the neck snapping it and it dies." (rand-ranged-hit-verb) defender-name)
              [:human :*       ranged-weapon? :body     :dead] (format "You %s the %s in the body damaging internal organs. It dies." (rand-ranged-hit-verb) defender-name)
              [:human :*       ranged-weapon? :leg      :dead] (format "You %s the %s in the leg severing it and it dies." (rand-ranged-hit-verb) defender-name)
              [:human :*       ranged-weapon? :face     :dead] (format "You %s the %s in the face. Pieces of face fly everywhere and it dies." (rand-ranged-hit-verb) defender-name)
              [:human :*       ranged-weapon? :abdomen  :dead] (format "You %s the %s in the abdomen. Internal organs fly everywhere and it dies." (rand-ranged-hit-verb) defender-name)
              [:human :*       ranged-weapon? :claw     :dead] (format "You %s the %s in the claw and it dies." (rand-ranged-hit-verb) defender-name)
              [:human :*       ranged-weapon? :tail     :dead] (format "You %s the %s in the tail causing massive injuries and it dies." (rand-ranged-hit-verb) defender-name)
              [:human :*       ranged-weapon? :wing     :dead] (format "You %s the %s in the wing ripping it clean off and it dies." (rand-ranged-hit-verb) defender-name)
              [:human :*       ranged-weapon? :eye      :dead] (format "You %s the %s in the eye exploding it upon impact and it dies." (rand-ranged-hit-verb) defender-name)
              [:human :*       ranged-weapon? :snout    :dead] (format "You %s the %s in the snout crushing it and it dies." (rand-ranged-hit-verb) defender-name)
              [:human :*       ranged-weapon? :arm      :dead] (format "You %s the %s in the arm crushing bones it and it dies." (rand-ranged-hit-verb) defender-name)
              [:human :*       ranged-weapon? :beak     :dead] (format "You %s the %s in the beak ripping it from its face and it dies." (rand-ranged-hit-verb) defender-name)
              [:human :*       ranged-weapon? :shell    :dead] (format "You %s the %s in the shell ripping to peices and it dies." (rand-ranged-hit-verb) defender-name)
              [:human :*       ranged-weapon? :tentacle :dead] (format "You %s the %s in the tentacle shredding it and it dies." (rand-ranged-hit-verb) defender-name)
              [:human :*       ranged-weapon? :*        :dead] (format "You %s the %s causing massive injuries and it dies." (rand-ranged-hit-verb) defender-name)
              [:human :*       :strangle      :neck     :hit]  (format "You strangle the %s for a bit." (name defender-race))
              [:human :*       :strangle      :neck     :miss] (format "You miss the %s's neck." (name defender-race))
              [:human :*       :strangle      :neck     :dead] (format "You finish off the %s as it suffocates to death." (name defender-race))
              [:human :*       :strangle      :*        :hit]  (format "You hit the %s." (name defender-race))
              [:human :*       :strangle      :*        :miss] (format "You miss the %s's neck." (name defender-race))
              [:human :*       :strangle      :*        :dead] (format "You finish off the %s by beating it." (name defender-race))
              [:human :*       :*             :*        :dead] (format "You %s the %s in the %s with the %s killing it." (rand-ranged-hit-verb) (name defender-race)
                                                                                                                         (name defender-body-part) (name attack))
              [:human :*       :*             :*        :hit]  (format "You %s the %s in the %s with the %s." (rand-ranged-hit-verb) (name defender-race)
                                                                                                                                     (name defender-body-part) (name attack))
              [:human :*       ranged-weapon? :*        :miss] (format "You shoot the %s at the %s, but miss." (name attack) (name defender-race))
              [:human :*       :*             :*        :miss] (format "You throw the %s at the %s, but miss." (name attack) (name defender-race))
              [:*     :human   :bite          :*        :miss] (format "The %s lunges at you with its mouth but misses." attacker-name)
              [:*     :human   :bite-venom    :*        :miss] (format "The %s snaps at you its mouth but misses." attacker-name)
              [:*     :human   :claw          :*        :miss] (format "The %s claws at you and narrowly misses." attacker-name)
              [:*     :human   :punch         :*        :miss] (format "The %s punches you but misses." attacker-name)
              [:*     :human   :gore          :*        :miss] (format "The %s lunges at you with its tusks." attacker-name)
              [:*     :human   :sting         :*        :miss] (format "The %s tries to sting you but misses." attacker-name)
              [:*     :human   :sting-venom   :*        :miss] (format "The %s tries to sting you but misses." attacker-name)
              [:*     :human   :squeeze       :*        :miss] (format "The %s starts to constrict around you but fumbles." attacker-name)
              [:*     :human   :clamp         :*        :miss] (format "The %s tries to clamp onto you but isn't fast enough." attacker-name)
              [:*     :human   :spike         :*        :miss] (format "You almost get poked by the %s's spikes." attacker-name)
              [:*     :human   :poisonous-gas :*        :miss] "You get a wiff of noxious gas."
              [:mosquito
                      :human   :bite          :*        :hit]  (format "The %s sinks its probiscus into your flesh." attacker-name)
              [:*     :human   :bite          :*        :hit]  (format "The %s sinks its teeth into your flesh." attacker-name)
              [:*     :human   :bite-venom    :*        :hit]  (format "The %s buries its teeth into your body and starts pumping poison into you." attacker-name)
              [:*     :human   :claw          :*        :hit]  (format "The %s claws into your flesh." attacker-name)
              [:*     :human   :punch         :*        :hit]  (format "The %s punches you." attacker-name)
              [:*     :human   :gore          :*        :hit]  (format "The %s gores into your body with it's tusks.`" attacker-name)
              [:*     :human   :sting         :*        :hit]  (format "The %s jabs you with its stinger." attacker-name)
              [:*     :human   :sting-venom   :*        :hit]  (format "The %s stings you, pumping you full of poison." attacker-name)
              [:*     :human   :squeeze       :*        :hit]  (format "The %s squeezes you leaving you gasping for breath." attacker-name)
              [:*     :human   :clamp         :*        :hit]  (format "The %s clamps down on your flesh crushing it." attacker-name)
              [:*     :human   :spike         :*        :hit]  (format "The %s's spikes drive into your body." attacker-name)
              [:*     :human   :poisonous-gas :*        :hit]  "The noxious gas enters your body."
              [:*     :human   :blowdart      :*        :hit]  "The blowdart pierces you with a sharp sting."
              [:trap  :*       :poisonous-gas :*        :dead] (format "The poisonous gas suffocates the %s killing it." defender-name)
              [:trap  :*       :poisonous-gas :*        :miss] (format "The poisonous gas waffs over the %s." defender-name)
              [:trap  :*       :poisonous-gas :*        :hit] (format "The fire engulfs the the %s" defender-name)
              [:trap  :*       :fire          :*        :dead] (format "The fire consumes the %s killing it." defender-name)
              [:trap  :*       :fire          :*        :miss] (format "The fire darts at the %s." defender-name)
              [:trap  :*       :fire          :*        :hit] (format "The poisonous gas enters the %s's body." defender-name)
              [:*     :*       :*             :*        :*  ]  (assert false (format "Missing combat message %s %s %s %s %s." attacker-race defender-race attack defender-body-part damage-type)))]
     (log/debug "attack message" msg)
     msg))

(defmulti is-hit? (fn [state attacker defender attack-type] attack-type))

(defmethod is-hit? :melee
  [state attacker defender attack-type]
  (let [attacker-stat   (dcp/get-attack-strength attacker state attack-type)
        defender-stat   (+ (dcp/get-attack-dexterity defender state attack-type)
                            (dcp/get-attack-toughness defender state attack-type))
        target-value     (/ 1 (inc (rmath/exp (/ (- defender-stat attacker-stat) 4))))]
    (log/info "hit target value"  target-value)
    (> (rr/uniform-double 0.2 1.7) target-value)))

(defmethod is-hit? :ranged
  [state attacker defender attack-type]
  (let [attacker-stat   (dcp/get-attack-dexterity attacker state attack-type)
        defender-stat   (+ (dcp/get-attack-dexterity defender state attack-type)
                            (dcp/get-attack-toughness defender state attack-type))
        target-value     (/ 1 (inc (rmath/exp (/ (- defender-stat attacker-stat) 4))))]
    (log/info "hit target value"  target-value)
    (> (rr/uniform-double 0.2 1.0) target-value)))

(defmethod is-hit? :thrown-item
  [state attacker defender attack-type]
  (let [attacker-stat   (dcp/get-attack-dexterity attacker state attack-type)
        defender-stat   (+ (dcp/get-attack-dexterity defender state attack-type)
                            (dcp/get-attack-toughness defender state attack-type))
        target-value     (/ 1 (inc (rmath/exp (/ (- defender-stat attacker-stat) 4))))]
    (log/info "hit target value"  target-value)
    (> (rr/uniform-double 0.2 1.0) target-value)))

(defmulti calc-dmg (fn [state attacker attack-type defender defender-body-part] attack-type))

(defmethod calc-dmg :melee
  [state attacker attack-type defender defender-body-part]
  #_(log/info "Attacker" attacker "Defender" defender "defender-type" (type defender))
  (log/info "attacker" (get attacker :race :human) "defender" (get defender :race :human))
  ;;Damage = Astr * (Adex / Dsp) * (Asize / Dsize) * (Atoughness / Dtoughness)
  (let [attacker-strength  (dcp/get-attack-strength attacker state attack-type)
        attacker-dexterity (dcp/get-attack-dexterity attacker state attack-type)
        defender-speed     (dcp/get-attack-speed defender state attack-type)
        attacker-size      (dcp/get-size attacker state)
        defender-size      (dcp/get-size defender state)
        attack-toughness   (dcp/get-attack-toughness defender state attack-type)
        defender-toughness (dcp/get-toughness defender state)]
    (* attacker-strength
       (/ (+ 5 (rr/uniform-double (* 10 attacker-dexterity))) (+ 15 defender-speed))
       (/ (+ 125 attacker-size) (+ 125 defender-size))
       (/ attack-toughness defender-toughness))))

(defn calc-dmg-ranged-or-thrown
  [state attacker attack-type defender defender-body-part]
  #_(log/info "Attacker" attacker "Defender" defender "defender-type" (type defender))
  (log/info "attacker" (get attacker :race :human) "defender" (get defender :race :human))
  ;;Damage = Astr * (Adex / Dsp) * (As / Ds) * (At / Dt)
  (let [attacker-strength  (dcp/get-attack-strength attacker state attack-type)
        attacker-dexterity (dcp/get-attack-dexterity attacker state attack-type)
        defender-speed     (dcp/get-speed defender state)
        attacker-size      (dcp/get-size attacker state)
        defender-size      (dcp/get-size defender state)
        attack-toughness   (dcp/get-attack-toughness attacker state attack-type)
        defender-toughness (dcp/get-toughness defender state)]
    (* attacker-dexterity
       (/ (+ 5 (rr/uniform-double (* 10 attacker-dexterity))) (+ 15 defender-speed))
       (/ (+ 125 attacker-size attacker-strength) (+ 125 defender-size))
       (/ (inc attack-toughness) defender-toughness))))

(defmethod calc-dmg :ranged
  [& more]
  (apply calc-dmg-ranged-or-thrown more))

(defmethod calc-dmg :thrown-item
  [& more]
  (apply calc-dmg-ranged-or-thrown more))

(defn assert-msg
  [v & msg]
  (assert v msg)
  v)

(defmacro log-with-line [v & msg]
  `(do (log/info
               ~*file*
               ":"
               ~(:line (meta &form))
               ">"
               ~@msg)
       (flush)
       (assert (some? ~v))
       ~v))

(defn attacker-or-path->attacker
  [state attacker-or-path]
  (if (record? attacker-or-path)
    attacker-or-path
    (get-in state attacker-or-path)))

(defn- attack-do-damage
  [state
   attacker-or-path
   attacker
   attack
   attack-type
   ranged-weapon
   attack-item
   thrown-item
   defender-path
   defender
   defender-body-part
   hit
   dmg
   is-wound]
  (as-> state state
            (log-with-line state "0")
            ;; modify defender hp
            (update-in state (conj defender-path :hp)
              (fn [hp] (- hp dmg)))
            ;; splatter blood
            (if is-wound
              (blood-splatter state defender-path (get defender :pos))
              state)
            (log-with-line state "1")
            ;; attacks use wielded weapons
            (if (and (= attack-type :melee attack-item))
              (ri/dec-item-utility state (get attack-item :hotkey))
              state)
            (log-with-line state "2")
            ;; awaken player if attacked while sleeping
            (if (and (contains? (set defender-path) :player)
                     (= (rw/current-state state) :sleep))
              (assoc-in state [:world :current-state] :normal)
              state)
            ;; degrade player clothing if worn
            (if (contains? (set defender-path) :player)
              (ri/update-worn-item-utility state dec)
              state)
            (log-with-line state "3")
            ;; provoke temperamental animal
            (if (= (get-in [state] (conj defender-path :temperament)) :hostile-after-attacked)
              (-> state
                (update-in (conj defender-path :status)         (fn [state] (conj state :hostile)))
                (assoc-in (conj defender-path :movement-policy) :follow-player-in-range-or-random))
              state)
            (log-with-line state "4")
            (if (= (get-in [state] (conj defender-path :temperament)) :retreat-after-attacked)
              (-> state
                (update-in (conj defender-path :status)         (fn [state] (conj state :hostile)))
                (assoc-in (conj defender-path :movement-policy) :hide-from-player-in-range-or-random))
              state)
            (log-with-line state "5")
            (log-with-line state "attack-type" attack-type)
            (let [msg (gen-attack-message attacker
                                          defender
                                          (case attack-type
                                            :melee attack
                                            :ranged (get ranged-weapon :item/id (get ranged-weapon :item/id))
                                            :thrown-item (get thrown-item :item/id (get thrown-item :item/id))
                                            (assert false (format "Unknown attack type %s" (name attack-type))))
                                          defender-body-part
                                          (if hit
                                            :hit
                                            :miss))]
              (log/debug "attack msg" msg)
              (rc/append-log state 
                             msg
                          (if hit
                            :red
                            :white)))
            (log-with-line state "6")
            ;; chance of being envenomed by venomous attacks
            #_(update-in state (conj defender-path :status) (fn [status] (if (and (re-find #"venom" (str attack))
                                                                                (= (rr/uniform-int 10) 0))
                                                                         (conj status :poisioned)
                                                                         status)))
            (log-with-line state "7")
            ;; chance of being paralyzed by frog
            (if (and (= (get defender :id) :player)
                     (mg/is-poisonous-frog? state (get attacker :race :human)))
              (rp/assoc-player-attribute :paralyzed-start-time (inc (rw/get-time state)))
              state)

            (log-with-line state "8")
            ;; ciance of being wounded
            (update-in state defender-path (fn [defender] (if (and is-wound
                                                                   (contains? defender :wounds))
                                                            (update-in defender [:wounds]
                                                              (fn [wounds] (merge-with (fn [w0 w1] {:time (max (get w0 :time) (get w1 :time))
                                                                                                    :dmg  (+   (get w0 :dmg)  (get w1 :dmg))})
                                                                             wounds
                                                                             {defender-body-part {:time (rw/get-time state)
                                                                                            :dmg dmg}})))
                                                            defender)))
            (log-with-line state "9")
            (if (and is-wound
                     (contains? (set defender-path) :player))
              (rc/append-log state "You have been wounded." :red)
              state)
            (log-with-line state "10")
            (ce/on-hit defender state)
            (log-with-line state "11")
            ;; show fx
            ;; Make blip actor and use character FX
            (if hit
              (rfx/conj-effect state :blip (get defender :pos)
                                        \♥
                                        (rcolor/color->rgb :red)
                                        [0 0 0 0]
                                        2)
              (rfx/conj-effect state :blip (get defender :pos)
                                       \/
                                       (rcolor/color->rgb :blue)
                                       [0 0 0 0]
                                       2))
            (log-with-line state "12")
            ;; some thrown items can stun npcs
            (if (and hit
                     (or
                       (ig/id-can-stun? (get attack-item :item/id))
                       (< (rr/next-float! rr/*rnd*) (get attack-item :stunned 0))))
              (->
                state
                ;; show fx
                (rfx/conj-effect :blip (get defender :pos)
                                  \!
                                  (rcolor/color->rgb :orange)
                                  [0 0 0 0]
                                  3)
                ; add tag to npc
                (update-in (conj defender-path :status) (fn [state] (conj state :stunned))))
              state)
            (log-with-line state "13")
            (if (contains? (set attacker-or-path) :player)
              (rp/update-npc-attacked state defender attack)
              state)))

(defn- attack-do-death
  [state attacker defender-path defender defender-body-part attack attack-type attack-item thrown-item ranged-weapon]
  ;(log/info "attack-do-death" (keys attack) (keys attack-item) thrown-item ranged-weapon)
  (let [
        {defender-x :x defender-y :y} (get defender :pos)]
  (if (contains? (set defender-path) :npcs)
            ;; defender is npc
            (-> state
              (assert-msg "state nil")
              ;; attacks use wielded weapons
              (as-> state
                (if (and (= attack-type :melee) attack-item)
                  (ri/dec-item-utility state (get attack-item :hotkey))
                  state))
              (assert-msg "state nil")
              ;; update stats and will-to-live
              (rp/update-npc-killed defender attack)
              (assert-msg "state nil")
              ;; trigger on-death script for defender
              ((partial ce/on-death defender))
              (assert-msg "state nil")
              ;; remove defender
              (rc/remove-in (butlast defender-path) (partial = defender))
              (assert-msg "state nil")
              ;; show fx
              (rfx/conj-effect :blip (get defender :pos)
                                \☻
                                (rcolor/color->rgb :red)
                                [0 0 0 0]
                                2)
              ;; maybe add corpse
              (assert-msg "state nil")
              (rw/update-cell-items defender-x defender-y
                (fn [items]
                  (if (> (rr/next-float! rr/*rnd*) 0.5)
                    (conj items (ig/gen-corpse defender))
                    items)))
              (rc/append-log (gen-attack-message attacker
                                              defender
                                              (case attack-type
                                                :melee attack
                                                :ranged (get ranged-weapon :item/id)
                                                :thrown-item (get thrown-item :item/id)
                                                (assert false (format "Unknown attack type %s" (name attack-type))))
                                              defender-body-part
                                              :dead)
                          :white))
            ;; defender is player
            (let [cause-of-death (format "%s %s's %s." (rc/noun->indefinite-article (get attacker :name))
                                                                                 (get attacker :name)
                                                                                 (name attack))]
              (-> state
                (assoc-in [:world :cause-of-death] cause-of-death)
                (rp/kill-player)
              (rp/update-player-died :combat))))))

(defn- attack-helper
  "Attack is one of
    :melee, :thrown-item, or :ranged"
  [state attacker attacker-or-path defender-path attack]
  {:pre [
    ; defender-path is a seq
    (sequential? defender-path)
    ; attacker is a (n)pc
    (satisfies? dcp/DynamicCharacterProperties attacker)
    ; defender is a (n)pc
    (satisfies? dcp/DynamicCharacterProperties
      (get-in state defender-path))
    ; attack is a keyword or item
    (or (keyword? attack)
        (contains? attack :item/id))]}
  (let [attack-type          (cond
                               (keyword attack)
                                 :melee
                               (get attack :wielded-ranged)
                                 :ranged
                               :else
                                 :thrown-item)
        attack-item          (case attack-type
                               :melee (rp/wielded-item attacker)
                               :ranged (rp/wielded-ranged-item attacker)
                               :thrown-item attack)
        ranged-weapon        (when (= attack-type :ranged)
                               attack)
        thrown-item          (when (= attack-type :thrown-item)
                               attack)
        shot-poisoned-arrow  (when thrown-item
                               (ig/arrow-poison-tipped? state thrown-item))
        defender             (get-in state defender-path)
        defender-body-part   (rr/rand-nth (vec (get defender :body-parts)))
        {defender-x :x defender-y :y} (get defender :pos)
        hp                   (get defender :hp)
        hit                  (is-hit? state attacker defender attack-type)
        defender             (rcmp/apply-mods
                               defender
                               (get attack-item :effects)
                               rcmp/ModDefenderOnAttackTemp
                               attacker defender)
        state                (if (contains? attacker :name)
                               (rnpc/update-npc
                                 state
                                 defender
                                 (fn [_]
                                   (rcmp/apply-mods
                                     defender
                                     (get attack-item :effects)
                                     rcmp/ModDefenderOnAttack
                                     attacker defender)))
                               state)
        dmg                  (cond
                               hit   (+ (calc-dmg state attacker attack-type defender defender-body-part) (if shot-poisoned-arrow 1 0))
                               :else 0)
        is-wound             (> dmg 1.5)]
    #_(log/info "attack" attacker-or-path "is attacking defender" defender-path)
    #_(log/info "attacker-detail" attacker)
    #_(log/info "defender-detail" defender)
    #_(log/debug "attack" attack)
    (log/info "attack-type" attack-type)
    (log/info "hit?" hit)
    (log/info "hp" hp)
    (log/info "max-hp" (if (= (get defender :race :human) :human)
                          (get defender :max-hp)
                          (get (mg/gen-monster (get defender :race :human)) :hp)))
    (log/debug "dmg" dmg)
    (try
      ;; defender still alive?
      (if (pos? (- hp dmg))
        (attack-do-damage
          state
          attacker-or-path
          attacker
          attack
          attack-type
          ranged-weapon
          attack-item
          thrown-item
          defender-path
          defender
          defender-body-part
          hit
          dmg
          is-wound)
        ;; defender dead? (0 or less hp)
        (attack-do-death
          state
          attacker
          defender-path
          defender
          defender-body-part
          attack
          attack-type
          attack-item
          thrown-item
          ranged-weapon))
      (catch Throwable ex
        (log/error "Caught exception while doing combat" ex)
        (re/log-exception ex state))
      (finally
        (log/info "End of attack")))))

(defn attack
  "Perform combat. The attacker fights the defender, but not vice-versa.
   Return a new state reflecting combat outcome."
  ([state attacker-or-path defender-path]
  {:pre [(or (or (record? attacker-or-path)
                  (every? (set (keys (get-in state attacker-or-path))) [:attacks]))
             (assert false (str "attacker under specified" attacker-or-path (get-in state attacker-or-path))))
         (some? state)]}
   (let [attacker           (attacker-or-path->attacker state attacker-or-path)
         attack-type (or (get (first (filter (fn [item] (contains? item :wielded))
                                             (get attacker :inventory [])))
                              :attack)
                         (rr/rand-nth (vec (get attacker :attacks))))]
     (attack state attacker-or-path defender-path attack-type)))
  ([state attacker-or-path defender-path attack]
  {:pre [(or (vector? attacker-or-path) (record? attacker-or-path))
         (vector? defender-path)
         (some? state)
         (or (every? (set (keys (get-in state defender-path))) [:hp :pos :body-parts])
             (assert false (str "defender under specified " defender-path (get-in state defender-path))))
         (vector? (get-in state [:world :npcs]))]
   :post [(some? %)
          (vector? (get-in % [:world :npcs]))]}
  #_(log/info "attacker-or-path" attacker-or-path "defender-path" defender-path "attack" attack)
  (let [attacker             (attacker-or-path->attacker state attacker-or-path)]
    (attack-helper state attacker attacker-or-path defender-path attack))))

(defn do-combat
  [attacker defender]
  (let [state {:current-state :normal
               :world         {:npcs [defender]
                               :player attacker
                               :time 0}}
        knife  (assoc (ig/gen-item :knife) :wielded true)
        #_#_state         (ri/add-to-inventory state [knife])
        attacker-path [:world :player]
        defender-path [:world :npcs 0]
        pass-through  (fn [m & _] m)]
    (with-redefs [rw/update-cell       pass-through
                  rw/get-time          (fn [state] 1)
                  blood-splatter       pass-through
                  ri/dec-item-utility  pass-through
                  rc/append-log        pass-through
                  ce/on-hit            (fn [_ state] state)
                  ce/on-death          (fn [_ state] state)
                  rw/update-cell-items pass-through]
      (loop [state state
             ticks 1]
        (let [state-after-combat  (attack state attacker-path defender-path)
              attacker (get-in state-after-combat attacker-path)
              defender (get-in state-after-combat defender-path)]
          (log/info "player" (select-keys attacker [:id :hp :max-hp]))
          (log/info "defender" (select-keys defender [:race :hp]))
          (if (nil? defender)
            [:npc-died ticks (get attacker :hp) (get attacker :max-hp)]
            (let [state-after-combat (attack state-after-combat defender-path attacker-path)
                  attacker (get-in state-after-combat attacker-path)
                  defender (get-in state-after-combat defender-path)]
              (log/info "player" (select-keys attacker [:id :hp :max-hp]))
              (log/info "defender" (select-keys defender [:race :hp]))
              (if (rp/player-dead? state)
                [:player-died ticks 0 (get attacker :max-hp)]
                (recur state-after-combat (inc ticks))))))))))

(defn mean-ticks-to-kill [coll]
  (float (/ (reduce + (map second coll)) (count coll))))

 
(defn std-dev-ticks-to-kill [samples]
  (let [n       (count samples)
        samples (map second samples)
        mean    (/ (reduce + samples) n)
        intermediate (map #(Math/pow (- %1 mean) 2) samples)]
          (Math/sqrt 
           (/ (reduce + intermediate) n))))    

(defn mean-health-ratio [samples]
  (float (/ (reduce + (map (fn [[_ _ hp max-hp :as sample]]
                             (/ hp max-hp))
                           samples))
            (count samples))))
 
(defn std-dev-health-ratio [samples]
  (let [n       (count samples)
        samples (map (fn [[_ _ hp max-hp]]
                       (float (/ hp max-hp)))
                      samples)
        mean    (/ (reduce + samples) n)
        intermediate (map #(Math/pow (- %1 mean) 2) samples)]
          (Math/sqrt 
           (/ (reduce + intermediate) n))))    

(defn float->str
  [f]
  (format "%3f" f))

(defn -main [& more]
  (let [player (rp/gen-player [] {:x 0 :y 0})
        level->land-monster-ids mg/land-monster-ids-by-level
        level->water-monster-ids mg/water-monster-ids-by-level]
    (log/set-level! :error)
    (clojure.pprint/print-table 
      (map (fn [[level monster-id]]
             (let [samples (for [i (range 2000)]
                             (do-combat player (assoc (mg/id->monster monster-id)
                                                      :pos {:x 1 :y 0}
                                                      :inventory [])))]
               {:monster-id            monster-id
                :dng-level             level
                :mean-ticks-to-kill    (float->str (mean-ticks-to-kill samples))
                :std-dev-ticks-to-kill (float->str (std-dev-ticks-to-kill samples))
                :player-win-ratio      (float->str
                                         (float
                                           (/ (count (filter (fn [[reason _ _ _]] (= reason :npc-died))
                                                       samples))
                                              (count samples))))
                :player-mean-health-ratio (float->str (mean-health-ratio samples))
                :player-std-dev-health-ratio (float->str (std-dev-health-ratio samples))}))
           ;; only simulate first [level monster-id] found
           ;; eg a [1 :rat] will be simulated but a subsequent [3 :rat] would be skipped.
           (reduce (fn [coll [level monster-id]]
                     (if (contains? (set (map second coll)) monster-id)
                       coll
                       (conj coll [level monster-id])))
                   []
                   ;; order monsters by level ascending
                   (sort-by first
                            ;; get list of [level monster-id]
                            (reduce-kv (fn [m k v] (apply conj m (map (fn [v] [k v]) v))) [] level->land-monster-ids)))))))
