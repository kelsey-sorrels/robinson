;; Functions that manipulate state to do what the user commands.)
(ns robinson.combat
  (:require [robinson.common :as rc]
            [robinson.random :as rr]
            [robinson.world :as rw]
            [robinson.player :as rp]
            [robinson.npc :as rnpc]
            [robinson.itemgen :as ig]
            [robinson.monstergen :as mg]
            [robinson.math :as rmath]
            [robinson.characterevents :as ce]
            [robinson.dynamiccharacterproperties :as dcp]
            [taoensso.timbre :as log]
            #?@(:clj (
                [robinson.macros :as rm]
                [clojure.stacktrace :refer [print-stack-trace]]
                [clojure.pprint :refer [pprint]])
                :cljs (
                [robinson.macros :as rm :include-macros true]
                [goog.string :as gstring]
                [goog.string.format])))
  #?(:clj
     (:import robinson.dynamiccharacterproperties.DynamicCharacterProperties)))


(defn sharp-weapon?
  [attack]
  (contains? #{:spear :axe :knife} attack))

(defn format [s & args]
  #?(:clj
     (apply clojure.core/format s args)
     :cljs
     (apply gstring/format s args)))

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
  (let [attacker-race      (get attacker :race)
        defender-race      (get defender :race)
        attacker-name      (get attacker :name)
        defender-name      (get defender :name)
        rand-punch-verb    (fn [] (rr/rand-nth ["wack" "punch" "hit" "pummel" "batter"
                                             "pound" "beat" "strike" "slug"]))
        rand-axe-verb      (fn [] (rr/rand-nth ["hit" "strike" "slash" "tear into" "cleave" "cut"]))
        _                  (log/debug "gen-attack-messsage first-vec-match" attacker-race defender-race attack defender-body-part damage-type)
        msg (rm/first-vec-match [attacker-race defender-race attack defender-body-part damage-type]
              [:human :*       :punch        :*        :miss] (format "You punch the %s but miss." defender-name)
              [:human :*       :punch        :*        :hit]  (rr/rand-nth [(format "You %s the %s %s %s the %s."
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
              [:human :*       :punch        :head     :dead] (format "You %s the %s in the head. Brains fly everywhere and it dies." (rand-punch-verb) defender-name)
              [:human :*       :punch        :neck     :dead] (format "You %s the %s in the neck snapping it and it dies." (rand-punch-verb) defender-name)
              [:human :*       :punch        :body     :dead] (format "You %s the %s in the body damaging internal organs. It dies." (rand-punch-verb) defender-name)
              [:human :*       :punch        :leg      :dead] (format "You %s the %s in the leg severing it and it dies." (rand-punch-verb) defender-name)
              [:human :*       :punch        :face     :dead] (format "You %s the %s in the face. Peices of face fly everywhere and it dies." (rand-punch-verb) defender-name)
              [:human :*       :punch        :abdomen  :dead] (format "You %s the %s in the abdomen. Internal organs fly everywhere and it dies." (rand-punch-verb) defender-name)
              [:human :*       :punch        :claw     :dead] (format "You %s the %s in the claw and it dies." (rand-punch-verb) defender-name)
              [:human :*       :punch        :tail     :dead] (format "You %s the %s in the tail causing massive injuries and it dies." (rand-punch-verb) defender-name)
              [:human :*       :punch        :wing     :dead] (format "You %s the %s in the wing ripping it clean off and it dies." (rand-punch-verb) defender-name)
              [:human :*       :punch        :eye      :dead] (format "You %s the %s in the eye exploding it upon impact and it dies." (rand-punch-verb) defender-name)
              [:human :*       :punch        :snout    :dead] (format "You %s the %s in the snount crushing it and it dies." (rand-punch-verb) defender-name)
              [:human :*       :punch        :arm      :dead] (format "You %s the %s in the arm crushing bones and it dies." (rand-punch-verb) defender-name)
              [:human :*       :punch        :beak     :dead] (format "You %s the %s in the beak ripping it from its face and it dies." (rand-punch-verb) defender-name)
              [:human :*       :punch        :shell    :dead] (format "You %s the %s in the shell ripping to peices and it dies." (rand-punch-verb) defender-name)
              [:human :*       :punch        :tentacle :dead] (format "You %s the %s in the tentacle shredding it and it dies." (rand-punch-verb) defender-name)
              [:human :*       :punch        :*        :dead] (format "You %s the %s causing massive injuries and it dies." (rand-punch-verb) defender-name)
              [:human :*       sharp-weapon? :*        :miss] (format "You swing at the %s but miss." defender-name)
              [:human :*       sharp-weapon? :*        :hit]  (rr/rand-nth [(format "You %s the %s %s %s the %s."
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
              [:human :*       sharp-weapon? :head     :dead] (format "You %s the %s in the head. Brains fly everywhere and it dies." (rand-axe-verb) defender-name)
              [:human :*       sharp-weapon? :neck     :dead] (format "You %s the %s in the neck snapping it and it dies." (rand-axe-verb) defender-name)
              [:human :*       sharp-weapon? :body     :dead] (format "You %s the %s in the body damaging internal organs. It dies." (rand-axe-verb) defender-name)
              [:human :*       sharp-weapon? :leg      :dead] (format "You %s the %s in the leg severing it and it dies." (rand-axe-verb) defender-name)
              [:human :*       sharp-weapon? :face     :dead] (format "You %s the %s in the face. Peices of face fly everywhere and it dies." (rand-axe-verb) defender-name)
              [:human :*       sharp-weapon? :abdomen  :dead] (format "You %s the %s in the abdomen. Internal organs fly everywhere and it dies." (rand-axe-verb) defender-name)
              [:human :*       sharp-weapon? :claw     :dead] (format "You %s the %s in the claw and it dies." (rand-axe-verb) defender-name)
              [:human :*       sharp-weapon? :tail     :dead] (format "You %s the %s in the tail causing massive injuries and it dies." (rand-axe-verb) defender-name)
              [:human :*       sharp-weapon? :wing     :dead] (format "You %s the %s in the wing ripping it clean off and it dies." (rand-axe-verb) defender-name)
              [:human :*       sharp-weapon? :eye      :dead] (format "You %s the %s in the eye exploding it upon impact and it dies." (rand-axe-verb) defender-name)
              [:human :*       sharp-weapon? :snout    :dead] (format "You %s the %s in the snount crushing it and it dies." (rand-axe-verb) defender-name)
              [:human :*       sharp-weapon? :arm      :dead] (format "You %s the %s in the arm crushing bones it and it dies." (rand-axe-verb) defender-name)
              [:human :*       sharp-weapon? :beak     :dead] (format "You %s the %s in the beak ripping it from its face and it dies." (rand-axe-verb) defender-name)
              [:human :*       sharp-weapon? :shell    :dead] (format "You %s the %s in the shell ripping to peices and it dies." (rand-axe-verb) defender-name)
              [:human :*       sharp-weapon? :tentacle :dead] (format "You %s the %s in the tentacle shredding it and it dies." (rand-axe-verb) defender-name)
              [:human :*       sharp-weapon? :*        :dead] (format "You %s the %s causing massive injuries and it dies." (rand-axe-verb) defender-name)
              [:*     :human   :bite         :*        :miss] (format "The %s lunges at you its mouth but misses." attacker-name)
              [:*     :human   :bite-venom   :*        :miss] (format "The %s snaps at you its mouth but misses." attacker-name)
              [:*     :human   :claw         :*        :miss] (format "The %s claws at you and narrowly misses." attacker-name)
              [:*     :human   :punch        :*        :miss] (format "The %s punches you but misses." attacker-name)
              [:*     :human   :gore         :*        :miss] (format "The %s lunges at you with it's tusks." attacker-name)
              [:*     :human   :sting        :*        :miss] (format "The %s tries to sting you but misses." attacker-name)
              [:*     :human   :sting-venom  :*        :miss] (format "The %s tries to sting you but misses." attacker-name)
              [:*     :human   :squeeze      :*        :miss] (format "The %s starts to constrict around you but fumbles." attacker-name)
              [:*     :human   :clamp        :*        :miss] (format "The %s tries to clamp onto you but isn't fast enough." attacker-name)
              [:*     :human   :spike        :*        :miss] (format "You almost get poked by the %s's spikes." attacker-name)
              [:*     :human   :bite         :*        :hit]  (format "The %s sinks its teeth into your flesh." attacker-name)
              [:*     :human   :bite-venom   :*        :hit]  (format "The %s buries its teeth into your body and starts pumping poison into you." attacker-name)
              [:*     :human   :claw         :*        :hit]  (format "The %s claws into your flesh." attacker-name)
              [:*     :human   :punch        :*        :hit]  (format "The %s punches you." attacker-name)
              [:*     :human   :gore         :*        :hit]  (format "The %s gores into your body with it's tusks.`" attacker-name)
              [:*     :human   :sting        :*        :hit]  (format "The %s jabs you with its stinger." attacker-name)
              [:*     :human   :sting-venom  :*        :hit]  (format "The %s stings you, pumping you full of poison." attacker-name)
              [:*     :human   :squeeze      :*        :hit]  (format "The %s squeezes you leaving you gasping for breath." attacker-name)
              [:*     :human   :clamp        :*        :hit]  (format "The %s clamps down on your flesh crushing it." attacker-name)
              [:*     :human   :spike        :*        :hit]  (format "The %s's spikes drive into your body." attacker-name)
              [:*     :*       :*            :*        :*  ]  (format "The %s hits you." attacker-name))]
     (log/debug "attack message" msg)
     msg))

(defn attack->toughness
  [attack]
  (case attack
  :bite        5
  :bite-venom  5
  :clamp       3
  :claw        5
  :gore        4
  :punch       1
  :spike      10
  :squeeze     3
  :shot-arrow 10
  :sting-venom 8
  :thrown-item 1
  :knife      15
  :saw         3
  :obsidian-knife 3
  :obsidian-axe 4
  :obsidian-spear 3
  :sharpened-stick 2
  #?(:clj
     (throw (Exception. (format "No value specified for %s" (name attack))))
     :cljs
     (throw (js/Error. (format "No value specified for %s" (name attack)))))))

(defn is-hit? [state attacker defender]
  (let [attacker-speed   (dcp/get-speed attacker state)
        defender-speed   (dcp/get-speed defender state)
        target-value     (/ 1 (inc (rmath/exp (/ (- defender-speed attacker-speed) 4))))]
    (log/info "hit target value"  target-value)
    (> (rr/uniform-double 0.2 1.0) target-value)))

(defn calc-dmg
  [state attacker attack defender defender-body-part]
    #_(log/info "Attacker" attacker "attacker-type" (type attacker) "Defernder" defender "defender-type" (type defender))
    (log/info "attacker" (:race attacker) "defender" (:race defender))
    ;;Damage = Astr * (Adex / Dsp) * (As / Ds) * (At / Dt)
    (let [attacker-strength  (dcp/get-strength attacker state)
          attacker-dexterity (dcp/get-dexterity attacker state)
          defender-speed     (dcp/get-speed defender state)
          attacker-size      (dcp/get-size attacker state)
          defender-size      (dcp/get-size defender state)
          attack-toughness   (attack->toughness attack)
          defender-toughness (dcp/get-toughness defender state)]
      (log/info "attacker-strength" attacker-strength)
      (log/info "attacker-dexterity" attacker-dexterity)
      (log/info "defender-speed" defender-speed)
      (log/info "attacker-size" attacker-size)
      (log/info "defender-size" defender-size)
      (log/info "attack-toughness" attack-toughness)
      (log/info "defender-toughnes" defender-toughness)
      (assert attacker-strength)
      (assert attacker-dexterity)
      (assert defender-speed)
      (assert defender-size)
      (assert attack-toughness  (str "attack" attack))
      (assert defender-toughness)
      (* attacker-strength
         (/ (+ 5 (rr/uniform-double (* 10 attacker-dexterity))) (+ 15 defender-speed))
         (/ (+ 125 attacker-size) (+ 125 defender-size))
         (/ attack-toughness defender-toughness))))



(defmacro log-with-line [v msg]
  `(do (log/info
               ~*file*
               ":"
               ~(:line (meta &form))
               ">"
               ~msg)
       (flush)
       (assert (some? ~v))
       ~v))

(defn assert-msg
  [v msg]
  (assert v msg)
  v)

(defn attack
  "Perform combat. The attacker fights the defender, but not vice-versa.
   Return a new state reflecting combat outcome."
  ([state attacker-path defender-path]
  {:pre [(every? (set (keys (get-in state attacker-path))) [:attacks])
         (some? state)]}
   (let [attacker           (get-in state attacker-path)
         attack-type (or (get (first (filter (fn [item] (contains? item :wielded))
                                     (get-in state (conj attacker-path :inventory) [])))
                              :attack)
                         (rr/rand-nth (vec (get attacker :attacks))))]
     (attack state attacker-path defender-path attack-type)))
  ([state attacker-path defender-path attack]
  {:pre [(vector? attacker-path)
         (vector? defender-path)
         (some? state)
         (every? (set (keys (get-in state defender-path))) [:hp :pos :race :body-parts :inventory])
         (vector? (get-in state [:world :npcs]))]
   :post [(some? %)
          (vector? (get-in % [:world :npcs]))]}
  (log/info "attacker-path" attacker-path "defender-path" defender-path)
  (let [defender             (get-in state defender-path)
        ;; 
        attacker             (get-in state attacker-path)
        attack-item          (rp/wielded-item attacker)
        bow-wielded          (= :bow
                                (let [item-id (get (or attack-item {}) :id)]
                                  (or item-id :non-bow)))
        thrown-item          (when-not (keyword? attack)
                               attack)
        shot-poisoned-arrow  (when thrown-item
                               (ig/arrow-poison-tipped? state thrown-item))
        attack               (cond
                               (keyword? attack)
                               attack
                               (and bow-wielded (= :arrow (get thrown-item :id)))
                               :shot-arrow
                               :else
                               :thrown-item)
        
        defender-body-part   (rr/rand-nth (vec (get defender :body-parts)))
        {x :x y :y}          (get defender :pos)
        hp                   (get defender :hp)
        hit                  (is-hit? state attacker defender)
        dmg                  (cond
                               hit   (+ (calc-dmg state attacker attack defender defender-body-part) (if shot-poisoned-arrow 1 0))
                               :else 0)
        is-wound             (> dmg 1.5)]
    (log/info "attack" attacker-path "is attacking defender" defender-path)
    #_(log/info "attacker-detail" attacker)
    #_(log/info "defender-detail" defender)
    (log/info "attack" attack)
    (log/info "hit?" hit)
    (log/info "hp" hp)
    (log/info "max-hp" (if (= (get defender :race) :human)
                          (get defender :max-hp)
                          (get (mg/gen-monster (get defender :race)) :hp)))
    (log/debug "dmg" dmg)
    (try
      (cond
        ;; defender still alive?
        (pos? (- hp dmg))
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
            (if attack-item
              (rp/dec-item-utility state (get attack-item :hotkey))
              state)
            (log-with-line state "2")
            ;; awaken player if attacked while sleeping
            (if (and (contains? (set defender-path) :player)
                     (= (rw/current-state state) :sleep))
              (assoc-in state [:world :current-state] :normal)
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
            (let [msg (gen-attack-message attacker
                                          defender
                                          attack
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
            (update-in state (conj defender-path :status) (fn [status] (if (and (re-find #"venom" (str attack))
                                                                                (= (rr/uniform-int 10) 0))
                                                                         (conj status :poisioned)
                                                                         status)))
            ;; chance of being paralyzed by frog
            (if (and (= (get defender :id) :player)
                     (mg/is-poisonous-frog? state (get attacker :race)))
              (rp/assoc-player-attribute :paralyzed-start-time (inc (rw/get-time state)))
              state)

            (log-with-line state "7")
            ;; chance of being wounded
            (update-in state defender-path (fn [defender] (if (and is-wound
                                                                   (contains? defender :wounds))
                                                            (update-in defender [:wounds]
                                                              (fn [wounds] (merge-with (fn [w0 w1] {:time (max (get w0 :time) (get w1 :time))
                                                                                                    :dmg  (+   (get w0 :dmg)  (get w1 :dmg))})
                                                                             wounds
                                                                             {defender-body-part {:time (rw/get-time state)
                                                                                            :dmg dmg}})))
                                                            defender)))
            (log-with-line state "8")
            (if (and is-wound
                     (contains? (set defender-path) :player))
              (rc/append-log state "You have been wounded." :red)
              state)
            (log-with-line state "9")
            (ce/on-hit defender state)
            (log-with-line state "10")
            (if (contains? (set attacker-path) :player)
              (rp/update-npc-attacked state defender attack)
              state))
        ;; defender dead? (0 or less hp)
        :else
          (if (contains? (set defender-path) :npcs)
            ;; defender is npc
            (-> state
              ;; update stats and will-to-live
              (rp/update-npc-killed defender attack)
              ;; trigger on-death script for defender
              ((partial ce/on-death defender))
              ;; remove defender
              (rc/remove-in (butlast defender-path) (partial = defender))
              ;; maybe add corpse
              (rw/update-cell-items x y
                (fn [items]
                  (if (> (rr/next-float! rr/*rnd*) 0.2)
                    (conj items (ig/gen-corpse defender))
                    items)))
              (rc/append-log (gen-attack-message attacker
                                              defender
                                              attack
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
              (rp/update-player-died :combat)))))
      (catch Exception ex
        (log/error "Caught exception while doing combat" ex)
        (print-stack-trace ex))
      (finally
        (log/info "End of attack"))))))

(defn do-combat
  [attacker defender]
  (let [state {:current-state :normal
               :world         {:npcs [defender]
                               :player attacker
                               :time 0}}
        knife  (assoc (ig/gen-item :knife) :wielded true)
        #_#_state         (rp/add-to-inventory state [knife])
        attacker-path [:world :player]
        defender-path [:world :npcs 0]
        pass-through  (fn [m & _] m)]
    (with-redefs [rw/update-cell       pass-through
                  rw/get-time          (fn [state] 1)
                  blood-splatter       pass-through
                  rp/dec-item-utility  pass-through
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
           (sort-by first (reduce-kv (fn [m k v] (apply conj m (map (fn [v] [k v]) v))) [] level->land-monster-ids))))))
