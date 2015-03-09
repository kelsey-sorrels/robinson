;; Functions that manipulate state to do what the user commands.
(ns robinson.combat
  (:require #+clj
            [robinson.macros :as rm]
            [robinson.random :as rr]
            [robinson.world :as rw]
            [robinson.player :as rp]
            [robinson.itemgen :as ig]
            [robinson.monstergen :as mg]
            #+clj
            [taoensso.timbre :as log]
            #+cljs
            [shodan.console :as log :include-macros true]
            #+cljs
            [goog.string :as gstring]
            #+cljs
            [goog.string.format])
  #+cljs
  (:require-macros [robinson.macros :as rm]))


(defn sharp-weapon?
  [attack]
  (contains? #{:spear :axe :knife} attack))

(defn format [s & args]
  #+clj
  (apply clojure.string/format s args)
  #+cljs
  (apply gstring/format s args))

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
        rand-axe-verb      (fn [] (rr/rand-nth ["hit" "strike" "slash" "tear into" "cleave" "cut"]))]
    (rm/first-vec-match [attacker-race defender-race attack defender-body-part damage-type]
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
      [:*       :human :bite         :*        :miss] (format "The %s lunges at you its mouth but misses." attacker-name)
      [:*       :human :bite-venom   :*        :miss] (format "The %s snaps at you its mouth but misses." attacker-name)
      [:*       :human :claw         :*        :miss] (format "The %s claws at you and narrowly misses." attacker-name)
      [:*       :human :punch        :*        :miss] (format "The %s punches you but misses." attacker-name)
      [:*       :human :gore         :*        :miss] (format "The %s lunges at you with it's tusks." attacker-name)
      [:*       :human :sting        :*        :miss] (format "The %s tries to sting you but misses." attacker-name)
      [:*       :human :sting-venom  :*        :miss] (format "The %s tries to sting you but misses." attacker-name)
      [:*       :human :squeeze      :*        :miss] (format "The %s starts to constrict around you but fumbles." attacker-name)
      [:*       :human :clamp        :*        :miss] (format "The %s tries to clamp onto you but isn't fast enough." attacker-name)
      [:*       :human :spike        :*        :miss] (format "You almost get poked by the %s's spikes." attacker-name)
      [:*       :human :bite         :*        :hit]  (format "The %s sinks its teeth into your flesh." attacker-name)
      [:*       :human :bite-venom   :*        :hit]  (format "The %s buries its teeth into your body and starts pumping poison into you." attacker-name)
      [:*       :human :claw         :*        :hit]  (format "The %s claws into your flesh." attacker-name)
      [:*       :human :punch        :*        :hit]  (format "The %s punches you." attacker-name)
      [:*       :human :gore         :*        :hit]  (format "The %s gores into your body with it's tusks.`" attacker-name)
      [:*       :human :sting        :*        :hit]  (format "The %s jabs you with its stinger." attacker-name)
      [:*       :human :sting-venom  :*        :hit]  (format "The %s stings you, pumping you full of poison." attacker-name)
      [:*       :human :squeeze      :*        :hit]  (format "The %s squeezes you leaving you gasping for breath." attacker-name)
      [:*       :human :clamp        :*        :hit]  (format "The %s clamps down on your flesh crushing it." attacker-name)
      [:*       :human :spike        :*        :hit]  (format "The %s's spikes drive into your body." attacker-name))))

(defn attack-toughness
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
  :knife       5
  :saw         3
  :obsidian-knife 3
  :obsidian-axe 4
  :obsidian-spear 3
  :sharpened-stick 2
  #+clj
  (throw (Exception. (format "No value specified for %s" (name attack))))
  #+cljs
  (throw (js/Error. (format "No value specified for %s" (name attack))))))

(defn calc-dmg
  [attacker attack defender defender-body-part]
    ;;Damage = Astr * (Adex / Dsp) * (As / Ds) * (At / Dt)
    (let [attacker-strength  (get attacker :strength)
          attacker-dexterity (get attacker :dexterity 0.1)
          defender-speed     (get defender :speed)
          attacker-size      (get attacker :size)
          defender-size      (get defender :size)
          attack-toughness   (attack-toughness attack)
          defender-toughness (get defender :toughness)]
      (log/info "attacker-strength" attacker-strength
            "attacker-dexterity" attacker-dexterity
            "defender-speed" defender-speed
            "attacker-size" attacker-size
            "defender-size" defender-size
            "attack-toughness" attack-toughness
            "defender-toughnes" defender-toughness)
      (* attacker-strength (/ (+ 5 attacker-dexterity) (+ 5 defender-speed)) (/ (+ 25 attacker-size) (+ 25 defender-size)) (/ attack-toughness defender-toughness))))

(defn attack
  "Perform combat. The attacker fights the defender, but not vice-versa.
   Return a new state reflecting combat outcome."
  ([state attacker-path defender-path]
  {:pre [(every? (set (keys (get-in state attacker-path))) [:attacks])]}
   (let [attacker           (get-in state attacker-path)
         attack-type (or (get (first (filter (fn [item] (contains? item :wielded))
                                     (get-in state (conj attacker-path :inventory) [])))
                              :attack)
                         (rr/rand-nth (vec (get attacker :attacks))))]
     (attack state attacker-path defender-path attack-type)))
  ([state attacker-path defender-path attack]
  {:pre [(vector? attacker-path)
         (vector? defender-path)
         (every? (set (keys (get-in state defender-path))) [:hp :pos :race :body-parts :inventory])
         (vector? (get-in state [:world :npcs]))]
   :post [(vector? (get-in % [:world :npcs]))]}
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
                               (ig/arrow-poison-tipped? thrown-item))
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
        hit-or-miss          (rr/rand-nth (concat (repeat (get attacker :speed) :hit)
                                               (repeat (get defender :speed) :miss)))
        dmg                  (cond
                               (= hit-or-miss :hit)
                                 (+ (calc-dmg attacker attack defender defender-body-part) (if shot-poisoned-arrow 1 0))
                               :else 0)
        is-wound             (> dmg 1.5)]
    (log/debug "attack" attacker-path "is attacking defender" defender-path)
    (log/debug "attacker-detail" attacker)
    (log/debug "defender-detail" defender)
    (log/debug "attack" attack)
    (log/debug "hit?" hit-or-miss)
    (log/debug "dmg" dmg)
    (cond
      ;; defender still alive?
      (pos? (- hp dmg))
        (-> state
          ;; modify defender hp
          (update-in (conj defender-path :hp)
            (fn [hp] (- hp dmg)))
          ;; attacks use wielded weapons
          (arg-when-> [state] attack-item
            (dec-item-utility (get attack-item :hotkey)))
          ;; awaken player if attacked while sleeping
          (arg-when-> [state] (and (contains? (set defender-path) :player)
                                   (= (current-state state) :sleep))
            (assoc-in [:world :current-state] :normal))
          ;; provoke temperamental animal
          (arg-when-> [state] (= (get-in [state] (conj defender-path :temperament)) :hostile-after-attacked)
            (->
              (update-in (conj defender-path :status)         (fn [state] (conj state :hostile)))
              (assoc-in (conj defender-path :movement-policy) :follow-player-in-range-or-random)))
          (arg-when-> [state] (= (get-in [state] (conj defender-path :temperament)) :retreat-after-attacked)
            (->
              (update-in (conj defender-path :status)         (fn [state] (conj state :hostile)))
              (assoc-in (conj defender-path :movement-policy) :hide-from-player-in-range-or-random)))
          (rw/append-log (gen-attack-message attacker
                                          defender
                                          attack
                                          defender-body-part
                                          hit-or-miss)
                      (case hit-or-miss
                        :hit :red
                        :miss :white))
          ;; chance of being envenomed by venomous attacks
          (update-in (conj defender-path :status) (fn [status] (if (and (re-find #"venom" (str attack))
                                                                        (= (uniform-int 10) 0))
                                                                 (conj status :poisioned)
                                                                 status)))
          ;; chance of being wounded
          (update-in defender-path (fn [defender] (if (and is-wound
                                                           (contains? defender :wounds))
                                                    (update-in defender [:wounds]
                                                      (fn [wounds] (merge-with (fn [w0 w1] {:time (max (get w0 :time) (get w1 :time))
                                                                                            :dmg  (+   (get w0 :dmg)  (get w1 :dmg))})
                                                                     wounds
                                                                     {defender-body-part {:time (get-in state [:world :time])
                                                                                    :dmg dmg}})))
                                                    defender)))
          ((fn [state] (if (and is-wound
                                (contains? (set defender-path) :player))
                         (rw/append-log state "You have been wounded." :red)
                         state))))
      ;; defender dead? (0 or less hp)
      :else
        (if (contains? (set defender-path) :npcs)
          ;; defender is npc
          (-> state
            ;; update stats and will-to-live
            (update-npc-killed defender attack)
            ;; remove defender
            (remove-in (butlast defender-path) (partial = defender))
            ;; maybe add corpse
            (update-cell-items x y
              (fn [items]
                (if (> (next-float! *rnd*) 0.2)
                  (conj items (ig/gen-corpse defender))
                  items)))
            (rw/append-log (gen-attack-message attacker
                                            defender
                                            attack
                                            defender-body-part
                                            :dead)
                        :white))
          ;; defender is player
          (-> state
            (assoc-in [:world :cause-of-death] (format "%s %s %s" (noun->indefinite-article (get attacker :name))
                                                                 (get attacker :name)
                                                                 (name attack)))
            (kill-player)
            (update-player-died :combat)))))))

(defn -main [& more]
  (let [player {:id :player
                :race :human
                :dexterity 1
                :speed 1
                :size 75
                :strength  2
                :toughness 2
                :hp 10
                :max-hp 10
                :body-parts #{:head :neck :face :abdomen :arm :leg :foot}
                :attacks #{:punch}}
        level->land-monster-ids {
                          0 [:red-frog
                             :orange-frog
                             :yellow-frog
                             :green-frog
                             :blue-frog
                             :purple-frog
                             :bird
                             :gecko]
                          1 [:rat
                             :mosquito]
                          2 [:spider
                             :centipede]
                          3 [:tarantula
                             :scorpion]
                          4 [:cobra
                             :snake]
                          5 [:bat
                             :turtle]
                          6 [:monitor-lizard
                             :crocodile]
                          7 [:parrot
                             :mongoose]
                          8 [:komodo-dragon]
                          9 [:boar
                             :monkey]}
        level->water-monster-ids {
                          0 [:clam
                             :hermit-crab]
                          1 [:jellyfish]
                          2 [:fish]
                          3 [:crab]
                          4 [:urchin]
                          5 [:sea-snake]
                          6 [:puffer-fish]
                          7 [:electric-eel]
                          8 [:octopus ]
                          9 [:squid
                             :shark]}
        level->land-monsters  (map (fn [[level ids]] [level (map mg/id->monster ids)]) level->land-monster-ids)
        level->water-monsters (map (fn [[level ids]] [level (map mg/id->monster ids)]) level->water-monster-ids)
        land-monsters         (sort-by first (mapcat (fn [[k vals]] (map (fn [v] [k v]) vals)) level->land-monsters))
        water-monsters        (sort-by first (mapcat (fn [[k vals]] (map (fn [v] [k v]) vals)) level->water-monsters))
        land-data             (map (fn [[level monster]]
                                     (let [attacker->defender-dmg (calc-dmg
                                                 player :knife monster (rand-nth (vec (get monster :body-parts))))
                                           defender->attacker-dmg (calc-dmg
                                                 monster (rand-nth (vec (get monster :attacks)))
                                                 player (rand-nth (vec (get player :body-parts))))
                                           hits-to-kill-defender (float (/ (get monster :hp) attacker->defender-dmg))
                                           hits-to-kill-attacker (float (/ (get player :hp) defender->attacker-dmg))
                                           winner (if (< hits-to-kill-attacker hits-to-kill-defender)
                                                    "defender" "attacker")]
                                       {:attacker "player"
                                        :attack "punch"
                                        :defender (get monster :name)
                                        :level level
                                        :attacker-damage (format "%.2f" attacker->defender-dmg)
                                        :hits-to-kill-defender (format "%.2f" hits-to-kill-defender)
                                        :defender-damage (format "%.2f" defender->attacker-dmg)
                                        :hits-to-kill-attacker (format "%.2f" hits-to-kill-attacker)
                                        :winner winner
                                        :difficulty (format "%.2f" (float (/ hits-to-kill-defender hits-to-kill-attacker)))}))
                                   land-monsters)]
    (print-table [:attacker :attack :defender :level :attacker-damage :hits-to-kill-defender
                  :defender-damage :hits-to-kill-attacker :winner :difficulty] land-data)))

