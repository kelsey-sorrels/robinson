;; Functions that manipulate state to do what the user commands.
(ns robinson.combat
  (:use     
    clojure.pprint
    robinson.common
    robinson.itemgen)
  (:require clojure.pprint
            clojure.contrib.core
            [clojure.stacktrace :as st]
            [taoensso.timbre :as timbre]))

(timbre/refer-timbre)

(defn- vec-match?
  [v0 v1]
  (let [arg-match? (fn [[arg0 arg1]]
    (cond
      (fn? arg0)  (arg0 arg1)
      (= :* arg0) true
      (set? arg0) (contains? arg0 arg1)
      :else       (= arg0 arg1)))]
  (every? arg-match? (map vector v0 v1))))

(defn- gen-attack-message
  "Logs an attack message to the global state.
   `attack` is one of :bite :claws :punch.
   `damage-type` is one of :miss :hit :dead"
  [attacker defender attack defender-body-part damage-type]
  (let [attacker-name      (get attacker :name)
        defender-name      (get defender :name)
        rand-punch-verb    (fn [] (rand-nth ["wack" "punch" "hit" "pummel" "batter"
                                             "pound" "beat" "strike" "slug"]))
        rand-axe-verb      (fn [] (rand-nth ["hit" "strike" "slash" "tear into" "cleave" "cut"]))]
    (condp vec-match? [attacker-name defender-name attack defender-body-part damage-type]
      ["Player" :*       :punch :*       :miss] (format "You punch the %s but miss." defender-name)
      ["Player" :*       :punch :*       :hit]  (rand-nth [(format "You %s the %s %s %s the %s."
                                                             (rand-punch-verb)
                                                             defender-name
                                                             (rand-nth ["solidly" "swiftly" "repeatedly"
                                                                        "perfectly" "competently"])
                                                             (rand-nth ["on" "in" "across"])
                                                             (name defender-body-part))
                                                           (format "You %s the %s %s the %s."
                                                             (rand-punch-verb)
                                                             defender-name
                                                             (rand-nth ["on" "in" "across"])
                                                             (name defender-body-part))
                                                           (format "You %s the %s."
                                                             (rand-punch-verb)
                                                             defender-name)])
      ["Player" :*       :punch       :head     :dead] (format "You %s the %s in the head. Brains fly everywhere and it dies." (rand-punch-verb) defender-name)
      ["Player" :*       :punch       :neck     :dead] (format "You %s the %s in the neck snapping it and it dies." (rand-punch-verb) defender-name)
      ["Player" :*       :punch       :body     :dead] (format "You %s the %s in the body damaging internal organs. It dies." (rand-punch-verb) defender-name)
      ["Player" :*       :punch       :leg      :dead] (format "You %s the %s in the leg severing it and it dies." (rand-punch-verb) defender-name)
      ["Player" :*       :punch       :face     :dead] (format "You %s the %s in the face. Peices of face fly everywhere and it dies." (rand-punch-verb) defender-name)
      ["Player" :*       :punch       :abdomen  :dead] (format "You %s the %s in the abdomen. Internal organs fly everywhere and it dies." (rand-punch-verb) defender-name)
      ["Player" :*       :punch       :claw     :dead] (format "You %s the %s in the claw and it dies." (rand-punch-verb) defender-name)
      ["Player" :*       :punch       :tail     :dead] (format "You %s the %s in the tail causing massive injuries and it dies." (rand-punch-verb) defender-name)
      ["Player" :*       :punch       :wing     :dead] (format "You %s the %s in the wing ripping it clean off and it dies." (rand-punch-verb) defender-name)
      ["Player" :*       :punch       :eye      :dead] (format "You %s the %s in the eye exploding it upon impact and it dies." (rand-punch-verb) defender-name)
      ["Player" :*       :punch       :snout    :dead] (format "You %s the %s in the snount crushing it and it dies." (rand-punch-verb) defender-name)
      ["Player" :*       :punch       :arm      :dead] (format "You %s the %s in the arm crushing bones it and it dies." (rand-punch-verb) defender-name)
      ["Player" :*       :punch       :beak     :dead] (format "You %s the %s in the beak ripping it from its face and it dies." (rand-punch-verb) defender-name)
      ["Player" :*       :punch       :shell    :dead] (format "You %s the %s in the shell ripping to peices and it dies." (rand-punch-verb) defender-name)
      ["Player" :*       :punch       :tentacle :dead] (format "You %s the %s in the tentacle shredding it and it dies." (rand-punch-verb) defender-name)
      ["Player" :*       :punch       :*        :dead] (format "You %s the %s causing massive injuries and it dies." (rand-punch-verb) defender-name)
      ["Player" :*       :axe :*       :miss] (format "You swing at the %s but miss." defender-name)
      ["Player" :*       :axe :*       :hit]  (rand-nth [(format "You %s the %s %s %s the %s."
                                                             (rand-axe-verb)
                                                             defender-name
                                                             (rand-nth ["solidly" "swiftly"
                                                                        "perfectly" "competently"])
                                                             (rand-nth ["on" "in" "across"])
                                                             (name defender-body-part))
                                                           (format "You %s the %s %s the %s."
                                                             (rand-axe-verb)
                                                             defender-name
                                                             (rand-nth ["on" "in" "across"])
                                                             (name defender-body-part))
                                                           (format "You %s the %s."
                                                             (rand-axe-verb)
                                                             defender-name)])
      ["Player" :*       :axe         :head     :dead] (format "You %s the %s in the head. Brains fly everywhere and it dies." (rand-axe-verb) defender-name)
      ["Player" :*       :axe         :neck     :dead] (format "You %s the %s in the neck snapping it and it dies." (rand-axe-verb) defender-name)
      ["Player" :*       :axe         :body     :dead] (format "You %s the %s in the body damaging internal organs. It dies." (rand-axe-verb) defender-name)
      ["Player" :*       :axe         :leg      :dead] (format "You %s the %s in the leg severing it and it dies." (rand-axe-verb) defender-name)
      ["Player" :*       :axe         :face     :dead] (format "You %s the %s in the face. Peices of face fly everywhere and it dies." (rand-axe-verb) defender-name)
      ["Player" :*       :axe         :abdomen  :dead] (format "You %s the %s in the abdomen. Internal organs fly everywhere and it dies." (rand-axe-verb) defender-name)
      ["Player" :*       :axe         :claw     :dead] (format "You %s the %s in the claw and it dies." (rand-axe-verb) defender-name)
      ["Player" :*       :axe         :tail     :dead] (format "You %s the %s in the tail causing massive injuries and it dies." (rand-axe-verb) defender-name)
      ["Player" :*       :axe         :wing     :dead] (format "You %s the %s in the wing ripping it clean off and it dies." (rand-axe-verb) defender-name)
      ["Player" :*       :axe         :eye      :dead] (format "You %s the %s in the eye exploding it upon impact and it dies." (rand-axe-verb) defender-name)
      ["Player" :*       :axe         :snout    :dead] (format "You %s the %s in the snount crushing it and it dies." (rand-axe-verb) defender-name)
      ["Player" :*       :axe         :arm      :dead] (format "You %s the %s in the arm crushing bones it and it dies." (rand-axe-verb) defender-name)
      ["Player" :*       :axe         :beak     :dead] (format "You %s the %s in the beak ripping it from its face and it dies." (rand-axe-verb) defender-name)
      ["Player" :*       :axe         :shell    :dead] (format "You %s the %s in the shell ripping to peices and it dies." (rand-axe-verb) defender-name)
      ["Player" :*       :axe         :tentacle :dead] (format "You %s the %s in the tentacle shredding it and it dies." (rand-axe-verb) defender-name)
      ["Player" :*       :axe         :*        :dead] (format "You %s the %s causing massive injuries and it dies." (rand-axe-verb) defender-name)
      [:*       "Player" :bite        :*        :miss] (format "The %s lunges at you its mouth but misses." (rand-punch-verb) attacker-name)
      [:*       "Player" :bite-venom  :*        :miss] (format "The %s snaps at you its mouth but misses." (rand-punch-verb) attacker-name)
      [:*       "Player" :claw        :*        :miss] (format "The %s claws at you and narrowly misses." (rand-punch-verb) attacker-name)
      [:*       "Player" :punch       :*        :miss] (format "The %s punches you but misses." attacker-name)
      [:*       "Player" :sting       :*        :miss] (format "The %s tries to sting you but misses." attacker-name)
      [:*       "Player" :sting-venom :*        :miss] (format "The %s tries to sting you but misses." attacker-name)
      [:*       "Player" :squeeze     :*        :miss] (format "The %s starts to constrict around you but fumbles." attacker-name)
      [:*       "Player" :clamp       :*        :miss] (format "The %s tries to clamp onto you but isn't fast enough." attacker-name)
      [:*       "Player" :spike       :*        :miss] (format "You almost get poked by the %'s spikes." attacker-name)
      [:*       "Player" :bite        :*        :hit]  (format "The %s sinks its teeth into your flesh." attacker-name)
      [:*       "Player" :bite-venom  :*        :hit]  (format "The %s buries its teeth into your body and starts pumping poison into you." attacker-name)
      [:*       "Player" :claw        :*        :hit]  (format "The %s claws into your flesh." attacker-name)
      [:*       "Player" :punch       :*        :hit]  (format "The %s punches you." attacker-name)
      [:*       "Player" :gore        :*        :hit]  (format "The %s gores into your body with it's tusks.`" attacker-name)
      [:*       "Player" :sting       :*        :hit]  (format "The %s jabs you with its stinger." attacker-name)
      [:*       "Player" :sting-venom :*        :hit]  (format "The %s stings you, pumping you full of poison." attacker-name)
      [:*       "Player" :squeeze     :*        :hit]  (format "The %s squeezes you leaving you gasping for breath." attacker-name)
      [:*       "Player" :clamp       :*        :hit]  (format "The %s clamps down on your flesh crushing it." attacker-name)
      [:*       "Player" :spike       :*        :hit]  (format "The %s's spikes drive into your body." attacker-name))))

(defn calc-dmg
  [attacker-race attack defender-race defender-body-part]
  (condp vec-match? [attacker-race attack defender-race defender-body-part]
    [:bat        :bite         :human      :abdomen]   (uniform-rand 0.5 1.5)
    [:bat        :bite         :human      :arm]       (uniform-rand 0.5 1.5)
    [:bat        :bite         :human      :face]      (uniform-rand 0.5 1.5)
    [:bat        :bite         :human      :foot]      (uniform-rand 0.5 1.5)
    [:bat        :bite         :human      :head]      (uniform-rand 0.5 1.5)
    [:bat        :bite         :human      :leg]       (uniform-rand 0.5 1.5)
    [:bat        :bite         :human      :neck]      (uniform-rand 0.9 1.9)
    [:bird       :bite         :human      :*]         (uniform-rand 0.5 0.9)
    [:bird       :claw         :human      :abdomen]   (uniform-rand 0.5 1.5)
    [:bird       :claw         :human      :face]      (uniform-rand 0.8 1.8)
    [:bird       :claw         :human      :neck]      (uniform-rand 0.7 1.7)
    [:bird       :claw         :human      :*]         (uniform-rand 0.5 1.5)
    [:boar       :bite         :human      :abdomen]   (uniform-rand 0.5 2.5)
    [:boar       :bite         :human      :arm]       (uniform-rand 0.5 3.5)
    [:boar       :bite         :human      :face]      (uniform-rand 0.5 2.5)
    [:boar       :bite         :human      :foot]      (uniform-rand 0.5 1.5)
    [:boar       :bite         :human      :head]      (uniform-rand 1.5 3.5)
    [:boar       :bite         :human      :leg]       (uniform-rand 0.9 1.8)
    [:boar       :bite         :human      :neck]      (uniform-rand 1.5 2.5)
    [:boar       :gore         :human      :abdomen]   (uniform-rand 2.5 4.5)
    [:boar       :gore         :human      :arm]       (uniform-rand 1.5 2.5)
    [:boar       :gore         :human      :face]      (uniform-rand 3.5 4.5)
    [:boar       :gore         :human      :foot]      (uniform-rand 1.5 2.5)
    [:boar       :gore         :human      :head]      (uniform-rand 3.5 4.5)
    [:boar       :gore         :human      :leg]       (uniform-rand 2.5 3.5)
    [:boar       :gore         :human      :neck]      (uniform-rand 3.5 4.5)
    [:centipede  :bite         :human      :*]         (uniform-rand 0.5 0.9)
    [:clam       :clamp        :human      :*]         (uniform-rand 0.1 0.3)
    [:fish       :bite         :human      :*]         (uniform-rand 0.2 1.7)
    [:frog       :claw         :human      :*]         (uniform-rand 0.1 0.5)
    [:gecko      :bite         :human      :*]         (uniform-rand 0.1 0.3)
    [:human      :punch        :bat        :body]      (uniform-rand 0.5 1.5)
    [:human      :punch        :bat        #{:face
                                             :head}]   (uniform-rand 0.8 1.8)
    [:human      :punch        :bat        :leg]       (uniform-rand 0.2 0.5)
    [:human      :punch        :bat        :wing]      (uniform-rand 0.8 1.8)
    [:human      :punch        :bird       :beak]      (uniform-rand 0.5 1.5)
    [:human      :punch        :bird       :body]      (uniform-rand 0.5 1.5)
    [:human      :punch        :bird       :head]      (uniform-rand 0.5 1.5)
    [:human      :punch        :bird       :leg]       (uniform-rand 0.5 1.0)
    [:human      :punch        :bird       :tail]      (uniform-rand 0.5 1.5)
    [:human      :punch        :bird       :wing]      (uniform-rand 1.5 2.5)
    [:human      :punch        :boar       :body]      (uniform-rand 0.5 1.5)
    [:human      :punch        :boar       :eye]       (uniform-rand 2.5 3.5)
    [:human      :punch        :boar       :face]      (uniform-rand 0.9 1.9)
    [:human      :punch        :boar       :head]      (uniform-rand 0.3 1.3)
    [:human      :punch        :boar       :leg]       (uniform-rand 0.7 1.7)
    [:human      :punch        :boar       :snout]     (uniform-rand 0.7 1.7)
    [:human      :punch        :boar       :tail]      (uniform-rand 0.1 1.1)
    [:human      :punch        :centipede  #{:body
                                             :head}]   (uniform-rand 0.9 1.9)
    [:human      :punch        :centipede  :leg]       (uniform-rand 0.1 0.5)
    [:human      :punch        :clam       :shell]     (uniform-rand 0.5 1.5)
    [:human      :punch        :fish       #{:body
                                             :head}]   (uniform-rand 0.5 1.5)
    [:human      :punch        :fish       #{:fin
                                             :tail}]   (uniform-rand 0.5 1.1)
    [:human      :punch        :frog       :*]         (uniform-rand 0.5 1.5)
    [:human      :punch        :gecko      :*]         (uniform-rand 0.5 1.5)
    [:human      :punch        :monkey     #{:arm
                                             :leg}]    (uniform-rand 0.5 1.5)
    [:human      :punch        :monkey     #{:body
                                             :face
                                             :head
                                             :neck}]   (uniform-rand 0.5 1.5)
    [:human      :punch        :monkey     :tail]      (uniform-rand 0.2 1.0)
    [:human      :punch        :octopus    #{:body
                                             :head}]   (uniform-rand 1.5 2.9)
    [:human      :punch        :octopus    :tentacle]  (uniform-rand 0.5 1.5)
    [:human      :punch        :parrot     #{:body
                                             :face
                                             :head}]   (uniform-rand 0.5 1.5)
    [:human      :punch        :parrot     #{:leg
                                             :tail}]   (uniform-rand 0.5 1.3)
    [:human      :punch        :parrot     :wing]      (uniform-rand 0.8 1.8)
    [:human      :punch        :rat        #{:body
                                             :face
                                             :head
                                             :neck}]   (uniform-rand 0.5 1.5)
    [:human      :punch        :rat        #{:leg
                                             :tail}]   (uniform-rand 0.5 1.2)
    [:human      :punch        :scorpion   #{:abdomen
                                             :head
                                             :tail}]   (uniform-rand 0.9 1.9)
    [:human      :punch        :scorpion   #{:claw
                                             :leg}]    (uniform-rand 0.5 1.5)
    [:human      :punch        :sea-snake  #{:body
                                             :head}]   (uniform-rand 0.9 1.9)
    [:human      :punch        :shark      #{:body
                                             :head
                                             :nose}]   (uniform-rand 0.5 1.5)
    [:human      :punch        :shark      #{:fin
                                             :tail}]   (uniform-rand 0.5 1.1)
    [:human      :punch        :snake      #{:body
                                             :head
                                             :tail}]   (uniform-rand 0.5 1.5)
    [:human      :punch        :spider     #{:abdomen
                                             :face}]   (uniform-rand 0.5 1.5)
    [:human      :punch        :spider     #{:leg}]    (uniform-rand 0.5 1.5)
    [:human      :punch        :squid      #{:body
                                             :head
                                             :tentacle}] (uniform-rand 0.5 1.5)
    [:human      :punch        :turtle     #{:body
                                             :face
                                             :head
                                             :neck}]   (uniform-rand 0.5 1.5)
    [:human      :punch        :turtle     :leg]       (uniform-rand 0.5 1.5)
    [:human      :punch        :turtle     :shell]     (uniform-rand 0.5 1.1)
    [:human      :punch        :urchin     :body]      (uniform-rand 0.5 1.5)
    [:human      :axe          :bat        :body]      (uniform-rand 1.5 2.5)
    [:human      :axe          :bat        #{:face
                                             :head}]   (uniform-rand 0.9 2.8)
    [:human      :axe          :bat        :leg]       (uniform-rand 0.9 1.5)
    [:human      :axe          :bat        :wing]      (uniform-rand 0.8 1.2)
    [:human      :axe          :bird       :beak]      (uniform-rand 1.5 2.5)
    [:human      :axe          :bird       :body]      (uniform-rand 1.5 2.5)
    [:human      :axe          :bird       :head]      (uniform-rand 1.5 2.5)
    [:human      :axe          :bird       :leg]       (uniform-rand 1.5 2.0)
    [:human      :axe          :bird       :tail]      (uniform-rand 1.5 2.5)
    [:human      :axe          :bird       :wing]      (uniform-rand 1.5 2.5)
    [:human      :axe          :boar       :body]      (uniform-rand 1.5 2.5)
    [:human      :axe          :boar       :eye]       (uniform-rand 3.5 5.5)
    [:human      :axe          :boar       :face]      (uniform-rand 0.9 4.9)
    [:human      :axe          :boar       :head]      (uniform-rand 0.3 5.3)
    [:human      :axe          :boar       :leg]       (uniform-rand 0.7 3.7)
    [:human      :axe          :boar       :snout]     (uniform-rand 0.7 3.7)
    [:human      :axe          :boar       :tail]      (uniform-rand 0.2 1.1)
    [:human      :axe          :centipede  #{:body
                                             :head}]   (uniform-rand 0.9 2.9)
    [:human      :axe          :centipede  :leg]       (uniform-rand 0.2 0.6)
    [:human      :axe          :clam       :shell]     (uniform-rand 2.5 4.5)
    [:human      :axe          :fish       #{:body
                                             :head}]   (uniform-rand 1.5 4.5)
    [:human      :axe          :fish       #{:fin
                                             :tail}]   (uniform-rand 0.5 1.5)
    [:human      :axe          :frog       :*]         (uniform-rand 0.5 3.5)
    [:human      :axe          :gecko      :*]         (uniform-rand 0.5 3.5)
    [:human      :axe          :monkey     #{:arm
                                             :leg}]    (uniform-rand 0.5 4.5)
    [:human      :axe          :monkey     #{:body
                                             :face
                                             :head
                                             :neck}]   (uniform-rand 1.5 6.5)
    [:human      :axe          :monkey     :tail]      (uniform-rand 0.2 1.8)
    [:human      :axe          :octopus    #{:body
                                             :head}]   (uniform-rand 1.5 5.9)
    [:human      :axe          :octopus    :tentacle]  (uniform-rand 0.5 3.5)
    [:human      :axe          :parrot     #{:body
                                             :face
                                             :head}]   (uniform-rand 0.5 3.5)
    [:human      :axe          :parrot     #{:leg
                                             :tail}]   (uniform-rand 0.5 2.3)
    [:human      :axe          :parrot     :wing]      (uniform-rand 0.8 2.8)
    [:human      :axe          :rat        #{:body
                                             :face
                                             :head
                                             :neck}]   (uniform-rand 0.5 4.5)
    [:human      :axe          :rat        #{:leg
                                             :tail}]   (uniform-rand 0.5 2.2)
    [:human      :axe          :scorpion   #{:abdomen
                                             :head
                                             :tail}]   (uniform-rand 0.9 3.9)
    [:human      :axe          :scorpion   #{:claw
                                             :leg}]    (uniform-rand 0.5 2.5)
    [:human      :axe          :sea-snake  #{:body
                                             :head}]   (uniform-rand 0.9 3.9)
    [:human      :axe          :shark      #{:body
                                             :head
                                             :nose}]   (uniform-rand 0.5 4.5)
    [:human      :axe          :shark      #{:fin
                                             :tail}]   (uniform-rand 0.5 2.1)
    [:human      :axe          :snake      #{:body
                                             :head
                                             :tail}]   (uniform-rand 0.5 4.5)
    [:human      :axe          :spider     #{:abdomen
                                             :face}]   (uniform-rand 0.5 3.5)
    [:human      :axe          :spider     #{:leg}]    (uniform-rand 0.5 3.5)
    [:human      :axe          :squid      #{:body
                                             :head
                                             :tentacle}] (uniform-rand 0.5 4.5)
    [:human      :axe          :turtle     #{:body
                                             :face
                                             :head
                                             :neck}]   (uniform-rand 0.5 4.5)
    [:human      :axe          :turtle     :leg]       (uniform-rand 0.5 3.5)
    [:human      :axe          :turtle     :shell]     (uniform-rand 0.5 2.1)
    [:human      :axe          :urchin     :body]      (uniform-rand 0.5 3.5)
    [:monkey     :bite         :human      #{:abdomen
                                             :head
                                             :face}]   (uniform-rand 0.5 1.5)
    [:monkey     :bite         :human      #{:arm
                                             :foot
                                             :leg}]    (uniform-rand 0.5 1.5)
    [:monkey     :bite         :human      #{:neck
                                             :abdomen
                                             :face
                                             :head}]   (uniform-rand 0.5 1.9)
    [:monkey     :punch        :human      #{:arm
                                             :foot
                                             :leg}]    (uniform-rand 0.5 1.1)
    [:monkey     :punch        :human      :neck]      (uniform-rand 0.5 1.8)
    [:octopus    :bite         :human      #{:abdomen
                                             :head
                                             :neck
                                             :face}]   (uniform-rand 0.5 1.5)
    [:octopus    :bite         :human      #{:foot
                                             :arm
                                             :leg}]    (uniform-rand 0.5 1.5)
    [:octopus    :bite-venom   :human      #{:abdomen
                                             :face
                                             :neck
                                             :head}]   (uniform-rand 0.5 1.5)
    [:octopus    :bite-venom   :human      #{:arm
                                             :foot
                                             :leg}]    (uniform-rand 0.5 1.5)
    [:octopus    :squeeze      :human      :neck]      (uniform-rand 1.5 3.5)
    [:octopus    :squeeze      :human      :*]         (uniform-rand 0.5 1.5)
    [:parrot     :bite         :human      #{:abdomen
                                             :head
                                             :face
                                             :neck}]   (uniform-rand 0.5 1.5)
    [:parrot     :bite         :human      #{:arm
                                             :foot
                                             :leg}]    (uniform-rand 0.5 1.0)
    [:parrot     :claw         :human      :*]         (uniform-rand 0.2 0.7)
    [:rat        :bite         :human      #{:abdomen
                                             :face
                                             :head
                                             :neck}]      (uniform-rand 0.5 1.5)
    [:rat        :bite         :human      #{:arm
                                             :foot
                                             :leg}]       (uniform-rand 0.5 1.5)
    [:rat        :claw         :human      :*]   (uniform-rand 0.5 1.5)
    [:scorpion   :bite         :human      :face]      (uniform-rand 0.5 1.5)
    [:scorpion   :bite         :human      :*]   (uniform-rand 0.5 1.5)
    [:scorpion   :claw         :human      :*]   (uniform-rand 0.5 1.5)
    [:scorpion   :sting-venom  :human      :abdomen]   (uniform-rand 0.5 1.5)
    [:scorpion   :sting-venom  :human      :arm]       (uniform-rand 0.5 1.0)
    [:scorpion   :sting-venom  :human      :face]      (uniform-rand 1.5 2.5)
    [:scorpion   :sting-venom  :human      :foot]      (uniform-rand 0.5 0.9)
    [:scorpion   :sting-venom  :human      :head]      (uniform-rand 1.0 1.5)
    [:scorpion   :sting-venom  :human      :leg]       (uniform-rand 0.8 1.4)
    [:scorpion   :sting-venom  :human      :neck]      (uniform-rand 1.0 1.5)
    [:sea-snake  :bite         :human      :*]         (uniform-rand 0.5 1.1)
    [:sea-snake  :bite-venom   :human      :abdomen]   (uniform-rand 0.6 1.5)
    [:sea-snake  :bite-venom   :human      :arm]       (uniform-rand 0.6 1.3)
    [:sea-snake  :bite-venom   :human      :face]      (uniform-rand 0.5 1.6)
    [:sea-snake  :bite-venom   :human      :foot]      (uniform-rand 0.5 1.2)
    [:sea-snake  :bite-venom   :human      :head]      (uniform-rand 0.7 1.5)
    [:sea-snake  :bite-venom   :human      :leg]       (uniform-rand 0.5 1.2)
    [:sea-snake  :bite-venom   :human      :neck]      (uniform-rand 0.8 1.5)
    [:shark      :bite         :human      :abdomen]   (uniform-rand 2.5 4.5)
    [:shark      :bite         :human      :arm]       (uniform-rand 1.5 3.5)
    [:shark      :bite         :human      :face]      (uniform-rand 2.5 4.5)
    [:shark      :bite         :human      :foot]      (uniform-rand 2.5 4.5)
    [:shark      :bite         :human      :head]      (uniform-rand 2.5 5.5)
    [:shark      :bite         :human      :leg]       (uniform-rand 2.5 4.5)
    [:shark      :bite         :human      :neck]      (uniform-rand 1.5 3.5)
    [:snake      :bite         :human      :*]         (uniform-rand 1.1 1.6)
    [:snake      :bite-venom   :human      :*]         (uniform-rand 0.6 1.2)
    [:squid      :bite         :human      :*]         (uniform-rand 0.5 2.5)
    [:squid      :squeeze      :human      :neck]      (uniform-rand 1.5 2.5)
    [:squid      :squeeze      :human      :*]         (uniform-rand 0.5 2.5)
    [:turtle     :bite         :human      :*]         (uniform-rand 0.5 1.5)
    [:urchin     :spike        :human      #{:head
                                             :neck}]   (uniform-rand 0.9 1.5)
    [:urchin     :spike        :human      :*]         (uniform-rand 0.5 1.2)
    [:* :* :* :*] (+ (rand) 0.5)))

(defn attack
  "Perform combat. The attacker fights the defender, but not vice-versa.
   Return a new state reflecting combat outcome."
  [state attacker-path defender-path]
  {:pre [(every? (set (keys (get-in state defender-path))) [:hp :pos :race :body-parts :inventory])
         (every? (set (keys (get-in state attacker-path))) [:attacks])
         (vector? (get-in state [:world :npcs]))]
   :post [(vector? (get-in % [:world :npcs]))]}
  (let [defender           (get-in state defender-path)
        ;; 
        attacker           (get-in state attacker-path)
        attack             (or (get (first (filter (fn [item] (contains? item :wielded))
                                           (get-in state (conj attacker-path :inventory) [])))
                                    :attack)
                               (rand-nth (vec (get attacker :attacks))))
        defender-body-part (rand-nth (vec (get defender :body-parts)))
        {x :x y :y}        (get defender :pos)
        hp                 (get defender :hp)
        dmg                (calc-dmg (get attacker :race) attack (get defender :race) defender-body-part)
        is-wound           (> dmg 1.5)]
    (debug "attack" attacker-path "is attacking defender" defender-path)
    (debug "attacker-detail" attacker)
    (debug "defender-detail" defender)
    (cond
      ;; defender still alive?
      (pos? (- hp dmg))
        (-> state
          ;; modify defender hp
          (update-in (conj defender-path :hp)
            (fn [hp] (- hp dmg)))
          (append-log (gen-attack-message attacker
                                          defender
                                          attack
                                          defender-body-part
                                          :hit))
          ;; chance of being envenomed by venomous attacks
          (update-in (conj defender-path :status) (fn [status] (if (and (re-find #"venom" (str attack))
                                                                        (= (rand-int 10) 0))
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
                         (append-log state "You have been wounded.")
                         state))))
      ;; defender dead? (0 or less hp)
      (not (pos? (- hp dmg)))
        (if (contains? (set defender-path) :npcs)
          ;; defender is npc
          (-> state
            ;; remove defender
            (remove-in (butlast defender-path) (partial = defender))
            ;; maybe add corpse
            (update-in [:world :places (current-place-id state) y x :items]
                       (fn [items]
                         (if (zero? (rand-int 3))
                           (conj items (gen-corpse defender))
                           items)))
            (append-log (gen-attack-message attacker
                                            defender
                                            attack
                                            defender-body-part
                                            :dead)))
          ;; defender is player
          (update-in state [:world :player :status]
            (fn [status] (conj status :dead)))))))

