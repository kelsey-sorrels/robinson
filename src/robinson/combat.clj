;; Functions that manipulate state to do what the user commands.
(ns robinson.combat
  (:use     
    clojure.pprint
    robinson.common
    robinson.world
    robinson.player
    robinson.itemgen)
  (:require clojure.pprint
            clojure.contrib.core
            [clojure.data.generators :as dg]
            [clojure.stacktrace :as st]
            [taoensso.timbre :as timbre]
            [robinson.monstergen :as mg]))

(timbre/refer-timbre)

(defn sharp-weapon?
  [attack]
  (contains? #{:spear :axe :knife} attack))

(defn- gen-attack-message
  "Logs an attack message to the global state.
   `attack` is one of :bite :claws :punch.
   `damage-type` is one of :miss :hit :dead"
  [attacker defender attack defender-body-part damage-type]
  (let [attacker-race      (get attacker :race)
        defender-race      (get defender :race)
        attacker-name      (get attacker :name)
        defender-name      (get defender :name)
        rand-punch-verb    (fn [] (dg/rand-nth ["wack" "punch" "hit" "pummel" "batter"
                                             "pound" "beat" "strike" "slug"]))
        rand-axe-verb      (fn [] (dg/rand-nth ["hit" "strike" "slash" "tear into" "cleave" "cut"]))]
    (first-vec-match [attacker-race defender-race attack defender-body-part damage-type]
      [:human :*       :punch        :*        :miss] (format "You punch the %s but miss." defender-name)
      [:human :*       :punch        :*        :hit]  (dg/rand-nth [(format "You %s the %s %s %s the %s."
                                                                     (rand-punch-verb)
                                                                     defender-name
                                                                     (dg/rand-nth ["solidly" "swiftly" "repeatedly"
                                                                                "perfectly" "competently"])
                                                                     (dg/rand-nth ["on" "in" "across"])
                                                                     (name defender-body-part))
                                                                   (format "You %s the %s %s the %s."
                                                                     (rand-punch-verb)
                                                                     defender-name
                                                                     (dg/rand-nth ["on" "in" "across"])
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
      [:human :*       sharp-weapon? :*        :hit]  (dg/rand-nth [(format "You %s the %s %s %s the %s."
                                                              (rand-axe-verb)
                                                              defender-name
                                                              (dg/rand-nth ["solidly" "swiftly"
                                                                         "perfectly" "competently"])
                                                              (dg/rand-nth ["on" "in" "across"])
                                                              (name defender-body-part))
                                                            (format "You %s the %s %s the %s."
                                                              (rand-axe-verb)
                                                              defender-name
                                                              (dg/rand-nth ["on" "in" "across"])
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

(defn calc-dmg
  [attacker-race attack defender-race defender-body-part]
  (condp vec-match? [attacker-race attack defender-race defender-body-part]
    [:bat        :bite          :human      :abdomen]   (dg/uniform 1.5 2.5)
    [:bat        :bite          :human      :arm]       (dg/uniform 1.5 2.5)
    [:bat        :bite          :human      :face]      (dg/uniform 1.5 2.5)
    [:bat        :bite          :human      :foot]      (dg/uniform 1.5 2.5)
    [:bat        :bite          :human      :head]      (dg/uniform 1.5 2.5)
    [:bat        :bite          :human      :leg]       (dg/uniform 1.5 2.5)
    [:bat        :bite          :human      :neck]      (dg/uniform 1.9 2.9)
    [:bird       :bite          :human      :*]         (dg/uniform 1.5 1.9)
    [:bird       :claw          :human      :abdomen]   (dg/uniform 1.5 2.5)
    [:bird       :claw          :human      :face]      (dg/uniform 1.8 2.8)
    [:bird       :claw          :human      :neck]      (dg/uniform 1.7 2.7)
    [:bird       :claw          :human      :*]         (dg/uniform 1.5 2.5)
    [:boar       :bite          :human      :abdomen]   (dg/uniform 1.5 3.5)
    [:boar       :bite          :human      :arm]       (dg/uniform 1.5 4.5)
    [:boar       :bite          :human      :face]      (dg/uniform 1.5 3.5)
    [:boar       :bite          :human      :foot]      (dg/uniform 1.5 2.5)
    [:boar       :bite          :human      :head]      (dg/uniform 2.5 4.5)
    [:boar       :bite          :human      :leg]       (dg/uniform 1.9 2.8)
    [:boar       :bite          :human      :neck]      (dg/uniform 2.5 3.5)
    [:boar       :gore          :human      :abdomen]   (dg/uniform 3.5 5.5)
    [:boar       :gore          :human      :arm]       (dg/uniform 2.5 3.5)
    [:boar       :gore          :human      :face]      (dg/uniform 4.5 5.5)
    [:boar       :gore          :human      :foot]      (dg/uniform 2.5 3.5)
    [:boar       :gore          :human      :head]      (dg/uniform 4.5 5.5)
    [:boar       :gore          :human      :leg]       (dg/uniform 3.5 4.5)
    [:boar       :gore          :human      :neck]      (dg/uniform 4.5 5.5)
    [:centipede  :bite          :human      :*]         (dg/uniform 1.5 1.9)
    [:clam       :clamp         :human      :*]         (dg/uniform 1.1 1.3)
    [:fish       :bite          :human      :*]         (dg/uniform 1.2 2.7)
    [:frog       :claw          :human      :*]         (dg/uniform 1.1 1.5)
    [:gecko      :bite          :human      :*]         (dg/uniform 1.1 1.3)
    [:human      :punch         :bat        :body]      (dg/uniform 1.5 2.5)
    [:human      :punch         :bat        #{:face
                                              :head}]   (dg/uniform 1.8 2.8)
    [:human      :punch         :bat        :leg]       (dg/uniform 1.2 1.5)
    [:human      :punch         :bat        :wing]      (dg/uniform 1.8 2.8)
    [:human      :punch         :bird       :beak]      (dg/uniform 1.5 2.5)
    [:human      :punch         :bird       :body]      (dg/uniform 1.5 2.5)
    [:human      :punch         :bird       :head]      (dg/uniform 1.5 2.5)
    [:human      :punch         :bird       :leg]       (dg/uniform 1.5 2.0)
    [:human      :punch         :bird       :tail]      (dg/uniform 1.5 2.5)
    [:human      :punch         :bird       :wing]      (dg/uniform 2.5 3.5)
    [:human      :punch         :boar       :body]      (dg/uniform 1.5 2.5)
    [:human      :punch         :boar       :eye]       (dg/uniform 3.5 4.5)
    [:human      :punch         :boar       :face]      (dg/uniform 1.9 2.9)
    [:human      :punch         :boar       :head]      (dg/uniform 1.3 2.3)
    [:human      :punch         :boar       :leg]       (dg/uniform 1.7 2.7)
    [:human      :punch         :boar       :snout]     (dg/uniform 1.7 2.7)
    [:human      :punch         :boar       :tail]      (dg/uniform 1.1 2.1)
    [:human      :punch         :centipede  #{:body
                                              :head}]   (dg/uniform 1.9 2.9)
    [:human      :punch         :centipede  :leg]       (dg/uniform 1.1 1.5)
    [:human      :punch         :clam       :shell]     (dg/uniform 1.5 2.5)
    [:human      :punch         :fish       #{:body
                                              :head}]   (dg/uniform 1.5 2.5)
    [:human      :punch         :fish       #{:fin
                                              :tail}]   (dg/uniform 1.5 2.1)
    [:human      :punch         :frog       :*]         (dg/uniform 1.5 2.5)
    [:human      :punch         :gecko      :*]         (dg/uniform 1.5 2.5)
    [:human      :punch         :monkey     #{:arm
                                              :leg}]    (dg/uniform 1.5 2.5)
    [:human      :punch         :monkey     #{:body
                                              :face
                                              :head
                                              :neck}]   (dg/uniform 1.5 2.5)
    [:human      :punch         :monkey     :tail]      (dg/uniform 1.2 2.0)
    [:human      :punch         :octopus    #{:body
                                              :head}]   (dg/uniform 2.5 3.9)
    [:human      :punch         :octopus    :tentacle]  (dg/uniform 1.5 2.5)
    [:human      :punch         :parrot     #{:body
                                              :face
                                              :head}]   (dg/uniform 1.5 2.5)
    [:human      :punch         :parrot     #{:leg
                                              :tail}]   (dg/uniform 1.5 2.3)
    [:human      :punch         :parrot     :wing]      (dg/uniform 1.8 2.8)
    [:human      :punch         :rat        #{:body
                                              :face
                                              :head
                                              :neck}]   (dg/uniform 1.5 2.5)
    [:human      :punch         :rat        #{:leg
                                              :tail}]   (dg/uniform 1.5 2.2)
    [:human      :punch         :scorpion   #{:abdomen
                                              :head
                                              :tail}]   (dg/uniform 1.9 2.9)
    [:human      :punch         :scorpion   #{:claw
                                              :leg}]    (dg/uniform 1.5 2.5)
    [:human      :punch         :sea-snake  #{:body
                                              :head}]   (dg/uniform 1.9 2.9)
    [:human      :punch         :shark      #{:body
                                              :head
                                              :nose}]   (dg/uniform 1.5 2.5)
    [:human      :punch         :shark      #{:fin
                                              :tail}]   (dg/uniform 1.5 2.1)
    [:human      :punch         :snake      #{:body
                                              :head
                                              :tail}]   (dg/uniform 1.5 2.5)
    [:human      :punch         :spider     #{:abdomen
                                              :face}]   (dg/uniform 1.5 2.5)
    [:human      :punch         :spider     #{:leg}]    (dg/uniform 1.5 2.5)
    [:human      :punch         :squid      #{:body
                                              :head
                                              :tentacle}] (dg/uniform 1.5 2.5)
    [:human      :punch         :turtle     #{:body
                                              :face
                                              :head
                                              :neck}]   (dg/uniform 1.5 2.5)
    [:human      :punch         :turtle     :leg]       (dg/uniform 1.5 2.5)
    [:human      :punch         :turtle     :shell]     (dg/uniform 1.5 2.1)
    [:human      :punch         :urchin     :body]      (dg/uniform 1.5 2.5)
    [:human      sharp-weapon?  :bat        :body]      (dg/uniform 2.5 3.5)
    [:human      sharp-weapon?  :bat        #{:face
                                              :head}]   (dg/uniform 1.9 3.8)
    [:human      sharp-weapon?  :bat        :leg]       (dg/uniform 1.9 2.5)
    [:human      sharp-weapon?  :bat        :wing]      (dg/uniform 1.8 2.2)
    [:human      sharp-weapon?  :bird       :beak]      (dg/uniform 2.5 3.5)
    [:human      sharp-weapon?  :bird       :body]      (dg/uniform 2.5 3.5)
    [:human      sharp-weapon?  :bird       :head]      (dg/uniform 2.5 3.5)
    [:human      sharp-weapon?  :bird       :leg]       (dg/uniform 2.5 3.0)
    [:human      sharp-weapon?  :bird       :tail]      (dg/uniform 2.5 3.5)
    [:human      sharp-weapon?  :bird       :wing]      (dg/uniform 2.5 3.5)
    [:human      sharp-weapon?  :boar       :body]      (dg/uniform 2.5 3.5)
    [:human      sharp-weapon?  :boar       :eye]       (dg/uniform 4.5 6.5)
    [:human      sharp-weapon?  :boar       :face]      (dg/uniform 1.9 5.9)
    [:human      sharp-weapon?  :boar       :head]      (dg/uniform 1.3 6.3)
    [:human      sharp-weapon?  :boar       :leg]       (dg/uniform 1.7 4.7)
    [:human      sharp-weapon?  :boar       :snout]     (dg/uniform 1.7 4.7)
    [:human      sharp-weapon?  :boar       :tail]      (dg/uniform 1.2 2.1)
    [:human      sharp-weapon?  :centipede  #{:body
                                              :head}]   (dg/uniform 1.9 3.9)
    [:human      sharp-weapon?  :centipede  :leg]       (dg/uniform 1.2 1.6)
    [:human      sharp-weapon?  :clam       :shell]     (dg/uniform 3.5 5.5)
    [:human      sharp-weapon?  :fish       #{:body
                                              :head}]   (dg/uniform 2.5 5.5)
    [:human      sharp-weapon?  :fish       #{:fin
                                              :tail}]   (dg/uniform 1.5 2.5)
    [:human      sharp-weapon?  :frog       :*]         (dg/uniform 1.5 4.5)
    [:human      sharp-weapon?  :gecko      :*]         (dg/uniform 1.5 4.5)
    [:human      sharp-weapon?  :monkey     #{:arm
                                              :leg}]    (dg/uniform 1.5 5.5)
    [:human      sharp-weapon?  :monkey     #{:body
                                              :face
                                              :head
                                              :neck}]   (dg/uniform 2.5 7.5)
    [:human      sharp-weapon?  :monkey     :tail]      (dg/uniform 1.2 2.8)
    [:human      sharp-weapon?  :octopus    #{:body
                                              :head}]   (dg/uniform 2.5 6.9)
    [:human      sharp-weapon?  :octopus    :tentacle]  (dg/uniform 1.5 4.5)
    [:human      sharp-weapon?  :parrot     #{:body
                                              :face
                                              :head}]   (dg/uniform 1.5 4.5)
    [:human      sharp-weapon?  :parrot     #{:leg
                                              :tail}]   (dg/uniform 1.5 3.3)
    [:human      sharp-weapon?  :parrot     :wing]      (dg/uniform 1.8 3.8)
    [:human      sharp-weapon?  :rat        #{:body
                                              :face
                                              :head
                                              :neck}]   (dg/uniform 1.5 5.5)
    [:human      sharp-weapon?  :rat        #{:leg
                                              :tail}]   (dg/uniform 1.5 3.2)
    [:human      sharp-weapon?  :scorpion   #{:abdomen
                                              :head
                                              :tail}]   (dg/uniform 1.9 4.9)
    [:human      sharp-weapon?  :scorpion   #{:claw
                                              :leg}]    (dg/uniform 1.5 3.5)
    [:human      sharp-weapon?  :sea-snake  #{:body
                                              :head}]   (dg/uniform 1.9 4.9)
    [:human      sharp-weapon?  :shark      #{:body
                                              :head
                                              :nose}]   (dg/uniform 1.5 5.5)
    [:human      sharp-weapon?  :shark      #{:fin
                                              :tail}]   (dg/uniform 1.5 3.1)
    [:human      sharp-weapon?  :snake      #{:body
                                              :head
                                              :tail}]   (dg/uniform 1.5 5.5)
    [:human      sharp-weapon?  :spider     #{:abdomen
                                              :face}]   (dg/uniform 1.5 4.5)
    [:human      sharp-weapon?  :spider     #{:leg}]    (dg/uniform 1.5 4.5)
    [:human      sharp-weapon?  :squid      #{:body
                                              :head
                                              :tentacle}] (dg/uniform 1.5 5.5)
    [:human      sharp-weapon?  :turtle     #{:body
                                              :face
                                              :head
                                              :neck}]   (dg/uniform 1.5 5.5)
    [:human      sharp-weapon?  :turtle     :leg]       (dg/uniform 1.5 4.5)
    [:human      sharp-weapon?  :turtle     :shell]     (dg/uniform 1.5 3.1)
    [:human      sharp-weapon?  :urchin     :body]      (dg/uniform 1.5 4.5)
    [:monkey     :bite          :human      #{:abdomen
                                              :head
                                              :face}]   (dg/uniform 1.5 2.5)
    [:monkey     :bite          :human      #{:arm
                                              :foot
                                              :leg}]    (dg/uniform 1.5 2.5)
    [:monkey     :bite          :human      #{:neck
                                              :abdomen
                                              :face
                                              :head}]   (dg/uniform 1.5 2.9)
    [:monkey     :punch         :human      #{:arm
                                              :foot
                                              :leg}]    (dg/uniform 1.5 2.1)
    [:monkey     :punch         :human      :neck]      (dg/uniform 1.5 2.8)
    [:octopus    :bite          :human      #{:abdomen
                                              :head
                                              :neck
                                              :face}]   (dg/uniform 1.5 2.5)
    [:octopus    :bite          :human      #{:foot
                                              :arm
                                              :leg}]    (dg/uniform 1.5 2.5)
    [:octopus    :bite-venom    :human      #{:abdomen
                                              :face
                                              :neck
                                              :head}]   (dg/uniform 1.5 2.5)
    [:octopus    :bite-venom    :human      #{:arm
                                              :foot
                                              :leg}]    (dg/uniform 1.5 2.5)
    [:octopus    :squeeze       :human      :neck]      (dg/uniform 2.5 4.5)
    [:octopus    :squeeze       :human      :*]         (dg/uniform 1.5 2.5)
    [:parrot     :bite          :human      #{:abdomen
                                              :head
                                              :face
                                              :neck}]   (dg/uniform 1.5 2.5)
    [:parrot     :bite          :human      #{:arm
                                              :foot
                                              :leg}]    (dg/uniform 1.5 2.0)
    [:parrot     :claw          :human      :*]         (dg/uniform 1.2 1.7)
    [:rat        :bite          :human      #{:abdomen
                                              :face
                                              :head
                                              :neck}]      (dg/uniform 1.5 2.5)
    [:rat        :bite          :human      #{:arm
                                              :foot
                                              :leg}]       (dg/uniform 1.5 2.5)
    [:rat        :claw          :human      :*]   (dg/uniform 1.5 2.5)
    [:scorpion   :bite          :human      :face]      (dg/uniform 1.5 2.5)
    [:scorpion   :bite          :human      :*]   (dg/uniform 1.5 2.5)
    [:scorpion   :claw          :human      :*]   (dg/uniform 1.5 2.5)
    [:scorpion   :sting-venom   :human      :abdomen]   (dg/uniform 1.5 2.5)
    [:scorpion   :sting-venom   :human      :arm]       (dg/uniform 1.5 2.0)
    [:scorpion   :sting-venom   :human      :face]      (dg/uniform 2.5 3.5)
    [:scorpion   :sting-venom   :human      :foot]      (dg/uniform 1.5 1.9)
    [:scorpion   :sting-venom   :human      :head]      (dg/uniform 2.0 2.5)
    [:scorpion   :sting-venom   :human      :leg]       (dg/uniform 1.8 2.4)
    [:scorpion   :sting-venom   :human      :neck]      (dg/uniform 2.0 2.5)
    [:sea-snake  :bite          :human      :*]         (dg/uniform 1.5 2.1)
    [:sea-snake  :bite-venom    :human      :abdomen]   (dg/uniform 1.6 2.5)
    [:sea-snake  :bite-venom    :human      :arm]       (dg/uniform 1.6 2.3)
    [:sea-snake  :bite-venom    :human      :face]      (dg/uniform 1.5 2.6)
    [:sea-snake  :bite-venom    :human      :foot]      (dg/uniform 1.5 2.2)
    [:sea-snake  :bite-venom    :human      :head]      (dg/uniform 1.7 2.5)
    [:sea-snake  :bite-venom    :human      :leg]       (dg/uniform 1.5 2.2)
    [:sea-snake  :bite-venom    :human      :neck]      (dg/uniform 1.8 2.5)
    [:shark      :bite          :human      :abdomen]   (dg/uniform 3.5 5.5)
    [:shark      :bite          :human      :arm]       (dg/uniform 2.5 4.5)
    [:shark      :bite          :human      :face]      (dg/uniform 3.5 5.5)
    [:shark      :bite          :human      :foot]      (dg/uniform 3.5 5.5)
    [:shark      :bite          :human      :head]      (dg/uniform 3.5 6.5)
    [:shark      :bite          :human      :leg]       (dg/uniform 3.5 5.5)
    [:shark      :bite          :human      :neck]      (dg/uniform 2.5 4.5)
    [:snake      :bite          :human      :*]         (dg/uniform 2.1 2.6)
    [:snake      :bite-venom    :human      :*]         (dg/uniform 1.6 2.2)
    [:squid      :bite          :human      :*]         (dg/uniform 1.5 3.5)
    [:squid      :squeeze       :human      :neck]      (dg/uniform 2.5 3.5)
    [:squid      :squeeze       :human      :*]         (dg/uniform 1.5 3.5)
    [:turtle     :bite          :human      :*]         (dg/uniform 1.5 2.5)
    [:urchin     :spike        :human      #{:head
                                             :neck}]   (dg/uniform 1.9 2.5)
    [:urchin     :spike        :human      :*]         (dg/uniform 1.5 2.2)
    [:* :* :* :*] (+ (dg/float) 1.5)))

(defn calc-thrown-item-dmg
  [item-id defender-race]
  (condp vec-match? [defender-race item-id]
    [:bat        :*] (dg/uniform 1.5 2.5)
    [:bird       :*] (dg/uniform 1.5 1.9)
    [:boar       :*] (dg/uniform 1.5 3.5)
    [:centipede  :*] (dg/uniform 1.5 1.9)
    [:clam       :*] (dg/uniform 1.1 1.3)
    [:fish       :*] (dg/uniform 1.2 2.7)
    [:frog       :*] (dg/uniform 1.1 1.5)
    [:gecko      :*] (dg/uniform 1.1 1.3)
    [:human      :*] (dg/uniform 1.5 2.5)
    [:monkey     :*] (dg/uniform 1.5 2.5)
    [:octopus    :*] (dg/uniform 1.5 2.5)
    [:parrot     :*] (dg/uniform 1.5 2.5)
    [:rat        :*] (dg/uniform 1.5 2.5)
    [:scorpion   :*] (dg/uniform 1.5 2.5)
    [:sea-snake  :*] (dg/uniform 1.5 2.1)
    [:shark      :*] (dg/uniform 3.5 5.5)
    [:snake      :*] (dg/uniform 2.1 2.6)
    [:squid      :*] (dg/uniform 2.5 3.5)
    [:turtle     :*] (dg/uniform 1.5 2.5)
    [:urchin     :*] (dg/uniform 1.9 2.5)
    [:*          :*] (+ (dg/float) 1.5)))

(defn attack
  "Perform combat. The attacker fights the defender, but not vice-versa.
   Return a new state reflecting combat outcome."
  ([state attacker-path defender-path]
   (let [attacker           (get-in state attacker-path)
         attack-type (or (get (first (filter (fn [item] (contains? item :wielded))
                                     (get-in state (conj attacker-path :inventory) [])))
                              :attack)
                         (dg/rand-nth (vec (get attacker :attacks))))]
     (attack state attacker-path defender-path attack-type)))
  ([state attacker-path defender-path attack]
  {:pre [(every? (set (keys (get-in state defender-path))) [:hp :pos :race :body-parts :inventory])
         (every? (set (keys (get-in state attacker-path))) [:attacks])
         (vector? (get-in state [:world :npcs]))]
   :post [(vector? (get-in % [:world :npcs]))]}
  (let [defender           (get-in state defender-path)
        ;; 
        attacker           (get-in state attacker-path)
        thrown-item        (when-not (keyword? attack)
                             attack)
        attack             (if (keyword? attack)
                             attack
                             :thrown-item)
        defender-body-part (dg/rand-nth (vec (get defender :body-parts)))
        {x :x y :y}        (get defender :pos)
        hp                 (get defender :hp)
        hit-or-miss        (dg/rand-nth (concat (repeat (get attacker :speed) :hit)
                                             (repeat (get defender :speed) :miss)))
        dmg                (cond
                             (and (= hit-or-miss :hit)
                                  (= attack :thrown-item))
                               (calc-thrown-item-dmg (get thrown-item :id) (get defender :race))
                             (= hit-or-miss :hit)
                               (calc-dmg (get attacker :race) attack (get defender :race) defender-body-part)
                             :else 0)
        is-wound           (> dmg 1.5)]
    (debug "attack" attacker-path "is attacking defender" defender-path)
    (debug "attacker-detail" attacker)
    (debug "defender-detail" defender)
    (debug "attack" attack)
    (debug "hit?" hit-or-miss)
    (debug "dmg" dmg)
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
                         (append-log state "You have been wounded." :red)
                         state))))
      ;; defender dead? (0 or less hp)
      (not (pos? (- hp dmg)))
        (if (contains? (set defender-path) :npcs)
          ;; defender is npc
          (-> state
            ;; update stats and will-to-live
            (update-npc-killed defender attack)
            ;; remove defender
            (remove-in (butlast defender-path) (partial = defender))
            ;; maybe add corpse
            (update-in [:world :places (current-place-id state) y x :items]
                       (fn [items]
                         (if (> (dg/float) 0.2)
                           (conj items (gen-corpse defender))
                           items)))
            (append-log (gen-attack-message attacker
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
            (update-in [:world :player :status]
              (fn [status] (conj status :dead)))))))))

