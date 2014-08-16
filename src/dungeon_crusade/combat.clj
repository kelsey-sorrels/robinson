;; Functions that manipulate state to do what the user commands.
(ns dungeon-crusade.combat
  (:use     
    clojure.pprint
    dungeon-crusade.common)
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
                                             "pound" "beat" "strike" "slug"]))]
    (condp vec-match? [attacker-name defender-name attack defender-body-part damage-type]
      ["Player" :*       :punch :*       :miss] (format "You punch the %s but miss." defender-name)
      ["Player" :*       :punch :*       :hit]  (format "You %s the %s %s %s the %s."
                                                        (rand-punch-verb)
                                                        defender-name
                                                        (rand-nth ["solidly" "swiftly" "repeatedly"
                                                                   "perfectly" "competently" "acceptably"])
                                                        (rand-nth ["on" "in" "across"])
                                                        (name defender-body-part))
      ["Player" :*       :punch :head    :dead] (format "You %s the %s in the head. Brains fly everywhere and it dies." (rand-punch-verb) defender-name)
      ["Player" :*       :punch :neck    :dead] (format "You %s the %s in the neck snapping it and it dies." (rand-punch-verb) defender-name)
      ["Player" :*       :punch :body    :dead] (format "You %s the %s in the body damaging internal organs. It dies." (rand-punch-verb) defender-name)
      ["Player" :*       :punch :leg     :dead] (format "You %s the %s in the leg severing it and it dies." (rand-punch-verb) defender-name)
      ["Player" :*       :punch :face    :dead] (format "You %s the %s in the face. Peices of face fly everywhere and it dies." (rand-punch-verb) defender-name)
      ["Player" :*       :punch :abdomen :dead] (format "You %s the %s in the abdomen. Internal organs fly everywhere and it dies." (rand-punch-verb) defender-name)
      ["Player" :*       :punch :claw    :dead] (format "You %s the %s in the claw and it dies." (rand-punch-verb) defender-name)
      ["Player" :*       :punch :tail    :dead] (format "You %s the %s in the tail causing massive injuries and it dies." (rand-punch-verb) defender-name)
      ["Player" :*       :punch :wing    :dead] (format "You %s the %s in the wing ripping it clean off and it dies." (rand-punch-verb) defender-name)
      ["Player" :*       :punch :eye     :dead] (format "You %s the %s in the eye exploding it upon impact and it dies." (rand-punch-verb) defender-name)
      ["Player" :*       :punch :snout   :dead] (format "You %s the %s in the snount crushing it and it dies." (rand-punch-verb) defender-name)
      ["Player" :*       :punch :arm     :dead] (format "You %s the %s in the arm crushing bones it and it dies." (rand-punch-verb) defender-name)
      ["Player" :*       :punch :beak    :dead] (format "You %s the %s in the beak ripping it from its face and it dies." (rand-punch-verb) defender-name)
      ["Player" :*       :punch :shell   :dead] (format "You %s the %s in the shell ripping to peices and it dies." (rand-punch-verb) defender-name)
      ["Player" :*       :punch :*       :dead] (format "You %s the %s causing massive injuries and it dies." (rand-punch-verb) defender-name)
      [:*       "Player" :bite  :*       :miss] (format "The %s lunges at you its mouth but misses." (rand-punch-verb) attacker-name)
      [:*       "Player" :claw  :*       :miss] (format "The %s claws at you and narrowly misses." (rand-punch-verb) attacker-name)
      [:*       "Player" :punch :*       :miss] (format "The %s punches you but misses" attacker-name)
      [:*       "Player" :sting :*       :miss] (format "The %s tries to sting you but misses" attacker-name)
      [:*       "Player" :bite  :*       :hit]  (format "The %s sinks its teeth into your flesh." attacker-name)
      [:*       "Player" :claw  :*       :hit]  (format "The %s claws into your flesh." attacker-name)
      [:*       "Player" :punch :*       :hit]  (format "The %s punches you.`" attacker-name)
      [:*       "Player" :sting :*       :hit]  (format "The %s stings you, pumping you full of poison." attacker-name))))

(defn attack
  "Perform combat. The attacker fights the defender, but not vice-versa.
   Return a new state reflecting combat outcome."
  [state attacker-path defender-path]
  {:pre [(every? (set (keys (get-in state defender-path))) [:hp :race :inventory])]}
  (let [defender    (get-in state defender-path)
        attacker    (get-in state attacker-path)
        {x :x y :y} (get defender :pos)
        hp          (get defender :hp)
        dmg         1]
    (debug "attack" attacker-path "is attacking defender" defender-path)
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
                                          (rand-nth (vec (get attacker :attacks)))
                                          (rand-nth (vec (get defender :body-parts)))
                                          :hit)))
      ;; defender dead? (0 or less hp)
      (not (pos? (- hp dmg)))
        (if (contains? (set defender-path) :npcs)
          ;; defender is npc
          (-> state
            ;; remove defender
            (remove-in (butlast defender-path) (partial = defender))
            ;; maybe add corpse
            (update-in [:world :places current-place-id y x :items]
                       (fn [items]
                         (if (zero? (rand-int 3))
                           (conj items {:type :food :name (format "%s corpse" (name (get defender :race))) :hunger 10})
                           items)))
            (append-log (gen-attack-message attacker
                                            defender
                                            (rand-nth (vec (get attacker :attacks)))
                                            (rand-nth (vec (get defender :body-parts)))
                                            :dead)))
          ;; defender is player
          (update-in state [:world :player :status]
            (fn [status] (conj status :dead)))))))

