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
  [attacker defender attack damage-type]
  (let [attacker-name (attacker :name)
        defender-name (defender :name)]
    (condp vec-match? [attacker-name defender-name attack damage-type]
      ["Player" :*       :punch :miss] (format "You punch the %s but miss." defender-name)
      ["Player" :*       :punch :hit]  (format "You punch the %s solidly." defender-name)
      ["Player" :*       :punch :dead] (format "You punch the %s and kill it dead." defender-name)
      [:*       "Player" :bite  :miss] (format "The %s lunges at you its mouth but misses." attacker-name)
      [:*       "Player" :claws :miss] (format "The %s claws at you and narrowly misses." attacker-name)
      [:*       "Player" :punch :miss] (format "The %s punches you but misses" attacker-name)
      [:*       "Player" :bite  :hit]  (format "The %s sinks its teeth into your flesh." attacker-name)
      [:*       "Player" :claws :hit]  (format "The %s claws into your flesh." attacker-name)
      [:*       "Player" :punch :hit]  (format "The %s punches you." attacker-name))))

(defn attack
  "Perform combat. The attacker fights the defender, but not vice-versa.
   Return a new state reflecting combat outcome."
  [state attacker-path defender-path]
  {:pre [(every? (get-in state defender-path) [:hp :race :inventory])]}
  (let [defender    (get-in state defender-path)
        attacker    (get-in state attacker-path)
        {x :x y :y} (defender :pos)
        hp          (defender :hp)
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
          (append-log state (gen-attack-message attacker defender :punch :hit)))
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
                           (conj items {:type :food :name (format "%s corpse" (name (defender :race))) :hunger 10})
                           items)))
            (append-log (gen-attack-message attacker defender :punch :dead)))
          ;; defender is player
          (update-in state [:world :player :status]
            (fn [status] (conj status :dead)))))))

