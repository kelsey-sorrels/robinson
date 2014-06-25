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

(defn attack
  "Perform combat. The attacker fights the defender, but not vice-versa.
   Return a new state reflecting combat outcome."
  [state attacker-path defender-path]
  {:pre [(every? (get-in state defender-path) [:hp :race :inventory])]}
  (let [defender    (get-in state defender-path)
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
            (fn [hp] (- hp dmg))))
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
                           items))))
          ;; defender is player
          (update-in state [:world :player :status]
            (fn [status] (conj status :dead)))))))

