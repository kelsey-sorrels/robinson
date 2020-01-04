(ns robinson.crafting.effects
  (:require [robinson.common :as rc]
            [robinson.random :as rr]
            [robinson.world :as rw]
            [robinson.player :as rp]
            [robinson.inventory :as ri]
            [robinson.crafting :as rcrafting]
            [robinson.crafting.mod :as rcmod]
            [robinson.crafting.mod-protocol :as rcmp]
            [robinson.itemgen :as rig]
            [taoensso.timbre :as log]))

; Weapon mods
(defn mod-accuracy
  [low high & [cause]]
  (let [n (rr/uniform-int low high)]
    (rcmod/adj-item-on-create "accuracy" "acc" :accuracy n :cause cause)))

(defn mod-damage
  [low high & [cause]]
  (let [n (rr/uniform-int low high)]
    (rcmod/adj-item-on-create "damage" "dmg" :damage n :cause cause)))

(defn mod-durability
  [low high & [cause]]
  (let [n (rr/uniform-int low high)]
    (rcmod/adj-item-on-create "durability" "dbl" :durability n :cause cause)))

; Reduces opponent toughness
(defn mod-piercing
  [low high]
  (let [n (rr/uniform-int low high)]
    (rcmod/adj-defender-on-attack-temp "piercing" "prc" :toughness n)))

; Increase attackers speed
(defn mod-haste
  [low high]
  (let [n (rr/uniform-int low high)]
    (rcmod/adj-attacker-on-attack-temp "haste" :speed n)))

; chance of scaring monster
(defn mod-scare-monster
  [low high]
  (let [n (rr/uniform-double low high)]
    (rcmod/tag-defender-on-attack "scare" "scr" :scared n)))

; chance of stunning monster
(defn mod-stunning
  [low high]
  (let [n (rr/uniform-double low high)]
    (rcmod/tag-defender-on-attack "stun" "stn" :stunned n)))


; chance of dismembering monster
(defn mod-dismembering
  [low high]
  (let [n (rr/uniform-double low high)]
    (rcmod/tag-defender-on-attack "dismember" "dsm" :dismembered n)))

; fire hardened
(defn mod-firehardened
  []
  (rcmod/adj-item-on-create "fire hardened" "frhd" :durability 10))

; chance of wounding monster
(defn mod-bleeding
  [low high]
  (let [n (rr/uniform-double low high)]
    (rcmod/tag-defender-on-attack "bleeding" "bld" :bleeding n)))

; chance of knockback
(defn mod-knockback
  [low high]
  (let [n (rr/uniform-double low high)]
    (rcmod/tag-defender-on-attack "knockback" "knb" :knockbacked n)))

; serrated
(defn mod-serrated
  []
  (rcmod/adj-item-on-create "serrated" "srr" :damage 10))

; condition mod based on opponent attributes
#_(defn mod-conditioned-heirarchy
  [id mod]
    (->ModWeapon "conditioned" :conditioned {:id id :mod mod}))

;; Creation mods
(defn mod-hp [low high]
  (let [n (rr/uniform-int low high)]
    (rcmod/adj-player-immediate "health" "hp" :hp n)))

(defn mod-hunger [low high]
  (let [n (rr/uniform-int low high)]
    (rcmod/adj-player-immediate "hunger" "hun" :hunger n)))

(defn mod-thirst [low high]
  (let [n (rr/uniform-int low high)]
    (rcmod/adj-player-immediate "thirst" "thr" :thirst n)))

(defn mod-wtl [low high]
  (let [n (rr/uniform-int low high)]
    (rcmod/adj-player-immediate "wtl" "wtl" :wtl n)))

(defn mod-wound [dmg-low dmg-high time-low time-high]
  (let [dmg (rr/uniform-int dmg-low dmg-high)
        t (rr/uniform-int time-low time-high)]
    (rcmod/conj-player-immediate "wound" "wnd" :wounds {:dmg dmg :time t})))

(defn mod-infected []
  (rcmod/conj-player-immediate "infected" "infc" :status :infected))

(defn mod-dec-inventory [hotkey]
  (rcmod/dec-inventory-by-hotkey "" "" hotkey nil))

(defn mod-dec-inventory-by-item-id [item-id]
  (rcmod/dec-inventory-by-item-id "" "" item-id nil))

(defn mod-remove-effect [effect]
  (rcmod/remove-effect "" "" effect nil))

(defn mod-wear-clothes [item]
  (rcmod/conj-player-immediate "wear" "wear" :wear item))

(defn assoc-current-state-immediate [state-name]
  (rcmod/assoc-current-state-immediate (name state-name) (name state-name) state-name nil))

(defn spawn-npc-immediate [monster-id]
  (rcmod/add-npc-immediate (str "Add " (name monster-id)) (str "Add " (name monster-id)) monster-id nil))

