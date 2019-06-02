(ns robinson.crafting.mod-protocol)

(defprotocol Mod
  (full-name [this])
  (short-name [this])
  ;; only merge mods with same id. Also used for icon
  (id [this])
  (merge [this other]))

(defprotocol ModQuantifiable
  (amount [this]))

(defprotocol ModPlayerImmediate
  (player-immediate [this player]))

(defprotocol ModItemOnCreate
  (item-on-create [this item]))

(defprotocol ModPlayerOnCreate
  (player-on-create [this player]))

(defprotocol ModCellOnCreate
  (cell-on-create [this cell]))

(defprotocol ModAttackerOnAttack
  (attacker-on-attack [this attacker defender]))

(defprotocol ModDefenderOnAttack
  (defender-on-attack [this attacker defender]))

(defprotocol ModAttackerOnAttackTemp
  (attacker-on-attack-temp [this attacker defender]))

(defprotocol ModDefenderOnAttackTemp
  (defender-on-attack-temp [this attacker defender]))

(defprotocol ModConditionedOnHierarchy
  ; returns mod when applies, otherwise nil
  (when-triggered [this h id v]))

