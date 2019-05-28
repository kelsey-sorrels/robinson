(ns robinson.crafting.mod
  (:require [robinson.crafting.mod-protocol :as rcmp]
            [taoensso.timbre :as log]))

(defn adj-val [v amount]
  (log/info v amount)
  (+ (or v 0) amount))

(defrecord UpdatePlayerImmediate [ks f-sym args]
  rcmp/ModPlayerImmediate
  (player-immediate [this player]
    (apply update-in player ks (resolve f-sym) args)))

(defn update-player-immediate [k amount]
  (->UpdatePlayerImmediate [k] 'adj-key [amount]))

(defrecord UpdateItemOnCreate [full-name short-name ks f-sym args]
  rcmp/Mod
  (full-name [this] full-name)
  (short-name [this] short-name)
  (merge [this other] (update-in this [:args 0] + (-> other :args first)))
  rcmp/ModItemOnCreate
  (item-on-create [this item]
    (log/info "f" (resolve f-sym))
    (if-let [f (resolve f-sym)]
      (apply update-in item ks f args)
      item)))

(defn short-name [name]
  (get {"damage" "dmg"
        "accuracy" "acc"
        "speed" "spd"
        "durability" "drb"
        "hunger" "hng"
        "thirst" "thr"}
       name name))

(defn full-name-short-name [k amount]
  (let [full-name (if (pos? amount)
                    (str (name k) " +" amount)
                    (str (name k) " " amount))
       short-name (if (pos? amount)
                     (str (short-name (name k)) "+" amount)
                     (str (short-name (name k)) "" amount))]
    [full-name short-name]))

(defn update-item-on-create [k amount]
  (let [[full-name short-name] (full-name-short-name k amount)]
    (log/info full-name short-name [k] 'adj-val amount)
    (->UpdateItemOnCreate full-name short-name [k] 'adj-val [amount])))

(defrecord UpdatePlayerOnCreate [full-name short-name ks f-sym args]
  rcmp/Mod
  (full-name [this] full-name)
  (short-name [this] short-name)
  (merge [this other] (update-in this [:args 0] + (-> other :args first)))
  rcmp/ModPlayerOnCreate
  (player-on-create [this player]
    (apply update player (resolve f-sym) args)))

(defn update-player-on-create [k amount]
  (let [[full-name short-name] (full-name-short-name k amount)]
    (->UpdatePlayerOnCreate full-name short-name [k] 'adj-key [amount])))

(defrecord UpdateAttackerOnAttack [ks f-sym args]
  rcmp/ModAttackerOnAttack
  (attacker-on-attack [this attacker defender]
    (apply update attacker (resolve f-sym) args)))

(defrecord UpdateDefenderOnAttack [ks f-sym args]
  rcmp/ModDefenderOnAttack
  (defender-on-attack [this attacker defender]
    (apply update attacker (resolve f-sym) args)))

(defrecord UpdateAttackerOnAttackTemp [ks f-sym args]
  rcmp/ModAttackerOnAttackTemp
  (attacker-on-attack-temp [this attacker defender]
    (apply update attacker (resolve f-sym) args)))

(defrecord UpdateDefenderOnAttackTemp [ks f-sym args]
  rcmp/ModDefenderOnAttackTemp
  (defender-on-attack-temp [this attacker defender]
    (apply update attacker (resolve f-sym) args)))

(defrecord ConditionedOnHierarchy [mod]
  rcmp/ModConditionedOnHierarchy
  ; returns mod when applies, otherwise nil
  (when-triggered [this h tag v]
    (when (contains? (ancestors h tag) v)
      mod)))

