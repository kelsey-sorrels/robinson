(ns robinson.crafting.mod
  (:require [robinson.crafting.mod-protocol :as rcmp]
            [taoensso.timbre :as log]))

(defn adj-val [v amount]
  (+ (or v 0) amount))

(defn- kebab-to-pascal [sym]
  (->>
    (-> sym name (clojure.string/split #"-"))
    (map clojure.string/capitalize)
    (clojure.string/join "")
    symbol))

(defmacro defmod-type [sym & args]
  (let [pascal (kebab-to-pascal sym)
        pascal# pascal
        body (take-last 2 args)
        constructor# (symbol (str "->" (name pascal)))
        {:keys [tag]} (drop-last 2 args)
        tag# tag
        body# body]
    `(do
      (defrecord ~pascal# ~'[full-name short-name k n]
        rcmp/Mod
        ~'(full-name [this] full-name)
        ~'(short-name [this] short-name)
        ~'(id [this] (hash [(type this) k]))
        ~`(~'merge [~'this ~'other]
          ~(if-not tag#
             `(update ~'this :n adj-val (get ~'other :n))
             `(update ~'this :n (fn [~'x ~'y] (or ~'x ~'y)) (get ~'other :n))))
        ~@(when-not tag#
            `(rcmp/ModQuantifiable
             ~'(amount [this] n)))
        ~@body#)
      (defn ~sym ~'[full-name short-name k amount]
        (~constructor# ~'full-name ~'short-name ~'k ~'amount)))))

(defmod-type adj-item-on-create
  rcmp/ModItemOnCreate
  (item-on-create [this item]
    (update item k adj-val n)))

(defmod-type tag-item-on-create
  :tag true
  rcmp/ModItemOnCreate
  (item-on-create [this item]
    (assoc item k n)))

(defmod-type adj-player-on-create
  rcmp/ModPlayerImmediate
  (player-immediate [this player]
    (update player k adj-val n)))

(defmod-type tag-player-on-create
  :tag true
  rcmp/ModPlayerImmediate
  (player-immediate [this player]
    (assoc player k n)))

(defmod-type adj-attacker-on-attack
  rcmp/ModAttackerOnAttack
  (attacker-on-attack [this attacker defender]
    (update attacker k adj-val n)))

(defmod-type tag-attacker-on-attack
  :tag true
  rcmp/ModAttackerOnAttack
  (attacker-on-attack [this attacker defender]
    (assoc attacker k n)))

(defmod-type adj-defender-on-attack
  rcmp/ModDefenderOnAttack
  (defender-on-attack [this attacker defender]
    (update defender k adj-val n)))

(defmod-type tag-defender-on-attack
  :tag true
  rcmp/ModDefenderOnAttack
  (defender-on-attack [this attacker defender]
    (assoc defender k n)))

(defmod-type adj-attacker-on-attack-temp
  rcmp/ModAttackerOnAttackTemp
  (attacker-on-attack-temp [this attacker defender]
    (update attacker k adj-val n)))

(defmod-type tag-attacker-on-attack-temp
  :tag true
  rcmp/ModAttackerOnAttackTemp
  (attacker-on-attack-temp [this attacker defender]
    (assoc attacker k n)))

(defmod-type adj-defender-on-attack-temp
  rcmp/ModDefenderOnAttackTemp
  (defender-on-attack-temp [this attacker defender]
    (update defender k adj-val n)))

(defmod-type tag-defender-on-attack-temp
  :tag true
  rcmp/ModDefenderOnAttackTemp
  (defender-on-attack-temp [this attacker defender]
    (assoc defender k n)))

(defrecord ConditionedOnHierarchy [mod]
  rcmp/ModConditionedOnHierarchy
  ; returns mod when applies, otherwise nil
  (when-triggered [this h tag v]
    (when (contains? (ancestors h tag) v)
      mod)))

(defn apply-mods
  [actor mods protocol & args]
  (let [m (first (keys (get protocol :method-map)))]
    (reduce (fn [actor mod]
              (if (satisfies? protocol mod)
                (apply (-> m name symbol resolve) mod args)
                actor))
            actor
            mods)))

