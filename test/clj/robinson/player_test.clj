(ns robinson.player-test
  (:require [robinson.common :as rc]
            [robinson.player :as rp]
            [robinson.monstergen :as mg]
            [robinson.itemgen :as ig]
            [clojure.test :as t
              :refer (is are deftest with-test run-tests testing)]))

(deftest test-add-items
  (let [state {:world {
                :time 0
                :remaining-hotkeys rc/hotkeys
                :player {
                  :inventory []}}}]
    (are
      [items expected]
        ; expected = actual
        (= expected
           (rp/player-inventory (rp/add-to-inventory state items)))
      ; items
      (let [item-ids [:rope
                      :stick
                      :feather
                      :knife
                      :bamboo
                      :rock]
        rat (mg/id->monster :rat)
        rat-corpse (ig/gen-corpse rat)
        rat-bones (ig/gen-bones rat-corpse)]
      (println rat-corpse)
      (println rat-bones)
      (concat [rat-corpse
               rat-bones]
            (map ig/gen-item item-ids)))
      ; inventory
      [{:item/id :rat-corpse,
        :type :food,
        :race :rat,
        :name "rat corpse",
        :name-plural "rat corpses",
        :hunger 12.079181246047625
        :hotkey \A}
       {:item/id :rat-bones,
        :race :rat,
        :name "rat bones",
        :name-plural "rat bones",
        :properties #{:tube-like :stick-like},
        :hotkey \B}
       {:item/id :rope,
        :name "rope",
        :name-plural "ropes",
        :fuel 10,
        :properties #{:flexible}
        :hotkey \C}
       {:item/id :stick,
        :name "stick",
        :name-plural "sticks",
        :fuel 100,
        :utility 100,
        :weight 0.5,
        :properties #{:stick-like},
        :item/materials #{:wood}
        :hotkey \D}
       {:item/id :feather,
        :name "feather",
        :name-plural "feathers"
        :hotkey \E}
       {:item/id :knife,
        :name "knife",
        :name-plural "knives",
        :attack :knife,
        :utility 100,
        :properties #{:edged}
        :hotkey \F}
       {:item/id :bamboo,
        :name "bamboo",
        :name-plural "bamboo",
        :fuel 100,
        :properties #{:tube-like :stick-like}
        :hotkey \G}
       {:item/id :rock,
        :name "rock",
        :name-plural "rocks",
        :attack :blunt,
        :ranged-attack :airborn-item
        :hotkey \H}])))

(deftest test-dec-items
  (let [state {:world {
                :time 0
                :remaining-hotkeys rc/hotkeys
                :player {
                  :inventory [
                    {:item/id :rope,
                     :name "rope",
                     :name-plural "ropes",
                     :fuel 10,
                     :count 10
                     :properties #{:flexible}
                     :hotkey \C}]}}}]
    (are
      [expected]
        ; expected = actual
        (= expected
           (-> state
            (rp/dec-item-count \C)
            rp/player-inventory))
      ; items
      [{:item/id :rope,
       :name "rope",
       :name-plural "ropes",
       :fuel 10,
       :count 9
       :properties #{:flexible}
       :hotkey \C}])))

(deftest test-add-additional-items
  (let [state {:world {
                :time 0
                :remaining-hotkeys rc/hotkeys
                :player {
                  :inventory []}}}]
    (are
      [items expected]
        ; expected = actual
        (= expected
           (-> state
             (rp/add-to-inventory (map ig/gen-item [:rope :knife]))
             (rp/add-to-inventory items)
             rp/player-inventory))
      ; items
      (let [rat (mg/id->monster :rat)
            rat-corpse (ig/gen-corpse rat)
            rat-bones (ig/gen-bones rat-corpse)]
        (println rat-corpse)
        (println rat-bones)
        [rat-corpse
         rat-bones])
      ; inventory
      [{:item/id :rope,
        :name "rope",
        :name-plural "ropes",
        :fuel 10,
        :properties #{:flexible}
        :hotkey \A}
       {:item/id :knife,
        :name "knife",
        :name-plural "knives",
        :attack :knife,
        :utility 100,
        :properties #{:edged}
        :hotkey \B}
       {:item/id :rat-corpse,
        :type :food,
        :race :rat,
        :name "rat corpse",
        :name-plural "rat corpses",
        :hunger 12.079181246047625
        :hotkey \C}
       {:item/id :rat-bones,
        :race :rat,
        :name "rat bones",
        :name-plural "rat bones",
        :properties #{:tube-like :stick-like},
        :hotkey \D}])))
