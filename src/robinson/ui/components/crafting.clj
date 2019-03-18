(ns robinson.ui.components.crafting
  (:require
            [robinson.ui.components.common :as ruicommon]
            [robinson.crafting :as rcrafting]
            [robinson.crafting.weapon-gen :as rcwg]
            [taoensso.timbre :as log]
            [zaffre.components :as zc]
            [zaffre.components.ui :as zcui]
            [zaffre.util :as zutil]
            [tinter.core :as tinter]
            [loom.graph :as lg]))
 
(zc/def-component RecipeChoice
  [this]
  (let [{:keys [recipe]} (zc/props this)
        current-stage (get recipe :current-stage)]
    (zc/csx
      [:view {:style {:width 30}} [
        [:text {} [(get current-stage :title "")]]
        [:text {} [""]]
        [:text {} [(get current-stage :description "")]]
        [ruicommon/MultiSelect {
          :style {:width 30}
          :title ""
          :items (map (fn [item] (if (keyword (get item :hotkey))
                                   (update item :hotkey name)
                                   item))
                      (get current-stage :choices [{:name "continue-ui" :hotkey :space}]))}]]])))

(zc/def-component TitledList
  [this]
  (let [{:keys [title names]} (zc/props this)]
    (zc/csx [:view {} (cons
      (zc/csx [:text {} [title]])
      (if (seq names)
        (map (fn [n] (zc/csx [:text {} [n]])) names)
        [(zc/csx [:text {} ["None"]])]))])))

(defn type-names [types]
  (case (count types)
    0 ["None"]
    1 (map name types)
    2 (cond
        (contains? (set types) :blunt)
          (cond
            (contains? (set types) :melee)
              ["Club"]
            (contains? (set types) :thrown)
              ["Rock"]
            (contains? (set types) :ranged)
              ["Sling"])
        (contains? (set types) :edged)
          (cond
            (contains? (set types) :melee)
              ["Dagger"]
            (contains? (set types) :thrown)
              ["Throwing Axe"]
            (contains? (set types) :ranged)
              ["Boomarang"])
        (contains? (set types) :piercing)
          (cond
            (contains? (set types) :melee)
              ["Spear"]
            (contains? (set types) :thrown)
              ["Throwing Spear"]
            (contains? (set types) :ranged)
              ["Bow/Blowgun"])
        (contains? (set types) :flexible)
          (cond
            (contains? (set types) :melee)
              ["Garrote"]
            (contains? (set types) :thrown)
              ["Bolas"]
            (contains? (set types) :ranged)
              ["Whip"]))))

(zc/def-component RecipeTotal
  [this]
  (let [{:keys [recipe]} (zc/props this)]
    #_(log/info (get recipe :effects))
    (zc/csx
      [:view {:style {:width 30}} [
        [TitledList {:title "Type:" :names (type-names (get recipe :types))}]
        [:text {} [""]]
        [TitledList {:title "Attributes:" :names (map rcrafting/mod-name (get recipe :effects))}]
        [:text {} [""]]
        [TitledList {:title "Materials:" :names (map (comp name :id) (get recipe :materials))}]]])))
