(ns robinson.ui.components.recipes
  (:require
            [robinson.ui.components.common :as ruicommon]
            [robinson.crafting :as rcrafting]
            [robinson.crafting.mod :as rcmod]
            [robinson.crafting.recipe-gen :as rcrg]
            [robinson.crafting.weapon-gen :as rcwg]
            [robinson.player :as rp]
            [robinson.itemgen :as ig]
            [taoensso.timbre :as log]
            [zaffre.components :as zc]
            [zaffre.components.ui :as zcui]
            [zaffre.util :as zutil]
            [tinter.core :as tinter]
            [loom.graph :as lg]))
 
(zc/def-component TitledList
  [this]
  (let [{:keys [title names]} (zc/props this)]
    (zc/csx [:view {} (cons
      (zc/csx [:text {} [title]])
      (if (seq names)
        (map (fn [n] (zc/csx [:text {} [n]])) names)
        [(zc/csx [:text {} ["None"]])]))])))

(defn type-names [recipe]
  (let [types (get recipe :types)]
  (case (count types)
    0 ["None"]
    1 (map name types)
    2 (let [r (rcrafting/recipe-name recipe)]
        (log/info types)
        (log/info r)
        [r]))))

(zc/def-component RecipeTotal
  [this]
  (let [{:keys [recipe]} (zc/props this)]
    (zc/csx
      [:view {:style {:width 30}} [
        [TitledList {:title "Type:" :names (type-names recipe)}]
        [:text {} [""]]
        [TitledList {:title "Attributes:" :names (map rcmod/mod-name (get recipe :effects))}]
        [:text {} [""]]
        [TitledList {:title "Materials:" :names (map (comp name :id) (get recipe :materials))}]]])))

(zc/def-component SelectRecipeNode
  [this]
  (let [{:keys [recipe]} (zc/props this)
        n (get recipe :current-node)
        layers (get recipe :layers)
        x (rcrg/node-x layers n)
        y (rcrg/node-y layers n)]
    (zc/csx [ruicommon/Cursor {:pos {:x (- x 39) :y (- y 13)}}])))


(zc/def-component RecipeChoice
  [this]
  (let [{:keys [recipe]} (zc/props this)
        current-stage (get recipe :current-stage)]
    (zc/csx
      [:view {:style {:left 1 :width 30}} [
        [:text {} [(get current-stage :title "")]]
        [:text {} [""]]
        [:text {} [(get current-stage :description "")]]
        [ruicommon/MultiSelect {
          :style {:width 30 :left 8}
          :title ""
          :items (map (fn [item] (if (keyword (get item :hotkey))
                                   (update item :hotkey name)
                                   item))
                      (get current-stage :choices [{:name "continue-ui" :hotkey :space}]))}]]])))


(zc/def-component CraftInProgressRecipe
  [this]
  (let [{:keys [game-state]} (zc/props this)
        recipe-type (get-in game-state [:world :in-progress-recipe-type])
        recipe (get-in game-state [:world :in-progress-recipes recipe-type])]
    (zc/csx [zcui/Popup {:style {:margin-top 3
                                 :height 16}} [
              [:text {:style {:bottom 1}} ["| New Recipe |"]]
              [:view {:style {:width 50 :display :flex
                              :justify-content :center
                              :flex-direction :row
                              #_#_:align-items :flex-start}} [
                [:view {:style {:width 9 :height 13
                                :margin-left 1
                                :left 1
                                #_#_:top 0}} [
                  (zc/csx [:img {:width 9 :height 13} (get recipe :img)])
                  [SelectRecipeNode {:recipe recipe}]]]
                [RecipeChoice {:recipe recipe}]
                [RecipeTotal {:recipe recipe}]]]]])))


(zc/def-component RecipeDetail
  [this]
  (let [{:keys [game-state]} (zc/props this)
        selected-recipe-hotkey (get-in game-state [:world :selected-recipe-hotkey])
        recipe (first (filter (fn [recipe] (= (get recipe :hotkey) selected-recipe-hotkey))
                                (rcrafting/player-recipes game-state)))]
    (zc/csx [:view {} [
      [RecipeTotal {:recipe recipe}]
      [:view {:style {:height 4}}]
      [:view {:style {:width 20 :display :flex
                              :flex-direction :row}}
        [:text {} [[ruicommon/Highlight {} ["r"]]
                   [:text {} ["-replace"]]]]
        [:text {} [[ruicommon/Highlight {} ["m"]]
                   [:text {} ["-make"]]]]]]])))


(zc/def-component SelectRecipeType
  [this]
  (let [{:keys [game-state]} (zc/props this)]
    (zc/csx [zcui/Popup {} [
                [ruicommon/MultiSelect {:title "Recipe Type"
                              :items [{:name "Weapon" :hotkey \w}
                                      {:name "Trap" :hotkey \t}
                                      {:name "Food" :hotkey \f}
                                      {:name "Signal" :hotkey \s}
                                      {:name "Survival" :hotkey \v}]}]]])))

(zc/def-component Recipes
  [this]
  (let [{:keys [game-state]} (zc/props this)
        selected-recipe-hotkey (get-in game-state [:world :selected-recipe-hotkey])
        items (rcrafting/player-recipes game-state)]
    (zc/csx [zcui/Popup {:style {:top -7}} [
              [:text {:style {:bottom 1}} ["| Recipes |"]]
              [:view {:style {:width 40 :display :flex
                              :justify-content :center
                              :flex-direction :row
                              #_#_:align-items :flex-start}} [
                [:view {:style {:width 20 :height 13
                                :margin-left 1
                                #_#_:left 1
                                #_#_:top 0}} [
                  [ruicommon/MultiSelect {:style {:width 40}
                                          :items items
                                          :selected-hotkeys #{selected-recipe-hotkey}}]]]
                [RecipeDetail {:game-state game-state}]]]]])))

