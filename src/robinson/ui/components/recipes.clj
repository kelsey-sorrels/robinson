(ns robinson.ui.components.recipes
  (:require
            [robinson.ui.components.common :as ruicommon]
            [robinson.crafting :as rcrafting]
            [robinson.crafting.mod-protocol :as rcmp]
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
 
(defn type-names [recipe]
  [(rcrafting/recipe-name recipe false)])

(zc/def-component RecipeTotal
  [this]
  (let [{:keys [recipe]} (zc/props this)]
    (zc/csx
      [:view {:style {:width 20}} [
        [ruicommon/TitledList {:title "Type:" :names (type-names recipe)}]
        [:text {} [""]]
        [ruicommon/TitledList {:title "Attributes:" :names (map rcrafting/full-name (get recipe :effects))}]
        [:text {} [""]]
        [ruicommon/TitledList {:title "Materials:" :names (map (comp name :id) (get recipe :materials))}]]])))

(zc/def-component SelectRecipeNode
  [this]
  (let [{:keys [recipe]} (zc/props this)
        n (get recipe :current-node)
        layers (get recipe :layers)
        x (rcrg/node-x layers n)
        y (rcrg/node-y layers n)]
    (zc/csx [ruicommon/Cursor {:pos {:x (- x 0) :y (- y 13)}}])))

(zc/def-component RecipeChoice
  [this]
  (let [{:keys [game-state recipe]} (zc/props this)
        current-stage (get recipe :current-stage)]
    (zc/csx
      [:view {:style {:display :flex
                      :align-items :center
                      :width 25 :margin-left 5 :margin-right 5}} [
        [:text {} [(get current-stage :title "")]]
        [:text {} [""]]
        [:text {} [(str (get current-stage :description ""))]]
        [ruicommon/MultiSelect {
          :style {}
          :title ""
          :items (map (fn [item]
                        (-> item
                           (cond->
                             (keyword (get item :hotkey))
                               (update :hotkey name))
                           (cond->
                             (get-in item [:material :amount])
                               (assoc :count (get-in item [:material :amount])))
                           (assoc :disabled
                             (when-let [{:keys [id amount]} (get item :material)]
                               (< (rp/inventory-id->count game-state id) amount)))))
                      (get current-stage :event/choices [{:name "continue" :hotkey :space}]))}]]])))

(zc/def-component CraftInProgressRecipe
  [this]
  (let [{:keys [game-state]} (zc/props this)
        recipe (rcrafting/current-recipe game-state)]
    (zc/csx [zcui/Popup {:style {:margin-top 3
                                 :height 16}} [
              [:text {:style {:left 30 :bottom 1}} ["| New Recipe |"]]
              [:view {:style {:display :flex
                              :flex-direction :row}} [
                [:view {:style {:width 9 :height 13
                                :margin-right 5
                                :left 1}} [
                  (zc/csx [:img {:width 9 :height 13} (get recipe :img)])
                  [SelectRecipeNode {:recipe recipe}]]]
                [RecipeChoice {:game-state game-state :recipe recipe}]
                [RecipeTotal {:recipe recipe}]]]]])))


(zc/def-component RecipeDetail
  [this]
  (let [{:keys [game-state]} (zc/props this)
        recipe (rcrafting/selected-recipe game-state)]
    (if recipe
      (zc/csx [:view {} [
        [RecipeTotal {:recipe recipe}]
        [:view {:style {:height 1}}]
        [:view {:style {:display :flex
                        :flex-direction :column}}
        (if (rcrafting/complete? recipe)
          [(zc/csx [ruicommon/HotkeyLabel {:hotkey \r :label "replace"}])
           (zc/csx [ruicommon/HotkeyLabel {:hotkey \m :label "make"}])]
          [(zc/csx [ruicommon/HotkeyLabel {:hotkey \n :label "continue"}])])]]])
      (zc/csx [:text {} ["Empty"]]))))


(zc/def-component SelectRecipeType
  [this]
  (let [{:keys [game-state]} (zc/props this)]
    (zc/csx [zcui/Popup {:style {:bottom 4 :padding 1}} [
                [:text {} ["Recipe Type"]]
                [:text {} [""]]
                [:view {} [
                  [ruicommon/MultiSelect {:items [{:name "Weapon" :hotkey \w}
                                                  {:name "Trap" :hotkey \t}
                                                  {:name "Food" :hotkey \f}
                                                  {:name "Signal" :hotkey \s}
                                                  {:name "Survival" :hotkey \v}]}]]]]])))

(zc/def-component Recipes
  [this]
  (let [{:keys [game-state]} (zc/props this)
        selected-recipe-hotkey (get-in game-state [:world :selected-recipe-hotkey])
        selected-empty (rcrafting/selected-recipe-empty? game-state)
        items (rcrafting/player-recipes game-state)]
    (zc/csx [zcui/Popup {:style {:top -7}} [
              [:text {:style {:bottom 1 :left 25}} ["| Recipes |"]]
              [:view {:style {:display :flex
                              :flex-direction :row
                              :align-items :flex-start}} [
                [:view {:style {:height 13
                                :padding-left 1}} [
                  [ruicommon/MultiSelect {:style {:width 40}
                                          :items items
                                          :selected-hotkeys #{selected-recipe-hotkey}}]]]
                (if selected-empty
                  (zc/csx [ruicommon/HotkeyLabel {:style {:margin-right 5} :hotkey \n :label "new"}])
                  (zc/csx [RecipeDetail {:game-state game-state}]))]]]])))

