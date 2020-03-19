(ns robinson.ui.components.recipes
  (:require
            [robinson.ui.components.common :as ruicommon]
            [robinson.crafting :as rcrafting]
            [robinson.crafting.mod-protocol :as rcmp]
            [robinson.player :as rp]
            [robinson.inventory :as ri]
            [robinson.itemgen :as ig]
            [robinson.color :as rcolor]
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
        [ruicommon/TitledList {:title "Type:" :names (or (get recipe :recipe/name)
                                                         ;(str (get recipe :type))
                                                         ;(get recipe :name)
                                                         "Unknown")}]
        [:text {} [""]]
        [ruicommon/TitledList {:title "Attributes:" :names (let [effects (get recipe :effects)]
                                                             (if (not-empty effects)
                                                               (map rcrafting/full-name effects)
                                                               ["None"]))}]
        (when (or true (= (get recipe :type) :boat))
          ;(log/info (get recipe :$/items))
          (zc/csx [:view {} [ 
            [:text {} [""]]
            [ruicommon/TitledList {:title "Items Used:"
                                   :names (->> (get recipe :$/items)
                                               (map :name)
                                               frequencies
                                               (map (fn [[item-name n]] (str item-name "(" n ")"))))}]]]))]])))

(zc/def-component VScrollBar
  [this]
  (let [{:keys [height num-items pos]} (zc/props this)]
    (zc/csx [:view {:style {:display :flex
                            :flex-direction :column
                            :width 1
                            :justify-content :space-between
                            :background-color (rcolor/color->rgb :dark-gray)
                            :height "100%"}} [
      [:view {} [
        [:text {:style {:color (rcolor/color->rgb :highlight)}} [ruicommon/up-arrow-char]]]]
      [:view {:style {:height (- height 2)
                      :background-color (rcolor/color->rgb :darker-gray)}} [
        [:text {:style {:color (rcolor/color->rgb :dark-gray)
                        :top (int (/ (* pos height) (dec num-items)))}} [ruicommon/cursor-char]]]]
      [:view {} [
        [:text {:style {:color (rcolor/color->rgb :highlight)}} [ruicommon/down-arrow-char]]]]]])))


(zc/def-component ScrollBar
  [this]
  (let [{:keys [width height num-items pos orientation]} (zc/props this)]
    (case orientation
      :vertical
        (zc/csx [VScrollBar {:height height :num-items num-items :pos pos}])
      ; TODO: max HScrollBar
      #_#_:horizontal
        (zc/csx [HScrollBar {:width width :num-items num-items :pos pos}]))))

(zc/def-component RecipeChoices
  [this]
  (let [{:keys [game-state recipe max-items]} (zc/props this)
        current-stage (get recipe :current-stage)
        choices (get current-stage :event/choices [{:name "continue" :hotkey :space}])
        done (some (fn [choice] (get choice :done)) choices)
        start-index (get current-stage :start-index 0)
        max-items (or max-items 8)
        start-index (min start-index (- (count choices) max-items))
        items (take max-items (drop start-index choices))
        show-scroll (< (count items) (count choices))]
    (zc/csx
      [:view {:style {:display :flex
                      :align-items :center
                      :width 26 :margin-left 5 :margin-right 5}} [
        [:text {} [(get current-stage :title "")]]
        [:text {} [""]]
        (if done
          (zc/csx [RecipeTotal {:recipe recipe}])
          (zc/csx [:text {} [(str (get current-stage :description ""))]]))
        [:view {:style {:display :flex
                        :flex-direction :row
                        :align-items :center
                        :width 25
                        :margin-top 3
                        :margin-left 5
                        :margin-right 5}} [
          [ruicommon/MultiSelect {
            :style {:width 20}
            :game-state game-state
            :items (map (fn [item]
                          (-> item
                             (cond->
                               (keyword (get item :hotkey))
                                 (update :hotkey name))
                             (cond->
                               (get-in item [:material :amount])
                                 (assoc :count (get-in item [:material :amount])))
                             (assoc :disabled
                               (not (rcrafting/choice-requirements-satisfied? game-state item)))))
                        items)}]
          (when show-scroll
            (zc/csx [ScrollBar {:orientation :vertical
                                :height (count items)
                                :num-items (count choices)
                                :pos start-index}]))]]]])))

(zc/def-component CraftInProgressRecipe
  [this]
  (let [{:keys [game-state]} (zc/props this)
        recipe (rcrafting/current-recipe game-state)]
    (zc/csx [zcui/Popup {:style {:margin-top 3
                                 :height 20}} [
              [:text {:style {:left 22 :bottom 1}} ["| New Recipe |"]]
              [:view {:style {:display :flex
                              :flex-direction :row}} [
                [RecipeChoices {:game-state game-state
                                :recipe recipe}]
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
                                                  {:name "Boat" :hotkey \b}
                                                  #_{:name "Trap" :hotkey \t}
                                                  #_{:name "Food" :hotkey \f}
                                                  #_{:name "Signal" :hotkey \s}
                                                  #_{:name "Survival" :hotkey \v}]}]]]]])))

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

