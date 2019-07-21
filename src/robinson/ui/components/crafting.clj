(ns robinson.ui.components.crafting
  (:require
            [robinson.ui.components.common :as ruicommon]
            [robinson.crafting :as rcrafting]
            [robinson.crafting.mod-protocol :as rcmp]
            [robinson.crafting.recipe-gen :as rcrg]
            [robinson.crafting.weapon-gen :as rcwg]
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
 
(zc/def-component RequirementItem
  [this]
  (let [{:keys [text satisfied style slot slot-item slot-selected]} (zc/props this)
        color (rcolor/color->rgb (cond
                                   satisfied :green
                                   slot-item :red
                                   :else :gray))]
    (zc/csx
      [:view (merge {:style {:display :flex
                             :flex-direction :row
                             :left 1
                             :color color}}
                    style) [
        [:text {} [(if satisfied "(∙)" "(X)")]]
        [:text {} [text]]
        (if slot
          (zc/csx [:view {:style {:display :flex :flex-direction :row}} [
            (if slot-item
              (zc/csx [:text {:style {:margin-left 1}} [(str "(" (get slot-item :name) ")")]])
              (zc/csx [:text {:style {:margin-left 1}} ["(Empty)"]]))
            [:text {:style {:color (rcolor/color->rgb :white)}} ["-"]]
            (if slot-selected
              (zc/csx [ruicommon/Cursor {} []])
              (zc/csx [ruicommon/Highlight {} [(str slot)]]))]])
          (zc/csx [:text {} [""]]))]])))

(zc/def-component Requirement-Each-Of
  [this]
  (let [{:keys [requirements satisfied slots-filled]} (zc/props this)]
    (zc/csx
      [RequirementItem {:satisfied satisfied :slot-item slots-filled :text "Each of:"}])))

(zc/def-component Requirement-And
  [this]
  (let [{:keys [requirements satisfied slot slot-item slot-selected]} (zc/props this)]
    (zc/csx
      [RequirementItem {:satisfied satisfied :slot slot :slot-item slot-item :slot-selected slot-selected :text "And:"}])))

(zc/def-component Requirement-Or
  [this]
  (let [{:keys [requirements satisfied slot slot-item slot-selected]} (zc/props this)]
    (zc/csx
      [RequirementItem {:satisfied satisfied :slot slot :slot-item slot-item :slot-selected slot-selected :text "Or:"}])))

(zc/def-component Requirement-Repeat
  [this]
  (let [{:keys [requirements satisfied slot slot-item slot-selected n]} (zc/props this)]
    (zc/csx
      [RequirementItem {:satisfied satisfied :slot slot :slot-item slot-item :slot-selected slot-selected
                        :text (str "Repeat " n ":")}])))

(zc/def-component Requirement-Tool
  [this]
  (let [{:keys [requirements satisfied slot slot-item slot-selected]} (zc/props this)]
    (zc/csx
      [RequirementItem {:satisfied satisfied :slot slot :slot-item slot-item :slot-selected slot-selected :text "Tool:"}])))


;; Drop down to fn->component definition due to recursive nature of this component
(def RequirementsTree (zc/fn->component
  (fn RequirementsTreeFn
    [this]
    (let [{:keys [game-state requirements slot slot-item slot-selected]} (zc/props this)]
      (if (coll? requirements)
        (let [head-requirements (first requirements)
              rest-requirements (rest requirements)]
          (zc/csx
            [:view {}
              (case head-requirements
                each-of
                  (let [satisfied (rcrafting/requirements-satisfied? game-state requirements)]
                    (cons
                      (zc/csx [Requirement-Each-Of {:satisfied satisfied
                                                    :slots-filled (every? (partial rcrafting/slot->item game-state)
                                                                          (range (count rest-requirements)))}])
                      (map-indexed
                        (fn [idx req]
                          (let [selected-slot (get-in game-state [:world :selected-slot])
                                slot-selected (= idx selected-slot)]
                            (zc/csx [:view {:style {:left 1}} [
                              [RequirementsTree {:game-state game-state
                                                 :requirements req
                                                 :slot idx
                                                 :slot-item (rcrafting/slot->item game-state idx)
                                                 :slot-selected slot-selected}]]])))
                        rest-requirements)))
                tool
                  (let [satisfied (rcrafting/item-satisfies-requirement-clause? slot-item requirements)]
                    (cons
                      (zc/csx [Requirement-Tool {:satisfied satisfied
                                                 :slot slot
                                                 :slot-item slot-item
                                                 :slot-selected slot-selected}])
                      (map (fn [req]
                             (zc/csx [:view {:style {:left 1}} [
                                 [RequirementsTree {:game-state game-state
                                                     :requirements req
                                                     :slot-item slot-item}]]]))
                        rest-requirements)))
                and
                  (let [satisfied (rcrafting/item-satisfies-requirement-clause? slot-item requirements)]
                    (cons
                      (zc/csx [Requirement-And {:satisfied satisfied
                                                :slot slot
                                                :slot-item slot-item
                                                :slot-selected slot-selected}])
                      (map (fn [req]
                             (zc/csx [:view {:style {:left 1}} [
                                [RequirementsTree {:game-state game-state
                                                    :requirements req
                                                    :slot-item slot-item}]]]))
                        rest-requirements)))
                or
                  (let [satisfied (rcrafting/item-satisfies-requirement-clause? slot-item requirements)]
                    (cons
                      (zc/csx [Requirement-Or {:satisfied satisfied
                                               :slot slot
                                               :slot-item slot-item
                                               :slot-selected slot-selected}])
                      (map (fn [req]
                             (zc/csx [:view {:style {:left 1}} [
                                 [RequirementsTree {:game-state game-state
                                                    :requirements req
                                                    :slot-item slot-item}]]]))
                        rest-requirements)))
                count
                  (let [satisfied (rcrafting/item-satisfies-requirement-clause? slot-item requirements)]
                    (cons
                      (zc/csx [Requirement-Repeat {:satisfied satisfied
                                                   :slot slot
                                                   :slot-item slot-item
                                                   :slot-selected slot-selected
                                                   :n (first rest-requirements)}])
                      (map (fn [req]
                             (zc/csx [:view {:style {:left 1}} [
                                 [RequirementsTree {:game-state game-state
                                                    :requirements req
                                                    :slot-item slot-item}]]]))
                        (rest rest-requirements)))))]))
        (let [satisfied (when slot-item
                          (rcrafting/item-satisfies-requirement-clause? slot-item requirements))]
          (zc/csx
            [RequirementItem {:satisfied satisfied
                              :slot slot
                              :slot-item slot-item
                              :slot-selected slot-selected
                              :text (str requirements)
                              :style {:left 1}}])))))
    "RequirementsTree"))

(zc/def-component Requirements
  [this]
  (let [{:keys [game-state requirements selected-items]} (zc/props this)]
    (zc/csx
      [:view {:style {:margin-left 5 :margin-right 5}} [
        [:text {} ["Requirements"]]
        [RequirementsTree {:game-state game-state :requirements requirements}] ]])))

(zc/def-component Craft
  [this]
  (let [{:keys [game-state]} (zc/props this)
        selected-recipe-hotkey (get-in game-state [:world :selected-recipe-hotkey])
        recipe (get-in game-state [:world :recipes selected-recipe-hotkey])
        recipe-name (rcrafting/recipe-name recipe)
        requirements (rcrafting/recipe-requirements recipe)
        items (ri/player-inventory game-state)]
    (zc/csx [zcui/Popup {:style {:top -7}} [
              [:text {:style {:bottom 1 :left 28}} ["| Craft |"]]
              [:view {:style {:padding-left 2 :padding-right 2}} [
                [:view {:style {:min-width 40
                                :display :flex
                                :justify-content :center
                                :flex-direction :row
                                :text-align :left
                                :min-height 12}} [
                  [:view {} [
                    [ruicommon/TitledList {:title "Type:" :names [recipe-name]}]
                    [:text {} [""]]
                    [ruicommon/TitledList {:title "Attributes:" :names (map rcrafting/full-name (get recipe :effects))}]]]
                  [Requirements {:game-state game-state :requirements requirements :items items}]
                  [:view {:style {}} [
                    [:text {} ["Inventory"]]
                    [ruicommon/MultiSelect {:style {}
                                            :items items
                                            :selected-hotkeys #{}}]]]]]
                [:view {:style {:display :flex
                                :flex-direction :row
                                :justify-content :space-evently
                                :margin-left 25}} [
                  [ruicommon/HotkeyLabel {:hotkey :escape :label "back" :style {:margin-right 2}}]
                  [ruicommon/HotkeyLabel {:hotkey :space :label "make"}]]]]]]])))
 

