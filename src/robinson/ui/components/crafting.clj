(ns robinson.ui.components.crafting
  (:require
            [robinson.ui.components.common :as ruicommon]
            [robinson.crafting :as rcrafting]
            [robinson.crafting.mod :as rcmod]
            [robinson.crafting.recipe-gen :as rcrg]
            [robinson.crafting.weapon-gen :as rcwg]
            [robinson.player :as rp]
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
    (log/info "requirement-item" text satisfied)
    (zc/csx
      [:text (merge {:style {:left 1
                             :color color}}
                    style) [
        [:text {} [(if satisfied "(∙)" "(X)")]]
        [:text {} [text]]
        (if slot
          (zc/csx [:text {} [
            (if slot-item
              (zc/csx [:text {} [" (" (get slot-item :name) ")"]])
              (zc/csx [:text {} [" (Empty)"]]))
            [:text {:style {:color (rcolor/color->rgb :white)}} ["-"]]
            (if slot-selected
              (zc/csx [ruicommon/Cursor {} []])
              (zc/csx [ruicommon/Highlight {} [(str slot)]]))]])
          (zc/csx [:text {} [""]]))]])))

(zc/def-component Requirement-Each-Of
  [this]
  (let [{:keys [requirements satisfied]} (zc/props this)]
    (zc/csx
      [RequirementItem {:satisfied satisfied :text "Each of:"}])))

(zc/def-component Requirement-And
  [this]
  (let [{:keys [requirements satisfied slot slot-item slot-selected]} (zc/props this)]
    (zc/csx
      [RequirementItem {:satisfied satisfied :slot slot :slot-item slot-item :slot-selected slot-selected :text "And:"}])))

(defn slot->item [state slot]
  (let [selected-recipe-hotkey (get-in state [:world :selected-recipe-hotkey])
        hotkey (get-in state [:world :player :recipes selected-recipe-hotkey :slots slot])]
    ;(log/info selected-recipe-hotkey hotkey slot)
    ;(log/info (type hotkey))
    (rp/inventory-hotkey->item state hotkey)))

(zc/def-component Requirements-Tree
  [this]
  (let [{:keys [game-state requirements slot slot-item slot-selected]} (zc/props this)]
    (if (coll? requirements)
      (let [head-requirements (first requirements)
            rest-requirements (rest requirements)]
        (zc/csx
          [:view {}
            (case head-requirements
              each-of
                (let [satisfied (every? identity
                                  (map-indexed (fn [idx req]
                                                 (let [slot-item (slot->item game-state idx)]
                                                   (rcrafting/item-satisfies-requirement-clause? slot-item req)))
                                    rest-requirements))]
                  (cons
                    (zc/csx [Requirement-Each-Of {:satisfied satisfied}])
                    (map-indexed (fn [idx req]
                      (let [selected-slot (get-in game-state [:world :selected-slot])
                            slot-selected (= idx selected-slot)]
                        (log/info req)
                        (zc/csx [:view {:style {:left 1}} [
                          [Requirements-Tree {:game-state game-state
                                              :requirements req
                                              :slot-item (slot->item game-state idx)
                                              :slot idx
                                              :slot-selected slot-selected}]]])))
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
                               [Requirements-Tree {:game-state game-state
                                                   :requirements req
                                                   :slot-item slot-item}]]]))
                      rest-requirements))))]))
      (let [satisfied (when slot-item
                        (rcrafting/item-satisfies-requirement-clause? slot-item requirements))]
        (log/info "req" requirements "item" slot-item)
        (zc/csx
          [RequirementItem {:satisfied satisfied
                            :slot slot
                            :slot-item slot-item
                            :slot-selected slot-selected
                            :text (str requirements)
                            :style {:left 1}}])))))

(zc/def-component Requirements
  [this]
  (let [{:keys [game-state requirements selected-items]} (zc/props this)]
    (zc/csx
      [:view {:style {:width 27 :height 13}} [
        [:text {} ["Requirements"]]
        [Requirements-Tree {:game-state game-state :requirements requirements}] ]])))

(zc/def-component Craft
  [this]
  (let [{:keys [game-state]} (zc/props this)
        selected-recipe-hotkey (get-in game-state [:world :selected-recipe-hotkey])
        recipe (get-in game-state [:world :player :recipes selected-recipe-hotkey])
        recipe-blueprint (-> recipe :types rcrafting/get-recipe-by-types)
        items (rp/player-inventory game-state)]
    (zc/csx [zcui/Popup {:style {:top -7}} [
              [:text {:style {:bottom 1}} ["| Craft |"]]
              [:view {:style {:width 40 :display :flex
                              :justify-content :center
                              :flex-direction :row
                              :text-align :left
                              #_#_:align-items :flex-start}} [
                [:view {:style {:width 20}} [
                  [ruicommon/TitledList {:title "Type:" :names [(-> recipe-blueprint :recipe/id ig/id->name)]}]
                  [:text {} [""]]
                  [ruicommon/TitledList {:title "Attributes:" :names (map rcmod/mod-name (get recipe :effects))}]]]
                [Requirements {:game-state game-state :requirements (get recipe-blueprint :recipe/requirements) :items items}]
                [:view {:style {:width 20 :height 13
                                #_#_:left 1
                                #_#_:top 0}} [
                  [:text {} ["Inventory"]]
                  [ruicommon/MultiSelect {:style {:width 40}
                                          :items items
                                          :selected-hotkeys #{selected-recipe-hotkey}}]]]]]]])))
 

