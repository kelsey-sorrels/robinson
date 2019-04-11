(ns robinson.ui.components.crafting
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
 
(zc/def-component Craft
  [this]
  (let [{:keys [game-state]} (zc/props this)
        selected-recipe-hotkey (get-in game-state [:world :selected-recipe-hotkey])
        items (rcrafting/player-recipes game-state)]
    (zc/csx [zcui/Popup {:style {:top -7}} [
              [:text {:style {:bottom 1}} ["| Craft |"]]
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
                #_[RecipeDetail {:game-state game-state}]]]]])))

