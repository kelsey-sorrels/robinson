(ns robinson.ui.components.crafting
  (:require
            [robinson.ui.components.common :as ruicommon]
            [robinson.crafting :as rcrafting]
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

(zc/def-component CraftSubmenu
  [this]
  (let [{:keys [game-state recipe-type]} (zc/props this)
        selected-recipe-path (get-in game-state [:world :craft-recipe-path])
        hotkey               (when selected-recipe-path
                               (last selected-recipe-path))
        recipes              (rcrafting/get-recipes-by-category game-state recipe-type)]
    (log/info recipe-type)
    ;(log/info (get-recipes game-state recipe-type))
    (zc/csx [:view {:style {:width 40
                            :height 10
                            :padding 1
                            :bottom 1
                            :display :flex
                            :flex-direction :row
                            :align-itmes :stretch}} [
              [:view {:style {:flex 1}} [
                (if (seq recipes)
                  (zc/csx
                    [ruicommon/MultiSelect {:style {:width 30}
                                  :title " Plans"
                                  :selected-hotkeys [hotkey]
                                  :items (map (fn [recipe]
                                                 #_(log/info (get recipe :hotkey))
                                                 {:name (rcrafting/recipe-name recipe) :hotkey (get recipe :hotkey)})
                                              recipes)}])
                  (zc/csx [:text {} ["No recipes"]]))
                [:text {} ["n - create new"]]]]
              [:view {:style {:flex 1}} [
                ;; render recipe-info
                (if hotkey
                  (let [matching-recipes   (filter (fn [recipe] (= (get recipe :hotkey) hotkey))
                                                   recipes)
                        recipe             (get (first matching-recipes) :recipe)
                        exhaust            (get recipe :exhaust [])
                        have               (get recipe :have-or [])
                        inventory-id-freqs (rp/inventory-id-freqs game-state)]
                    #_(log/info "exhaust" exhaust "have" have)
                    (zc/csx [ruicommon/ItemList {:style {:width "75%"}
                                       :items
                                        (concat
                                          [{:s "" :fg :black :bg :white :style #{}}
                                           {:s "Consumes" :fg :black :bg :white :style #{}}]
                                          (if (empty? exhaust)
                                            [{:s "N/A" :fg :black :bg :white :style #{}}]
                                            (let [idx-ids (mapcat (fn [ids]
                                                             (map-indexed vector ids))
                                                           (partition-by identity (sort exhaust)))]
                                              (println "idx-ids" idx-ids)
                                              (reduce (fn [lines [idx id]]
                                                        (conj lines
                                                              (if (< idx (get inventory-id-freqs id 0))
                                                                {:s (ig/id->name id) :fg :black :bg :white :style #{}}
                                                                {:s (ig/id->name id) :fg :gray :bg :white :style #{}})))
                                                     []
                                                     idx-ids)))
                                          [{:s "" :fg :black :bg :white :style #{}}
                                           {:s "Required tools" :fg :black :bg :white :style #{}}]
                                          (if (empty? have)
                                            [{:s "N/A" :fg :black :bg :white :style #{}}]
                                            (map (fn [id] {:s (ig/id->name id) :fg :black :bg :white :style #{}}) have)))}]))
                  (zc/csx [:text {} ["Select a recipe"]]))]]]])))

(zc/def-component CraftWeapon
  [this]
  (let [{:keys [game-state]} (zc/props this)]
    (zc/csx [zcui/Popup {:style {:margin-top 5}} [
              [:text {:style {:bottom 1}} ["| Craft Weapon |"]]
              [CraftSubmenu {:game-state game-state :recipe-type :weapons}]]])))

(zc/def-component CraftSurvival
  [this]
  (let [{:keys [game-state]} (zc/props this)]
    (zc/csx [zcui/Popup {:style {:margin-top 5
                                 :height 17}} [
              [:text {:style {:bottom 1}} ["| Craft Survival |"]]
    (zc/csx [CraftSubmenu {:game-state game-state :recipe-type :survival}])]])))

(zc/def-component CraftShelter
  [this]
  (let [{:keys [game-state]} (zc/props this)]
    (zc/csx [zcui/Popup {:style {:margin-top 5}} [
              [:text {:style {:bottom 1}} ["| Craft Shelter |"]]
    (zc/csx [CraftSubmenu {:game-state game-state :recipe-type :shelter}])]])))

(zc/def-component CraftTransportation
  [this]
  (let [{:keys [game-state]} (zc/props this)]
    (zc/csx [zcui/Popup {:style {:margin-top 5
                                 :height 15}} [
              [:text {:style {:bottom 1}} ["| Craft Transportation |"]]
    (zc/csx [CraftSubmenu {:game-state game-state :recipe-type :transportation}])]])))


(zc/def-component SelectRecipeNode
  [this]
  (let [{:keys [recipe]} (zc/props this)
        n (get recipe :current-node)
        layers (get recipe :layers)
        x (rcrg/node-x layers n)
        y (rcrg/node-y layers n)]
    (zc/csx [ruicommon/Cursor {:pos {:x (- x 39) :y (- y 13)}}])))

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
(zc/def-component EmptyRecipeDetail
  [this]
  (zc/csx
    [:view {} [
      [:text {} ["Empty"]]
      [:view {:style {:height 10}}]
      [:view {:style {:width 20 :display :flex
                              :flex-direction :row}}
        [:text {} [[ruicommon/Highlight {} ["n"]]
                   [:text {} ["-new"]]]]
        [:text {} [[ruicommon/Highlight {} ["Esc"]]
                   [:text {} ["-back"]]]]]]]))

(zc/def-component NonEmptyRecipeDetail
  [this]
  (let [{:keys [recipe]} (zc/props this)]
    (zc/csx [:text {} [(str (get recipe :name "Unknown Name"))]])))

(zc/def-component RecipeDetail
  [this]
  (let [{:keys [game-state]} (zc/props this)
        selected-recipe-hotkey (get-in game-state [:world :selected-recipe-hotkey])
        player-recipe (first (filter (fn [recipe] (= (get recipe :hotkey) selected-recipe-hotkey))
                                     (rcrafting/player-recipes game-state)))]
    (if (get player-recipe :empty)
      (zc/csx [EmptyRecipeDetail {}])
      (zc/csx [NonEmptyRecipeDetail {:recipe player-recipe}]))))

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

