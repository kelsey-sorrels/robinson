(ns robinson.ui.components.common
  (:require
            [robinson.common :as rc]
            [robinson.color :as rcolor]
            [robinson.update :as ru]
            [taoensso.timbre :as log]
            [zaffre.components :as zc]
            [zaffre.components.ui :as zcui]
            [zaffre.util :as zutil]
            [taoensso.timbre :as log]
            [tinter.core :as tinter]
            [loom.graph :as lg]))
 
(zc/def-component Highlight
  [this]
  (let [{:keys [style children]} (zc/props this)]
    (zc/csx
      [:text {:style (merge style {:color [229 155 8 255]})} children])))

(zc/def-component InverseHighlight
  [this]
  (let [{:keys [style children]} (zc/props this)]
    (zc/csx
      [:text {:style (merge style {:color (rcolor/color->rgb :black)
                                   :background-color [229 155 8 255]})} children])))

(defn hotkey->str [hotkey]
  (cond
    (= hotkey :escape)
      "Esc"
    (keyword? hotkey)
      (name hotkey)
    :default
      (str hotkey)))

(zc/def-component HotkeyLabel
  [this]
  (let [{:keys [hotkey sep label style]} (zc/props this)
        {:keys [hover] :or {hover false}} (zc/state this)
        hotkey-str (hotkey->str hotkey)
        white (rcolor/color->rgb :white)
        black (rcolor/color->rgb :black)
        owner zc/*current-owner*]
    ;(log/info hover)
    (zc/csx 
      [:view {:on-click (fn [{:keys [target game-state]}]
                                (ru/update-state game-state hotkey))
              :on-mouse-enter (fn [{:keys [target game-state]}]
                                (binding [zc/*current-owner* owner]
                                  (log/info (log/info "HotkeyLabelEnter" hotkey (-> target second :zaffre/layout)))
                                  (zc/set-state! this {:hover true}))
                                game-state)
              :on-mouse-leave (fn [{:keys [target game-state]}]
                                (binding [zc/*current-owner* owner]
                                  (log/info (log/info "HotkeyLabelLeave" hotkey (-> target second :zaffre/layout)))
                                  (zc/set-state! this {:hover false}))
                                game-state)
              :style (merge
                       {:display :flex
                        :flex-direction :row}
                       style)} [
        (if hover
          (zc/csx [InverseHighlight {} [(or hotkey-str "")]])
          (zc/csx [Highlight {} [(or hotkey-str "")]]))
        [:text {} [(or sep "-")]]
        [:text {#_#_:style {:color (if hover black white)
                        :background-color (if hover white black)}} [label]]]])))
  
(zc/def-component Cursor
  [this]
  (let [{:keys [pos]} (zc/props this)
        pos-style (if pos
                    (let [[x y] (rc/pos->xy pos)]
                      {:position :fixed :top y :left x})
                    {})]
    (if (< (mod (/ (System/currentTimeMillis) 300) 2) 1)
      (let [color (rcolor/color->rgb :highlight 255)
            background-color (rcolor/color->rgb :black 255)]
        (zc/csx [:view {} [
                  [:text {:style (merge pos-style
                                  {:color color
                                   :background-color background-color})} ["\u2592"]]]]))
      (zc/csx [:view {:style {:width 1}}]))))

(zc/def-component TitledList
  [this]
  (let [{:keys [title names]} (zc/props this)]
    (zc/csx [:view {} (cons
      (zc/csx [:text {} [title]])
      (if (seq names)
        (map (fn [n] (zc/csx [:text {} [n]])) names)
        [(zc/csx [:text {} ["None"]])]))])))

(defn- attributes
  [item]
  (let [r (reduce
    (fn [s k]
      (if-let [elem
                (case k
                  :damage
                    (zc/csx [:view {:style {:display :flex
                                            :flex-direction :row
                                            :margin-right 1}} [
                      [:text {:style {:color (rcolor/color->rgb :red)}} ["♥"]]
                      [:text {:style {}} [(format "%+d" (get item k))]]]])
                  :accuracy
                    (zc/csx [:view {:style {:display :flex
                                            :flex-direction :row
                                            :margin-right 1}} [
                      [:text {:style {:color (rcolor/color->rgb :brilliant-blue)}} ["»"]]
                      [:text {:style {}} [(format "%+d" (get item k))]]]])
                  :speed
                    (zc/csx [:view {:style {:display :flex
                                            :flex-direction :row
                                            :margin-right 1}} [
                      [:text {:style {:color (rcolor/color->rgb :green)}} ["»"]]
                      [:text {:style {}} [(format "%+d" (get item k))]]]])
                  :durability
                    (zc/csx [:view {:style {:display :flex
                                            :flex-direction :row
                                            :margin-right 1}} [
                      [:text {:style {:color (rcolor/color->rgb :white)}} ["≡"]]
                      [:text {:style {}} [(format "%+d" (get item k))]]]])
                  nil)]
       (conj s elem)
       s))
    []
    (keys item))]
    r))

(zc/def-component MultiSelectItem
  [this]
  (let [{:keys [style hotkey selected count name utility wielded wielded-ranged worn alt disabled] :as props} (zc/props this)
        hotkey-str (str (or (hotkey->str hotkey) ""))
        count-str (when count (format "%dx " (int count)))
        {:keys [hover] :or {hover false}} (zc/state this)
        owner zc/*current-owner*]
    (zc/csx 
      [:view {:on-click (fn [{:keys [target game-state]}]
                                (ru/update-state game-state hotkey))
              :on-mouse-enter (fn [{:keys [target game-state]}]
                                (binding [zc/*current-owner* owner]
                                  (log/info (log/info "MenuSelectItemEnter" hotkey (-> target second :zaffre/layout)))
                                  (zc/set-state! this {:hover true}))
                                game-state)
              :on-mouse-leave (fn [{:keys [target game-state]}]
                                (binding [zc/*current-owner* owner]
                                  (log/info (log/info "MenuSelectItemExit" hotkey (-> target second :zaffre/layout)))
                                  (zc/set-state! this {:hover false}))
                                game-state)
              :style {:color (rcolor/color->rgb (if disabled :gray :white))
                      :display :flex
                      :flex-direction :row
                      :align-items :flex-start}} [
        (if hover
          (zc/csx [InverseHighlight {} [(or hotkey-str "")]])
          (zc/csx [Highlight {} [(or hotkey-str "")]]))
        [:text {} [(format "%c"
                     (if hotkey
                       (if selected
                         \+
                         \-)
                       \space))]]
        [:view {:style {:display :flex
                        :flex-direction :column
                        :align-items :flex-start}} [
          [:view {:style {:display :flex
                          :flex-direction :row
                          :align-items :flex-start}} [
            (when count-str
              (zc/csx [:text {} [count-str]]))
            [:text {} [name]]
            [:view {:style {:display :flex
                            :flex-direction :row
                            :margin-left 1
                            :margin-right 1}} (attributes props)]
            (when utility
              (zc/csx [:text {:style {:margin-left 1}} [(format "(%d%%)" (int utility))]]))
            [:text {} [(cond
              wielded
                "(wielded)"
              wielded-ranged
                "(wielded ranged)"
              worn
                "(worn)"
              :else
                "")]]]]
          (when alt
            (zc/csx [:text {:style {:color (rcolor/color->rgb :gray)}}
                           [(str alt)]]))]]]])))

(zc/def-component MultiSelect
  [this]
  (let [{:keys [style]} (zc/props this)
        default-props {:selected-hotkeys []
                       :items []
                       :disable? (fn [_] false)}
        {:keys [title selected-hotkeys items disable?]} (merge default-props (zc/props this))
        children (map (fn [item]
                        (let [hotkey (get item :hotkey)]
                          (zc/csx [:view {:style {:color (rcolor/color->rgb (if (or (not (disable? item))
                                                                                    (get item :applicable))
                                                                              :white
                                                                              :light-gray))}} [
                                      [MultiSelectItem (merge item
                                                              {:selected (contains? (set selected-hotkeys) hotkey)}) ]]])))
                      items)]
    (zc/csx [:view {:style (merge {:display :flex
                                   :flex-direction :column
                                   :align-items :flex-start} style)}
                   (concat (when title
                             [(zc/csx [:text {:style {:color (rcolor/color->rgb :white)}} [title]])
                              (zc/csx [:text {} [" "]])])
                           children)])))



(zc/def-component ItemList
  [this]
  (let [{:keys [items style]} (zc/props this)]
    (zc/csx
      [:view {}
          (map (fn [{:keys [fg bg s]}]
            (zc/csx [:view {:style (merge style {:fg fg :bg bg})} [
                      [:text {} [s]]]]))
            items)])))

(zc/def-component SelectItemListItem
  [this]
  (let [{:keys [hotkey selected text]} (zc/props this)]
    (zc/csx
      [:text {} [
        [Highlight {} [(str hotkey " ")]]
        [:text {} [(format "%s %s"
                     (if selected
                       "+"
                       "-")
                     text)]]]])))

(zc/def-component SelectItemList
  [this]
  (let [{:keys [title selected-hotkeys use-applicable items]} (zc/props this)]
    (zc/csx [:view {:style {:width "60%"}} [
              [:text {} [title]]
              [:view {:style {:top 2 :left 10}}
                     (map (fn [item]
                            (zc/csx [SelectItemListItem {:hotkey
                                                           (or (item :hotkey)
                                                               \ )
                                                         :selected
                                                           (contains? selected-hotkeys (get item :hotkey))
                                                         :text
                                                           (str (get item :name)
                                                             (if (contains? item :count)
                                                               (format " (%dx)" (int (get item :count)))
                                                               ""))}]))
                          items)]
              [:text {:style {:top 5 :left 10}} [
                [Highlight {} ["enter "]]
                [:text {} ["to continue"]]]]]])))

