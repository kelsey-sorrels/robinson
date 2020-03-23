(ns robinson.ui.components.common
  (:require
            [robinson.common :as rc]
            [robinson.color :as rcolor]
            [robinson.update :as ru]
            [robinson.crafting.mod-protocol :as rcmp]
            [taoensso.timbre :as log]
            [zaffre.components :as zc]
            [zaffre.components.ui :as zcui]
            [zaffre.util :as zutil]
            [taoensso.timbre :as log]
            [tinter.core :as tinter]
            [loom.graph :as lg]))
 
(def up-arrow-char    "\u2191")
(def down-arrow-char  "\u2193")
(def cursor-char      "\u2592")
 
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

(zc/def-component GradientText
  [this]
  (let [{:keys [fg-start fg-end bg-start bg-end style children]} (zc/props this)]
    (assert (< 0 (count (first children))) (str "GradientText zero length string" (first children) this))
    (zc/csx
      [:view {:style {:flex-direction :row}} (map (fn [c fg bg]
                  (zc/csx [:text {:style {:color fg :background-color bg}} [(str c)]]))
                 (first children)
                 (rcolor/gradient (or fg-start (rcolor/color->rgb :white))
                                  (or fg-end (rcolor/color->rgb :white))
                                  (count (first children)))
                 (rcolor/gradient (or bg-start (rcolor/color->rgb :black))
                                  (or bg-end (rcolor/color->rgb :black))
                                  (count (first children))))])))

(defn hotkey->str [hotkey]
  (cond
    (= hotkey :escape)
      "Esc"
    (= hotkey :enter)
      "enter"
    (= hotkey :space)
      "space"
    (= hotkey \space)
      "space"
    (keyword? hotkey)
      (name hotkey)
    :default
      (str hotkey)))

#_(zc/def-component HotkeyLabel
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
                                ;(binding [zc/*current-owner* owner]
                                  (log/info (log/info "HotkeyLabelEnter" hotkey (-> target second :zaffre/layout)))
                                  (zc/set-state! this {:hover true}))
                                ;game-state)
              :on-mouse-leave (fn [{:keys [target game-state]}]
                                ;(binding [zc/*current-owner* owner]
                                  (log/info (log/info "HotkeyLabelLeave" hotkey (-> target second :zaffre/layout)))
                                  (zc/set-state! this {:hover false}))
                                ;game-state)
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

(def HotkeyLabel
  (let [white (rcolor/color->rgb :white)
        black (rcolor/color->rgb :black)]
  (zc/create-react-class {
    :display-name "HotkeyLabel"
    :get-initial-state (fn []
      {:hover false})
    :get-default-props (fn hotkey-label-get-default-props [] {
      :on-click (fn [this e]
                  (let [{:keys [hotkey target game-state]} (zc/props this)]
                    (ru/update-state game-state hotkey)))
      :on-mouse-enter (fn [this e]
                        (let [{:keys [hotkey target]} (zc/props this)]
                          (log/info (log/info "HotkeyLabelEnter" hotkey (-> target second :zaffre/layout)))
                          (zc/set-state! this {:hover true})))
      :on-mouse-leave (fn [this e]
                        (let [{:keys [hotkey target]} (zc/props this)]
                            (log/info (log/info "HotkeyLabelLeave" hotkey (-> target second :zaffre/layout)))
                            (zc/set-state! this {:hover false})))})
    :render 
      (fn [this]
        (let [{:keys [hotkey sep label style]} (zc/props this)
              {:keys [hover] :or {hover false}} (zc/state this)
              hotkey-str (hotkey->str hotkey)]
          (zc/csx 
            [:view {:style (merge
                             {:display :flex
                              :flex-direction :row}
                             style)} [
              (if hover
                (zc/csx [InverseHighlight {} [(or hotkey-str "")]])
                (zc/csx [Highlight {} [(or hotkey-str "")]]))
              [:text {} [(or sep "-")]]
              [:text {#_#_:style {:color (if hover black white)
                              :background-color (if hover white black)}} [label]]]])))})))
  
(zc/def-component Cursor
  [this]
  (let [{:keys [pos]} (zc/props this)
        pos-style (if pos
                    (let [[x y] (rc/pos->xy pos)]
                      {:position :absolute
                       :top y
                       :left x
                       :width 1
                       :height 1})
                    {})]
    (if (< (mod (/ (System/currentTimeMillis) 300) 2) 1)
      (let [color (rcolor/color->rgb :highlight 255)
            background-color (rcolor/color->rgb :black 255)]
        (zc/csx [:view {} [
                  [:text {:style (merge pos-style
                                  {:color color
                                   :background-color background-color})} [cursor-char]]]]))
      (zc/csx [:view {:style {:width 1}}]))))

(zc/def-component TitledList
  [this]
  (let [{:keys [title names]} (zc/props this)]
    (zc/csx [:view {} (cons
      (zc/csx [:text {} [title]])
      (cond
        (coll? names)
          (map (fn [n] (zc/csx [:text {} [(str n)]])) names)
        (string? names)
          [(zc/csx [:text {} [(str names)]])]
        :else
          [(zc/csx [:text {} ["None"]])]))])))

(defn attribute-icon
  ([s color]
    (attribute-icon s color nil))
  ([s color amount]
    (let [text (cond
                 amount
                   s
                 (< 0 (count s))
                   (format "+%s" s)
                 :else
                   s )]
      (zc/csx [:view {:style {:display :flex
                              :flex-direction :row
                              :margin-right 1}} [
        [:text {:style {:color (rcolor/color->rgb color)}} [text]]
        (when amount
          (zc/csx [:text {:style {:color (rcolor/color->rgb :white)}} [(format "%+d" amount)]]))]]))))

(defn- attributes
  [item]
  (let [icon-color-map {:damage
                          ["♥" :red]
                        :accuracy
                          ["»" :brilliant-blue]
                        :speed
                          ["»" :green]
                        :durability
                          ["≡" :white]
                        :stunned
                          ["stunning" :white]}
        r (reduce
          (fn [s effect]
            (let [[icon color] (get icon-color-map (get effect :k) [(rcmp/full-name effect) :white])
                  elem (if (satisfies? rcmp/ModQuantifiable effect)
                         (attribute-icon icon color (rcmp/amount effect))
                         (attribute-icon icon color))]
             (conj s elem)))
          []
          (get item :effects))]
    r))

(defn- effect-elems
  [effects]
  (let [item (reduce (fn [item effect]
                       (cond
                         (satisfies? rcmp/ModItemOnCreate effect)
                           (rcmp/item-on-create effect item)
                         :else
                           item))
                     {} effects)]
    (attributes item)))

(zc/def-component MultiSelectItem
  [this]
  (let [{:keys [style hotkey selected count name utility wielded wielded-ranged worn detail disabled effects] :as props} (zc/props this)
        hotkey-str (str (or (hotkey->str hotkey) ""))
        count-str (when count (format "%dx " (int count)))
        {:keys [hover] :or {hover false}} (zc/state this)
        gray (rcolor/color->rgb :gray)
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
        (if disabled
          (zc/csx [:text {:style {:color gray}} [(or hotkey-str "")]])
          (if hover
            (zc/csx [InverseHighlight {} [(or hotkey-str "")]])
            (zc/csx [Highlight {} [(or hotkey-str "")]])))
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
            (when (< 0 (clojure.core/count (str name)))
              (zc/csx [GradientText (select-keys props [:fg-start :fg-end :bg-start :bg-end]) [(or (str name) "unknown")]]))
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
          (when detail
            (if (string? detail)
              (zc/csx [:text {:style {:color (rcolor/color->rgb :gray)}}
                             [(str detail)]])
              (zc/csx 
                [:view {:style {:display :flex
                                :flex-direction :row
                                :margin-left 1
                                :margin-right 1}} (concat (attributes props) (effect-elems effects))])
  ))]]]])))

(zc/def-component MultiSelect
  [this]
  (let [{:keys [style game-state]} (zc/props this)
        default-props {:selected-hotkeys []
                       :items []
                       :disable? (fn [_] false)}
        {:keys [title selected-hotkeys items disable?]} (merge default-props (zc/props this))
        children (map (fn [item]
                        (let [hotkey (get item :hotkey)
                              pred (get item :pred)
                              enabled (if (and pred game-state)
                                          (let [f (ns-resolve *ns* pred)]
                                            (f game-state))
                                        true)]
                          (zc/csx [:view {:style {:color (rcolor/color->rgb (if (or (not (disable? item))
                                                                                    (get item :applicable))
                                                                              :white
                                                                              :light-gray))}} [
                                      [MultiSelectItem (merge item
                                                              {:selected (contains? (set selected-hotkeys) hotkey)
                                                               :disabled (not enabled)})]]])))
                      items)]
    (zc/csx [:view {:style (merge {:display :flex
                                   :flex-direction :column
                                   :align-items :flex-start} style)}
                   (cons (when title
                           (zc/csx [:view {:style {:margin-bottom 1}} [
                             [:text {:style {:color (rcolor/color->rgb :white)}} [title]]]]))
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
                                                           (str (get item :name "Unknown")
                                                             (if (contains? item :count)
                                                               (format " (%dx)" (int (get item :count)))
                                                               ""))}]))
                          items)]
              [:text {:style {:top 5 :left 10}} [
                [Highlight {} ["enter "]]
                [:text {} ["to continue"]]]]]])))

