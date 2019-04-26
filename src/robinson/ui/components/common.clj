(ns robinson.ui.components.common
  (:require
            [robinson.common :as rc]
            [robinson.color :as rcolor]
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

(defn hotkey->str [hotkey]
  (cond
    (= hotkey \space)
      "space"
    (keyword? hotkey)
      (name hotkey)
    :default
      (str hotkey)))

(zc/def-component HotkeyLabel
  [this]
  (let [{:keys [hotkey sep text style]} (zc/props this)
        hotkey-str (hotkey->str hotkey)]
    (zc/csx 
      [:view {:style (merge
                       {:display :flex
                      :width 10
                      :flex-direction :row}
                       style)} [
        [Highlight {:style {:width (count hotkey-str)}} [(or hotkey-str "")]]
        [:text {:style {:width 1}} [(or sep "-")]]
        [:text {:style {:width (count text)}} [text]]]])))
  
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
      (zc/csx [:view {}]))))

(zc/def-component TitledList
  [this]
  (let [{:keys [title names]} (zc/props this)]
    (zc/csx [:view {} (cons
      (zc/csx [:text {} [title]])
      (if (seq names)
        (map (fn [n] (zc/csx [:text {} [n]])) names)
        [(zc/csx [:text {} ["None"]])]))])))

(zc/def-component MultiSelectItem
  [this]
  (let [{:keys [style hotkey selected count name utility wielded wielded-ranged worn alt]} (zc/props this)
        hotkey-str (str (or (hotkey->str hotkey) ""))
        s (format "%s%s%s%s"
            (if count
              (format "%dx " (int count))
              "")
            name
            (if utility
              (format "(%d%%)" (int utility))
              "")
            (cond
              wielded
                "(wielded)"
              wielded-ranged
                "(wielded ranged)"
              worn
                "(worn)"
              :else
                ""))]
    (zc/csx 
      [:view {:style {:display :flex
                      :width 20
                      :flex-direction :row
                      ;:background-color (rcolor/color->rgb :red)
                      :align-items :flex-start}} [
        [Highlight {:style {:flex 1 :width (clojure.core/count hotkey-str)}} [hotkey-str]]
        [:text {:style {:width 1 :flex 2}} [(format "%c"
                     (if hotkey
                       (if selected
                         \+
                         \-)
                       \space))]]
        [:view {:style {:flex 3}} [
          [:text {} [s]]
          (when alt
            (zc/csx [:text {:style {:color (rcolor/color->rgb :gray)}}
                           [alt]]))]]]])))

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

    (zc/csx [:view {:style (or {:flex-direction :column} style)}
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

