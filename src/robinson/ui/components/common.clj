(ns robinson.ui.components.common
  (:require
            [robinson.common :as rc]
            [robinson.color :as rcolor]
            [taoensso.timbre :as log]
            [zaffre.components :as zc]
            [zaffre.components.ui :as zcui]
            [zaffre.util :as zutil]
            [tinter.core :as tinter]
            [loom.graph :as lg]))
 
(zc/def-component Highlight
  [this]
  (let [{:keys [children]} (zc/props this)]
    (zc/csx
      [:text {:style {:color [229 155 8 255]}} children])))



(defn hotkey->str [hotkey]
  (cond
    (= hotkey \space)
      "space"
    :default
      (str hotkey)))

(zc/def-component Cursor
  [this]
  (let [{:keys [pos]} (zc/props this)
        [x y] (rc/pos->xy pos)]
    (if (< (mod (/ (System/currentTimeMillis) 300) 2) 1)
      (let [color (rcolor/color->rgb :highlight 255)
            background-color (rcolor/color->rgb :black 255)]
        (zc/csx [:view {} [
                  [:text {:style {:position :fixed :top y :left x
                                  :color color
                                  :background-color background-color}} ["\u2592"]]]]))
      (zc/csx [:view {}]))))

(zc/def-component MultiSelect
  [this]
  (let [{:keys [style]} (zc/props this)
        default-props {:selected-hotkeys []
                       :items []
                       :disable? (fn [_] false)}
        {:keys [title selected-hotkeys items disable?]} (merge default-props (zc/props this))
        children (map (fn [item]
                        (zc/csx [:text {:style {:color (rcolor/color->rgb (if (or (not (disable? item))
                                                                                  (get item :applicable))
                                                                            :white
                                                                            :-light-gray))}} [
                                  [Highlight {} [(hotkey->str (get item :hotkey))]]
                                  [:text {} [(format "%c%s%s %s %s"
                                               (if-let [hotkey (get item :hotkey)]
                                                 (if (contains? (set selected-hotkeys) hotkey)
                                                   \+
                                                   \-)
                                                 \space)
                                               (if (contains? item :count)
                                                 (format "%dx " (int (get item :count)))
                                                 "")
                                               (get item :name)
                                               (if (contains? item :utility)
                                                 (format "(%d%%)" (int (get item :utility)))
                                                 "")
                                               (cond
                                                 (contains? item :wielded)
                                                   "(wielded)"
                                                 (contains? item :wielded-ranged)
                                                   "(wielded ranged)"
                                                 (contains? item :worn)
                                                   "(worn)"
                                                 :else
                                                   ""))]]]]))
                      items)]
    (zc/csx [:view {:style (or style {})} (concat [(zc/csx [:text {:style {:color (rcolor/color->rgb :white)}} [title]])
                               (zc/csx [:text {} [""]])]
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

