(ns robinson.ui.components.common
  (:require
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


