(require
  '[robinson.player :as rp]
  '[robinson.itemgen :as ig]
  '[robinson.monstergen :as mg])

(-> *state*
  (assoc-in 
    [:world :recipes] {})
  (assoc-in
    [:world :current-recipe] nil))
