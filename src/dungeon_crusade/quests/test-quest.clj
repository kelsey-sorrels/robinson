(ns dungeon-crusade.quests.test-quest)


(def test-quest {
  :id "TQ-0"
  :name "Kill Rats Test"
  :stages {
    :0 {
      :name "Pre"
      :pred (fn [state] true)
      :update (fn [state] state)
      :nextstagefn (fn [state] :10)}
    :10 {
      :pred (fn [state] false)
      :update (fn [state] state)
      :nextstagefn (fn [state] nil)}}})
