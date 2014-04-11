(ns dungeon-crusade.quests.test-quest-2)


(def test-quest-2 {
  :id "TQ-1"
  :name "Kill MOAR Rats Test"
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
 
