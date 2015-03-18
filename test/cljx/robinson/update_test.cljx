(ns robinson.update-test
  (:use clojure.test
        robinson.common
        robinson.update))

(def test-state-0
 {:world {:seed 0
          :viewport {:width 5
                     :height 5
                     :pos {:x 0 :y 0}}
          :width 5
          :height 5
          :volcano-pos {:x 100 :y 100}
          :volcano-xys []
          :player {:pos {:x 2 :y 2}
                   :inventory []}
          :npcs []
          :places {[0 0] [[{:type :vertical-wall}  {:type :vertical-wall}{:type :vertical-wall}{:type :vertical-wall}{:type :vertial-wall}]
                          [{:type :horizontal-wall}{:type :floor}        {:type :floor}        {:type :floor}        {:type :horizontal-wall}]
                          [{:type :horizontal-wall}{:type :floor}        {:type :floor}        {:type :floor}        {:type :horizontal-wall}]
                          [{:type :horizontal-wall}{:type :floor}        {:type :floor}        {:type :floor}        {:type :horizontal-wall}]
                          [{:type :vertical-wall}  {:type :vertical-wall}{:type :vertical-wall}{:type :vertical-wall}{:type :vertial-wall}]]}}})

(deftest first-collidable-cells-0
  (let [state test-state-0
        state (move state :up)]
    (is (= (get-in state [:world :player :pos]) {:x 2 :y 1}))))


(deftest pick-up-0
  (let [state     test-state-0
        state     (-> state
                    (assoc-in [:world :remaining-hotkeys] [\a])
                    (assoc-in [:world :places [0 0] 2 2 :items] [{:id :test-item}])
                    (assoc-in [:world :selected-hotkeys] #{\a}))
        new-state (pick-up state)]
    (is (= (get-in new-state [:world :player :inventory 0 :id]) :test-item))
    (is (= (get-in new-state [:world :player :inventory 0 :hotkey]) \a))))
