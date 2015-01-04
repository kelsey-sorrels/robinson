(ns robinson.common-test
  (:use clojure.test
        robinson.common
        robinson.world))

(deftest test-adjacent-to-player-0
  (is (false? (adjacent-to-player? {:world {:player {:pos {:x 26, :y 9}}}}
                                  {:x 34 :y 7}))))

(deftest test-adjacent-to-player-1
  (is (true? (adjacent-to-player? {:world {:player {:pos {:x 26, :y 9}}}}
                                  {:x 25 :y 9}))))

(def direction->cells-state
 {:world {:viewport {:width 6
                     :height 5}
          :width 6
          :height 5
          :player {:pos {:x 2 :y 2}}
          :places {[0 0] [[:a :b :c :d :e :f]
                          [:g :h :i :j :k :l]
                          [:m :n :o :p :q :r]
                          [:s :t :y :v :w :x]
                          [:y :z :0 :1 :2 :3]]}}})

(deftest direction->cells-0
  (is (= (direction->cells
            direction->cells-state
            :left
            2)
         [:n :m])))

(deftest direction->cells-1
  (is (= (direction->cells
            direction->cells-state
            :right
            3)
         [:p :q :r])))

(deftest direction->cells-2
  (is (= (direction->cells
            direction->cells-state
            :up
            2)
         [:i :c])))

(deftest direction->cells-3
  (is (= (direction->cells
            direction->cells-state
            :down
            2)
         [:y :0])))
          

(def first-collidable-cells-state
 {:world {:viewport {:width 5
                     :height 5}
          :width 5
          :height 5
          :player {:pos {:x 2 :y 2}}
          :npcs []
          :places {[0 0] [[{:type :vertical-wall}  {:type :vertical-wall}{:type :vertical-wall}{:type :vertical-wall}{:type :vertial-wall}]
                          [{:type :horizontal-wall}{:type :floor}        {:type :floor}        {:type :floor}        {:type :horizontal-wall}]
                          [{:type :horizontal-wall}{:type :floor}        {:type :floor}        {:type :floor}        {:type :horizontal-wall}]
                          [{:type :horizontal-wall}{:type :floor}        {:type :floor}        {:type :floor}        {:type :horizontal-wall}]
                          [{:type :vertical-wall}  {:type :vertical-wall}{:type :vertical-wall}{:type :vertical-wall}{:type :vertial-wall}]]}}})

(deftest first-collidable-cells-0
  (is (= {:cell {:type :vertical-wall} :pos {:x 2 :y 0}}
         (let [state first-collidable-cells-state] 
           (first-collidable-object state :up 3)))))

