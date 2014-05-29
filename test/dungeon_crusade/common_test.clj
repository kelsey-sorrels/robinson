(ns dungeon-crusade.common-test
  (:use clojure.test
        dungeon-crusade.common))

(deftest test-adjacent-to-player-0
  (is (false? (adjacent-to-player? {:world {:player {:pos {:x 26, :y 9}}}}
                                  {:x 34 :y 7}))))

(deftest test-adjacent-to-player-1
  (is (true? (adjacent-to-player? {:world {:player {:pos {:x 26, :y 9}}}}
                                  {:x 25 :y 9}))))

(deftest map-indexed-in-p-0
  (is (= (map-indexed-in-p {:a [5 4 3 2 1]} [:a] even? (fn [idx item] idx))
         {:a [5 0 3 1 1]})))

(deftest remove-in-0
  (let [state    {:world {:0 [[{:items [{:type :$} {:type :a} {:type :c}]}]]}}
        expected {:world {:0 [[{:items [{:type :a} {:type :c}]}]]}}]
    (is (= (remove-in state [:world :0 0 0 :items]
                            (fn [item] (= (item :type) :$)))
           expected))))

(def direction->cells-state
 {:world {:player {:pos {:x 2 :y 2}}
          :current-place :0
          :places {:0 [[:a :b :c :d :e :f]
                       [:g :h :i :j :k :l]
                       [:m :n :o :p :q :r]
                       [:s :t :y :v :w :x]
                       [:y :z :0 :1 :2 :3]]}}})

(deftest direction->cells-0
  (is (= (direction->cells
            direction->cells-state
            :left)
         [:n :m])))

(deftest direction->cells-1
  (is (= (direction->cells
            direction->cells-state
            :right)
         [:p :q :r])))

(deftest direction->cells-2
  (is (= (direction->cells
            direction->cells-state
            :up)
         [:i :c])))

(deftest direction->cells-3
  (is (= (direction->cells
            direction->cells-state
            :down)
         [:y :0])))
          

(def first-collidable-cells-state
 {:world {:player {:pos {:x 2 :y 2}}
          :current-place :0
          :npcs []
          :places {:0 [[{:type :vertical-wall}  {:type :vertical-wall}{:type :vertical-wall}{:type :vertical-wall}{:type :vertial-wall}]
                       [{:type :horizontal-wall}{:type :floor}        {:type :floor}        {:type :floor}        {:type :horizontal-wall}]
                       [{:type :horizontal-wall}{:type :floor}        {:type :floor}        {:type :floor}        {:type :horizontal-wall}]
                       [{:type :horizontal-wall}{:type :floor}        {:type :floor}        {:type :floor}        {:type :horizontal-wall}]
                       [{:type :vertical-wall}  {:type :vertical-wall}{:type :vertical-wall}{:type :vertical-wall}{:type :vertial-wall}]]}}})

(deftest first-collidable-cells-0
  (is (= {:cell {:type :vertical-wall}}
         (let [state first-collidable-cells-state] 
           (first-collidable-cell state (direction->cellsxy state :up))))))

