(ns robinson.world-test
  (:require [robinson.world :as rw]
            [clojure.test :as t
              :refer (is deftest with-test run-tests testing)]))


(deftest test-adjacent-to-player-0
  (is (false? (rw/adjacent-to-player? {:world {:player {:pos {:x 26, :y 9}}}}
                                      {:x 34 :y 7}))))

(deftest test-adjacent-to-player-1
  (is (true? (rw/adjacent-to-player? {:world {:player {:pos {:x 26, :y 9}}}}
                                     {:x 25 :y 9}))))

(deftest direction->xys-0
  (is (= [[2 3] [2 2]]
         (rw/direction->xys {:world {:width 5 :height 5}} 2 3 :up 2))))

(deftest direction->xys-1
  (is (= [[2 1] [2 2]]
         (rw/direction->xys {:world {:width 5 :height 5}} 2 1 :down 2))))

(deftest direction->xys-2
  (is (= [[2 1] [1 1]]
         (rw/direction->xys {:world {:width 5 :height 5}} 2 1 :left 2))))

(deftest direction->xys-3
  (is (= [[2 1] [3 1]]
         (rw/direction->xys {:world {:width 5 :height 5}} 2 1 :right 2))))

(def direction->cells-state
 {:world {:viewport {:width 6
                     :height 5}
          :width 6
          :height 5
          :player {:pos {:x 2 :y 2}}
          :places {[0 0] {:cells
                         [[:a :b :c :d :e :f]
                          [:g :h :i :j :k :l]
                          [:m :n :o :p :q :r]
                          [:s :t :y :v :w :x]
                          [:y :z :0 :1 :2 :3]]}}}})
(deftest get-cell-0
  (is (= :a (rw/get-cell direction->cells-state 0 0))))

(deftest get-cell-1
  (is (= :b (rw/get-cell direction->cells-state 1 0))))

(deftest get-cell-2
  (is (= :g (rw/get-cell direction->cells-state 0 1))))

(deftest direction->cells-0
  (is (= (rw/direction->cells
            direction->cells-state
            :left
            2)
         [:n :m])))

(deftest direction->cells-1
  (is (= (rw/direction->cells
            direction->cells-state
            :right
            3)
         [:p :q :r])))

(deftest direction->cells-2
  (is (= (rw/direction->cells
            direction->cells-state
            :up
            2)
         [:i :c])))

(deftest direction->cells-3
  (is (= (rw/direction->cells
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
          :places {[0 0] {:cells
                          [[{:type :vertical-wall}  {:type :vertical-wall}{:type :vertical-wall}{:type :vertical-wall}{:type :vertial-wall}]
                          [{:type :horizontal-wall}{:type :floor}        {:type :floor}        {:type :floor}        {:type :horizontal-wall}]
                          [{:type :horizontal-wall}{:type :floor}        {:type :floor}        {:type :floor}        {:type :horizontal-wall}]
                          [{:type :horizontal-wall}{:type :floor}        {:type :floor}        {:type :floor}        {:type :horizontal-wall}]
                          [{:type :vertical-wall}  {:type :vertical-wall}{:type :vertical-wall}{:type :vertical-wall}{:type :vertial-wall}]]}}}})

(deftest direction->cellsxy-0
  (is (= [[{:type :floor} 2 1]
          [{:type :vertical-wall} 2 0]]
         (rw/direction->cellsxy first-collidable-cells-state 2 1 :up 3))))

(deftest first-collidable-cells-0
  (is (= {:cell {:type :vertical-wall} :pos {:x 2 :y 0}}
         (let [state first-collidable-cells-state] 
           (rw/first-collidable-object state :up 3)))))

(deftest first-collidable-cells-1
  (is (= {:player {:pos {:x 2 :y 2}}}
         (let [state first-collidable-cells-state] 
           (rw/first-collidable-object state 1 2 :right 3)))))

(def assoc-cells-state
  {:world {:viewport {:width 5
                     :height 5}
          :width 5
          :height 5
          :player {:pos {:x 2 :y 2}}
          :npcs []
          :places {[0 0] {:cells
                         [[{:type :vertical-wall}  {:type :vertical-wall}{:type :vertical-wall}{:type :vertical-wall}{:type :vertial-wall}]
                          [{:type :horizontal-wall}{:type :floor}        {:type :floor}        {:type :floor}        {:type :horizontal-wall}]
                          [{:type :horizontal-wall}{:type :floor}        {:type :floor}        {:type :floor}        {:type :horizontal-wall}]
                          [{:type :horizontal-wall}{:type :floor}        {:type :floor}        {:type :floor}        {:type :horizontal-wall}]
                          [{:type :vertical-wall}  {:type :vertical-wall}{:type :vertical-wall}{:type :vertical-wall}{:type :vertial-wall}]]}}}})

(def assoc-cells-result
  {:world {:viewport {:width 5
                     :height 5}
          :width 5
          :height 5
          :player {:pos {:x 2 :y 2}}
          :npcs []
          :places {[0 0] {:cells
                         [[{:type :vertical-wall}  {:type :vertical-wall}        {:type :vertical-wall}{:type :vertical-wall}{:type :vertial-wall}]
                          [{:type :horizontal-wall}{:type :floor :discovered 10} {:type :floor}        {:type :floor}        {:type :horizontal-wall}]
                          [{:type :horizontal-wall}{:type :sand :discovered 10} {:type :floor}        {:type :floor}        {:type :horizontal-wall}]
                          [{:type :horizontal-wall}{:type :floor}                {:type :floor}        {:type :floor}        {:type :horizontal-wall}]
                          [{:type :vertical-wall}  {:type :vertical-wall}        {:type :vertical-wall}{:type :vertical-wall}{:type :vertial-wall}]]}}}})

(deftest assoc-cells-state-0
  (is (= (rw/assoc-cells assoc-cells-state {[1 1] {:discovered 10}
                                            [1 2] {:discovered 10 :type :sand}})
         assoc-cells-result)))


(def m
  {[ 0  0] :a
   [0  -1] :b
   [-1 -1] :c
   [-1  0] :d})

(deftest get-map-test-0
  (is (= (get-in m [[0   0]]) :a))
  (is (= (get-in m [[0  -1]]) :b))
  (is (= (get-in m [[-1 -1]]) :c))
  (is (= (get-in m [[-1  0]]) :d)))

