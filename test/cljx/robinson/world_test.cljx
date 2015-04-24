(ns robinson.world-test
  (:require [robinson.world :as rw]
            #+clj
            [clojure.test :as t
              :refer (is deftest with-test run-tests testing)]
            #+cljs
            [cemerick.cljs.test :as t])
 (:require-macros [cemerick.cljs.test
                    :refer (is deftest with-test run-tests testing test-var)]))


(deftest test-adjacent-to-player-0
  (is (false? (rw/adjacent-to-player? {:world {:player {:pos {:x 26, :y 9}}}}
                                      {:x 34 :y 7}))))

(deftest test-adjacent-to-player-1
  (is (true? (rw/adjacent-to-player? {:world {:player {:pos {:x 26, :y 9}}}}
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
          :places {[0 0] [[{:type :vertical-wall}  {:type :vertical-wall}{:type :vertical-wall}{:type :vertical-wall}{:type :vertial-wall}]
                          [{:type :horizontal-wall}{:type :floor}        {:type :floor}        {:type :floor}        {:type :horizontal-wall}]
                          [{:type :horizontal-wall}{:type :floor}        {:type :floor}        {:type :floor}        {:type :horizontal-wall}]
                          [{:type :horizontal-wall}{:type :floor}        {:type :floor}        {:type :floor}        {:type :horizontal-wall}]
                          [{:type :vertical-wall}  {:type :vertical-wall}{:type :vertical-wall}{:type :vertical-wall}{:type :vertial-wall}]]}}})

(deftest first-collidable-cells-0
  (is (= {:cell {:type :vertical-wall} :pos {:x 2 :y 0}}
         (let [state first-collidable-cells-state] 
           (rw/first-collidable-object state :up 3)))))

(def assoc-cells-state
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

(def assoc-cells-result
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

(deftest assoc-cells-state
  (is (= (assoc-cells assoc-cells-state {[1 1] {:discovered 10}
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

