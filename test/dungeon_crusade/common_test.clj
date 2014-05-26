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
