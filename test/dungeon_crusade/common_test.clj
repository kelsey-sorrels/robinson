(ns dungeon-crusade.common-test
  (:use clojure.test
        dungeon-crusade.common))

(deftest map-indexed-in-p-0
  (is (= (map-indexed-in-p {:a [5 4 3 2 1]} [:a] even? (fn [idx item] idx))
         {:a [5 0 3 1 1]})))

