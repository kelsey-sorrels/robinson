(ns robinson.apply-item-test
  (:require [robinson.crafting :as rcrafting]
            [robinson.itemgen :as rig]
            [robinson.apply-item :as rai]
            [clojure.test :as t
              :refer (is are deftest with-test run-tests testing)]))

(deftest test-has-prerequisites
  (let [recipe (rcrafting/get-recipe :garrote)
        items [(assoc (rig/id->item :stick) :count 2)
               (assoc (rig/id->item :rope) :count 2)]]
    (is (rcrafting/valid-recipe? items recipe))))

