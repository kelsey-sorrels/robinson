(ns robinson.apply-item-test
  (:require [robinson.crafting :as rcrafting]
            [robinson.itemgen :as rig]
            [robinson.apply-item :as rai]
            [clojure.test :as t
              :refer (is are deftest with-test run-tests testing)]))

(deftest test-apply-knife
  (let [recipe (rcrafting/get-recipe :garrote)
        items [(assoc (rig/id->item :stick) :count 2)
               (assoc (rig/id->item :rope) :count 2)]]
  (are [recipe items expected] (= (rcrafting/has-prerequisites? (assoc-in {} [:world :player :inventory] items) recipe) expected)
    recipe items true)))

