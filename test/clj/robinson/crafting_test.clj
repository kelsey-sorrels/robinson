(ns robinson.crafting-test
  (:require [robinson.crafting :as rcrafting]
            [robinson.itemgen :as ig]
            [clojure.test :as t
              :refer (is are deftest with-test run-tests testing)]))

(deftest test-has-prerequisites?
  (let [recipe (rcrafting/get-recipe :garrote)
        items [(assoc (ig/id->item :stick) :count 2)
               (assoc (ig/id->item :rope) :count 2)]]
  (are [recipe items expected] (= (rcrafting/has-prerequisites? (assoc-in {} [:world :player :inventory] items) recipe) expected)
    recipe items true)))

(deftest test-item-satisfies-requirement-clause?
  (are [recipe-id clause-idx item-id]
    (rcrafting/item-satisfies-requirement-clause?
      (ig/id->item item-id)
      (-> recipe-id
        rcrafting/get-recipe
        (get-in [:recipe/requirements clause-idx])))
    :throwing-hammer 1 :stick
    :throwing-hammer 2 :rock
    :throwing-hammer 3 :plant-fiber))

(deftest test-item-fails-to-satisfy-requirement-clause?
  (are [recipe-id clause-idx item-id]
    (not (rcrafting/item-satisfies-requirement-clause?
      (ig/id->item item-id)
      (-> recipe-id
        rcrafting/get-recipe
        (get-in [:recipe/requirements clause-idx]))))
    :throwing-hammer 2 :stick
    :throwing-hammer 3 :rock
    :throwing-hammer 1 :plant-fiber))

(deftest test-requirements-satisfied?
  (are [recipe-id item-ids] (rcrafting/requirements-satisfied?
                              (reduce (fn [recipe [idx item]]
                                                   (assoc-in recipe [:slots idx] item))
                                      (rcrafting/get-recipe recipe-id)
                                      (->> item-ids
                                        (map ig/id->item)
                                        (map-indexed vector))))
    :throwing-hammer [:stick :knife]))

(deftest test-valid-recipes
  (are [item-ids recipe-ids]
    (= (set (map rcrafting/get-recipe recipe-ids))
       (rcrafting/valid-recipes
         (map ig/id->item item-ids)
         (rcrafting/get-recipes-by-category :weapon)))
    ; empty
    [] []
    ; club
    [:stick]
    #{:club}
    ; throwing hammer
    [:rock :stick :plant-fiber]
    #{:throwing-hammer}
    ; spear/dagger
    [:knife
     :stick]
    #{:dagger
     :spear}
    ; throwing axe
    [:knife
     :stick
     :plant-fiber]
    #{:throwing-axe}))
