(ns robinson.random-test
  (:require [robinson.random :as rr]
            #+clj
            [clojure.test :as t
              :refer (is deftest with-test run-tests testing)]
            #+cljs
            [cemerick.cljs.test :as t])
  (:require-macros [cemerick.cljs.test
                    :refer (is deftest with-test run-tests testing test-var)]))

(t/deftest test-same-seed-produces-same-output
  (let [rnd0 (rr/create-random 42)
        rnd1 (rr/create-random 42)]
  (t/is false)
  (t/is (= (rr/next-int! rnd0)
         (rr/next-int! rnd1)))))

