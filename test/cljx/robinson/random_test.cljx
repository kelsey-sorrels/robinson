(ns robinson.random-test
  (:require [clojure.test :as test]
            [robinson.random :as rr]))

(test/deftest test-same-seed-produces-same-output
  (let [rnd0 (rr/create-random 42)
        rnd1 (rr/create-random 42)]
  (test/is (= (rr/next-int! rnd0)
              (rr/next-int! rnd1)))))

