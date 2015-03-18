(ns robinson.viewport-test
  (:require [robinson.viewport :as rv]
            #+clj
            [clojure.test :as t
              :refer (is deftest with-test run-tests testing)]
            #+cljs
            [cemerick.cljs.test :as t])
 (:require-macros [cemerick.cljs.test
                    :refer (is deftest with-test run-tests testing test-var)]))


(def cells-in-viewport-state
 {:world {:viewport {:pos    {:x 2 :y 1}
                     :width  3
                     :height 3}
          :places {[0 0] [[:a :b :c]
                          [:g :h :i]
                          [:m :n :o]]
                   [1 0] [[:d :e :f]
                          [:j :k :l]
                          [:p :q :r]]
                   [0 1] [[:s :t :u]
                          [:y :z :0]
                          [:4 :5 :6]]
                   [1 1] [[:v :w :x]
                          [:1 :2 :3]
                          [:7 :8 :9]]}}})

(deftest cells-in-viewport-test-0
  (is (= (rv/cells-in-viewport
            cells-in-viewport-state)
         [[:i :j :k]
          [:o :p :q]
          [:u :v :w]])))

(deftest cells-in-viewport-test-1
  (is (= (rv/cells-in-viewport
          (assoc-in cells-in-viewport-state [:world :viewport :pos] {:x 0 :y 0}))
        [[:a :b :c]
         [:g :h :i]
         [:m :n :o]])))
