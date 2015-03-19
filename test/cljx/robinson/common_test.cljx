(ns robinson.common-test
  (:require [robinson.common :as rc]
            #+clj
            [clojure.test :as t
              :refer (is are deftest with-test run-tests testing)]
            #+cljs
            [cemerick.cljs.test :as t])
  (:require-macros [cemerick.cljs.test
                     :refer (is are deftest with-test run-tests testing test-var)]))
            

(deftest pos->xy-test
  (are [in out] (= (rc/pos->xy in) out)
    {:x -1 :y -2} [-1 -2]
    {:x 0.0 :y 2} [0.0 2]))

(deftest xy->pos-test
  (are [in out] (= (apply rc/xy->pos in) out)
    [-1 -2] {:x -1 :y -2}
    [0.0 2] {:x 0.0 :y 2}))

(deftest concat-in-test
  (are [m ks v out] (= (rc/concat-in m ks v) out)
    {:a {:b {:c [:d :e :f]}}} [:a :b :c] [:g :h] {:a {:b {:c [:d :e :f :g :h]}}}))

(deftest reduce-in-test
  (are [m ks f v out] (= (rc/reduce-in m ks f v) out)
    {:a {:b {:c [1 2 3]}}} [:a :b :c] + 0 {:a {:b {:c 6}}}
    {:a {:b {:c [1 2 3]}}} [:a :b :c] + 1 {:a {:b {:c 7}}}))

(deftest map-indexed-in-p-0
  (is (= (rc/map-indexed-in-p {:a [5 4 3 2 1]} [:a] even? (fn [idx item] idx))
         {:a [5 0 3 1 1]})))

(deftest remove-in-0
  (let [state    {:world {:0 [[{:items [{:type :$} {:type :a} {:type :c}]}]]}}
        expected {:world {:0 [[{:items [{:type :a} {:type :c}]}]]}}]
    (is (= (rc/remove-in state [:world :0 0 0 :items]
                               (fn [item] (= (item :type) :$)))
           expected))))


(deftest farther-than-test
  (are [p1 p2 d r] (= (rc/farther-than? p1 p2 d) r)
  {:x 0 :y 0}  {:x 1 :y 1}  1 true
  {:x 0 :y 0}  {:x 1 :y 1}  1.1 true
  {:x 0 :y 0}  {:x 1 :y 1}  2 false
  {:x 1 :y 1}  {:x 0 :y 0}  1 true
  {:x 1 :y 1}  {:x 0 :y 0}  2 false
  {:x 1 :y 1}  {:x 2 :y 2}  1 true
  {:x -1 :y -1} {:x 1 :y 1} 2 true
  {:x -1 :y -1} {:x 1 :y 1} 3 false))

