(ns robinson.common-test
  (:use clojure.test
        robinson.common))

(deftest arg-when->test
  (are [in out] (= (-> in
                     (arg-when-> [x] (> x 3)
                       (* 3)))
                   out)
    0 0
    1 1
    2 2
    3 3
    4 12
    5 15))

(deftest arg-if->test
  (are [in out] (= (-> in
                     (arg-if-> [x] (> x 3)
                       (* 3)
                       (* 2)))
                   out)
    0 0
    1 2
    2 4
    3 6
    4 12
    5 15))

(deftest pos->xy-test
  (are [in out] (= (pos->xy in) out)
    {:x -1 :y -2} [-1 -2]
    {:x 0.0 :y 2} [0.0 2]))

(deftest xy->pos-test
  (are [in out] (= (apply xy->pos in) out)
    [-1 -2] {:x -1 :y -2}
    [0.0 2] {:x 0.0 :y 2}))

(deftest concat-in-test
  (are [m ks v out] (= (concat-in m ks v) out)
    {:a {:b {:c [:d :e :f]}}} [:a :b :c] [:g :h] {:a {:b {:c [:d :e :f :g :h]}}}))

(deftest reduce-in-test
  (are [m ks f v out] (= (reduce-in m ks f v) out)
    {:a {:b {:c [1 2 3]}}} [:a :b :c] + 0 {:a {:b {:c 6}}}
    {:a {:b {:c [1 2 3]}}} [:a :b :c] + 1 {:a {:b {:c 7}}}))

(deftest map-indexed-in-p-0
  (is (= (map-indexed-in-p {:a [5 4 3 2 1]} [:a] even? (fn [idx item] idx))
         {:a [5 0 3 1 1]})))

(deftest remove-in-0
  (let [state    {:world {:0 [[{:items [{:type :$} {:type :a} {:type :c}]}]]}}
        expected {:world {:0 [[{:items [{:type :a} {:type :c}]}]]}}]
    (is (= (remove-in state [:world :0 0 0 :items]
                            (fn [item] (= (item :type) :$)))
           expected))))


(deftest farther-than-test
  (are [p1 p2 d r] (= (farther-than? p1 p2 d) r)
  {:x 0 :y 0}  {:x 1 :y 1}  1 true
  {:x 0 :y 0}  {:x 1 :y 1}  1.1 true
  {:x 0 :y 0}  {:x 1 :y 1}  2 false
  {:x 1 :y 1}  {:x 0 :y 0}  1 true
  {:x 1 :y 1}  {:x 0 :y 0}  2 false
  {:x 1 :y 1}  {:x 2 :y 2}  1 true
  {:x -1 :y -1} {:x 1 :y 1} 2 true
  {:x -1 :y -1} {:x 1 :y 1} 3 false))

