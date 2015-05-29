(ns robinson.player-test
  (:require [robinson.player :as rp]
            #+clj
            [clojure.test :as t
              :refer (is deftest with-test run-tests testing)]
            #+cljs
            [cemerick.cljs.test :as t])
  #+cljs
  (:require-macros [cemerick.cljs.test
                     :refer (is deftest with-test run-tests testing test-var)]))
            

;; bind private fns
(def merge-items #'robinson.player/merge-items)

(deftest test-merge-items-0
  (let [item1 {:id :rock :count 2}
        item2 {:id :rock :count 2}]
    (is (= (rp/merge-items item1 item2)
           {:id :rock :count 4}))))

(deftest test-merge-items-1
  (let [item1 {:id :rock}
        item2 {:id :rock :count 2}]
    (is (= (rp/merge-items item1 item2)
           {:id :rock :count 3}))))

(deftest test-merge-items-0
  (let [item1 {:id :rock :count 2}
        item2 {:id :rock}]
    (is (= (rp/merge-items item1 item2)
           {:id :rock :count 3}))))

(deftest test-merge-items-0
  (let [item1 {:id :rock}
        item2 {:id :rock}]
    (is (= (rp/merge-items item1 item2)
           {:id :rock :count 2}))))

(deftest test-add-to-inventory-0
  (let [state {:world {:player {:inventory []}
                       :remaining-hotkeys [\a \b \c]}}
        items [{:id :rock :count 2} {:id :rock :count 2}]]
    (is (= (rp/add-to-inventory state items)
           {:world {:player {:inventory [{:id :rock :count 4 :hotkey \a}]}
                    :remaining-hotkeys [\b \c]
                    :log [{:text "null-a", :time nil, :color :gray} {:text "null-a", :time nil, :color :gray}]}}))))

(deftest test-add-to-inventory-1
  (let [state {:world {:player {:inventory []}
                       :remaining-hotkeys [\b \c]}}
        items [{:id :rock :count 2 :hotkey \a} {:id :rock :count 2}]]
    (is (= (rp/add-to-inventory state items)
           {:world {:player {:inventory [{:id :rock :count 4 :hotkey \a}]}
                    :remaining-hotkeys [\b \c]
                    :log [{:text "null-a", :time nil, :color :gray} {:text "null-a", :time nil, :color :gray}]}}))))

(deftest test-add-to-inventory-2
  (let [state {:world {:player {:inventory []}
                       :remaining-hotkeys [\b \c]}}
        items [{:id :rock :count 2} {:id :rock :count 2 :hotkey \a}]]
    (is (= (rp/add-to-inventory state items)
           {:world {:player {:inventory [{:id :rock :count 4 :hotkey \a}]}
                    :remaining-hotkeys [\b \c]
                    :log [{:text "null-a", :time nil, :color :gray} {:text "null-a", :time nil, :color :gray}]}}))))

(deftest test-add-to-inventory-3
  (let [state {:world {:player {:inventory [{:id :rock :count 2 :hotkey \a}]}
                       :remaining-hotkeys [\c]}}
        items [{:id :rock :count 2} {:id :rock :count 2 :hotkey \b}]]
    (is (= (rp/add-to-inventory state items)
           {:world {:player {:inventory [{:id :rock :count 6 :hotkey \a}]}
                    :remaining-hotkeys [\b \c]
                    :log [{:text "null-a", :time nil, :color :gray} {:text "null-a", :time nil, :color :gray}]}}))))

