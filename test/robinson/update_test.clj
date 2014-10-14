(ns robinson.common-test
  (:use clojure.test
        robinson.common
        robinson.update))

(deftest more-log
  (let [state {:world {:time 10 :log []}}
        state (reduce append-log state ["first message" "second message"
                                        "thirst lonnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnng message"
                                        "fourth message"])
        state (init-log-scrolling state)]
    (is (current-state state) :more-log)
    (is (get-in state [:world :logs-viewed]) 1)
    (is (nth (get-in state [:world :log]) 1) "first message second message")
    (let [state (scroll-log state)]
      (is (current-state state) :more-log)
      (is (get-in state [:world :logs-viewed]) 2)
      (is (nth (get-in state [:world :log]) 2) "thirst lonnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnng message")
      (let [state (scroll-log state)]
        (is (current-state state) :normal)
        (is (get-in state [:world :logs-viewed]) 3)
        (is (nth (get-in state [:world :log]) 2) "fourth message")))))


