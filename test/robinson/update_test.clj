(ns robinson.update-test
  (:use clojure.test
        robinson.common
        robinson.world
        robinson.update))

(deftest more-log
  (let [state {:world {:time 10 :log []}}
        state (-> state
                (append-log "first message")
                (append-log "second message")
                (append-log "third message")
                (append-log "fourth lonnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnng message")
                (append-log "fifth message"))
        state (init-log-scrolling state)
        logs-viewed (get-in state [:world :logs-viewed])
        logs        (filter #(= (get % :time) (get-time state)) (get-in state [:world :log]))
        message     (nth logs (dec logs-viewed))]
    (is (current-state state) :more-log)
    (is logs-viewed 1)
    (is message "first message second message third message")
    (let [state (scroll-log state)
          logs-viewed (get-in state [:world :logs-viewed])
          logs        (filter #(= (get % :time) (get-time state)) (get-in state [:world :log]))
          message     (nth logs (dec logs-viewed))]
      (is (current-state state) :more-log)
      (is logs-viewed 2)
      (is message "fourth lonnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnng message")
      (let [state (scroll-log state)
            logs-viewed (get-in state [:world :logs-viewed])
            logs        (filter #(= (get % :time) (get-time state)) (get-in state [:world :log]))
            message     (nth logs (dec logs-viewed))]
        (is (current-state state) :normal)
        (is logs-viewed 3)
        (is message "fifth message")))))


