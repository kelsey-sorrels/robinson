(ns dungeon-crusade.main)

;; Example setup and tick fns
(defn setup [] {:state "hello world from file 2!" :world [nil] :players [1]})

(defn tick [state]
  (do
    (println "new tick" state)
    (Thread/sleep 4000)
    state))

