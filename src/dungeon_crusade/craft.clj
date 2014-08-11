;; Functions for crafting items.
(ns dungeon-crusade.craft
  (:require [clojure.math.combinatorics :as combo]))

;; item types
;; :food
;; :ring
;; :scroll
;; :reeds
;;

(defn- initial-inventory []
  [{:type :reeds}])


(defn- get-craft-fns
  [inventory]
  

(defn craft
  [inventory input]
  (((get-craft-fns inventory) input)))

(defn -main
  [& args]
  (loop [inv (initial-inventory)]
    (let [input (keyword (read-line))]
      (recur (craft inv input)))))
