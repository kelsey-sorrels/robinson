(ns robinson.log
  (:require [robinson.world :as rw]
            [taoensso.timbre :as log]))

(def set-level! log/set-level!)

(def trace #^{:macro true} #'log/trace)
(def debug #^{:macro true} #'log/debug)
(def info  #^{:macro true} #'log/info)
(def warn  #^{:macro true} #'log/warn)
(def error #^{:macro true} #'log/error)

(defn current-logs [state]
  (let [current-time (rw/get-time state)]
    (filter #(<= current-time (get % :time)) (get-in state [:world :log]))))

(defn last-log
  [state]
  (last (get-in state [:world :log])))




