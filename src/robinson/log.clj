(ns robinson.log
  (:require [taoensso.timbre :as log]))

(def set-level! log/set-level!)

(def trace #^{:macro true} #'log/trace)
(def debug #^{:macro true} #'log/debug)
(def info  #^{:macro true} #'log/info)
(def warn  #^{:macro true} #'log/warn)
(def error #^{:macro true} #'log/error)
