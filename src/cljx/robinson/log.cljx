(ns robinson.log
  (:require 
            #+clj
            [taoensso.timbre :as log]
            #+cljs
            [shodan.console :as log :include-macros true]))


(def ^:private log-levels
  {:none -1
   :trace 0
   :debug 1
   :info  2
   :warn  3
   :error 4})

(def ^:private log-level (atom :info :validator (fn [level] (contains? log-levels level))))

(defn set-log-level! [level]
  (reset! log-level level))

(defn log-level-sufficient? [level]
  (>= (get log-levels level) (get log-levels @log-level)))

(defn trace [& more]
  (when (log-level-sufficient? :trace)
    #+clj
    (log/trace more)
    #+cljs
    (log/info (str more))))

(defn debug [& more]
  (when (log-level-sufficient? :debug)
    #+clj
    (log/debug more)
    #+cljs
    (log/debug (str more))))

(defn info [& more]
  (when (log-level-sufficient? :info)
    #+clj
    (log/info more)
    #+cljs
    (log/info (str more))))

(defn warn [& more]
  (when (log-level-sufficient? :warn)
    #+clj
    (log/warn more)
    #+cljs
    (log/warn (str more))))

(defn error [& more]
  (when (log-level-sufficient? :error)
    #+clj
    (log/error more)
    #+cljs
    (log/error (str more))))

