(defproject dungeon-crusade "0.0.1-SNAPSHOT"
  :description "dungeon-crusade"
  :plugins      [[lein-marginalia "0.7.1"]]
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [ns-tracker "0.2.2"]
                 [org.clojure/math.combinatorics "0.0.7"]
                 [org.clojure/clojure-contrib "1.2.0"]
                 [org.clojars.vishk/algotools "0.1.0"]
                 [clj-tiny-astar "0.1.0-SNAPSHOT"]
                 [org.clojars.folcon/clojure-lanterna "0.9.5-SNAPSHOT"]]
  :main dungeon-crusade.core
  :repl-init dungeon-crusade.main)
