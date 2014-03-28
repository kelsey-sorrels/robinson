(defproject dungeon-crusade "0.0.1-SNAPSHOT"
  :description "dungeon-crusade"
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [ns-tracker "0.2.2"]
                 ;[com.googlecode.lanterna/lanterna "2.1.5"]
                 [org.clojars.folcon/clojure-lanterna "0.9.5-SNAPSHOT"]]
  :main dungeon-crusade.core
  :repl-init dungeon-crusade.main
  :java-source-paths ["src/main/java/dungeoncrusade"])
