(defproject robinson "0.0.1-SNAPSHOT"
  :description "robinson"
  :plugins      [[lein-marginalia "0.7.1"]
                 [lein-kibit "0.0.8"]]
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/core.async "0.1.303.0-886421-alpha"]
                 [org.clojure/core.memoize "0.5.6"]
                 [org.clojure/tools.reader "0.8.4"]
                 [org.clojure/core.typed "0.2.44"]
                 [org.clojure/math.combinatorics "0.0.7"]
                 [org.clojure/clojure-contrib "1.2.0"]
                 [org.clojars.vishk/algotools "0.1.0"]
                 [net.mikera/clisk "0.10.0"]
                 [com.palletops/thread-expr "1.3.0"]
                 [tinter "0.1.1-SNAPSHOT"]
                 [clj-tiny-astar "0.1.1-SNAPSHOT"]
                 [dorothy "0.0.3"]
                 [com.taoensso/timbre "3.2.1"]
                 ;;[aaron-santos/lanterna "3.0.0-SNAPSHOT"]]
                 [org.clojars.folcon/clojure-lanterna "0.9.5-SNAPSHOT"]]
  :main robinson.core
  :repl-init robinson.main
  ;:aot [robinson.common
  ;      robinson.update
  ;      robinson.render
  ;      robinson.swingterminal
  ;      robinson.npc]
  :jvm-opts [
             ;"-agentpath:/home/santos/bin/yjp-2014-build-14096/bin/linux-x86-64/libyjpagent.so"
             "-XX:+UseParNewGC"
             "-XX:+UseConcMarkSweepGC"
             "-XX:+CMSConcurrentMTEnabled"
             "-XX:MaxGCPauseMillis=20"
             "-Dsun.java2d.opengl=true"
             ;"-Dsun.java2d.trace=log"
             "-Dsun.java2d.opengl.fbobject=true"])
