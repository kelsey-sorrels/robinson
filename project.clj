(defproject robinson "0.0.1-SNAPSHOT"
  :description "robinson"
  :plugins      [[lein-ancient "0.5.5"]
                 [lein-autoreload "0.1.0"]
                 [lein-bikeshed "0.1.8"]
                 [lein-idefiles "0.2.1"]
                 [lein-marginalia "0.7.1"]
                 [lein-kibit "0.0.8"]
                 [lein-cloverage "1.0.2"]
                 [lein-tarsier "0.10.0"]]
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/core.async "0.1.346.0-17112a-alpha"]
                 [org.clojure/core.memoize "0.5.6"]
                 [org.clojure/tools.reader "0.8.13"]
                 [org.clojure/core.typed "0.2.72"]
                 [org.clojure/data.generators "0.1.2"]
                 [org.clojure/math.combinatorics "0.0.8"]
                 [org.clojure/clojure-contrib "1.2.0"]
                 [org.clojars.vishk/algotools "0.1.0"]
                 [org.clojure/data.json "0.2.5"]
                 [clj-http "1.0.1"]
                 [net.mikera/clisk "0.10.0"]
                 [com.palletops/thread-expr "1.3.0"]
                 [ns-tracker "0.2.2"]
                 [tinter "0.1.1-SNAPSHOT"]
                 [clj-tiny-astar "0.1.1-SNAPSHOT"]
                 [dorothy "0.0.6"]
                 [com.taoensso/timbre "3.3.1"]
                 [com.taoensso/nippy "2.7.1"]
                 [org.clojars.folcon/clojure-lanterna "0.9.5"]]
  :main robinson.core
  :repl-init robinson.core
  ;:aot :all
;[;robinson.common
        ;robinson.update
        ;robinson.render
        ;robinson.swingterminal
        ;robinson.npc
   ;     robinson.core]
  :jvm-opts [
             ;"-agentpath:/home/santos/bin/yjp-2014-build-14096/bin/linux-x86-64/libyjpagent.so"
             "-XX:+UseParNewGC"
             "-XX:+UseConcMarkSweepGC"
             "-XX:+CMSConcurrentMTEnabled"
             "-XX:MaxGCPauseMillis=20"
             "-Dhttps.protocols=TLSv1"
             "-Dsun.java2d.opengl=true"
             ;"-Dsun.java2d.trace=log"
             "-Dsun.java2d.opengl.fbobject=true"])
