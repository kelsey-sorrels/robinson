(defproject robinson "0.0.1-SNAPSHOT"
  :description "robinson"
  :plugins      [[lein-ancient "0.6.7"]
                 [lein-autoreload "0.1.0"]
                 [lein-bikeshed "0.1.8"]
                 [lein-idefiles "0.2.1"]
                 [lein-marginalia "0.8.0"]
                 [lein-kibit "0.0.8"]
                 [lein-typed "0.3.5"]
                 [lein-cloverage "1.0.2"]
                 [lein-tarsier "0.10.0"]
                 [com.cemerick/clojurescript.test "0.3.3"]]
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [org.clojure/core.async "0.1.346.0-17112a-alpha"]
                 [org.clojure/core.match "0.3.0-alpha4"]
                 [org.clojure/core.memoize "0.5.6"]
                 [org.clojure/tools.reader "0.8.13"]
                 [org.clojure/core.typed "0.2.77"]
                 [org.clojure/data.generators "0.1.2"]
                 [org.clojure/math.combinatorics "0.0.8"]
                 [org.clojure/clojure-contrib "1.2.0"]
                 [org.clojars.vishk/algotools "0.1.0"]
                 [org.clojure/data.json "0.2.5"]
                 ;[org.clojure/clojurescript "0.0-3030"]
                 [org.clojure/clojurescript "0.0-3269"]
                 [clj-http "1.0.1"]
                 [cljs-webgl "0.1.5-SNAPSHOT"]
                 [ns-tracker "0.2.2"]
                 [jamesmacaulay/cljs-promises "0.1.0"]
                 [tailrecursion/cljson "1.0.7"]
                 [servant "0.1.3"]
                 [aaron-santos/tinter "0.1.1-SNAPSHOT"]
                 [aaron-santos/clj-tiny-astar "0.1.1-SNAPSHOT"]
                 [dorothy "0.0.6"]
                 [com.taoensso/nippy "2.9.0"]
                 [com.taoensso/timbre "4.0.1"]
                 [alandipert/storage-atom "1.2.4"]
                 [rm-hull/monet "0.2.1"]]
  :main robinson.core
  :repl-init robinson.core
  :source-paths
  ["src/clj"
   "src/cljc"]

  :test-paths
  ["test/clj"
   "test/cljc"]

  ;:auto-clean false

  :cljsbuild {:builds {:dev {
                        :source-paths ["src/cljs"
                                       "src/cljc"]
                        :compiler {:output-to "target/main.js"
                                   ;:output-dir "target"
                                   :foreign-libs [{
                                     :file "src/js/gl-matrix-min.js" :provides ["mat4","mat3","vec3"]}]
;
                                   :language-in  :ecmascript5
                                   ;:optimizations :advanced
                                   :optimizations :simple
                                   ;:source-map true
                                   :pretty-print true}}
                      :test {
                        :source-paths ["src/cljs"
                                       "test/cljs"]
                        :compiler {:output-to "target/unit-test.js"
                                   :foreign-libs [{
                                     :file "src/js/gl-matrix-min.js" :provides ["mat4","mat3","vec3"]}]
                                   :optimizations :whitespace
                                   :pretty-print true}}}
              :test-commands {"unit-tests" [;"rhino" "-opt" "-1" :rhino-runner
                                            "phantomjs" :runner
                                            "target/unit-test.js"]}}
  :profiles {
    :dev {:dependencies
          [#_[org.clojure/clojurescript "0.0-3030"]]

          :plugins
          [[com.cemerick/piggieback "0.1.5-SNAPSHOT"]
           [lein-cljsbuild "1.0.6"]]}}


  ;:aot :all
;[;robinson.common
        ;robinson.update
        ;robinson.render
        ;robinson.swingterminal
        ;robinson.npc
   ;     robinson.core]
  :core.typed {:check [robinson.common robinson.crafting robinson.itemgen robinson.mapgen robinson.npc robinson.startgame robinson.update robinson.world
                       robinson.combat robinson.core robinson.describe robinson.endgame robinson.lineofsight robinson.main robinson.monstergen
                       robinson.player robinson.render robinson.swingterminal robinson.viewport robinson.worldgen]}
  :repl-options {:timeout 920000}
  :jvm-opts [
             ;"-agentpath:/home/santos/bin/yjp-2014-build-14096/bin/linux-x86-64/libyjpagent.so"
             "-Xdebug"
             ;"-Xrunjdwp:transport=dt_socket,server=y,suspend=n,address=5005"
             "-XX:+UnlockCommercialFeatures"
             ;"-XX:+FlightRecorder"
             "-XX:-OmitStackTraceInFastThrow"
             "-XX:+UseParNewGC"
             "-XX:+UseConcMarkSweepGC"
             "-XX:+CMSConcurrentMTEnabled"
             "-XX:MaxGCPauseMillis=20"
             "-Dhttps.protocols=TLSv1"
             "-Dsun.java2d.opengl=true"
             ;"-Dsun.java2d.trace=log"
             "-Dsun.java2d.opengl.fbobject=true"])
