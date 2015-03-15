(defproject robinson "0.0.1-SNAPSHOT"
  :description "robinson"
  :plugins      [[lein-ancient "0.6.0"]
                 [lein-autoreload "0.1.0"]
                 [lein-bikeshed "0.1.8"]
                 [lein-idefiles "0.2.1"]
                 [lein-marginalia "0.8.0"]
                 [lein-kibit "0.0.8"]
                 [lein-typed "0.3.5"]
                 [lein-cloverage "1.0.2"]
                 [lein-tarsier "0.10.0"]]
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/core.async "0.1.346.0-17112a-alpha"]
                 [org.clojure/core.memoize "0.5.6"]
                 [org.clojure/tools.reader "0.8.13"]
                 [org.clojure/core.typed "0.2.77"]
                 [org.clojure/data.generators "0.1.2"]
                 [org.clojure/math.combinatorics "0.0.8"]
                 [org.clojure/clojure-contrib "1.2.0"]
                 [org.clojars.vishk/algotools "0.1.0"]
                 [org.clojure/data.json "0.2.5"]
                 [org.clojure/clojurescript "0.0-3030"]
                 [clj-http "1.0.1"]
                 [cljs-webgl "0.1.5-SNAPSHOT"]
                 [com.palletops/thread-expr "1.3.0"]
                 [ns-tracker "0.2.2"]
                 [aaron-santos/tinter "0.1.1-SNAPSHOT"]
                 ;; TODO: point to fork
                 [aaron-santos/clj-tiny-astar "0.1.1-SNAPSHOT"]
                 [dorothy "0.0.6"]
                 [shodan "0.4.1"]
                 [com.taoensso/timbre "3.4.0"]
                 [com.taoensso/nippy "2.8.0"]
                 [rm-hull/monet "0.2.1"]]
  :main robinson.core
  :repl-init robinson.core
  :source-paths
  ["src/clj"
   "src/clj-macros"
   "target/generated-src/clj"
   "target/generated-src/cljs"]

  :test-paths
  ["test"]

  ;:auto-clean false

  :cljsbuild {:builds [{:source-paths ["src/clj-macros"
                                       "src/cljs"
                                       "target/generated-src/cljs"]
                        :compiler {:output-to "target/main.js"
                                   ;:output-dir "target"
                                   :foreign-libs [{
                                     :file "src/js/gl-matrix-min.js" :provides ["mat4","mat3","vec3"]}]
                                   ;:optimizations :simple
                                   ;:source-map true
                                   :pretty-print true}}]}
  :cljx
  {:builds [{:source-paths ["src/cljx"]
             :output-path "target/generated-src/clj"
             :rules :clj}
            {:source-paths ["src/cljx"]
             :output-path "target/generated-src/cljs"
             :rules :cljs}]}

  :prep-tasks [["cljx" "once"]]

  :profiles {
    :dev {:dependencies
          [#_[org.clojure/clojurescript "0.0-3030"]]

          :plugins
          [[com.keminglabs/cljx "0.6.0"]
           [com.cemerick/piggieback "0.1.5-SNAPSHOT"]
           [lein-cljsbuild "1.0.4"]]
          :repl-options {:nrepl-middleware [cljx.repl-middleware/wrap-cljx]}}}


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
             ;"-Xdebug"
             ;"-Xrunjdwp:transport=dt_socket,server=y,suspend=n,address=5005"
             "-XX:+UseParNewGC"
             "-XX:+UseConcMarkSweepGC"
             "-XX:+CMSConcurrentMTEnabled"
             "-XX:MaxGCPauseMillis=20"
             "-Dhttps.protocols=TLSv1"
             "-Dsun.java2d.opengl=true"
             ;"-Dsun.java2d.trace=log"
             "-Dsun.java2d.opengl.fbobject=true"])
